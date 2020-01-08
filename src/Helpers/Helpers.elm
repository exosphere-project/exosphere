module Helpers.Helpers exposing
    ( authUrlWithPortAndVersion
    , checkFloatingIpState
    , flavorLookup
    , getExternalNetwork
    , getProjectId
    , getServerExouserPassword
    , getServerFloatingIp
    , getServerUiStatus
    , getServerUiStatusColor
    , getServerUiStatusStr
    , getServersWithVolAttached
    , getVolsAttachedToServer
    , hostnameFromUrl
    , imageLookup
    , iso8601StringToPosix
    , jetstreamToOpenstackCreds
    , modelUpdateProject
    , modelUpdateUnscopedProvider
    , newServerNetworkOptions
    , processError
    , processOpenRc
    , projectLookup
    , projectUpdateServer
    , projectUpdateServers
    , providerLookup
    , renderUserDataTemplate
    , serverLookup
    , serviceCatalogToEndpoints
    , sortedFlavors
    , stringIsUuidOrDefault
    , titleFromHostname
    , toastConfig
    , volumeIsAttachedToServer
    )

import Color
import Debug
import Error.Error
import Framework.Color
import Html
import Html.Attributes
import ISO8601
import Maybe.Extra
import OpenStack.Types as OSTypes
import Regex
import RemoteData
import Time
import Toasty
import Toasty.Defaults
import Types.HelperTypes as HelperTypes
import Types.Types
    exposing
        ( CockpitLoginStatus(..)
        , CreateServerRequest
        , Endpoints
        , FloatingIpState(..)
        , JetstreamCreds
        , JetstreamProvider(..)
        , Model
        , Msg(..)
        , NewServerNetworkOptions(..)
        , Project
        , ProjectIdentifier
        , Server
        , ServerUiStatus(..)
        , Toast
        , UnscopedProvider
        )
import Url


alwaysRegex : String -> Regex.Regex
alwaysRegex regexStr =
    Regex.fromString regexStr |> Maybe.withDefault Regex.never


toastConfig : Toasty.Config Msg
toastConfig =
    let
        containerAttrs : List (Html.Attribute msg)
        containerAttrs =
            [ Html.Attributes.style "position" "fixed"
            , Html.Attributes.style "top" "60"
            , Html.Attributes.style "right" "0"
            , Html.Attributes.style "width" "100%"
            , Html.Attributes.style "max-width" "300px"
            , Html.Attributes.style "list-style-type" "none"
            , Html.Attributes.style "padding" "0"
            , Html.Attributes.style "margin" "0"
            ]
    in
    Toasty.Defaults.config
        |> Toasty.delay 60000
        |> Toasty.containerAttrs containerAttrs


processError : Model -> a -> ( Model, Cmd Msg )
processError model error =
    let
        errorString =
            Debug.toString error

        newMsgs =
            errorString :: model.messages

        newModel =
            { model | messages = newMsgs }

        toast =
            Toast (Error.Error.ErrorContext "foobar" Error.Error.ErrorCrit Nothing) errorString
    in
    Toasty.addToastIfUnique toastConfig ToastyMsg toast ( newModel, Cmd.none )


stringIsUuidOrDefault : String -> Bool
stringIsUuidOrDefault str =
    -- We accept some login fields from user (e.g. Keystone domains) that could be a name or a UUID.
    -- Further, OpenStack treats "default" as a special case that can be passed in UUID fields.
    -- This function helps functions like Rest.requestAuthToken specify the right JSON field (name or ID).
    let
        stringIsUuid =
            let
                strNoHyphens =
                    String.filter (\c -> c /= '-') str

                isValidHex : Char -> Bool
                isValidHex c =
                    String.any (\h -> c == h) "0123456789abcdef"

                isValidLength =
                    String.length strNoHyphens == 32
            in
            String.all isValidHex strNoHyphens && isValidLength

        stringIsDefault =
            String.toLower str == "default"
    in
    stringIsUuid || stringIsDefault


processOpenRc : OSTypes.OpenstackLogin -> String -> OSTypes.OpenstackLogin
processOpenRc existingCreds openRc =
    let
        regexes =
            { authUrl = alwaysRegex "export OS_AUTH_URL=\"?([^\"\n]*)\"?"
            , projectDomain = alwaysRegex "export OS_PROJECT_DOMAIN(?:_NAME|_ID)=\"?([^\"\n]*)\"?"
            , projectName = alwaysRegex "export OS_PROJECT_NAME=\"?([^\"\n]*)\"?"
            , userDomain = alwaysRegex "export OS_USER_DOMAIN(?:_NAME|_ID)=\"?([^\"\n]*)\"?"
            , username = alwaysRegex "export OS_USERNAME=\"?([^\"\n]*)\"?"
            , password = alwaysRegex "export OS_PASSWORD=\"(.*)\""
            }

        getMatch text regex =
            Regex.findAtMost 1 regex text
                |> List.head
                |> Maybe.map (\x -> x.submatches)
                |> Maybe.andThen List.head
                |> Maybe.Extra.join

        newField regex oldField =
            getMatch openRc regex
                |> Maybe.withDefault oldField
    in
    OSTypes.OpenstackLogin
        (newField regexes.authUrl existingCreds.authUrl)
        (newField regexes.projectDomain existingCreds.projectDomain)
        (newField regexes.projectName existingCreds.projectName)
        (newField regexes.userDomain existingCreds.userDomain)
        (newField regexes.username existingCreds.username)
        (newField regexes.password existingCreds.password)


authUrlWithPortAndVersion : HelperTypes.Url -> HelperTypes.Url
authUrlWithPortAndVersion authUrlStr =
    -- If user does not provide a port and path in OpenStack auth URL then we guess port 5000 and path "/v3"
    let
        authUrlStrWithProto =
            -- If user doesn't provide a protocol then we add one so that the URL will actually parse
            if String.startsWith "http://" authUrlStr || String.startsWith "https://" authUrlStr then
                authUrlStr

            else
                "https://" ++ authUrlStr

        maybeAuthUrl =
            Url.fromString authUrlStrWithProto
    in
    case maybeAuthUrl of
        Nothing ->
            -- We can't parse this URL so we just return it unmodified
            authUrlStr

        Just authUrl ->
            let
                port_ =
                    case authUrl.port_ of
                        Just _ ->
                            authUrl.port_

                        Nothing ->
                            Just 5000

                path =
                    case authUrl.path of
                        "" ->
                            "/v3"

                        "/" ->
                            "/v3"

                        _ ->
                            authUrl.path
            in
            Url.toString <|
                Url.Url
                    authUrl.protocol
                    authUrl.host
                    port_
                    path
                    -- Query and fragment may not be needed / accepted by OpenStack
                    authUrl.query
                    authUrl.fragment


hostnameFromUrl : HelperTypes.Url -> String
hostnameFromUrl urlStr =
    let
        maybeUrl =
            Url.fromString urlStr
    in
    case maybeUrl of
        Just url ->
            url.host

        Nothing ->
            "placeholder-url-unparseable"


titleFromHostname : String -> String
titleFromHostname hostname =
    let
        r =
            alwaysRegex "^(.*?)\\..*"

        matches =
            Regex.findAtMost 1 r hostname

        maybeMaybeTitle =
            matches
                |> List.head
                |> Maybe.map (\x -> x.submatches)
                |> Maybe.andThen List.head
    in
    case maybeMaybeTitle of
        Just (Just title) ->
            title

        _ ->
            hostname


iso8601StringToPosix : String -> Result String Time.Posix
iso8601StringToPosix str =
    ISO8601.fromString str
        |> Result.map ISO8601.toPosix


serviceCatalogToEndpoints : OSTypes.ServiceCatalog -> Endpoints
serviceCatalogToEndpoints catalog =
    Endpoints
        (getServicePublicUrl "cinderv3" catalog)
        (getServicePublicUrl "glance" catalog)
        (getServicePublicUrl "keystone" catalog)
        (getServicePublicUrl "nova" catalog)
        (getServicePublicUrl "neutron" catalog)


getServicePublicUrl : String -> OSTypes.ServiceCatalog -> HelperTypes.Url
getServicePublicUrl serviceName catalog =
    getServiceFromCatalog serviceName catalog
        |> Maybe.andThen getPublicEndpointFromService
        |> Maybe.map .url
        |> Maybe.withDefault ""


getServiceFromCatalog : String -> OSTypes.ServiceCatalog -> Maybe OSTypes.Service
getServiceFromCatalog serviceName catalog =
    List.filter (\s -> s.name == serviceName) catalog
        |> List.head


getPublicEndpointFromService : OSTypes.Service -> Maybe OSTypes.Endpoint
getPublicEndpointFromService service =
    List.filter (\e -> e.interface == OSTypes.Public) service.endpoints
        |> List.head


getExternalNetwork : Project -> Maybe OSTypes.Network
getExternalNetwork project =
    List.filter (\n -> n.isExternal) project.networks |> List.head


checkFloatingIpState : OSTypes.ServerDetails -> FloatingIpState -> FloatingIpState
checkFloatingIpState serverDetails floatingIpState =
    let
        hasFixedIp =
            List.filter (\a -> a.openstackType == OSTypes.IpAddressFixed) serverDetails.ipAddresses
                |> List.isEmpty
                |> not

        hasFloatingIp =
            List.filter (\a -> a.openstackType == OSTypes.IpAddressFloating) serverDetails.ipAddresses
                |> List.isEmpty
                |> not

        isActive =
            serverDetails.openstackStatus == OSTypes.ServerActive
    in
    case floatingIpState of
        RequestedWaiting ->
            if hasFloatingIp then
                Success

            else
                RequestedWaiting

        Failed ->
            Failed

        _ ->
            if hasFloatingIp then
                Success

            else if hasFixedIp && isActive then
                Requestable

            else
                NotRequestable


serverLookup : Project -> OSTypes.ServerUuid -> Maybe Server
serverLookup project serverUuid =
    List.filter (\s -> s.osProps.uuid == serverUuid) (RemoteData.withDefault [] project.servers) |> List.head


projectLookup : Model -> ProjectIdentifier -> Maybe Project
projectLookup model projectIdentifier =
    model.projects
        |> List.filter (\p -> p.auth.project.name == projectIdentifier.name)
        |> List.filter (\p -> p.endpoints.keystone == projectIdentifier.authUrl)
        |> List.head


getProjectId : Project -> ProjectIdentifier
getProjectId project =
    ProjectIdentifier project.auth.project.name project.endpoints.keystone


flavorLookup : Project -> OSTypes.FlavorUuid -> Maybe OSTypes.Flavor
flavorLookup project flavorUuid =
    List.filter
        (\f -> f.uuid == flavorUuid)
        project.flavors
        |> List.head


imageLookup : Project -> OSTypes.ImageUuid -> Maybe OSTypes.Image
imageLookup project imageUuid =
    List.filter
        (\i -> i.uuid == imageUuid)
        project.images
        |> List.head


providerLookup : Model -> OSTypes.KeystoneUrl -> Maybe UnscopedProvider
providerLookup model keystoneUrl =
    List.filter
        (\uP -> uP.authUrl == keystoneUrl)
        model.unscopedProviders
        |> List.head


modelUpdateProject : Model -> Project -> Model
modelUpdateProject model newProject =
    let
        otherProjects =
            List.filter (\p -> getProjectId p /= getProjectId newProject) model.projects

        newProjects =
            newProject :: otherProjects

        newProjectsSorted =
            newProjects
                |> List.sortBy (\p -> p.auth.project.name)
                |> List.sortBy (\p -> hostnameFromUrl p.endpoints.keystone)
    in
    { model | projects = newProjectsSorted }


projectUpdateServer : Project -> Server -> Project
projectUpdateServer project server =
    let
        otherServers =
            List.filter
                (\s -> s.osProps.uuid /= server.osProps.uuid)
                (RemoteData.withDefault [] project.servers)

        newServers =
            server :: otherServers

        newServersSorted =
            List.sortBy (\s -> s.osProps.name) newServers
    in
    { project | servers = RemoteData.Success newServersSorted }


projectUpdateServers : Project -> List Server -> Project
projectUpdateServers project servers =
    List.foldl (\s p -> projectUpdateServer p s) project servers


modelUpdateUnscopedProvider : Model -> UnscopedProvider -> Model
modelUpdateUnscopedProvider model newProvider =
    let
        otherProviders =
            List.filter
                (\p -> p.authUrl /= newProvider.authUrl)
                model.unscopedProviders

        newProviders =
            newProvider :: otherProviders

        newProvidersSorted =
            List.sortBy (\p -> p.authUrl) newProviders
    in
    { model | unscopedProviders = newProvidersSorted }


getServerFloatingIp : List OSTypes.IpAddress -> Maybe String
getServerFloatingIp ipAddresses =
    let
        isFloating ipAddress =
            ipAddress.openstackType == OSTypes.IpAddressFloating
    in
    List.filter isFloating ipAddresses
        |> List.head
        |> Maybe.map .address


getServerExouserPassword : OSTypes.ServerDetails -> Maybe String
getServerExouserPassword serverDetails =
    List.filter (\i -> i.key == "exouserPassword") serverDetails.metadata
        |> List.head
        |> Maybe.map .value


getServerUiStatus : Server -> ServerUiStatus
getServerUiStatus server =
    case server.osProps.details.openstackStatus of
        OSTypes.ServerActive ->
            case server.exoProps.cockpitStatus of
                NotChecked ->
                    ServerUiStatusPartiallyActive

                CheckedNotReady ->
                    ServerUiStatusPartiallyActive

                Ready ->
                    ServerUiStatusReady

        OSTypes.ServerPaused ->
            ServerUiStatusPaused

        OSTypes.ServerReboot ->
            ServerUiStatusReboot

        OSTypes.ServerSuspended ->
            ServerUiStatusSuspended

        OSTypes.ServerShutoff ->
            ServerUiStatusShutoff

        OSTypes.ServerStopped ->
            ServerUiStatusStopped

        OSTypes.ServerSoftDeleted ->
            ServerUiStatusSoftDeleted

        OSTypes.ServerError ->
            ServerUiStatusError

        OSTypes.ServerBuilding ->
            ServerUiStatusBuilding

        OSTypes.ServerRescued ->
            ServerUiStatusRescued

        OSTypes.ServerShelved ->
            ServerUiStatusShelved

        OSTypes.ServerShelvedOffloaded ->
            ServerUiStatusShelved


getServerUiStatusStr : ServerUiStatus -> String
getServerUiStatusStr status =
    case status of
        ServerUiStatusUnknown ->
            "Unknown"

        ServerUiStatusBuilding ->
            "Building"

        ServerUiStatusPartiallyActive ->
            "Partially Active"

        ServerUiStatusReady ->
            "Ready"

        ServerUiStatusPaused ->
            "Paused"

        ServerUiStatusReboot ->
            "Reboot"

        ServerUiStatusSuspended ->
            "Suspended"

        ServerUiStatusShutoff ->
            "Shut off"

        ServerUiStatusStopped ->
            "Stopped"

        ServerUiStatusSoftDeleted ->
            "Soft-deleted"

        ServerUiStatusError ->
            "Error"

        ServerUiStatusRescued ->
            "Rescued"

        ServerUiStatusShelved ->
            "Shelved"


getServerUiStatusColor : ServerUiStatus -> Color.Color
getServerUiStatusColor status =
    case status of
        ServerUiStatusUnknown ->
            Framework.Color.grey

        ServerUiStatusBuilding ->
            Framework.Color.yellow

        ServerUiStatusPartiallyActive ->
            Framework.Color.yellow

        ServerUiStatusReady ->
            Framework.Color.green

        ServerUiStatusReboot ->
            Framework.Color.yellow

        ServerUiStatusPaused ->
            Framework.Color.grey

        ServerUiStatusSuspended ->
            Framework.Color.grey

        ServerUiStatusShutoff ->
            Framework.Color.grey

        ServerUiStatusStopped ->
            Framework.Color.grey

        ServerUiStatusSoftDeleted ->
            Framework.Color.grey

        ServerUiStatusError ->
            Framework.Color.red

        ServerUiStatusRescued ->
            Framework.Color.red

        ServerUiStatusShelved ->
            Framework.Color.grey


sortedFlavors : List OSTypes.Flavor -> List OSTypes.Flavor
sortedFlavors flavors =
    flavors
        |> List.sortBy .disk_ephemeral
        |> List.sortBy .disk_root
        |> List.sortBy .ram_mb
        |> List.sortBy .vcpu


renderUserDataTemplate : Project -> CreateServerRequest -> String
renderUserDataTemplate project createServerRequest =
    {- If user has selected an SSH public key, add it to authorized_keys for exouser -}
    let
        getPublicKeyFromKeypairName : String -> Maybe String
        getPublicKeyFromKeypairName keypairName =
            project.keypairs
                |> List.filter (\kp -> kp.name == keypairName)
                |> List.head
                |> Maybe.map .publicKey

        generateYamlFromPublicKey : String -> String
        generateYamlFromPublicKey selectedPublicKey =
            "ssh-authorized-keys:\n      - " ++ selectedPublicKey

        renderUserData : String -> String
        renderUserData authorizedKeyYaml =
            String.replace "{ssh-authorized-keys}\n" authorizedKeyYaml createServerRequest.userData
    in
    createServerRequest.keypairName
        |> Maybe.andThen getPublicKeyFromKeypairName
        |> Maybe.map generateYamlFromPublicKey
        |> Maybe.withDefault ""
        |> renderUserData


newServerNetworkOptions : Project -> NewServerNetworkOptions
newServerNetworkOptions project =
    {- When creating a new server, make a reasonable choice of project network, if we can. -}
    let
        -- First, filter on networks that are status ACTIVE, adminStateUp, and not external
        projectNets =
            project.networks
                |> List.filter (\n -> n.status == "ACTIVE")
                |> List.filter (\n -> n.adminStateUp == True)
                |> List.filter (\n -> n.isExternal == False)

        maybeAutoAllocatedNet =
            projectNets
                |> List.filter (\n -> n.name == "auto_allocated_network")
                |> List.head

        maybeProjectNameNet =
            projectNets
                |> List.filter (\n -> String.contains project.auth.project.name n.name)
                |> List.head
    in
    case projectNets of
        -- If there is no suitable network then we specify "auto" and hope that OpenStack will create one for us
        [] ->
            NoNetsAutoAllocate

        firstNet :: otherNets ->
            if List.isEmpty otherNets then
                -- If there is only one network then we pick that one
                OneNet firstNet

            else
                -- If there are multiple networks then we let user choose and try to guess a good default
                let
                    ( guessNet, goodGuess ) =
                        case maybeAutoAllocatedNet of
                            Just n ->
                                ( n, True )

                            Nothing ->
                                case maybeProjectNameNet of
                                    Just n ->
                                        ( n, True )

                                    Nothing ->
                                        ( firstNet, False )
                in
                MultipleNetsWithGuess projectNets guessNet goodGuess



{- Future todo come up with some rational scheme for whether these functions should accept the full resource types (e.g. Volume) or just an identifier (e.g. VolumeUuid) -}


getVolsAttachedToServer : Project -> Server -> List OSTypes.Volume
getVolsAttachedToServer project server =
    project.volumes
        |> RemoteData.withDefault []
        |> List.filter (\v -> List.member v.uuid server.osProps.details.volumesAttached)


volumeIsAttachedToServer : OSTypes.VolumeUuid -> Server -> Bool
volumeIsAttachedToServer volumeUuid server =
    server.osProps.details.volumesAttached
        |> List.filter (\v -> v == volumeUuid)
        |> List.isEmpty
        |> not


getServersWithVolAttached : Project -> OSTypes.Volume -> List OSTypes.ServerUuid
getServersWithVolAttached _ volume =
    volume.attachments |> List.map .serverUuid


jetstreamToOpenstackCreds : JetstreamCreds -> List OSTypes.OpenstackLogin
jetstreamToOpenstackCreds jetstreamCreds =
    let
        authUrlBases =
            case jetstreamCreds.jetstreamProviderChoice of
                {- TODO should we hard-code these elsewhere? -}
                IUCloud ->
                    [ "iu.jetstream-cloud.org" ]

                TACCCloud ->
                    [ "tacc.jetstream-cloud.org" ]

                BothJetstreamClouds ->
                    [ "iu.jetstream-cloud.org"
                    , "tacc.jetstream-cloud.org"
                    ]

        authUrls =
            List.map
                (\baseUrl -> "https://" ++ baseUrl ++ ":5000/v3/auth/tokens")
                authUrlBases
    in
    List.map
        (\authUrl ->
            OSTypes.OpenstackLogin
                authUrl
                "tacc"
                jetstreamCreds.jetstreamProjectName
                "tacc"
                jetstreamCreds.taccUsername
                jetstreamCreds.taccPassword
        )
        authUrls
