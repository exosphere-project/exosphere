module Helpers.Helpers exposing
    ( authUrlWithPortAndVersion
    , buildProxyUrl
    , checkFloatingIpState
    , computeQuotaFlavorAvailServers
    , flavorLookup
    , getBootVol
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
    , isBootVol
    , jetstreamToOpenstackCreds
    , modelUpdateProject
    , modelUpdateUnscopedProvider
    , newGuacMetadata
    , newServerMetadata
    , newServerNetworkOptions
    , overallQuotaAvailServers
    , processOpenRc
    , processStringError
    , processSynchronousApiError
    , projectDeleteServer
    , projectLookup
    , projectSetServerLoading
    , projectSetServersLoading
    , projectUpdateServer
    , providerLookup
    , renderUserDataTemplate
    , serverFromThisExoClient
    , serverLessThanThisOld
    , serverLookup
    , serverNeedsFrequentPoll
    , serverOrigin
    , serviceCatalogToEndpoints
    , sortedFlavors
    , stringIsUuidOrDefault
    , titleFromHostname
    , toastConfig
    , volDeviceToMountpoint
    , volumeIsAttachedToServer
    , volumeLookup
    , volumeQuotaAvail
    )

import Debug
import Dict
import Element
import Helpers.Error exposing (ErrorContext, ErrorLevel(..), HttpErrorWithBody)
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.Time exposing (iso8601StringToPosix)
import Html
import Html.Attributes
import Http
import Json.Decode as Decode
import Json.Encode
import Maybe.Extra
import OpenStack.Error as OSError
import OpenStack.Types as OSTypes
import Regex
import RemoteData
import ServerDeploy
import Task
import Time
import Toasty
import Toasty.Defaults
import Types.Guacamole as GuacTypes
import Types.HelperTypes as HelperTypes
import Types.Types
    exposing
        ( CockpitLoginStatus(..)
        , Endpoints
        , ExoServerVersion
        , ExoSetupStatus(..)
        , FloatingIpState(..)
        , JetstreamCreds
        , JetstreamProvider(..)
        , LogMessage
        , Model
        , Msg(..)
        , NewServerNetworkOptions(..)
        , Project
        , ProjectIdentifier
        , Server
        , ServerFromExoProps
        , ServerOrigin(..)
        , ServerUiStatus(..)
        , Toast
        , UnscopedProvider
        , UserAppProxyHostname
        )
import UUID
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


processStringError : Model -> ErrorContext -> String -> ( Model, Cmd Msg )
processStringError model errorContext error =
    let
        logMessageProto =
            LogMessage
                error
                errorContext

        toast =
            Toast
                errorContext
                error

        cmd =
            Task.perform
                (\posix -> NewLogMessage (logMessageProto posix))
                Time.now
    in
    Toasty.addToastIfUnique toastConfig ToastyMsg toast ( model, cmd )


processSynchronousApiError : Model -> ErrorContext -> HttpErrorWithBody -> ( Model, Cmd Msg )
processSynchronousApiError model errorContext httpError =
    let
        apiErrorDecodeResult =
            Decode.decodeString
                OSError.decodeSynchronousErrorJson
                httpError.body

        formattedError =
            case httpError.error of
                Http.BadStatus code ->
                    case apiErrorDecodeResult of
                        Ok syncApiError ->
                            syncApiError.message
                                ++ " (response code: "
                                ++ String.fromInt syncApiError.code
                                ++ ")"

                        Err _ ->
                            httpError.body
                                ++ " (response code: "
                                ++ String.fromInt code
                                ++ ")"

                _ ->
                    Debug.toString httpError
    in
    processStringError model errorContext formattedError


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
            str == "default"
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


serviceCatalogToEndpoints : OSTypes.ServiceCatalog -> Result String Endpoints
serviceCatalogToEndpoints catalog =
    let
        novaUrlWithMicroversionSupport : String -> String
        novaUrlWithMicroversionSupport url =
            -- Future optimization, use a real URL parser
            if String.contains "/v2/" url then
                String.replace "/v2/" "/v2.1/" url

            else if String.contains "/v2.0/" url then
                String.replace "/v2.0/" "/v2.1/" url

            else
                url

        maybeEndpointsDict : Dict.Dict String (Maybe String)
        maybeEndpointsDict =
            Dict.fromList
                [ ( "cinder", getServicePublicUrl "volumev3" catalog )
                , ( "glance", getServicePublicUrl "image" catalog )
                , ( "keystone", getServicePublicUrl "identity" catalog )
                , ( "nova", getServicePublicUrl "compute" catalog |> Maybe.map novaUrlWithMicroversionSupport )
                , ( "neutron", getServicePublicUrl "network" catalog )
                ]
    in
    -- I am not super proud of this factoring
    case
        [ "cinder", "glance", "keystone", "nova", "neutron" ]
            |> List.map (\k -> Dict.get k maybeEndpointsDict)
            |> List.map (Maybe.withDefault Nothing)
    of
        [ Just cinderUrl, Just glanceUrl, Just keystoneUrl, Just novaUrl, Just neutronUrl ] ->
            Ok <| Endpoints cinderUrl glanceUrl keystoneUrl novaUrl neutronUrl

        _ ->
            let
                unfoundServices =
                    Dict.filter (\_ v -> v == Nothing) maybeEndpointsDict
                        |> Dict.keys
            in
            Err
                ("Could not locate URL(s) in service catalog for the following service(s):"
                    ++ Debug.toString unfoundServices
                )


getServicePublicUrl : String -> OSTypes.ServiceCatalog -> Maybe HelperTypes.Url
getServicePublicUrl serviceType catalog =
    getServiceFromCatalog serviceType catalog
        |> Maybe.andThen getPublicEndpointFromService
        |> Maybe.map .url


getServiceFromCatalog : String -> OSTypes.ServiceCatalog -> Maybe OSTypes.Service
getServiceFromCatalog serviceType catalog =
    List.filter (\s -> s.type_ == serviceType) catalog
        |> List.head


getPublicEndpointFromService : OSTypes.Service -> Maybe OSTypes.Endpoint
getPublicEndpointFromService service =
    List.filter (\e -> e.interface == OSTypes.Public) service.endpoints
        |> List.head


getExternalNetwork : Project -> Maybe OSTypes.Network
getExternalNetwork project =
    case project.networks.data of
        RDPP.DoHave networks _ ->
            List.filter (\n -> n.isExternal) networks |> List.head

        RDPP.DontHave ->
            Nothing


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

        Success ->
            Success

        _ ->
            if hasFloatingIp then
                Success

            else if hasFixedIp && isActive then
                Requestable

            else
                NotRequestable


serverLookup : Project -> OSTypes.ServerUuid -> Maybe Server
serverLookup project serverUuid =
    List.filter (\s -> s.osProps.uuid == serverUuid) (RDPP.withDefault [] project.servers) |> List.head


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


volumeLookup : Project -> OSTypes.VolumeUuid -> Maybe OSTypes.Volume
volumeLookup project volumeUuid =
    List.filter
        (\v -> v.uuid == volumeUuid)
        (RemoteData.withDefault [] project.volumes)
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
    case project.servers.data of
        RDPP.DontHave ->
            -- We don't do anything if we don't already have servers. Is this a silent failure that should be
            -- handled differently?
            project

        RDPP.DoHave servers recTime ->
            let
                otherServers =
                    List.filter
                        (\s -> s.osProps.uuid /= server.osProps.uuid)
                        servers

                newServers =
                    server :: otherServers

                newServersSorted =
                    List.sortBy (\s -> s.osProps.name) newServers

                oldServersRDPP =
                    project.servers

                newServersRDPP =
                    -- Should we update received time when we update a server? Thinking probably not given how this
                    -- function is actually used. We're generally updating exoProps, not osProps.
                    { oldServersRDPP | data = RDPP.DoHave newServersSorted recTime }
            in
            { project | servers = newServersRDPP }


projectDeleteServer : Project -> OSTypes.ServerUuid -> Project
projectDeleteServer project serverUuid =
    case project.servers.data of
        RDPP.DontHave ->
            project

        RDPP.DoHave servers recTime ->
            let
                otherServers =
                    List.filter
                        (\s -> s.osProps.uuid /= serverUuid)
                        servers

                oldServersRDPP =
                    project.servers

                newServersRDPP =
                    -- Should we update received time when we update a server? Thinking probably not given how this
                    -- function is actually used. We're generally updating exoProps, not osProps.
                    { oldServersRDPP | data = RDPP.DoHave otherServers recTime }
            in
            { project | servers = newServersRDPP }


projectSetServersLoading : Time.Posix -> Project -> Project
projectSetServersLoading time project =
    { project | servers = RDPP.setLoading project.servers time }


projectSetServerLoading : Project -> OSTypes.ServerUuid -> Project
projectSetServerLoading project serverUuid =
    case serverLookup project serverUuid of
        Nothing ->
            -- We can't do anything lol
            project

        Just server ->
            let
                oldExoProps =
                    server.exoProps

                newExoProps =
                    { oldExoProps
                        | loadingSeparately = True
                    }

                newServer =
                    { server | exoProps = newExoProps }
            in
            projectUpdateServer project newServer


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
    let
        newLocation =
            List.filter (\t -> String.startsWith "exoPw:" t) serverDetails.tags
                |> List.head
                |> Maybe.map (String.dropLeft 6)

        oldLocation =
            List.filter (\i -> i.key == "exouserPassword") serverDetails.metadata
                |> List.head
                |> Maybe.map .value
    in
    case newLocation of
        Just password ->
            Just password

        Nothing ->
            oldLocation


getServerUiStatus : Server -> ServerUiStatus
getServerUiStatus server =
    -- TODO move this to view helpers
    -- TODO reconcile this with orchestration engine's concept of when provisioning is complete
    case server.osProps.details.openstackStatus of
        OSTypes.ServerActive ->
            case server.exoProps.serverOrigin of
                ServerFromExo serverFromExoProps ->
                    case serverFromExoProps.cockpitStatus of
                        NotChecked ->
                            ServerUiStatusPartiallyActive

                        CheckedNotReady ->
                            ServerUiStatusPartiallyActive

                        Ready ->
                            ServerUiStatusReady

                        ReadyButRecheck ->
                            ServerUiStatusReady

                ServerNotFromExo ->
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

        OSTypes.ServerDeleted ->
            ServerUiStatusDeleted


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

        ServerUiStatusDeleted ->
            "Deleted"


getServerUiStatusColor : ServerUiStatus -> Element.Color
getServerUiStatusColor status =
    case status of
        ServerUiStatusUnknown ->
            -- gray
            Element.rgb255 122 122 122

        ServerUiStatusBuilding ->
            -- yellow
            Element.rgb255 255 221 87

        ServerUiStatusPartiallyActive ->
            -- yellow
            Element.rgb255 255 221 87

        ServerUiStatusReady ->
            -- green
            Element.rgb255 35 209 96

        ServerUiStatusReboot ->
            -- yellow
            Element.rgb255 255 221 87

        ServerUiStatusPaused ->
            -- gray
            Element.rgb255 122 122 122

        ServerUiStatusSuspended ->
            -- gray
            Element.rgb255 122 122 122

        ServerUiStatusShutoff ->
            -- gray
            Element.rgb255 122 122 122

        ServerUiStatusStopped ->
            -- gray
            Element.rgb255 122 122 122

        ServerUiStatusSoftDeleted ->
            -- gray
            Element.rgb255 122 122 122

        ServerUiStatusError ->
            -- red
            Element.rgb255 255 56 96

        ServerUiStatusRescued ->
            -- red
            Element.rgb255 255 56 96

        ServerUiStatusShelved ->
            -- gray
            Element.rgb255 122 122 122

        ServerUiStatusDeleted ->
            -- gray
            Element.rgb255 122 122 122


sortedFlavors : List OSTypes.Flavor -> List OSTypes.Flavor
sortedFlavors flavors =
    flavors
        |> List.sortBy .disk_ephemeral
        |> List.sortBy .disk_root
        |> List.sortBy .ram_mb
        |> List.sortBy .vcpu


renderUserDataTemplate : Project -> String -> Maybe String -> Bool -> String
renderUserDataTemplate project userDataTemplate maybeKeypairName deployGuacamole =
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
            "ssh-authorized-keys:\n      - " ++ selectedPublicKey ++ "\n"

        guacamoleSetupCmds : String
        guacamoleSetupCmds =
            if deployGuacamole then
                ServerDeploy.guacamoleUserData

            else
                "echo \"Not deploying Guacamole\""

        renderUserData : String -> String
        renderUserData authorizedKeyYaml =
            [ ( "{ssh-authorized-keys}\n", authorizedKeyYaml )
            , ( "{guacamole-setup}\n", guacamoleSetupCmds )
            ]
                |> List.foldl (\t -> String.replace (Tuple.first t) (Tuple.second t)) userDataTemplate
    in
    maybeKeypairName
        |> Maybe.andThen getPublicKeyFromKeypairName
        |> Maybe.map generateYamlFromPublicKey
        |> Maybe.withDefault ""
        |> renderUserData


newServerMetadata : ExoServerVersion -> UUID.UUID -> Bool -> String -> List ( String, Json.Encode.Value )
newServerMetadata exoServerVersion exoClientUuid deployGuacamole exoCreatorUsername =
    let
        guacMetadata =
            if deployGuacamole then
                [ ( "exoGuac"
                  , Json.Encode.string
                        """{"v":"1","ssh":true,"vnc":false,"deployComplete":false}"""
                  )
                ]

            else
                []
    in
    List.concat
        [ guacMetadata
        , [ ( "exoServerVersion"
            , Json.Encode.string (String.fromInt exoServerVersion)
            )
          , ( "exoClientUuid"
            , Json.Encode.string (UUID.toString exoClientUuid)
            )
          , ( "exoCreatorUsername"
            , Json.Encode.string exoCreatorUsername
            )
          , ( "exoSetup"
            , Json.Encode.string "waiting"
            )
          ]
        ]


newGuacMetadata : GuacTypes.LaunchedWithGuacProps -> String
newGuacMetadata launchedWithGuacProps =
    Json.Encode.object
        [ ( "v", Json.Encode.int 1 )
        , ( "ssh", Json.Encode.bool launchedWithGuacProps.sshSupported )
        , ( "vnc", Json.Encode.bool launchedWithGuacProps.vncSupported )
        , ( "deployComplete", Json.Encode.bool launchedWithGuacProps.deployComplete )
        ]
        |> Json.Encode.encode 0


newServerNetworkOptions : Project -> NewServerNetworkOptions
newServerNetworkOptions project =
    {- When creating a new server, make a reasonable choice of project network, if we can. -}
    let
        -- First, filter on networks that are status ACTIVE, adminStateUp, and not external
        projectNets =
            case project.networks.data of
                RDPP.DoHave networks _ ->
                    networks
                        |> List.filter (\n -> n.status == "ACTIVE")
                        |> List.filter (\n -> n.adminStateUp == True)
                        |> List.filter (\n -> n.isExternal == False)

                RDPP.DontHave ->
                    []

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


isBootVol : Maybe OSTypes.ServerUuid -> OSTypes.Volume -> Bool
isBootVol maybeServerUuid volume =
    -- If a serverUuid is passed, determines whether volume backs that server; otherwise just determines whether volume backs any server
    volume.attachments
        |> List.filter
            (\a ->
                case maybeServerUuid of
                    Just serverUuid ->
                        a.serverUuid == serverUuid

                    Nothing ->
                        True
            )
        |> List.filter
            (\a ->
                List.member
                    a.device
                    [ "/dev/sda", "/dev/vda" ]
            )
        |> List.isEmpty
        |> not


getBootVol : List OSTypes.Volume -> OSTypes.ServerUuid -> Maybe OSTypes.Volume
getBootVol vols serverUuid =
    vols
        |> List.filter (isBootVol <| Just serverUuid)
        |> List.head


volDeviceToMountpoint : OSTypes.VolumeAttachmentDevice -> Maybe String
volDeviceToMountpoint device =
    -- Converts e.g. "/dev/sdc" to "/media/volume/sdc"
    device
        |> String.split "/"
        |> List.reverse
        |> List.head
        |> Maybe.map (String.append "/media/volume/")


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


computeQuotaFlavorAvailServers : OSTypes.ComputeQuota -> OSTypes.Flavor -> Maybe Int
computeQuotaFlavorAvailServers computeQuota flavor =
    -- Given a compute quota and a flavor, determine how many servers of that flavor can be launched
    [ computeQuota.cores.limit
        |> Maybe.map
            (\coreLimit ->
                (coreLimit - computeQuota.cores.inUse) // flavor.vcpu
            )
    , computeQuota.ram.limit
        |> Maybe.map
            (\ramLimit ->
                (ramLimit - computeQuota.ram.inUse) // flavor.ram_mb
            )
    , computeQuota.instances.limit
        |> Maybe.map
            (\countLimit ->
                countLimit - computeQuota.instances.inUse
            )
    ]
        |> List.filterMap identity
        |> List.minimum


volumeQuotaAvail : OSTypes.VolumeQuota -> ( Maybe Int, Maybe Int )
volumeQuotaAvail volumeQuota =
    -- Returns tuple showing # volumes and # total gigabytes that are available given quota and usage.
    -- Nothing implies no limit.
    ( volumeQuota.volumes.limit
        |> Maybe.map
            (\volLimit ->
                volLimit - volumeQuota.volumes.inUse
            )
    , volumeQuota.gigabytes.limit
        |> Maybe.map
            (\gbLimit ->
                gbLimit - volumeQuota.gigabytes.inUse
            )
    )


overallQuotaAvailServers : Maybe OSTypes.VolumeSize -> OSTypes.Flavor -> OSTypes.ComputeQuota -> OSTypes.VolumeQuota -> Maybe Int
overallQuotaAvailServers maybeVolBackedGb flavor computeQuota volumeQuota =
    let
        computeQuotaAvailServers =
            computeQuotaFlavorAvailServers computeQuota flavor
    in
    case maybeVolBackedGb of
        Nothing ->
            computeQuotaAvailServers

        Just volBackedGb ->
            let
                ( volumeQuotaAvailVolumes, volumeQuotaAvailGb ) =
                    volumeQuotaAvail volumeQuota

                volumeQuotaAvailGbCount =
                    volumeQuotaAvailGb
                        |> Maybe.map
                            (\availGb ->
                                availGb // volBackedGb
                            )
            in
            [ computeQuotaAvailServers
            , volumeQuotaAvailVolumes
            , volumeQuotaAvailGbCount
            ]
                |> List.filterMap identity
                |> List.minimum


serverOrigin : OSTypes.ServerDetails -> ServerOrigin
serverOrigin serverDetails =
    let
        exoServerVersion =
            let
                maybeDecodedVersion =
                    List.filter (\i -> i.key == "exoServerVersion") serverDetails.metadata
                        |> List.head
                        |> Maybe.map .value
                        |> Maybe.andThen String.toInt

                version0 =
                    List.filter (\i -> i.key == "exouserPassword") serverDetails.metadata
                        |> List.isEmpty
                        |> not
            in
            case maybeDecodedVersion of
                Just v ->
                    Just v

                Nothing ->
                    if version0 then
                        Just 0

                    else
                        Nothing

        exoSetupStatus =
            let
                maybeStrStatus =
                    List.filter (\i -> i.key == "exoSetup") serverDetails.metadata
                        |> List.head
                        |> Maybe.map .value
            in
            case maybeStrStatus of
                Nothing ->
                    ExoSetupUnknown

                Just strStatus ->
                    case strStatus of
                        "waiting" ->
                            ExoSetupWaiting

                        "running" ->
                            ExoSetupRunning

                        "complete" ->
                            ExoSetupComplete

                        "error" ->
                            ExoSetupError

                        _ ->
                            ExoSetupUnknown

        decodeGuacamoleProps : Decode.Decoder GuacTypes.LaunchedWithGuacProps
        decodeGuacamoleProps =
            Decode.map4
                GuacTypes.LaunchedWithGuacProps
                (Decode.field "ssh" Decode.bool)
                (Decode.field "vnc" Decode.bool)
                (Decode.field "deployComplete" Decode.bool)
                (Decode.succeed RDPP.empty)

        guacamoleStatus =
            case
                List.filter (\i -> i.key == "exoGuac") serverDetails.metadata
                    |> List.head
            of
                Nothing ->
                    GuacTypes.NotLaunchedWithGuacamole

                Just item ->
                    case Decode.decodeString decodeGuacamoleProps item.value of
                        Ok launchedWithGuacProps ->
                            GuacTypes.LaunchedWithGuacamole launchedWithGuacProps

                        Err _ ->
                            GuacTypes.NotLaunchedWithGuacamole

        creatorName =
            List.filter (\i -> i.key == "exoCreatorUsername") serverDetails.metadata
                |> List.head
                |> Maybe.map .value
    in
    case exoServerVersion of
        Just v ->
            ServerFromExo <|
                ServerFromExoProps v exoSetupStatus NotChecked RDPP.empty guacamoleStatus creatorName

        Nothing ->
            ServerNotFromExo


serverFromThisExoClient : UUID.UUID -> Server -> Bool
serverFromThisExoClient clientUuid server =
    -- Determine if server was created by this Exosphere client
    List.member (OSTypes.MetadataItem "exoClientUuid" (UUID.toString clientUuid)) server.osProps.details.metadata


serverNeedsFrequentPoll : Server -> Bool
serverNeedsFrequentPoll server =
    case
        ( server.exoProps.deletionAttempted
        , server.exoProps.targetOpenstackStatus
        , server.exoProps.serverOrigin
        )
    of
        ( False, Nothing, ServerNotFromExo ) ->
            False

        ( False, Nothing, ServerFromExo exoOriginProps ) ->
            case exoOriginProps.cockpitStatus of
                Ready ->
                    False

                _ ->
                    True

        _ ->
            True


serverLessThanThisOld : Server -> Time.Posix -> Int -> Bool
serverLessThanThisOld server currentTime maxServerAgeMillis =
    let
        curTimeMillis =
            Time.posixToMillis currentTime
    in
    case iso8601StringToPosix server.osProps.details.created of
        -- Defaults to False if cannot determine server created time
        Err _ ->
            False

        Ok createdTime ->
            (curTimeMillis - Time.posixToMillis createdTime) < maxServerAgeMillis


buildProxyUrl : UserAppProxyHostname -> OSTypes.IpAddressValue -> Int -> String -> Bool -> String
buildProxyUrl proxyHostname destinationIp port_ path https_upstream =
    [ "https://"
    , if https_upstream then
        ""

      else
        "http-"
    , destinationIp |> String.replace "." "-"
    , "-"
    , String.fromInt port_
    , "."
    , proxyHostname
    , path
    ]
        |> String.concat
