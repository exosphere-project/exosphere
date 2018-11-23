module Rest.Rest exposing
    ( addFloatingIpInServerDetails
    , createProvider
    , decodeFlavors
    , decodeFloatingIpCreation
    , decodeImages
    , decodeKeypairs
    , decodeNetworks
    , decodePorts
    , decodeServerDetails
    , decodeServers
    , flavorDecoder
    , getFloatingIpRequestPorts
    , imageDecoder
    , imageStatusDecoder
    , ipAddressOpenstackTypeDecoder
    , keypairDecoder
    , networkDecoder
    , openstackEndpointDecoder
    , openstackEndpointInterfaceDecoder
    , openstackServiceDecoder
    , portDecoder
    , receiveAuthToken
    , receiveCockpitLoginStatus
    , receiveConsoleUrl
    , receiveCreateExoSecurityGroupAndRequestCreateRules
    , receiveCreateFloatingIp
    , receiveCreateServer
    , receiveDeleteFloatingIp
    , receiveDeleteServer
    , receiveFlavors
    , receiveFloatingIps
    , receiveImages
    , receiveKeypairs
    , receiveNetworks
    , receivePortsAndRequestFloatingIp
    , receiveSecurityGroupsAndEnsureExoGroup
    , receiveServerDetail
    , receiveServers
    , requestAuthToken
    , requestConsoleUrl
    , requestCreateExoSecurityGroupRules
    , requestCreateFloatingIp
    , requestCreateFloatingIpIfRequestable
    , requestCreateServer
    , requestDeleteFloatingIp
    , requestDeleteServer
    , requestDeleteServers
    , requestFlavors
    , requestFloatingIps
    , requestImages
    , requestKeypairs
    , requestNetworks
    , requestServerDetail
    , requestServers
    , serverDecoder
    , serverIpAddressDecoder
    , serverPowerStateDecoder
    )

import Array
import Base64
import Dict
import Helpers.Helpers as Helpers
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import RemoteData
import Rest.Helpers exposing (..)
import Time
import Types.OpenstackTypes as OSTypes
import Types.Types exposing (..)



{- HTTP Requests -}


requestAuthToken : Creds -> Cmd Msg
requestAuthToken creds =
    let
        idOrName str =
            case Helpers.stringIsUuidOrDefault str of
                True ->
                    "id"

                False ->
                    "name"

        requestBody =
            Encode.object
                [ ( "auth"
                  , Encode.object
                        [ ( "identity"
                          , Encode.object
                                [ ( "methods", Encode.list Encode.string [ "password" ] )
                                , ( "password"
                                  , Encode.object
                                        [ ( "user"
                                          , Encode.object
                                                [ ( "name", Encode.string creds.username )
                                                , ( "domain"
                                                  , Encode.object
                                                        [ ( idOrName creds.userDomain, Encode.string creds.userDomain )
                                                        ]
                                                  )
                                                , ( "password", Encode.string creds.password )
                                                ]
                                          )
                                        ]
                                  )
                                ]
                          )
                        , ( "scope"
                          , Encode.object
                                [ ( "project"
                                  , Encode.object
                                        [ ( "name", Encode.string creds.projectName )
                                        , ( "domain"
                                          , Encode.object
                                                [ ( idOrName creds.projectDomain, Encode.string creds.projectDomain )
                                                ]
                                          )
                                        ]
                                  )
                                ]
                          )
                        ]
                  )
                ]
    in
    {- https://stackoverflow.com/questions/44368340/get-request-headers-from-http-request -}
    Http.request
        { method = "POST"
        , headers = []
        , url = creds.authUrl
        , body = Http.jsonBody requestBody

        {- Todo handle no response? -}
        , expect = Http.expectStringResponse (\response -> Ok response)
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send (ReceiveAuthToken creds)


requestImages : Provider -> Cmd Msg
requestImages provider =
    openstackCredentialedRequest
        provider
        Get
        (provider.endpoints.glance ++ "/v2/images?limit=999999")
        Http.emptyBody
        (Http.expectJson decodeImages)
        (\result -> ProviderMsg provider.name (ReceiveImages result))


requestServers : Provider -> Cmd Msg
requestServers provider =
    openstackCredentialedRequest
        provider
        Get
        (provider.endpoints.nova ++ "/servers")
        Http.emptyBody
        (Http.expectJson decodeServers)
        (\result -> ProviderMsg provider.name (ReceiveServers result))


requestServerDetail : Provider -> OSTypes.ServerUuid -> Cmd Msg
requestServerDetail provider serverUuid =
    openstackCredentialedRequest
        provider
        Get
        (provider.endpoints.nova ++ "/servers/" ++ serverUuid)
        Http.emptyBody
        (Http.expectJson decodeServerDetails)
        (\result -> ProviderMsg provider.name (ReceiveServerDetail serverUuid result))


requestConsoleUrl : Provider -> OSTypes.ServerUuid -> Cmd Msg
requestConsoleUrl provider serverUuid =
    -- This is a deprecated call, will eventually need to be updated
    -- See https://gitlab.com/exosphere/exosphere/issues/183
    let
        body =
            Encode.object
                [ ( "os-getSPICEConsole"
                  , Encode.object
                        [ ( "type", Encode.string "spice-html5" )
                        ]
                  )
                ]
    in
    openstackCredentialedRequest
        provider
        Post
        (provider.endpoints.nova ++ "/servers/" ++ serverUuid ++ "/action")
        (Http.jsonBody body)
        (Http.expectJson decodeConsoleUrl)
        (\result -> ProviderMsg provider.name (ReceiveConsoleUrl serverUuid result))


requestFlavors : Provider -> Cmd Msg
requestFlavors provider =
    openstackCredentialedRequest
        provider
        Get
        (provider.endpoints.nova ++ "/flavors/detail")
        Http.emptyBody
        (Http.expectJson decodeFlavors)
        (\result -> ProviderMsg provider.name (ReceiveFlavors result))


requestKeypairs : Provider -> Cmd Msg
requestKeypairs provider =
    openstackCredentialedRequest
        provider
        Get
        (provider.endpoints.nova ++ "/os-keypairs")
        Http.emptyBody
        (Http.expectJson decodeKeypairs)
        (\result -> ProviderMsg provider.name (ReceiveKeypairs result))


requestCreateServer : Provider -> CreateServerRequest -> Cmd Msg
requestCreateServer provider createServerRequest =
    let
        getServerCount =
            Maybe.withDefault 1 (String.toInt createServerRequest.count)

        instanceNumbers =
            List.range 1 getServerCount

        generateServerName : String -> Int -> Int -> String
        generateServerName baseName serverCount index =
            if serverCount == 1 then
                baseName

            else
                baseName ++ " " ++ String.fromInt index ++ " of " ++ String.fromInt getServerCount

        instanceNames =
            instanceNumbers
                |> List.map (generateServerName createServerRequest.name getServerCount)

        baseServerProps innerCreateServerRequest instanceName =
            let
                maybeKeypairJson =
                    case innerCreateServerRequest.keypairName of
                        Nothing ->
                            []

                        Just keypairName ->
                            [ ( "key_name", Encode.string keypairName ) ]
            in
            List.append
                maybeKeypairJson
                [ ( "name", Encode.string instanceName )
                , ( "flavorRef", Encode.string innerCreateServerRequest.flavorUuid )
                , case innerCreateServerRequest.networkUuid of
                    "auto" ->
                        ( "networks", Encode.string "auto" )

                    netUuid ->
                        ( "networks"
                        , Encode.list Encode.object
                            [ [ ( "uuid", Encode.string innerCreateServerRequest.networkUuid ) ] ]
                        )
                , ( "user_data", Encode.string (Base64.encode innerCreateServerRequest.userData) )
                , ( "security_groups", Encode.array Encode.object (Array.fromList [ [ ( "name", Encode.string "exosphere" ) ] ]) )
                , ( "adminPass", Encode.string createServerRequest.exouserPassword )
                , ( "metadata", Encode.object [ ( "exouserPassword", Encode.string createServerRequest.exouserPassword ) ] )
                ]

        buildRequestOuterJson props =
            Encode.object [ ( "server", Encode.object props ) ]

        buildRequestBody instanceName =
            case createServerRequest.volBacked of
                False ->
                    ( "imageRef", Encode.string createServerRequest.imageUuid )
                        :: baseServerProps createServerRequest instanceName
                        |> buildRequestOuterJson

                True ->
                    ( "block_device_mapping_v2"
                    , Encode.list Encode.object
                        [ [ ( "boot_index", Encode.string "0" )
                          , ( "uuid", Encode.string createServerRequest.imageUuid )
                          , ( "source_type", Encode.string "image" )
                          , ( "volume_size", Encode.string createServerRequest.volBackedSizeGb )
                          , ( "destination_type", Encode.string "volume" )
                          , ( "delete_on_termination", Encode.bool True )
                          ]
                        ]
                    )
                        :: baseServerProps createServerRequest instanceName
                        |> buildRequestOuterJson

        requestBodies =
            instanceNames
                |> List.map buildRequestBody
    in
    Cmd.batch
        (requestBodies
            |> List.map
                (\requestBody ->
                    openstackCredentialedRequest
                        provider
                        Post
                        (provider.endpoints.nova ++ "/servers")
                        (Http.jsonBody requestBody)
                        (Http.expectJson (Decode.field "server" serverDecoder))
                        (\result -> ProviderMsg provider.name (ReceiveCreateServer result))
                )
        )


requestDeleteServer : Provider -> Server -> Cmd Msg
requestDeleteServer provider server =
    let
        maybeFloatingIp =
            server.osProps.details
                |> Maybe.map .ipAddresses
                |> Maybe.andThen Helpers.getServerFloatingIp
    in
    openstackCredentialedRequest
        provider
        Delete
        (provider.endpoints.nova ++ "/servers/" ++ server.osProps.uuid)
        Http.emptyBody
        Http.expectString
        (\result -> ProviderMsg provider.name (ReceiveDeleteServer server.osProps.uuid maybeFloatingIp result))


requestDeleteServers : Provider -> List Server -> Cmd Msg
requestDeleteServers provider serversToDelete =
    let
        deleteRequests =
            List.map (requestDeleteServer provider) serversToDelete
    in
    Cmd.batch deleteRequests


requestNetworks : Provider -> Cmd Msg
requestNetworks provider =
    openstackCredentialedRequest
        provider
        Get
        (provider.endpoints.neutron ++ "/v2.0/networks")
        Http.emptyBody
        (Http.expectJson decodeNetworks)
        (\result -> ProviderMsg provider.name (ReceiveNetworks result))


requestFloatingIps : Provider -> Cmd Msg
requestFloatingIps provider =
    openstackCredentialedRequest
        provider
        Get
        (provider.endpoints.neutron ++ "/v2.0/floatingips")
        Http.emptyBody
        (Http.expectJson decodeFloatingIps)
        (\result -> ProviderMsg provider.name (ReceiveFloatingIps result))


getFloatingIpRequestPorts : Provider -> Server -> Cmd Msg
getFloatingIpRequestPorts provider server =
    openstackCredentialedRequest
        provider
        Get
        (provider.endpoints.neutron ++ "/v2.0/ports")
        Http.emptyBody
        (Http.expectJson decodePorts)
        (\result -> ProviderMsg provider.name (GetFloatingIpReceivePorts server.osProps.uuid result))


requestCreateFloatingIpIfRequestable : Model -> Provider -> OSTypes.Network -> OSTypes.Port -> OSTypes.ServerUuid -> ( Model, Cmd Msg )
requestCreateFloatingIpIfRequestable model provider network port_ serverUuid =
    case Helpers.serverLookup provider serverUuid of
        Nothing ->
            Helpers.processError model "We should have a server here but we don't"

        Just server ->
            case server.exoProps.floatingIpState of
                Requestable ->
                    requestCreateFloatingIp model provider network port_ server

                _ ->
                    ( model, Cmd.none )


requestCreateFloatingIp : Model -> Provider -> OSTypes.Network -> OSTypes.Port -> Server -> ( Model, Cmd Msg )
requestCreateFloatingIp model provider network port_ server =
    let
        newServer =
            let
                oldExoProps =
                    server.exoProps
            in
            Server server.osProps { oldExoProps | floatingIpState = RequestedWaiting }

        newProvider =
            Helpers.providerUpdateServer provider newServer

        newModel =
            Helpers.modelUpdateProvider model newProvider

        requestBody =
            Encode.object
                [ ( "floatingip"
                  , Encode.object
                        [ ( "floating_network_id", Encode.string network.uuid )
                        , ( "port_id", Encode.string port_.uuid )
                        ]
                  )
                ]

        requestCmd =
            openstackCredentialedRequest
                newProvider
                Post
                (provider.endpoints.neutron ++ "/v2.0/floatingips")
                (Http.jsonBody requestBody)
                (Http.expectJson decodeFloatingIpCreation)
                (\result -> ProviderMsg provider.name (ReceiveCreateFloatingIp server.osProps.uuid result))
    in
    ( newModel, requestCmd )


requestDeleteFloatingIp : Provider -> OSTypes.IpAddressUuid -> Cmd Msg
requestDeleteFloatingIp provider uuid =
    openstackCredentialedRequest
        provider
        Delete
        (provider.endpoints.neutron ++ "/v2.0/floatingips/" ++ uuid)
        Http.emptyBody
        Http.expectString
        (\result -> ProviderMsg provider.name (ReceiveDeleteFloatingIp uuid result))


requestSecurityGroups : Provider -> Cmd Msg
requestSecurityGroups provider =
    openstackCredentialedRequest
        provider
        Get
        (provider.endpoints.neutron ++ "/v2.0/security-groups")
        Http.emptyBody
        (Http.expectJson decodeSecurityGroups)
        (\result -> ProviderMsg provider.name (ReceiveSecurityGroups result))


requestCreateExoSecurityGroup : Provider -> Cmd Msg
requestCreateExoSecurityGroup provider =
    let
        desc =
            "Security group for instances launched via Exosphere"

        requestBody =
            Encode.object
                [ ( "security_group"
                  , Encode.object
                        [ ( "name", Encode.string "exosphere" )
                        , ( "description", Encode.string desc )
                        ]
                  )
                ]
    in
    openstackCredentialedRequest
        provider
        Post
        (provider.endpoints.neutron ++ "/v2.0/security-groups")
        (Http.jsonBody requestBody)
        (Http.expectJson decodeNewSecurityGroup)
        (\result -> ProviderMsg provider.name (ReceiveCreateExoSecurityGroup result))


requestCreateExoSecurityGroupRules : Model -> Provider -> ( Model, Cmd Msg )
requestCreateExoSecurityGroupRules model provider =
    let
        maybeSecurityGroup =
            List.filter (\g -> g.name == "exosphere") provider.securityGroups |> List.head
    in
    case maybeSecurityGroup of
        Nothing ->
            Helpers.processError model "Error: expecting to find an Exosphere security group but none was found."

        Just group ->
            let
                makeRequestBody port_number desc =
                    Encode.object
                        [ ( "security_group_rule"
                          , Encode.object
                                [ ( "security_group_id", Encode.string group.uuid )
                                , ( "ethertype", Encode.string "IPv4" )
                                , ( "direction", Encode.string "ingress" )
                                , ( "protocol", Encode.string "tcp" )
                                , ( "port_range_min", Encode.string port_number )
                                , ( "port_range_max", Encode.string port_number )
                                , ( "description", Encode.string desc )
                                ]
                          )
                        ]

                buildRequestCmd body =
                    openstackCredentialedRequest
                        provider
                        Post
                        (provider.endpoints.neutron ++ "/v2.0/security-group-rules")
                        (Http.jsonBody body)
                        Http.expectString
                        (\result -> ProviderMsg provider.name (ReceiveCreateExoSecurityGroupRules result))

                bodies =
                    [ makeRequestBody "22" "SSH"
                    , makeRequestBody "9090" "Cockpit"
                    ]

                cmds =
                    List.map (\b -> buildRequestCmd b) bodies
            in
            ( model, Cmd.batch cmds )


requestCockpitLogin : Provider -> OSTypes.ServerUuid -> String -> String -> Cmd Msg
requestCockpitLogin provider serverUuid password ipAddress =
    let
        authHeaderValue =
            "Basic " ++ Base64.encode ("exouser:" ++ password)

        request =
            Http.request
                { method = "GET"
                , headers = [ Http.header "Authorization" authHeaderValue ]
                , url = "http://" ++ ipAddress ++ ":9090/cockpit/login"
                , body = Http.emptyBody
                , expect = Http.expectString
                , timeout = Just 3000
                , withCredentials = True
                }

        resultMsg provider2 serverUuid2 result =
            ProviderMsg provider2.name (ReceiveCockpitLoginStatus serverUuid2 result)
    in
    Http.send (resultMsg provider serverUuid) request



{- HTTP Response Handling -}


receiveAuthToken : Model -> Creds -> Result Http.Error (Http.Response String) -> ( Model, Cmd Msg )
receiveAuthToken model creds responseResult =
    case responseResult of
        Err error ->
            Helpers.processError model error

        Ok response ->
            -- If we don't have a provider then create one, if we do then update its OSTypes.AuthToken
            case List.filter (\p -> p.creds.authUrl == creds.authUrl) model.providers |> List.head of
                Nothing ->
                    createProvider model creds response

                Just provider ->
                    providerUpdateAuthToken model provider response


createProvider : Model -> Creds -> Http.Response String -> ( Model, Cmd Msg )
createProvider model creds response =
    -- Create new provider
    case decodeAuthToken response of
        Err error ->
            Helpers.processError model error

        Ok authToken ->
            let
                endpoints =
                    Helpers.serviceCatalogToEndpoints authToken.catalog

                newProvider =
                    { name = Helpers.providerNameFromUrl model.creds.authUrl
                    , creds = creds
                    , auth = authToken

                    -- Maybe todo, eliminate parallel data structures in auth and endpoints?
                    , endpoints = endpoints
                    , images = []
                    , servers = RemoteData.NotAsked
                    , flavors = []
                    , keypairs = []
                    , networks = []
                    , floatingIps = []
                    , ports = []
                    , securityGroups = []
                    , pendingCredentialedRequests = []
                    }

                newProviders =
                    newProvider :: model.providers

                newModel =
                    { model
                        | providers = newProviders
                        , viewState = ProviderView newProvider.name ListProviderServers
                    }
            in
            ( newModel, Cmd.batch [ requestServers newProvider, requestSecurityGroups newProvider, requestFloatingIps newProvider ] )


providerUpdateAuthToken : Model -> Provider -> Http.Response String -> ( Model, Cmd Msg )
providerUpdateAuthToken model provider response =
    -- Update auth token for existing provider
    case decodeAuthToken response of
        Err error ->
            Helpers.processError model error

        Ok authToken ->
            let
                newProvider =
                    { provider | auth = authToken }

                newModel =
                    Helpers.modelUpdateProvider model newProvider
            in
            sendPendingRequests newModel newProvider


sendPendingRequests : Model -> Provider -> ( Model, Cmd Msg )
sendPendingRequests model provider =
    -- Fires any pending commands which were waiting for auth token renewal
    -- This function assumes our token is valid (does not check for expiry).
    let
        -- Hydrate cmds with auth token
        cmds =
            List.map (\pqr -> pqr provider.auth.tokenValue) provider.pendingCredentialedRequests

        -- Clear out pendingCredentialedRequests
        newProvider =
            { provider | pendingCredentialedRequests = [] }

        newModel =
            Helpers.modelUpdateProvider model newProvider
    in
    ( newModel, Cmd.batch cmds )


receiveImages : Model -> Provider -> Result Http.Error (List OSTypes.Image) -> ( Model, Cmd Msg )
receiveImages model provider result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok images ->
            let
                newProvider =
                    { provider | images = images }

                newModel =
                    Helpers.modelUpdateProvider model newProvider
            in
            ( newModel, Cmd.none )


receiveServers : Model -> Provider -> Result Http.Error (List OSTypes.Server) -> ( Model, Cmd Msg )
receiveServers model provider result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok newOpenstackServers ->
            -- Enrich new list of servers with any exoProps and osProps.details from old list of servers
            let
                defaultExoProps =
                    ExoServerProps Unknown False NotChecked False

                enrichNewServer : OSTypes.Server -> Server
                enrichNewServer newOpenstackServer =
                    case Helpers.serverLookup provider newOpenstackServer.uuid of
                        Nothing ->
                            Server newOpenstackServer defaultExoProps

                        Just oldServer ->
                            let
                                oldDetails =
                                    oldServer.osProps.details
                            in
                            Server { newOpenstackServer | details = oldDetails } oldServer.exoProps

                newServers =
                    List.map enrichNewServer newOpenstackServers

                newProvider =
                    { provider | servers = RemoteData.Success newServers }

                newModel =
                    Helpers.modelUpdateProvider model newProvider
            in
            ( newModel, Cmd.none )


receiveServerDetail : Model -> Provider -> OSTypes.ServerUuid -> Result Http.Error OSTypes.ServerDetails -> ( Model, Cmd Msg )
receiveServerDetail model provider serverUuid result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok serverDetails ->
            let
                maybeServer =
                    Helpers.serverLookup provider serverUuid
            in
            case maybeServer of
                Nothing ->
                    Helpers.processError
                        model
                        "No server found when receiving server details"

                Just server ->
                    let
                        newServer =
                            let
                                oldOSProps =
                                    server.osProps

                                oldExoProps =
                                    server.exoProps
                            in
                            Server { oldOSProps | details = Just serverDetails } { oldExoProps | floatingIpState = floatingIpState }

                        newProvider =
                            Helpers.providerUpdateServer provider newServer

                        newModel =
                            Helpers.modelUpdateProvider model newProvider

                        floatingIpState =
                            Helpers.checkFloatingIpState
                                serverDetails
                                server.exoProps.floatingIpState

                        requestFloatingIpCmds =
                            case floatingIpState of
                                Requestable ->
                                    [ getFloatingIpRequestPorts newProvider newServer
                                    , requestNetworks newProvider
                                    ]

                                _ ->
                                    []

                        requestConsoleUrlCmds =
                            case serverDetails.openstackStatus of
                                OSTypes.ServerActive ->
                                    [ requestConsoleUrl provider serverUuid ]

                                _ ->
                                    [ Cmd.none ]

                        requestCockpitLoginCmds =
                            case floatingIpState of
                                Success ->
                                    let
                                        maybeFloatingIp =
                                            Helpers.getServerFloatingIp
                                                serverDetails.ipAddresses
                                    in
                                    {- If we have a floating IP address and exouser password then try to log into Cockpit -}
                                    case maybeFloatingIp of
                                        Just floatingIp ->
                                            case List.filter (\i -> i.key == "exouserPassword") serverDetails.metadata |> List.head of
                                                Just passwordMetaItem ->
                                                    let
                                                        password =
                                                            passwordMetaItem.value
                                                    in
                                                    [ requestCockpitLogin newProvider server.osProps.uuid password floatingIp ]

                                                Nothing ->
                                                    [ Cmd.none ]

                                        -- Maybe in the future show an error here? Missing metadata
                                        Nothing ->
                                            [ Cmd.none ]

                                -- Maybe in the future show an error here? Missing floating IP
                                _ ->
                                    [ Cmd.none ]

                        allCmds =
                            [ requestFloatingIpCmds, requestConsoleUrlCmds, requestCockpitLoginCmds ]
                                |> List.concat
                                |> Cmd.batch
                    in
                    ( newModel, allCmds )


receiveConsoleUrl : Model -> Provider -> OSTypes.ServerUuid -> Result Http.Error OSTypes.ConsoleUrl -> ( Model, Cmd Msg )
receiveConsoleUrl model provider serverUuid result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok consoleUrl ->
            let
                maybeServer =
                    Helpers.serverLookup provider serverUuid
            in
            case maybeServer of
                Nothing ->
                    ( model, Cmd.none )

                -- This is an error state (server not found) but probably not one worth throwing an error at the user over. Someone might have just deleted their server
                Just server ->
                    let
                        oldOsProps =
                            server.osProps

                        newOsProps =
                            { oldOsProps | consoleUrl = Just consoleUrl }

                        newServer =
                            { server | osProps = newOsProps }

                        newProvider =
                            Helpers.providerUpdateServer provider newServer

                        newModel =
                            Helpers.modelUpdateProvider model newProvider
                    in
                    ( newModel, Cmd.none )


receiveFlavors : Model -> Provider -> Result Http.Error (List OSTypes.Flavor) -> ( Model, Cmd Msg )
receiveFlavors model provider result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok flavors ->
            let
                newProvider =
                    { provider | flavors = flavors }

                -- If we have a CreateServerRequest with no flavor UUID, populate it with the smallest flavor.
                -- This is the start of a code smell because we need to reach way into the viewState to update
                -- the createServerRequest. Good candidate for future refactoring to bring CreateServerRequest
                -- outside of model.viewState.
                -- This could also benefit from some "railway-oriented programming" to avoid repetition of
                -- "otherwise just model.viewState" statments.
                viewState =
                    case model.viewState of
                        ProviderView _ providerViewConstructor ->
                            case providerViewConstructor of
                                CreateServer createServerRequest ->
                                    if createServerRequest.flavorUuid == "" then
                                        let
                                            maybeSmallestFlavor =
                                                Helpers.sortedFlavors flavors |> List.head
                                        in
                                        case maybeSmallestFlavor of
                                            Just smallestFlavor ->
                                                ProviderView provider.name (CreateServer { createServerRequest | flavorUuid = smallestFlavor.uuid })

                                            Nothing ->
                                                model.viewState

                                    else
                                        model.viewState

                                _ ->
                                    model.viewState

                        _ ->
                            model.viewState

                newModel =
                    Helpers.modelUpdateProvider { model | viewState = viewState } newProvider
            in
            ( newModel, Cmd.none )


receiveKeypairs : Model -> Provider -> Result Http.Error (List OSTypes.Keypair) -> ( Model, Cmd Msg )
receiveKeypairs model provider result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok keypairs ->
            let
                newProvider =
                    { provider | keypairs = keypairs }

                newModel =
                    Helpers.modelUpdateProvider model newProvider
            in
            ( newModel, Cmd.none )


receiveCreateServer : Model -> Provider -> Result Http.Error OSTypes.Server -> ( Model, Cmd Msg )
receiveCreateServer model provider result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok _ ->
            let
                newModel =
                    { model | viewState = ProviderView provider.name ListProviderServers }
            in
            ( newModel
            , Cmd.batch
                [ requestServers provider
                , requestNetworks provider
                ]
            )


receiveDeleteServer : Model -> Provider -> OSTypes.ServerUuid -> Result Http.Error String -> ( Model, Cmd Msg )
receiveDeleteServer model provider serverUuid result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok _ ->
            let
                newServers =
                    List.filter (\s -> s.osProps.uuid /= serverUuid) (RemoteData.withDefault [] provider.servers)

                newProvider =
                    { provider | servers = RemoteData.Success newServers }

                newModel =
                    Helpers.modelUpdateProvider model newProvider
            in
            ( newModel, Cmd.none )


receiveNetworks : Model -> Provider -> Result Http.Error (List OSTypes.Network) -> ( Model, Cmd Msg )
receiveNetworks model provider result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok networks ->
            let
                newProvider =
                    { provider | networks = networks }

                -- If we have a CreateServerRequest with no network UUID, populate it with a reasonable guess of a private network.
                -- Same comments above (in receiveFlavors) apply here.
                viewState =
                    case model.viewState of
                        ProviderView _ providerViewConstructor ->
                            case providerViewConstructor of
                                CreateServer createServerRequest ->
                                    if createServerRequest.networkUuid == "" then
                                        let
                                            defaultNetUuid =
                                                case Helpers.newServerNetworkOptions newProvider of
                                                    NoNetsAutoAllocate ->
                                                        "auto"

                                                    OneNet net ->
                                                        net.uuid

                                                    MultipleNetsWithGuess _ guessNet _ ->
                                                        guessNet.uuid
                                        in
                                        ProviderView provider.name (CreateServer { createServerRequest | networkUuid = defaultNetUuid })

                                    else
                                        model.viewState

                                _ ->
                                    model.viewState

                        _ ->
                            model.viewState

                newModel =
                    Helpers.modelUpdateProvider { model | viewState = viewState } newProvider
            in
            ( newModel, Cmd.none )


receiveFloatingIps : Model -> Provider -> Result Http.Error (List OSTypes.IpAddress) -> ( Model, Cmd Msg )
receiveFloatingIps model provider result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok floatingIps ->
            let
                newProvider =
                    { provider | floatingIps = floatingIps }

                newModel =
                    Helpers.modelUpdateProvider model newProvider
            in
            ( newModel, Cmd.none )


receivePortsAndRequestFloatingIp : Model -> Provider -> OSTypes.ServerUuid -> Result Http.Error (List OSTypes.Port) -> ( Model, Cmd Msg )
receivePortsAndRequestFloatingIp model provider serverUuid result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok ports ->
            let
                newProvider =
                    { provider | ports = ports }

                newModel =
                    Helpers.modelUpdateProvider model newProvider

                maybeExtNet =
                    Helpers.getExternalNetwork newProvider

                maybePortForServer =
                    List.filter (\port_ -> port_.deviceUuid == serverUuid) ports
                        |> List.head
            in
            case maybeExtNet of
                Just extNet ->
                    case maybePortForServer of
                        Just port_ ->
                            requestCreateFloatingIpIfRequestable
                                newModel
                                newProvider
                                extNet
                                port_
                                serverUuid

                        Nothing ->
                            Helpers.processError
                                newModel
                                "We should have a port here but we don't!?"

                Nothing ->
                    Helpers.processError
                        newModel
                        "We should have an external network here but we don't"


receiveCreateFloatingIp : Model -> Provider -> OSTypes.ServerUuid -> Result Http.Error OSTypes.IpAddress -> ( Model, Cmd Msg )
receiveCreateFloatingIp model provider serverUuid result =
    case Helpers.serverLookup provider serverUuid of
        Nothing ->
            Helpers.processError
                model
                "We should have a server here but we don't"

        Just server ->
            {- This repeats a lot of code in receiveCockpitStatus, badly needs a refactor -}
            case result of
                Err error ->
                    let
                        newServer =
                            let
                                oldExoProps =
                                    server.exoProps
                            in
                            Server server.osProps { oldExoProps | floatingIpState = Failed }

                        newProvider =
                            Helpers.providerUpdateServer provider newServer

                        newModel =
                            Helpers.modelUpdateProvider model newProvider
                    in
                    Helpers.processError newModel error

                Ok ipAddress ->
                    let
                        newServer =
                            let
                                oldOSProps =
                                    server.osProps

                                oldExoProps =
                                    server.exoProps

                                details =
                                    addFloatingIpInServerDetails
                                        server.osProps.details
                                        ipAddress
                            in
                            Server
                                { oldOSProps | details = details }
                                { oldExoProps | floatingIpState = Success }

                        newProvider =
                            Helpers.providerUpdateServer provider newServer

                        newModel =
                            Helpers.modelUpdateProvider model newProvider
                    in
                    ( newModel, Cmd.none )


receiveDeleteFloatingIp : Model -> Provider -> OSTypes.IpAddressUuid -> Result Http.Error String -> ( Model, Cmd Msg )
receiveDeleteFloatingIp model provider uuid result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok _ ->
            let
                newFloatingIps =
                    List.filter (\f -> f.uuid /= Just uuid) provider.floatingIps

                newProvider =
                    { provider | floatingIps = newFloatingIps }

                newModel =
                    Helpers.modelUpdateProvider model newProvider
            in
            ( newModel, Cmd.none )


addFloatingIpInServerDetails : Maybe OSTypes.ServerDetails -> OSTypes.IpAddress -> Maybe OSTypes.ServerDetails
addFloatingIpInServerDetails maybeDetails ipAddress =
    case maybeDetails of
        Nothing ->
            Nothing

        Just details ->
            let
                newIps =
                    ipAddress :: details.ipAddresses
            in
            Just { details | ipAddresses = newIps }


receiveSecurityGroupsAndEnsureExoGroup : Model -> Provider -> Result Http.Error (List OSTypes.SecurityGroup) -> ( Model, Cmd Msg )
receiveSecurityGroupsAndEnsureExoGroup model provider result =
    {- Create an "exosphere" security group unless one already exists -}
    case result of
        Err error ->
            Helpers.processError model error

        Ok securityGroups ->
            let
                newProvider =
                    { provider | securityGroups = securityGroups }

                newModel =
                    Helpers.modelUpdateProvider model newProvider

                cmds =
                    case List.filter (\a -> a.name == "exosphere") securityGroups |> List.head of
                        Just _ ->
                            []

                        Nothing ->
                            [ requestCreateExoSecurityGroup newProvider ]
            in
            ( newModel, Cmd.batch cmds )


receiveCreateExoSecurityGroupAndRequestCreateRules : Model -> Provider -> Result Http.Error OSTypes.SecurityGroup -> ( Model, Cmd Msg )
receiveCreateExoSecurityGroupAndRequestCreateRules model provider result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok newSecGroup ->
            let
                newSecGroups =
                    newSecGroup :: provider.securityGroups

                newProvider =
                    { provider | securityGroups = newSecGroups }

                newModel =
                    Helpers.modelUpdateProvider model newProvider
            in
            requestCreateExoSecurityGroupRules newModel newProvider


receiveCockpitLoginStatus : Model -> Provider -> OSTypes.ServerUuid -> Result Http.Error String -> ( Model, Cmd Msg )
receiveCockpitLoginStatus model provider serverUuid result =
    case Helpers.serverLookup provider serverUuid of
        Nothing ->
            Helpers.processError
                model
                "We should have a server here but we don't"

        Just server ->
            {- This repeats a lot of code in receiveFloatingIp, badly needs a refactor -}
            let
                cockpitStatus =
                    case result of
                        -- TODO more error chcking, e.g. handle case of invalid credentials rather than telling user "still not ready yet"
                        Err error ->
                            CheckedNotReady

                        Ok str ->
                            Ready

                oldExoProps =
                    server.exoProps

                newServer =
                    Server server.osProps { oldExoProps | cockpitStatus = cockpitStatus }

                newProvider =
                    Helpers.providerUpdateServer provider newServer

                newModel =
                    Helpers.modelUpdateProvider model newProvider
            in
            ( newModel, Cmd.none )



{- JSON Decoders -}


decodeAuthToken : Http.Response String -> Result String OSTypes.AuthToken
decodeAuthToken response =
    case Decode.decodeString decodeAuthTokenDetails response.body of
        Err error ->
            Err (Debug.toString error)

        Ok tokenDetailsWithoutTokenString ->
            let
                authTokenString =
                    Maybe.withDefault "" (Dict.get "X-Subject-Token" response.headers)
            in
            Ok (tokenDetailsWithoutTokenString authTokenString)


decodeAuthTokenDetails : Decode.Decoder (OSTypes.AuthTokenString -> OSTypes.AuthToken)
decodeAuthTokenDetails =
    let
        iso8601StringToPosixDecodeError str =
            case Helpers.iso8601StringToPosix str of
                Ok posix ->
                    Decode.succeed posix

                Err error ->
                    Decode.fail error
    in
    Decode.map6 OSTypes.AuthToken
        (Decode.at [ "token", "catalog" ] (Decode.list openstackServiceDecoder))
        (Decode.at [ "token", "project", "id" ] Decode.string)
        (Decode.at [ "token", "project", "name" ] Decode.string)
        (Decode.at [ "token", "user", "id" ] Decode.string)
        (Decode.at [ "token", "user", "name" ] Decode.string)
        (Decode.at [ "token", "expires_at" ] Decode.string
            |> Decode.andThen iso8601StringToPosixDecodeError
        )


openstackServiceDecoder : Decode.Decoder OSTypes.Service
openstackServiceDecoder =
    Decode.map3 OSTypes.Service
        (Decode.field "name" Decode.string)
        (Decode.field "type" Decode.string)
        (Decode.field "endpoints" (Decode.list openstackEndpointDecoder))


openstackEndpointDecoder : Decode.Decoder OSTypes.Endpoint
openstackEndpointDecoder =
    Decode.map2 OSTypes.Endpoint
        (Decode.field "interface" Decode.string
            |> Decode.andThen openstackEndpointInterfaceDecoder
        )
        (Decode.field "url" Decode.string)


openstackEndpointInterfaceDecoder : String -> Decode.Decoder OSTypes.EndpointInterface
openstackEndpointInterfaceDecoder interface =
    case interface of
        "public" ->
            Decode.succeed OSTypes.Public

        "admin" ->
            Decode.succeed OSTypes.Admin

        "internal" ->
            Decode.succeed OSTypes.Internal

        _ ->
            Decode.fail "unrecognized interface type"


decodeImages : Decode.Decoder (List OSTypes.Image)
decodeImages =
    Decode.field "images" (Decode.list imageDecoder)


imageDecoder : Decode.Decoder OSTypes.Image
imageDecoder =
    Decode.map8 OSTypes.Image
        (Decode.field "name" Decode.string)
        (Decode.field "status" Decode.string |> Decode.andThen imageStatusDecoder)
        (Decode.field "id" Decode.string)
        (Decode.field "size" (Decode.nullable Decode.int))
        (Decode.field "checksum" (Decode.nullable Decode.string))
        (Decode.field "disk_format" (Decode.nullable Decode.string))
        (Decode.field "container_format" (Decode.nullable Decode.string))
        (Decode.field "tags" (Decode.list Decode.string))


imageStatusDecoder : String -> Decode.Decoder OSTypes.ImageStatus
imageStatusDecoder status =
    case status of
        "queued" ->
            Decode.succeed OSTypes.ImageQueued

        "saving" ->
            Decode.succeed OSTypes.ImageSaving

        "active" ->
            Decode.succeed OSTypes.ImageActive

        "killed" ->
            Decode.succeed OSTypes.ImageKilled

        "deleted" ->
            Decode.succeed OSTypes.ImageDeleted

        "pending_delete" ->
            Decode.succeed OSTypes.ImagePendingDelete

        "deactivated" ->
            Decode.succeed OSTypes.ImageDeactivated

        _ ->
            Decode.fail "Unrecognized image status"


decodeServers : Decode.Decoder (List OSTypes.Server)
decodeServers =
    Decode.field "servers" (Decode.list serverDecoder)


serverDecoder : Decode.Decoder OSTypes.Server
serverDecoder =
    Decode.map4 OSTypes.Server
        (Decode.oneOf
            [ Decode.field "name" Decode.string
            , Decode.succeed ""
            ]
        )
        (Decode.field "id" Decode.string)
        (Decode.succeed Nothing)
        (Decode.succeed Nothing)


decodeServerDetails : Decode.Decoder OSTypes.ServerDetails
decodeServerDetails =
    let
        flattenAddressesObject kVPairs =
            {- Takes a list of key-value pairs, the keys being network names and the values being OSTypes.IpAddress
               Returns a flat list of OSTypes.IpAddress
            -}
            List.foldl (\kVPair resultList -> Tuple.second kVPair :: resultList) [] kVPairs
                |> List.concat
    in
    Decode.map8 OSTypes.ServerDetails
        (Decode.at [ "server", "status" ] Decode.string |> Decode.andThen serverOpenstackStatusDecoder)
        (Decode.at [ "server", "created" ] Decode.string)
        (Decode.at [ "server", "OS-EXT-STS:power_state" ] Decode.int
            |> Decode.andThen serverPowerStateDecoder
        )
        (Decode.oneOf
            [ Decode.at [ "server", "image", "id" ] Decode.string
            , Decode.succeed ""
            ]
        )
        (Decode.at [ "server", "flavor", "id" ] Decode.string)
        (Decode.at [ "server", "key_name" ] (Decode.nullable Decode.string))
        (Decode.oneOf
            [ Decode.at [ "server", "addresses" ] (Decode.map flattenAddressesObject (Decode.keyValuePairs (Decode.list serverIpAddressDecoder)))
            , Decode.succeed []
            ]
        )
        (Decode.at [ "server", "metadata" ] metadataDecoder)


serverOpenstackStatusDecoder : String -> Decode.Decoder OSTypes.ServerStatus
serverOpenstackStatusDecoder status =
    case String.toLower status of
        "paused" ->
            Decode.succeed OSTypes.ServerPaused

        "suspended" ->
            Decode.succeed OSTypes.ServerSuspended

        "active" ->
            Decode.succeed OSTypes.ServerActive

        "shutoff" ->
            Decode.succeed OSTypes.ServerShutoff

        "rescued" ->
            Decode.succeed OSTypes.ServerRescued

        "stopped" ->
            Decode.succeed OSTypes.ServerStopped

        "soft_deleted" ->
            Decode.succeed OSTypes.ServerSoftDeleted

        "error" ->
            Decode.succeed OSTypes.ServerError

        "build" ->
            Decode.succeed OSTypes.ServerBuilding

        "shelved" ->
            Decode.succeed OSTypes.ServerShelved

        "shelved_offloaded" ->
            Decode.succeed OSTypes.ServerShelvedOffloaded

        _ ->
            Decode.fail "Ooooooops, unrecognised server OpenStack status"


serverPowerStateDecoder : Int -> Decode.Decoder OSTypes.ServerPowerState
serverPowerStateDecoder int =
    case int of
        0 ->
            Decode.succeed OSTypes.PowerNoState

        1 ->
            Decode.succeed OSTypes.PowerRunning

        3 ->
            Decode.succeed OSTypes.PowerPaused

        4 ->
            Decode.succeed OSTypes.PowerShutdown

        6 ->
            Decode.succeed OSTypes.PowerCrashed

        7 ->
            Decode.succeed OSTypes.PowerSuspended

        _ ->
            Decode.fail "Ooooooops, unrecognised server power state"


serverIpAddressDecoder : Decode.Decoder OSTypes.IpAddress
serverIpAddressDecoder =
    Decode.map3 OSTypes.IpAddress
        (Decode.succeed Nothing)
        (Decode.field "addr" Decode.string)
        (Decode.field "OS-EXT-IPS:type" Decode.string
            |> Decode.andThen ipAddressOpenstackTypeDecoder
        )


ipAddressOpenstackTypeDecoder : String -> Decode.Decoder OSTypes.IpAddressType
ipAddressOpenstackTypeDecoder string =
    case string of
        "fixed" ->
            Decode.succeed OSTypes.IpAddressFixed

        "floating" ->
            Decode.succeed OSTypes.IpAddressFloating

        _ ->
            Decode.fail "oooooooops, unrecognised IP address type"


metadataDecoder : Decode.Decoder (List OSTypes.MetadataItem)
metadataDecoder =
    {- There has got to be a better way to do this -}
    Decode.keyValuePairs Decode.string
        |> Decode.map (\pairs -> List.map (\pair -> OSTypes.MetadataItem (Tuple.first pair) (Tuple.second pair)) pairs)


decodeConsoleUrl : Decode.Decoder OSTypes.ConsoleUrl
decodeConsoleUrl =
    Decode.at [ "console", "url" ] Decode.string


decodeFlavors : Decode.Decoder (List OSTypes.Flavor)
decodeFlavors =
    Decode.field "flavors" (Decode.list flavorDecoder)


flavorDecoder : Decode.Decoder OSTypes.Flavor
flavorDecoder =
    Decode.map6 OSTypes.Flavor
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "vcpus" Decode.int)
        (Decode.field "ram" Decode.int)
        (Decode.field "disk" Decode.int)
        (Decode.field "OS-FLV-EXT-DATA:ephemeral" Decode.int)


decodeKeypairs : Decode.Decoder (List OSTypes.Keypair)
decodeKeypairs =
    Decode.field "keypairs" (Decode.list keypairDecoder)


keypairDecoder : Decode.Decoder OSTypes.Keypair
keypairDecoder =
    Decode.map3 OSTypes.Keypair
        (Decode.at [ "keypair", "name" ] Decode.string)
        (Decode.at [ "keypair", "public_key" ] Decode.string)
        (Decode.at [ "keypair", "fingerprint" ] Decode.string)


decodeNetworks : Decode.Decoder (List OSTypes.Network)
decodeNetworks =
    Decode.field "networks" (Decode.list networkDecoder)


networkDecoder : Decode.Decoder OSTypes.Network
networkDecoder =
    Decode.map5 OSTypes.Network
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "admin_state_up" Decode.bool)
        (Decode.field "status" Decode.string)
        (Decode.field "router:external" Decode.bool)


decodeFloatingIps : Decode.Decoder (List OSTypes.IpAddress)
decodeFloatingIps =
    Decode.field "floatingips" (Decode.list floatingIpDecoder)


floatingIpDecoder : Decode.Decoder OSTypes.IpAddress
floatingIpDecoder =
    Decode.map3 OSTypes.IpAddress
        (Decode.field "id" Decode.string |> Decode.map (\i -> Just i))
        (Decode.field "floating_ip_address" Decode.string)
        (Decode.succeed OSTypes.IpAddressFloating)


decodePorts : Decode.Decoder (List OSTypes.Port)
decodePorts =
    Decode.field "ports" (Decode.list portDecoder)


portDecoder : Decode.Decoder OSTypes.Port
portDecoder =
    Decode.map4 OSTypes.Port
        (Decode.field "id" Decode.string)
        (Decode.field "device_id" Decode.string)
        (Decode.field "admin_state_up" Decode.bool)
        (Decode.field "status" Decode.string)


decodeFloatingIpCreation : Decode.Decoder OSTypes.IpAddress
decodeFloatingIpCreation =
    Decode.map3 OSTypes.IpAddress
        (Decode.at [ "floatingip", "id" ] Decode.string |> Decode.map (\i -> Just i))
        (Decode.at [ "floatingip", "floating_ip_address" ] Decode.string)
        (Decode.succeed OSTypes.IpAddressFloating)


decodeSecurityGroups : Decode.Decoder (List OSTypes.SecurityGroup)
decodeSecurityGroups =
    Decode.field "security_groups" (Decode.list securityGroupDecoder)


decodeNewSecurityGroup : Decode.Decoder OSTypes.SecurityGroup
decodeNewSecurityGroup =
    Decode.field "security_group" securityGroupDecoder


securityGroupDecoder : Decode.Decoder OSTypes.SecurityGroup
securityGroupDecoder =
    Decode.map4 OSTypes.SecurityGroup
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "description" (Decode.nullable Decode.string))
        (Decode.field "security_group_rules" (Decode.list securityGroupRuleDecoder))


securityGroupRuleDecoder : Decode.Decoder OSTypes.SecurityGroupRule
securityGroupRuleDecoder =
    Decode.map7 OSTypes.SecurityGroupRule
        (Decode.field "id" Decode.string)
        (Decode.field "ethertype" Decode.string |> Decode.andThen securityGroupRuleEthertypeDecoder)
        (Decode.field "direction" Decode.string |> Decode.andThen securityGroupRuleDirectionDecoder)
        (Decode.field "protocol" (Decode.nullable (Decode.string |> Decode.andThen securityGroupRuleProtocolDecoder)))
        (Decode.field "port_range_min" (Decode.nullable Decode.int))
        (Decode.field "port_range_max" (Decode.nullable Decode.int))
        (Decode.field "remote_group_id" (Decode.nullable Decode.string))


securityGroupRuleEthertypeDecoder : String -> Decode.Decoder OSTypes.SecurityGroupRuleEthertype
securityGroupRuleEthertypeDecoder ethertype =
    case ethertype of
        "IPv4" ->
            Decode.succeed OSTypes.Ipv4

        "IPv6" ->
            Decode.succeed OSTypes.Ipv6

        _ ->
            Decode.fail "Ooooooops, unrecognised security group rule ethertype"


securityGroupRuleDirectionDecoder : String -> Decode.Decoder OSTypes.SecurityGroupRuleDirection
securityGroupRuleDirectionDecoder dir =
    case dir of
        "ingress" ->
            Decode.succeed OSTypes.Ingress

        "egress" ->
            Decode.succeed OSTypes.Egress

        _ ->
            Decode.fail "Ooooooops, unrecognised security group rule direction"


securityGroupRuleProtocolDecoder : String -> Decode.Decoder OSTypes.SecurityGroupRuleProtocol
securityGroupRuleProtocolDecoder prot =
    case prot of
        "any" ->
            Decode.succeed OSTypes.AnyProtocol

        "icmp" ->
            Decode.succeed OSTypes.Icmp

        "icmpv6" ->
            Decode.succeed OSTypes.Icmpv6

        "tcp" ->
            Decode.succeed OSTypes.Tcp

        "udp" ->
            Decode.succeed OSTypes.Udp

        _ ->
            Decode.fail "Ooooooops, unrecognised security group rule protocol"
