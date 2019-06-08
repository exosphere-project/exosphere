module Rest.Rest exposing
    ( addFloatingIpInServerDetails
    , createProject
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
    , receiveServer
    , receiveServers
    , requestAuthToken
    , requestConsoleUrls
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
    , requestServer
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
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import OpenStack.Types as OSTypes
import RemoteData
import Rest.Helpers exposing (..)
import Types.HelperTypes as HelperTypes
import Types.Types exposing (..)



{- HTTP Requests -}


requestAuthToken : Maybe HelperTypes.Url -> Creds -> Cmd Msg
requestAuthToken maybeProxyUrl creds =
    let
        idOrName str =
            if Helpers.stringIsUuidOrDefault str then
                "id"

            else
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

        correctedUrlPath =
            if String.contains "/auth/tokens" creds.authUrl then
                -- We previously expected users to provide "/auth/tokens" to be in the Keystone Auth URL; this case statement avoids breaking the app for users who still have that
                creds.authUrl

            else
                creds.authUrl ++ "/auth/tokens"

        ( url, headers ) =
            case maybeProxyUrl of
                Nothing ->
                    ( correctedUrlPath, [] )

                Just proxyUrl ->
                    proxyifyRequest proxyUrl correctedUrlPath
    in
    {- https://stackoverflow.com/questions/44368340/get-request-headers-from-http-request -}
    Http.request
        { method = "POST"
        , headers = headers
        , url = url
        , body = Http.jsonBody requestBody

        {- Todo handle no response? -}
        , expect =
            Http.expectStringResponse
                (ReceiveAuthToken creds)
                (\response ->
                    case response of
                        Http.BadUrl_ url_ ->
                            Err (Http.BadUrl url_)

                        Http.Timeout_ ->
                            Err Http.Timeout

                        Http.NetworkError_ ->
                            Err Http.NetworkError

                        Http.BadStatus_ metadata _ ->
                            Err (Http.BadStatus metadata.statusCode)

                        Http.GoodStatus_ metadata body ->
                            Ok ( metadata, body )
                )
        , timeout = Nothing
        , tracker = Nothing
        }


requestImages : Project -> Maybe HelperTypes.Url -> Cmd Msg
requestImages project maybeProxyUrl =
    openstackCredentialedRequest
        project
        maybeProxyUrl
        Get
        (project.endpoints.glance ++ "/v2/images?limit=999999")
        Http.emptyBody
        (Http.expectJson (\result -> ProjectMsg (Helpers.getProjectId project) (ReceiveImages result)) decodeImages)


requestServers : Project -> Maybe HelperTypes.Url -> Cmd Msg
requestServers project maybeProxyUrl =
    openstackCredentialedRequest
        project
        maybeProxyUrl
        Get
        (project.endpoints.nova ++ "/servers/detail")
        Http.emptyBody
        (Http.expectJson
            (\result -> ProjectMsg (Helpers.getProjectId project) (ReceiveServers result))
            decodeServers
        )


requestServer : Project -> Maybe HelperTypes.Url -> OSTypes.ServerUuid -> Cmd Msg
requestServer project maybeProxyUrl serverUuid =
    openstackCredentialedRequest
        project
        maybeProxyUrl
        Get
        (project.endpoints.nova ++ "/servers/" ++ serverUuid)
        Http.emptyBody
        (Http.expectJson
            (\result -> ProjectMsg (Helpers.getProjectId project) (ReceiveServer serverUuid result))
            (Decode.at [ "server" ] decodeServerDetails)
        )


requestConsoleUrls : Project -> Maybe HelperTypes.Url -> OSTypes.ServerUuid -> Cmd Msg
requestConsoleUrls project maybeProxyUrl serverUuid =
    -- This is a deprecated call, will eventually need to be updated
    -- See https://gitlab.com/exosphere/exosphere/issues/183
    let
        reqParams =
            [ { objectName = "os-getVNCConsole"
              , consoleType = "novnc"
              }
            , { objectName = "os-getSPICEConsole"
              , consoleType = "spice-html5"
              }
            ]

        buildReq params =
            let
                reqBody =
                    Encode.object
                        [ ( params.objectName
                          , Encode.object
                                [ ( "type", Encode.string params.consoleType )
                                ]
                          )
                        ]
            in
            openstackCredentialedRequest
                project
                maybeProxyUrl
                Post
                (project.endpoints.nova ++ "/servers/" ++ serverUuid ++ "/action")
                (Http.jsonBody reqBody)
                (Http.expectJson
                    (\result -> ProjectMsg (Helpers.getProjectId project) (ReceiveConsoleUrl serverUuid result))
                    decodeConsoleUrl
                )
    in
    List.map buildReq reqParams
        |> Cmd.batch


requestFlavors : Project -> Maybe HelperTypes.Url -> Cmd Msg
requestFlavors project maybeProxyUrl =
    openstackCredentialedRequest
        project
        maybeProxyUrl
        Get
        (project.endpoints.nova ++ "/flavors/detail")
        Http.emptyBody
        (Http.expectJson
            (\result -> ProjectMsg (Helpers.getProjectId project) (ReceiveFlavors result))
            decodeFlavors
        )


requestKeypairs : Project -> Maybe HelperTypes.Url -> Cmd Msg
requestKeypairs project maybeProxyUrl =
    openstackCredentialedRequest
        project
        maybeProxyUrl
        Get
        (project.endpoints.nova ++ "/os-keypairs")
        Http.emptyBody
        (Http.expectJson
            (\result -> ProjectMsg (Helpers.getProjectId project) (ReceiveKeypairs result))
            decodeKeypairs
        )


requestCreateServer : Project -> Maybe HelperTypes.Url -> CreateServerRequest -> Cmd Msg
requestCreateServer project maybeProxyUrl createServerRequest =
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

        renderedUserData =
            Helpers.renderUserDataTemplate project createServerRequest

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
                , ( "user_data", Encode.string (Base64.encode renderedUserData) )
                , ( "security_groups", Encode.array Encode.object (Array.fromList [ [ ( "name", Encode.string "exosphere" ) ] ]) )
                , ( "adminPass", Encode.string createServerRequest.exouserPassword )
                , ( "metadata", Encode.object [ ( "exouserPassword", Encode.string createServerRequest.exouserPassword ) ] )
                ]

        buildRequestOuterJson props =
            Encode.object [ ( "server", Encode.object props ) ]

        buildRequestBody instanceName =
            if not createServerRequest.volBacked then
                ( "imageRef", Encode.string createServerRequest.imageUuid )
                    :: baseServerProps createServerRequest instanceName
                    |> buildRequestOuterJson

            else
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

        serverUuidDecoder : Decode.Decoder OSTypes.ServerUuid
        serverUuidDecoder =
            Decode.field "id" Decode.string
    in
    Cmd.batch
        (requestBodies
            |> List.map
                (\requestBody ->
                    openstackCredentialedRequest
                        project
                        maybeProxyUrl
                        Post
                        (project.endpoints.nova ++ "/servers")
                        (Http.jsonBody requestBody)
                        (Http.expectJson
                            (\result -> ProjectMsg (Helpers.getProjectId project) (ReceiveCreateServer result))
                            (Decode.field "server" serverUuidDecoder)
                        )
                )
        )


requestDeleteServer : Project -> Maybe HelperTypes.Url -> Server -> Cmd Msg
requestDeleteServer project maybeProxyUrl server =
    let
        getFloatingIp =
            server.osProps.details.ipAddresses
                |> Helpers.getServerFloatingIp
    in
    openstackCredentialedRequest
        project
        maybeProxyUrl
        Delete
        (project.endpoints.nova ++ "/servers/" ++ server.osProps.uuid)
        Http.emptyBody
        (Http.expectString
            (\result -> ProjectMsg (Helpers.getProjectId project) (ReceiveDeleteServer server.osProps.uuid getFloatingIp result))
        )


requestDeleteServers : Project -> Maybe HelperTypes.Url -> List Server -> Cmd Msg
requestDeleteServers project maybeProxyUrl serversToDelete =
    let
        deleteRequests =
            List.map (requestDeleteServer project maybeProxyUrl) serversToDelete
    in
    Cmd.batch deleteRequests


requestNetworks : Project -> Maybe HelperTypes.Url -> Cmd Msg
requestNetworks project maybeProxyUrl =
    openstackCredentialedRequest
        project
        maybeProxyUrl
        Get
        (project.endpoints.neutron ++ "/v2.0/networks")
        Http.emptyBody
        (Http.expectJson
            (\result -> ProjectMsg (Helpers.getProjectId project) (ReceiveNetworks result))
            decodeNetworks
        )


requestFloatingIps : Project -> Maybe HelperTypes.Url -> Cmd Msg
requestFloatingIps project maybeProxyUrl =
    openstackCredentialedRequest
        project
        maybeProxyUrl
        Get
        (project.endpoints.neutron ++ "/v2.0/floatingips")
        Http.emptyBody
        (Http.expectJson
            (\result -> ProjectMsg (Helpers.getProjectId project) (ReceiveFloatingIps result))
            decodeFloatingIps
        )


getFloatingIpRequestPorts : Project -> Maybe HelperTypes.Url -> Server -> Cmd Msg
getFloatingIpRequestPorts project maybeProxyUrl server =
    openstackCredentialedRequest
        project
        maybeProxyUrl
        Get
        (project.endpoints.neutron ++ "/v2.0/ports")
        Http.emptyBody
        (Http.expectJson
            (\result -> ProjectMsg (Helpers.getProjectId project) (GetFloatingIpReceivePorts server.osProps.uuid result))
            decodePorts
        )


requestCreateFloatingIpIfRequestable : Model -> Project -> Maybe HelperTypes.Url -> OSTypes.Network -> OSTypes.Port -> OSTypes.ServerUuid -> ( Model, Cmd Msg )
requestCreateFloatingIpIfRequestable model project maybeProxyUrl network port_ serverUuid =
    case Helpers.serverLookup project serverUuid of
        Nothing ->
            -- Server not found, may have been deleted, nothing to do
            ( model, Cmd.none )

        Just server ->
            case server.exoProps.floatingIpState of
                Requestable ->
                    requestCreateFloatingIp model project maybeProxyUrl network port_ server

                _ ->
                    ( model, Cmd.none )


requestCreateFloatingIp : Model -> Project -> Maybe HelperTypes.Url -> OSTypes.Network -> OSTypes.Port -> Server -> ( Model, Cmd Msg )
requestCreateFloatingIp model project maybeProxyUrl network port_ server =
    let
        newServer =
            let
                oldExoProps =
                    server.exoProps
            in
            Server server.osProps { oldExoProps | floatingIpState = RequestedWaiting }

        newProject =
            Helpers.projectUpdateServer project newServer

        newModel =
            Helpers.modelUpdateProject model newProject

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
                newProject
                maybeProxyUrl
                Post
                (project.endpoints.neutron ++ "/v2.0/floatingips")
                (Http.jsonBody requestBody)
                (Http.expectJson
                    (\result -> ProjectMsg (Helpers.getProjectId project) (ReceiveCreateFloatingIp server.osProps.uuid result))
                    decodeFloatingIpCreation
                )
    in
    ( newModel, requestCmd )


requestDeleteFloatingIp : Project -> Maybe HelperTypes.Url -> OSTypes.IpAddressUuid -> Cmd Msg
requestDeleteFloatingIp project maybeProxyUrl uuid =
    openstackCredentialedRequest
        project
        maybeProxyUrl
        Delete
        (project.endpoints.neutron ++ "/v2.0/floatingips/" ++ uuid)
        Http.emptyBody
        (Http.expectString
            (\result -> ProjectMsg (Helpers.getProjectId project) (ReceiveDeleteFloatingIp uuid result))
        )


requestSecurityGroups : Project -> Maybe HelperTypes.Url -> Cmd Msg
requestSecurityGroups project maybeProxyUrl =
    openstackCredentialedRequest
        project
        maybeProxyUrl
        Get
        (project.endpoints.neutron ++ "/v2.0/security-groups")
        Http.emptyBody
        (Http.expectJson
            (\result -> ProjectMsg (Helpers.getProjectId project) (ReceiveSecurityGroups result))
            decodeSecurityGroups
        )


requestCreateExoSecurityGroup : Project -> Maybe HelperTypes.Url -> Cmd Msg
requestCreateExoSecurityGroup project maybeProxyUrl =
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
        project
        maybeProxyUrl
        Post
        (project.endpoints.neutron ++ "/v2.0/security-groups")
        (Http.jsonBody requestBody)
        (Http.expectJson
            (\result -> ProjectMsg (Helpers.getProjectId project) (ReceiveCreateExoSecurityGroup result))
            decodeNewSecurityGroup
        )


requestCreateExoSecurityGroupRules : Model -> Project -> Maybe HelperTypes.Url -> ( Model, Cmd Msg )
requestCreateExoSecurityGroupRules model project maybeProxyUrl =
    let
        maybeSecurityGroup =
            List.filter (\g -> g.name == "exosphere") project.securityGroups |> List.head
    in
    case maybeSecurityGroup of
        Nothing ->
            -- No security group found, may have been deleted? Nothing to do
            ( model, Cmd.none )

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
                        project
                        maybeProxyUrl
                        Post
                        (project.endpoints.neutron ++ "/v2.0/security-group-rules")
                        (Http.jsonBody body)
                        (Http.expectString
                            (\result -> ProjectMsg (Helpers.getProjectId project) (ReceiveCreateExoSecurityGroupRules result))
                        )

                bodies =
                    [ makeRequestBody "22" "SSH"
                    , makeRequestBody "9090" "Cockpit"
                    ]

                cmds =
                    List.map (\b -> buildRequestCmd b) bodies
            in
            ( model, Cmd.batch cmds )


requestConsoleUrlIfRequestable : Project -> Maybe HelperTypes.Url -> Server -> Cmd Msg
requestConsoleUrlIfRequestable project maybeProxyUrl server =
    case server.osProps.details.openstackStatus of
        OSTypes.ServerActive ->
            requestConsoleUrls project maybeProxyUrl server.osProps.uuid

        _ ->
            Cmd.none


requestCockpitIfRequestable : Project -> Server -> Cmd Msg
requestCockpitIfRequestable project server =
    let
        serverDetails =
            server.osProps.details

        floatingIpState =
            Helpers.checkFloatingIpState
                serverDetails
                server.exoProps.floatingIpState
    in
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
                    case Helpers.getServerExouserPassword serverDetails of
                        Just password ->
                            requestCockpitLogin project server.osProps.uuid password floatingIp

                        Nothing ->
                            Cmd.none

                -- Maybe in the future show an error here? Missing metadata
                Nothing ->
                    Cmd.none

        -- Maybe in the future show an error here? Missing floating IP
        _ ->
            Cmd.none


requestCockpitLogin : Project -> OSTypes.ServerUuid -> String -> String -> Cmd Msg
requestCockpitLogin project serverUuid password ipAddress =
    let
        authHeaderValue =
            "Basic " ++ Base64.encode ("exouser:" ++ password)

        resultMsg project2 serverUuid2 result =
            ProjectMsg (Helpers.getProjectId project2) (ReceiveCockpitLoginStatus serverUuid2 result)
    in
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" authHeaderValue ]
        , url = "http://" ++ ipAddress ++ ":9090/cockpit/login"
        , body = Http.emptyBody
        , expect = Http.expectString (resultMsg project serverUuid)
        , timeout = Just 3000
        , tracker = Nothing
        }



{- HTTP Response Handling -}


receiveAuthToken : Model -> Creds -> Result Http.Error ( Http.Metadata, String ) -> ( Model, Cmd Msg )
receiveAuthToken model creds responseResult =
    case responseResult of
        Err error ->
            Helpers.processError model error

        Ok ( metadata, response ) ->
            -- If we don't have a project with same name + authUrl then create one, if we do then update its OSTypes.AuthToken
            -- This code ensures we don't end up with duplicate projects on the same provider in our model.
            case
                model.projects
                    |> List.filter (\p -> p.creds.authUrl == creds.authUrl)
                    |> List.filter (\p -> p.creds.projectName == creds.projectName)
                    |> List.head
            of
                Nothing ->
                    createProject model creds (Http.GoodStatus_ metadata response)

                Just project ->
                    projectUpdateAuthToken model project (Http.GoodStatus_ metadata response)


createProject : Model -> Creds -> Http.Response String -> ( Model, Cmd Msg )
createProject model creds response =
    -- Create new project
    case decodeAuthToken response of
        Err error ->
            Helpers.processError model error

        Ok authToken ->
            let
                endpoints =
                    Helpers.serviceCatalogToEndpoints authToken.catalog

                newProject =
                    { creds = creds
                    , auth = authToken

                    -- Maybe todo, eliminate parallel data structures in auth and endpoints?
                    , endpoints = endpoints
                    , images = []
                    , servers = RemoteData.NotAsked
                    , flavors = []
                    , keypairs = []
                    , volumes = RemoteData.NotAsked
                    , networks = []
                    , floatingIps = []
                    , ports = []
                    , securityGroups = []
                    , pendingCredentialedRequests = []
                    }

                newProjects =
                    newProject :: model.projects

                newModel =
                    { model
                        | projects = newProjects
                        , viewState = ProjectView (Helpers.getProjectId newProject) ListProjectServers
                    }
            in
            ( newModel
            , [ requestServers
              , requestSecurityGroups
              , requestFloatingIps
              ]
                |> List.map (\x -> x newProject model.proxyUrl)
                |> Cmd.batch
            )


projectUpdateAuthToken : Model -> Project -> Http.Response String -> ( Model, Cmd Msg )
projectUpdateAuthToken model project response =
    -- Update auth token for existing project
    case decodeAuthToken response of
        Err error ->
            Helpers.processError model error

        Ok authToken ->
            let
                newProject =
                    { project | auth = authToken }

                newModel =
                    Helpers.modelUpdateProject model newProject
            in
            sendPendingRequests newModel newProject


sendPendingRequests : Model -> Project -> ( Model, Cmd Msg )
sendPendingRequests model project =
    -- Fires any pending commands which were waiting for auth token renewal
    -- This function assumes our token is valid (does not check for expiry).
    let
        -- Hydrate cmds with auth token
        cmds =
            List.map (\pqr -> pqr project.auth.tokenValue) project.pendingCredentialedRequests

        -- Clear out pendingCredentialedRequests
        newProject =
            { project | pendingCredentialedRequests = [] }

        newModel =
            Helpers.modelUpdateProject model newProject
    in
    ( newModel, Cmd.batch cmds )


receiveImages : Model -> Project -> Result Http.Error (List OSTypes.Image) -> ( Model, Cmd Msg )
receiveImages model project result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok images ->
            let
                newProject =
                    { project | images = images }

                newModel =
                    Helpers.modelUpdateProject model newProject
            in
            ( newModel, Cmd.none )


receiveServers : Model -> Project -> Result Http.Error (List OSTypes.Server) -> ( Model, Cmd Msg )
receiveServers model project result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok newOpenstackServers ->
            -- Enrich new list of servers with any exoProps and osProps.details from old list of servers
            let
                defaultExoProps =
                    ExoServerProps Unknown False NotChecked False Nothing

                enrichNewServer : OSTypes.Server -> Server
                enrichNewServer newOpenstackServer =
                    case Helpers.serverLookup project newOpenstackServer.uuid of
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

                newServersSorted =
                    List.sortBy (\s -> s.osProps.name) newServers

                newProject =
                    { project | servers = RemoteData.Success newServersSorted }

                newModel =
                    Helpers.modelUpdateProject model newProject

                requestCockpitCommands =
                    List.map (requestCockpitIfRequestable project) newServersSorted
                        |> Cmd.batch
            in
            ( newModel, requestCockpitCommands )


receiveServer : Model -> Project -> OSTypes.ServerUuid -> Result Http.Error OSTypes.ServerDetails -> ( Model, Cmd Msg )
receiveServer model project serverUuid result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok serverDetails ->
            let
                maybeServer =
                    Helpers.serverLookup project serverUuid
            in
            case maybeServer of
                Nothing ->
                    Helpers.processError
                        model
                        "No server found when receiving server details"

                Just server ->
                    let
                        floatingIpState =
                            Helpers.checkFloatingIpState
                                serverDetails
                                server.exoProps.floatingIpState

                        newServer =
                            let
                                oldOSProps =
                                    server.osProps

                                oldExoProps =
                                    server.exoProps

                                newTargetOpenstackStatus =
                                    case oldExoProps.targetOpenstackStatus of
                                        Nothing ->
                                            Nothing

                                        Just statuses ->
                                            if List.member serverDetails.openstackStatus statuses then
                                                Nothing

                                            else
                                                Just statuses
                            in
                            Server
                                { oldOSProps | details = serverDetails }
                                { oldExoProps | floatingIpState = floatingIpState, targetOpenstackStatus = newTargetOpenstackStatus }

                        newProject =
                            Helpers.projectUpdateServer project newServer

                        newModel =
                            Helpers.modelUpdateProject model newProject

                        floatingIpCmd =
                            case floatingIpState of
                                Requestable ->
                                    [ getFloatingIpRequestPorts newProject model.proxyUrl newServer
                                    , requestNetworks project model.proxyUrl
                                    ]
                                        |> Cmd.batch

                                _ ->
                                    Cmd.none

                        consoleUrlCmd =
                            requestConsoleUrlIfRequestable newProject model.proxyUrl newServer

                        cockpitLoginCmd =
                            requestCockpitIfRequestable newProject newServer

                        allCmds =
                            [ floatingIpCmd, consoleUrlCmd, cockpitLoginCmd ]
                                |> Cmd.batch
                    in
                    ( newModel, allCmds )


receiveConsoleUrl : Model -> Project -> OSTypes.ServerUuid -> Result Http.Error OSTypes.ConsoleUrl -> ( Model, Cmd Msg )
receiveConsoleUrl model project serverUuid result =
    let
        maybeServer =
            Helpers.serverLookup project serverUuid
    in
    case maybeServer of
        Nothing ->
            ( model, Cmd.none )

        -- This is an error state (server not found) but probably not one worth throwing an error at the user over. Someone might have just deleted their server
        Just server ->
            case server.osProps.consoleUrl of
                RemoteData.Success _ ->
                    -- Don't overwrite a potentially successful call to get console URL with a failed call
                    ( model, Cmd.none )

                _ ->
                    let
                        consoleUrl =
                            case result of
                                Err error ->
                                    RemoteData.Failure error

                                Ok url ->
                                    RemoteData.Success url

                        oldOsProps =
                            server.osProps

                        newOsProps =
                            { oldOsProps | consoleUrl = consoleUrl }

                        newServer =
                            { server | osProps = newOsProps }

                        newProject =
                            Helpers.projectUpdateServer project newServer

                        newModel =
                            Helpers.modelUpdateProject model newProject
                    in
                    ( newModel, Cmd.none )


receiveFlavors : Model -> Project -> Result Http.Error (List OSTypes.Flavor) -> ( Model, Cmd Msg )
receiveFlavors model project result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok flavors ->
            let
                newProject =
                    { project | flavors = flavors }

                -- If we have a CreateServerRequest with no flavor UUID, populate it with the smallest flavor.
                -- This is the start of a code smell because we need to reach way into the viewState to update
                -- the createServerRequest. Good candidate for future refactoring to bring CreateServerRequest
                -- outside of model.viewState.
                -- This could also benefit from some "railway-oriented programming" to avoid repetition of
                -- "otherwise just model.viewState" statments.
                viewState =
                    case model.viewState of
                        ProjectView _ projectViewConstructor ->
                            case projectViewConstructor of
                                CreateServer createServerRequest ->
                                    if createServerRequest.flavorUuid == "" then
                                        let
                                            maybeSmallestFlavor =
                                                Helpers.sortedFlavors flavors |> List.head
                                        in
                                        case maybeSmallestFlavor of
                                            Just smallestFlavor ->
                                                ProjectView (Helpers.getProjectId project) (CreateServer { createServerRequest | flavorUuid = smallestFlavor.uuid })

                                            Nothing ->
                                                model.viewState

                                    else
                                        model.viewState

                                _ ->
                                    model.viewState

                        _ ->
                            model.viewState

                newModel =
                    Helpers.modelUpdateProject { model | viewState = viewState } newProject
            in
            ( newModel, Cmd.none )


receiveKeypairs : Model -> Project -> Result Http.Error (List OSTypes.Keypair) -> ( Model, Cmd Msg )
receiveKeypairs model project result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok keypairs ->
            let
                newProject =
                    { project | keypairs = keypairs }

                newModel =
                    Helpers.modelUpdateProject model newProject
            in
            ( newModel, Cmd.none )


receiveCreateServer : Model -> Project -> Result Http.Error OSTypes.ServerUuid -> ( Model, Cmd Msg )
receiveCreateServer model project result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok _ ->
            let
                newModel =
                    { model | viewState = ProjectView (Helpers.getProjectId project) ListProjectServers }
            in
            ( newModel
            , [ requestServers
              , requestNetworks
              ]
                |> List.map (\x -> x project model.proxyUrl)
                |> Cmd.batch
            )


receiveDeleteServer : Model -> Project -> OSTypes.ServerUuid -> Result Http.Error String -> ( Model, Cmd Msg )
receiveDeleteServer model project serverUuid result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok _ ->
            let
                newServers =
                    List.filter (\s -> s.osProps.uuid /= serverUuid) (RemoteData.withDefault [] project.servers)

                newProject =
                    { project | servers = RemoteData.Success newServers }

                newModel =
                    Helpers.modelUpdateProject model newProject
            in
            ( newModel, Cmd.none )


receiveNetworks : Model -> Project -> Result Http.Error (List OSTypes.Network) -> ( Model, Cmd Msg )
receiveNetworks model project result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok networks ->
            let
                newProject =
                    { project | networks = networks }

                -- If we have a CreateServerRequest with no network UUID, populate it with a reasonable guess of a private network.
                -- Same comments above (in receiveFlavors) apply here.
                viewState =
                    case model.viewState of
                        ProjectView _ projectViewConstructor ->
                            case projectViewConstructor of
                                CreateServer createServerRequest ->
                                    if createServerRequest.networkUuid == "" then
                                        let
                                            defaultNetUuid =
                                                case Helpers.newServerNetworkOptions newProject of
                                                    NoNetsAutoAllocate ->
                                                        "auto"

                                                    OneNet net ->
                                                        net.uuid

                                                    MultipleNetsWithGuess _ guessNet _ ->
                                                        guessNet.uuid
                                        in
                                        ProjectView (Helpers.getProjectId project) (CreateServer { createServerRequest | networkUuid = defaultNetUuid })

                                    else
                                        model.viewState

                                _ ->
                                    model.viewState

                        _ ->
                            model.viewState

                newModel =
                    Helpers.modelUpdateProject { model | viewState = viewState } newProject
            in
            ( newModel, Cmd.none )


receiveFloatingIps : Model -> Project -> Result Http.Error (List OSTypes.IpAddress) -> ( Model, Cmd Msg )
receiveFloatingIps model project result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok floatingIps ->
            let
                newProject =
                    { project | floatingIps = floatingIps }

                newModel =
                    Helpers.modelUpdateProject model newProject
            in
            ( newModel, Cmd.none )


receivePortsAndRequestFloatingIp : Model -> Project -> OSTypes.ServerUuid -> Result Http.Error (List OSTypes.Port) -> ( Model, Cmd Msg )
receivePortsAndRequestFloatingIp model project serverUuid result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok ports ->
            let
                newProject =
                    { project | ports = ports }

                newModel =
                    Helpers.modelUpdateProject model newProject

                maybeExtNet =
                    Helpers.getExternalNetwork newProject

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
                                newProject
                                model.proxyUrl
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


receiveCreateFloatingIp : Model -> Project -> OSTypes.ServerUuid -> Result Http.Error OSTypes.IpAddress -> ( Model, Cmd Msg )
receiveCreateFloatingIp model project serverUuid result =
    case Helpers.serverLookup project serverUuid of
        Nothing ->
            -- No server found, may have been deleted, nothing to do
            ( model, Cmd.none )

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

                        newProject =
                            Helpers.projectUpdateServer project newServer

                        newModel =
                            Helpers.modelUpdateProject model newProject
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

                        newProject =
                            Helpers.projectUpdateServer project newServer

                        newModel =
                            Helpers.modelUpdateProject model newProject
                    in
                    ( newModel, Cmd.none )


receiveDeleteFloatingIp : Model -> Project -> OSTypes.IpAddressUuid -> Result Http.Error String -> ( Model, Cmd Msg )
receiveDeleteFloatingIp model project uuid result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok _ ->
            let
                newFloatingIps =
                    List.filter (\f -> f.uuid /= Just uuid) project.floatingIps

                newProject =
                    { project | floatingIps = newFloatingIps }

                newModel =
                    Helpers.modelUpdateProject model newProject
            in
            ( newModel, Cmd.none )


addFloatingIpInServerDetails : OSTypes.ServerDetails -> OSTypes.IpAddress -> OSTypes.ServerDetails
addFloatingIpInServerDetails details ipAddress =
    let
        newIps =
            ipAddress :: details.ipAddresses
    in
    { details | ipAddresses = newIps }


receiveSecurityGroupsAndEnsureExoGroup : Model -> Project -> Result Http.Error (List OSTypes.SecurityGroup) -> ( Model, Cmd Msg )
receiveSecurityGroupsAndEnsureExoGroup model project result =
    {- Create an "exosphere" security group unless one already exists -}
    case result of
        Err error ->
            Helpers.processError model error

        Ok securityGroups ->
            let
                newProject =
                    { project | securityGroups = securityGroups }

                newModel =
                    Helpers.modelUpdateProject model newProject

                cmds =
                    case List.filter (\a -> a.name == "exosphere") securityGroups |> List.head of
                        Just _ ->
                            []

                        Nothing ->
                            [ requestCreateExoSecurityGroup newProject model.proxyUrl ]
            in
            ( newModel, Cmd.batch cmds )


receiveCreateExoSecurityGroupAndRequestCreateRules : Model -> Project -> Result Http.Error OSTypes.SecurityGroup -> ( Model, Cmd Msg )
receiveCreateExoSecurityGroupAndRequestCreateRules model project result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok newSecGroup ->
            let
                newSecGroups =
                    newSecGroup :: project.securityGroups

                newProject =
                    { project | securityGroups = newSecGroups }

                newModel =
                    Helpers.modelUpdateProject model newProject
            in
            requestCreateExoSecurityGroupRules newModel newProject model.proxyUrl


receiveCockpitLoginStatus : Model -> Project -> OSTypes.ServerUuid -> Result Http.Error String -> ( Model, Cmd Msg )
receiveCockpitLoginStatus model project serverUuid result =
    case Helpers.serverLookup project serverUuid of
        Nothing ->
            -- No server found, may have been deleted, nothing to do
            ( model, Cmd.none )

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

                newProject =
                    Helpers.projectUpdateServer project newServer

                newModel =
                    Helpers.modelUpdateProject model newProject
            in
            ( newModel, Cmd.none )



{- JSON Decoders -}


decodeAuthToken : Http.Response String -> Result String OSTypes.AuthToken
decodeAuthToken response =
    case response of
        Http.GoodStatus_ metadata body ->
            case Decode.decodeString decodeAuthTokenDetails body of
                Ok tokenDetailsWithoutTokenString ->
                    let
                        authTokenFromHeader : Result String String
                        authTokenFromHeader =
                            case Dict.get "X-Subject-Token" metadata.headers of
                                Just token ->
                                    Ok token

                                Nothing ->
                                    -- https://github.com/elm/http/issues/31
                                    case Dict.get "x-subject-token" metadata.headers of
                                        Just token2 ->
                                            Ok token2

                                        Nothing ->
                                            Err "Could not find an auth token in response headers"
                    in
                    case authTokenFromHeader of
                        Ok authTokenString ->
                            Ok (tokenDetailsWithoutTokenString authTokenString)

                        Err errStr ->
                            Err errStr

                Err error ->
                    Err (Debug.toString error)

        Http.BadStatus_ _ body ->
            Err (Debug.toString body)

        _ ->
            Err (Debug.toString "foo")


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
        decodeServerDetails
        (Decode.succeed RemoteData.NotAsked)


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
    Decode.succeed OSTypes.ServerDetails
        |> Pipeline.required "status" (Decode.string |> Decode.andThen serverOpenstackStatusDecoder)
        |> Pipeline.required "created" Decode.string
        |> Pipeline.required "OS-EXT-STS:power_state" (Decode.int |> Decode.andThen serverPowerStateDecoder)
        |> Pipeline.optionalAt [ "image", "id" ] Decode.string ""
        |> Pipeline.requiredAt [ "flavor", "id" ] Decode.string
        |> Pipeline.optional "key_name" (Decode.string |> Decode.andThen (\s -> Decode.succeed <| Just s)) Nothing
        |> Pipeline.optional "addresses" (Decode.map flattenAddressesObject (Decode.keyValuePairs (Decode.list serverIpAddressDecoder))) []
        |> Pipeline.required "metadata" metadataDecoder
        |> Pipeline.required "os-extended-volumes:volumes_attached" (Decode.list (Decode.at [ "id" ] Decode.string))


serverOpenstackStatusDecoder : String -> Decode.Decoder OSTypes.ServerStatus
serverOpenstackStatusDecoder status =
    case String.toLower status of
        "paused" ->
            Decode.succeed OSTypes.ServerPaused

        "suspended" ->
            Decode.succeed OSTypes.ServerSuspended

        "active" ->
            Decode.succeed OSTypes.ServerActive

        "reboot" ->
            Decode.succeed OSTypes.ServerReboot

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
