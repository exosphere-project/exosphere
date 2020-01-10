module Rest.Rest exposing
    ( addFloatingIpInServerDetails
    , decodeFlavors
    , decodeFloatingIpCreation
    , decodeImages
    , decodeKeypairs
    , decodeNetworks
    , decodePorts
    , decodeScopedAuthToken
    , decodeServerDetails
    , decodeServers
    , decodeUnscopedAuthToken
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
    , requestAppCredential
    , requestConsoleUrls
    , requestCreateExoSecurityGroupRules
    , requestCreateFloatingIp
    , requestCreateFloatingIpIfRequestable
    , requestCreateServer
    , requestCreateServerImage
    , requestDeleteFloatingIp
    , requestDeleteServer
    , requestDeleteServers
    , requestFlavors
    , requestFloatingIps
    , requestImages
    , requestKeypairs
    , requestNetworks
    , requestScopedAuthToken
    , requestSecurityGroups
    , requestServer
    , requestServers
    , requestUnscopedAuthToken
    , requestUnscopedProjects
    , serverDecoder
    , serverIpAddressDecoder
    , serverPowerStateDecoder
    )

import Array
import Base64
import Dict
import Error exposing (ErrorContext, ErrorLevel(..))
import Helpers.Helpers as Helpers
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import OpenStack.Types as OSTypes
import RemoteData
import Rest.Helpers exposing (idOrName, iso8601StringToPosixDecodeError, keystoneUrlWithVersion, openstackCredentialedRequest, proxyifyRequest, resultToMsg)
import Time
import Types.HelperTypes as HelperTypes
import Types.Types
    exposing
        ( CockpitLoginStatus(..)
        , CreateServerRequest
        , ExoServerProps
        , FloatingIpState(..)
        , HttpRequestMethod(..)
        , Model
        , Msg(..)
        , NewServerNetworkOptions(..)
        , Project
        , ProjectSpecificMsgConstructor(..)
        , ProjectViewConstructor(..)
        , Server
        , UnscopedProvider
        , UnscopedProviderProject
        , ViewState(..)
        )
import Url



{- HTTP Requests -}


requestScopedAuthToken : Maybe HelperTypes.Url -> OSTypes.CredentialsForAuthToken -> Cmd Msg
requestScopedAuthToken maybeProxyUrl input =
    let
        requestBody =
            case input of
                OSTypes.AppCreds _ _ appCred ->
                    Encode.object
                        [ ( "auth"
                          , Encode.object
                                [ ( "identity"
                                  , Encode.object
                                        [ ( "methods", Encode.list Encode.string [ "application_credential" ] )
                                        , ( "application_credential"
                                          , Encode.object
                                                [ ( "id", Encode.string appCred.uuid )
                                                , ( "secret", Encode.string appCred.secret )
                                                ]
                                          )
                                        ]
                                  )
                                ]
                          )
                        ]

                OSTypes.PasswordCreds creds ->
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

        inputUrl =
            case input of
                OSTypes.PasswordCreds creds ->
                    creds.authUrl

                OSTypes.AppCreds url _ _ ->
                    url

        maybePassword =
            case input of
                OSTypes.PasswordCreds c ->
                    Just c.password

                _ ->
                    Nothing

        errorContext =
            let
                projectLabel =
                    case input of
                        OSTypes.AppCreds _ projectName _ ->
                            projectName

                        OSTypes.PasswordCreds creds ->
                            creds.projectName
            in
            ErrorContext
                ("log into OpenStack project named \"" ++ projectLabel ++ "\"")
                ErrorCrit
                (Just "Check with your cloud administrator to ensure you have access to this project.")
    in
    requestAuthTokenHelper
        requestBody
        inputUrl
        maybeProxyUrl
        (resultToMsg errorContext (ReceiveScopedAuthToken maybePassword))


requestUnscopedAuthToken : Maybe HelperTypes.Url -> OSTypes.OpenstackLogin -> Cmd Msg
requestUnscopedAuthToken maybeProxyUrl creds =
    let
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
                        ]
                  )
                ]

        errorContext =
            ErrorContext
                "log into OpenStack"
                ErrorCrit
                (Just "Make sure your login credentials including password are correct!")
    in
    requestAuthTokenHelper
        requestBody
        creds.authUrl
        maybeProxyUrl
        (resultToMsg errorContext (ReceiveUnscopedAuthToken creds.authUrl creds.password))


requestAuthTokenHelper : Encode.Value -> HelperTypes.Url -> Maybe HelperTypes.Url -> (Result Http.Error ( Http.Metadata, String ) -> Msg) -> Cmd Msg
requestAuthTokenHelper requestBody authUrl maybeProxyUrl resultMsg =
    let
        correctedUrl =
            let
                maybeUrl =
                    Url.fromString authUrl
            in
            case maybeUrl of
                -- Cannot parse URL, so uh, don't make changes to it. We should never be here
                Nothing ->
                    authUrl

                Just url_ ->
                    { url_ | path = "/v3/auth/tokens" } |> Url.toString

        ( finalUrl, headers ) =
            case maybeProxyUrl of
                Nothing ->
                    ( correctedUrl, [] )

                Just proxyUrl ->
                    proxyifyRequest proxyUrl correctedUrl
    in
    {- https://stackoverflow.com/questions/44368340/get-request-headers-from-http-request -}
    Http.request
        { method = "POST"
        , headers = headers
        , url = finalUrl
        , body = Http.jsonBody requestBody

        {- Todo handle no response? -}
        , expect =
            Http.expectStringResponse
                resultMsg
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


requestAppCredential : Project -> Maybe HelperTypes.Url -> Time.Posix -> Cmd Msg
requestAppCredential project maybeProxyUrl posixTime =
    let
        appCredentialName =
            "exosphere-" ++ (String.fromInt <| Time.posixToMillis posixTime)

        requestBody =
            Encode.object
                [ ( "application_credential"
                  , Encode.object
                        [ ( "name", Encode.string appCredentialName )
                        ]
                  )
                ]

        urlWithVersion =
            keystoneUrlWithVersion project.endpoints.keystone

        errorContext =
            ErrorContext
                ("request application credential for project named \"" ++ project.auth.project.name ++ "\"")
                ErrorCrit
                (Just "Perhaps you are trying to use a cloud that is too old to support Application Credentials? Exosphere supports OpenStack Queens release and newer. Check with your cloud administrator if you are unsure.")

        resultToMsg_ =
            resultToMsg
                errorContext
                (\appCred ->
                    ProjectMsg
                        (Helpers.getProjectId project)
                        (ReceiveAppCredential appCred)
                )
    in
    openstackCredentialedRequest
        project
        maybeProxyUrl
        Post
        (urlWithVersion ++ "/users/" ++ project.auth.user.uuid ++ "/application_credentials")
        (Http.jsonBody requestBody)
        (Http.expectJson resultToMsg_ decodeAppCredential)


requestUnscopedProjects : UnscopedProvider -> Maybe HelperTypes.Url -> Cmd Msg
requestUnscopedProjects provider maybeProxyUrl =
    let
        correctedUrl =
            let
                maybeUrl =
                    Url.fromString provider.authUrl
            in
            case maybeUrl of
                -- Cannot parse URL, so uh, don't make changes to it. We should never be here
                Nothing ->
                    provider.authUrl

                Just url_ ->
                    { url_ | path = "/v3/users/" ++ provider.token.user.uuid ++ "/projects" } |> Url.toString

        ( url, headers ) =
            case maybeProxyUrl of
                Just proxyUrl ->
                    proxyifyRequest proxyUrl correctedUrl

                Nothing ->
                    ( correctedUrl, [] )

        errorContext =
            ErrorContext
                ("get a list of projects accessible by user \"" ++ provider.token.user.name ++ "\"")
                ErrorCrit
                Nothing

        expect =
            Http.expectJson
                (resultToMsg
                    errorContext
                    (ReceiveUnscopedProjects provider.authUrl)
                )
                decodeUnscopedProjects
    in
    Http.request
        { method = "GET"
        , headers = Http.header "X-Auth-Token" provider.token.tokenValue :: headers
        , url = url
        , body = Http.emptyBody
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }


requestImages : Project -> Maybe HelperTypes.Url -> Cmd Msg
requestImages project maybeProxyUrl =
    let
        errorContext =
            ErrorContext
                ("get a list of images for project \"" ++ project.auth.project.name ++ "\"")
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsg
                errorContext
                (\images -> ProjectMsg (Helpers.getProjectId project) <| ReceiveImages images)
    in
    openstackCredentialedRequest
        project
        maybeProxyUrl
        Get
        (project.endpoints.glance ++ "/v2/images?limit=999999")
        Http.emptyBody
        (Http.expectJson
            resultToMsg_
            decodeImages
        )


requestServers : Project -> Maybe HelperTypes.Url -> Cmd Msg
requestServers project maybeProxyUrl =
    let
        errorContext =
            ErrorContext
                ("get details of servers for project \"" ++ project.auth.project.name ++ "\"")
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsg
                errorContext
                (\servers ->
                    ProjectMsg
                        (Helpers.getProjectId project)
                        (ReceiveServers servers)
                )
    in
    openstackCredentialedRequest
        project
        maybeProxyUrl
        Get
        (project.endpoints.nova ++ "/servers/detail")
        Http.emptyBody
        (Http.expectJson
            resultToMsg_
            decodeServers
        )


requestServer : Project -> Maybe HelperTypes.Url -> OSTypes.ServerUuid -> Cmd Msg
requestServer project maybeProxyUrl serverUuid =
    let
        errorContext =
            ErrorContext
                ("get details of server with UUID \"" ++ serverUuid ++ "\"")
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsg
                errorContext
                (\server ->
                    ProjectMsg
                        (Helpers.getProjectId project)
                        (ReceiveServer serverUuid server)
                )
    in
    openstackCredentialedRequest
        project
        maybeProxyUrl
        Get
        (project.endpoints.nova ++ "/servers/" ++ serverUuid)
        Http.emptyBody
        (Http.expectJson
            resultToMsg_
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
    let
        errorContext =
            ErrorContext
                ("get details of flavors for project \"" ++ project.auth.project.name ++ "\"")
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsg
                errorContext
                (\flavors -> ProjectMsg (Helpers.getProjectId project) <| ReceiveFlavors flavors)
    in
    openstackCredentialedRequest
        project
        maybeProxyUrl
        Get
        (project.endpoints.nova ++ "/flavors/detail")
        Http.emptyBody
        (Http.expectJson
            resultToMsg_
            decodeFlavors
        )


requestKeypairs : Project -> Maybe HelperTypes.Url -> Cmd Msg
requestKeypairs project maybeProxyUrl =
    let
        errorContext =
            ErrorContext
                ("get details of keypairs for project \"" ++ project.auth.project.name ++ "\"")
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsg
                errorContext
                (\keypairs -> ProjectMsg (Helpers.getProjectId project) <| ReceiveKeypairs keypairs)
    in
    openstackCredentialedRequest
        project
        maybeProxyUrl
        Get
        (project.endpoints.nova ++ "/os-keypairs")
        Http.emptyBody
        (Http.expectJson
            resultToMsg_
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
                , if innerCreateServerRequest.networkUuid == "auto" then
                    ( "networks", Encode.string "auto" )

                  else
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

        errorContext =
            let
                plural =
                    case createServerRequest.count of
                        "1" ->
                            ""

                        _ ->
                            "s"
            in
            ErrorContext
                ("create " ++ createServerRequest.count ++ " server" ++ plural)
                ErrorCrit
                (Just <| "It's possible your quota is not large enough to launch the requested server" ++ plural)

        resultToMsg_ =
            resultToMsg
                errorContext
                (\serverUuid ->
                    ProjectMsg
                        (Helpers.getProjectId project)
                        (ReceiveCreateServer serverUuid)
                )
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
                            resultToMsg_
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

        errorContext =
            ErrorContext
                ("delete server with UUID " ++ server.osProps.uuid)
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsg
                errorContext
                (\_ ->
                    ProjectMsg
                        (Helpers.getProjectId project)
                        (ReceiveDeleteServer server.osProps.uuid getFloatingIp)
                )
    in
    openstackCredentialedRequest
        project
        maybeProxyUrl
        Delete
        (project.endpoints.nova ++ "/servers/" ++ server.osProps.uuid)
        Http.emptyBody
        (Http.expectString
            resultToMsg_
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
    let
        errorContext =
            ErrorContext
                ("get list of networks for project \"" ++ project.auth.project.name ++ "\"")
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsg
                errorContext
                (\nets ->
                    ProjectMsg
                        (Helpers.getProjectId project)
                        (ReceiveNetworks nets)
                )
    in
    openstackCredentialedRequest
        project
        maybeProxyUrl
        Get
        (project.endpoints.neutron ++ "/v2.0/networks")
        Http.emptyBody
        (Http.expectJson
            resultToMsg_
            decodeNetworks
        )


requestFloatingIps : Project -> Maybe HelperTypes.Url -> Cmd Msg
requestFloatingIps project maybeProxyUrl =
    let
        errorContext =
            ErrorContext
                ("get list of floating IPs for project \"" ++ project.auth.project.name ++ "\"")
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsg
                errorContext
                (\ips ->
                    ProjectMsg
                        (Helpers.getProjectId project)
                        (ReceiveFloatingIps ips)
                )
    in
    openstackCredentialedRequest
        project
        maybeProxyUrl
        Get
        (project.endpoints.neutron ++ "/v2.0/floatingips")
        Http.emptyBody
        (Http.expectJson
            resultToMsg_
            decodeFloatingIps
        )


getFloatingIpRequestPorts : Project -> Maybe HelperTypes.Url -> Server -> Cmd Msg
getFloatingIpRequestPorts project maybeProxyUrl server =
    let
        errorContext =
            ErrorContext
                ("get list of ports for project \"" ++ project.auth.project.name ++ "\"")
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsg
                errorContext
                (\ports ->
                    ProjectMsg
                        (Helpers.getProjectId project)
                        (GetFloatingIpReceivePorts server.osProps.uuid ports)
                )
    in
    openstackCredentialedRequest
        project
        maybeProxyUrl
        Get
        (project.endpoints.neutron ++ "/v2.0/ports")
        Http.emptyBody
        (Http.expectJson
            resultToMsg_
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

        errorContext =
            ErrorContext
                ("create a floating IP address on network " ++ network.name ++ "for port " ++ port_.uuid)
                ErrorCrit
                (Just "It's possible your cloud has run out of public IP address space; ask your cloud administrator.")

        resultToMsg_ =
            resultToMsg
                errorContext
                (\ip ->
                    ProjectMsg
                        (Helpers.getProjectId project)
                        (ReceiveCreateFloatingIp server.osProps.uuid ip)
                )

        requestCmd =
            openstackCredentialedRequest
                newProject
                maybeProxyUrl
                Post
                (project.endpoints.neutron ++ "/v2.0/floatingips")
                (Http.jsonBody requestBody)
                (Http.expectJson
                    resultToMsg_
                    decodeFloatingIpCreation
                )
    in
    ( newModel, requestCmd )


requestDeleteFloatingIp : Project -> Maybe HelperTypes.Url -> OSTypes.IpAddressUuid -> Cmd Msg
requestDeleteFloatingIp project maybeProxyUrl uuid =
    let
        errorContext =
            ErrorContext
                ("delete floating IP address with UUID " ++ uuid)
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsg
                errorContext
                (\_ ->
                    ProjectMsg
                        (Helpers.getProjectId project)
                        (ReceiveDeleteFloatingIp uuid)
                )
    in
    openstackCredentialedRequest
        project
        maybeProxyUrl
        Delete
        (project.endpoints.neutron ++ "/v2.0/floatingips/" ++ uuid)
        Http.emptyBody
        (Http.expectString
            resultToMsg_
        )


requestSecurityGroups : Project -> Maybe HelperTypes.Url -> Cmd Msg
requestSecurityGroups project maybeProxyUrl =
    let
        errorContext =
            ErrorContext
                ("get a list of security groups for project " ++ project.auth.project.name)
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsg
                errorContext
                (\groups ->
                    ProjectMsg
                        (Helpers.getProjectId project)
                        (ReceiveSecurityGroups groups)
                )
    in
    openstackCredentialedRequest
        project
        maybeProxyUrl
        Get
        (project.endpoints.neutron ++ "/v2.0/security-groups")
        Http.emptyBody
        (Http.expectJson
            resultToMsg_
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

        errorContext =
            ErrorContext
                ("create security group for Exosphere in project " ++ project.auth.project.name)
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsg
                errorContext
                (\group ->
                    ProjectMsg
                        (Helpers.getProjectId project)
                        (ReceiveCreateExoSecurityGroup group)
                )
    in
    openstackCredentialedRequest
        project
        maybeProxyUrl
        Post
        (project.endpoints.neutron ++ "/v2.0/security-groups")
        (Http.jsonBody requestBody)
        (Http.expectJson
            resultToMsg_
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
                makeRequestBodyTcp port_number desc =
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

                makeRequestBodyIcmp desc =
                    Encode.object
                        [ ( "security_group_rule"
                          , Encode.object
                                [ ( "security_group_id", Encode.string group.uuid )
                                , ( "ethertype", Encode.string "IPv4" )
                                , ( "direction", Encode.string "ingress" )
                                , ( "protocol", Encode.string "icmp" )
                                , ( "description", Encode.string desc )
                                ]
                          )
                        ]

                errorContext =
                    ErrorContext
                        "create rules for Exosphere security group"
                        ErrorCrit
                        Nothing

                buildRequestCmd body =
                    openstackCredentialedRequest
                        project
                        maybeProxyUrl
                        Post
                        (project.endpoints.neutron ++ "/v2.0/security-group-rules")
                        (Http.jsonBody body)
                        (Http.expectString
                            (resultToMsg errorContext (\_ -> NoOp))
                        )

                bodies =
                    [ makeRequestBodyTcp "22" "SSH"
                    , makeRequestBodyTcp "9090" "Cockpit"
                    , makeRequestBodyIcmp "Ping"
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
    -- Future todo handle errors with this API call, e.g. a timeout should not generate error to user but other errors should be handled differently
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" authHeaderValue ]
        , url = "http://" ++ ipAddress ++ ":9090/cockpit/login"
        , body = Http.emptyBody
        , expect = Http.expectString (resultMsg project serverUuid)
        , timeout = Just 3000
        , tracker = Nothing
        }


requestCreateServerImage : Project -> Maybe HelperTypes.Url -> OSTypes.ServerUuid -> String -> Cmd Msg
requestCreateServerImage project maybeProxyUrl serverUuid imageName =
    let
        body =
            Encode.object
                [ ( "createImage"
                  , Encode.object
                        [ ( "name", Encode.string imageName )
                        , ( "metadata"
                          , Encode.object
                                [ ( "from-exosphere", Encode.string "true" )
                                ]
                          )
                        ]
                  )
                ]

        errorContext =
            ErrorContext
                ("create an image for server with UUID " ++ serverUuid)
                ErrorCrit
                Nothing
    in
    openstackCredentialedRequest
        project
        maybeProxyUrl
        Post
        (project.endpoints.nova ++ "/servers/" ++ serverUuid ++ "/action")
        (Http.jsonBody body)
        (Http.expectString
            (resultToMsg errorContext (\_ -> NoOp))
        )



{- HTTP Response Handling -}


receiveImages : Model -> Project -> List OSTypes.Image -> ( Model, Cmd Msg )
receiveImages model project images =
    let
        newProject =
            { project | images = images }

        newModel =
            Helpers.modelUpdateProject model newProject
    in
    ( newModel, Cmd.none )


receiveServers : Model -> Project -> List OSTypes.Server -> ( Model, Cmd Msg )
receiveServers model project servers =
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
            List.map enrichNewServer servers

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


receiveServer : Model -> Project -> OSTypes.ServerUuid -> OSTypes.ServerDetails -> ( Model, Cmd Msg )
receiveServer model project serverUuid serverDetails =
    let
        maybeServer =
            Helpers.serverLookup project serverUuid
    in
    case maybeServer of
        Nothing ->
            Helpers.processError
                model
                (ErrorContext
                    "look for a server to populate with details from the API"
                    ErrorCrit
                    Nothing
                )
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


receiveFlavors : Model -> Project -> List OSTypes.Flavor -> ( Model, Cmd Msg )
receiveFlavors model project flavors =
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


receiveKeypairs : Model -> Project -> List OSTypes.Keypair -> ( Model, Cmd Msg )
receiveKeypairs model project keypairs =
    let
        newProject =
            { project | keypairs = keypairs }

        newModel =
            Helpers.modelUpdateProject model newProject
    in
    ( newModel, Cmd.none )


receiveCreateServer : Model -> Project -> OSTypes.ServerUuid -> ( Model, Cmd Msg )
receiveCreateServer model project _ =
    let
        newModel =
            { model
                | viewState =
                    ProjectView (Helpers.getProjectId project) <|
                        ListProjectServers { onlyOwnServers = False }
            }
    in
    ( newModel
    , [ requestServers
      , requestNetworks
      ]
        |> List.map (\x -> x project model.proxyUrl)
        |> Cmd.batch
    )


receiveDeleteServer : Model -> Project -> OSTypes.ServerUuid -> ( Model, Cmd Msg )
receiveDeleteServer model project serverUuid =
    let
        newServers =
            List.filter (\s -> s.osProps.uuid /= serverUuid) (RemoteData.withDefault [] project.servers)

        newProject =
            { project | servers = RemoteData.Success newServers }

        newModel =
            Helpers.modelUpdateProject model newProject
    in
    ( newModel, Cmd.none )


receiveNetworks : Model -> Project -> List OSTypes.Network -> ( Model, Cmd Msg )
receiveNetworks model project networks =
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


receiveFloatingIps : Model -> Project -> List OSTypes.IpAddress -> ( Model, Cmd Msg )
receiveFloatingIps model project floatingIps =
    let
        newProject =
            { project | floatingIps = floatingIps }

        newModel =
            Helpers.modelUpdateProject model newProject
    in
    ( newModel, Cmd.none )


receivePortsAndRequestFloatingIp : Model -> Project -> OSTypes.ServerUuid -> List OSTypes.Port -> ( Model, Cmd Msg )
receivePortsAndRequestFloatingIp model project serverUuid ports =
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
                        (ErrorContext
                            ("look for a network port belonging to server " ++ serverUuid)
                            ErrorCrit
                            Nothing
                        )
                        ("Cannot find port belonging to server " ++ serverUuid ++ " in Exosphere's data model")

        Nothing ->
            Helpers.processError
                newModel
                (ErrorContext
                    "look for a usable external network"
                    ErrorCrit
                    (Just "Ask your cloud administrator if your OpenStack project has access to an external network for floating IP addresses.")
                )
                "Cannot find a usable external network in Exosphere's data model"


receiveCreateFloatingIp : Model -> Project -> OSTypes.ServerUuid -> OSTypes.IpAddress -> ( Model, Cmd Msg )
receiveCreateFloatingIp model project serverUuid ipAddress =
    case Helpers.serverLookup project serverUuid of
        Nothing ->
            -- No server found, may have been deleted, nothing to do
            ( model, Cmd.none )

        Just server ->
            {- This repeats a lot of code in receiveCockpitStatus, badly needs a refactor -}
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


receiveDeleteFloatingIp : Model -> Project -> OSTypes.IpAddressUuid -> ( Model, Cmd Msg )
receiveDeleteFloatingIp model project uuid =
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


receiveSecurityGroupsAndEnsureExoGroup : Model -> Project -> List OSTypes.SecurityGroup -> ( Model, Cmd Msg )
receiveSecurityGroupsAndEnsureExoGroup model project securityGroups =
    {- Create an "exosphere" security group unless one already exists -}
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


receiveCreateExoSecurityGroupAndRequestCreateRules : Model -> Project -> OSTypes.SecurityGroup -> ( Model, Cmd Msg )
receiveCreateExoSecurityGroupAndRequestCreateRules model project newSecGroup =
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
                        Err _ ->
                            CheckedNotReady

                        Ok _ ->
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


decodeScopedAuthToken : Http.Response String -> Result String OSTypes.ScopedAuthToken
decodeScopedAuthToken response =
    decodeAuthTokenHelper response decodeScopedAuthTokenDetails


decodeUnscopedAuthToken : Http.Response String -> Result String OSTypes.UnscopedAuthToken
decodeUnscopedAuthToken response =
    decodeAuthTokenHelper response decodeUnscopedAuthTokenDetails


decodeAuthTokenHelper : Http.Response String -> Decode.Decoder (OSTypes.AuthTokenString -> a) -> Result String a
decodeAuthTokenHelper response tokenDetailsDecoder =
    case response of
        Http.GoodStatus_ metadata body ->
            case Decode.decodeString tokenDetailsDecoder body of
                Ok tokenDetailsWithoutTokenString ->
                    case authTokenFromHeader metadata of
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


authTokenFromHeader : Http.Metadata -> Result String String
authTokenFromHeader metadata =
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


decodeScopedAuthTokenDetails : Decode.Decoder (OSTypes.AuthTokenString -> OSTypes.ScopedAuthToken)
decodeScopedAuthTokenDetails =
    Decode.map6 OSTypes.ScopedAuthToken
        (Decode.at [ "token", "catalog" ] (Decode.list openstackServiceDecoder))
        (Decode.map2
            OSTypes.NameAndUuid
            (Decode.at [ "token", "project", "name" ] Decode.string)
            (Decode.at [ "token", "project", "id" ] Decode.string)
        )
        (Decode.map2
            OSTypes.NameAndUuid
            (Decode.at [ "token", "project", "domain", "name" ] Decode.string)
            (Decode.at [ "token", "project", "domain", "id" ] Decode.string)
        )
        (Decode.map2
            OSTypes.NameAndUuid
            (Decode.at [ "token", "user", "name" ] Decode.string)
            (Decode.at [ "token", "user", "id" ] Decode.string)
        )
        (Decode.map2
            OSTypes.NameAndUuid
            (Decode.at [ "token", "user", "domain", "name" ] Decode.string)
            (Decode.at [ "token", "user", "domain", "id" ] Decode.string)
        )
        (Decode.at [ "token", "expires_at" ] Decode.string
            |> Decode.andThen iso8601StringToPosixDecodeError
        )


decodeUnscopedAuthTokenDetails : Decode.Decoder (OSTypes.AuthTokenString -> OSTypes.UnscopedAuthToken)
decodeUnscopedAuthTokenDetails =
    Decode.map3 OSTypes.UnscopedAuthToken
        (Decode.map2
            OSTypes.NameAndUuid
            (Decode.at [ "token", "user", "name" ] Decode.string)
            (Decode.at [ "token", "user", "id" ] Decode.string)
        )
        (Decode.map2
            OSTypes.NameAndUuid
            (Decode.at [ "token", "user", "domain", "name" ] Decode.string)
            (Decode.at [ "token", "user", "domain", "id" ] Decode.string)
        )
        (Decode.at [ "token", "expires_at" ] Decode.string
            |> Decode.andThen iso8601StringToPosixDecodeError
        )


decodeAppCredential : Decode.Decoder OSTypes.ApplicationCredential
decodeAppCredential =
    Decode.map2 OSTypes.ApplicationCredential
        (Decode.at [ "application_credential", "id" ] Decode.string)
        (Decode.at [ "application_credential", "secret" ] Decode.string)


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


decodeUnscopedProjects : Decode.Decoder (List UnscopedProviderProject)
decodeUnscopedProjects =
    Decode.field "projects" <|
        Decode.list unscopedProjectDecoder


unscopedProjectDecoder : Decode.Decoder UnscopedProviderProject
unscopedProjectDecoder =
    Decode.map4 UnscopedProviderProject
        (Decode.field "name" Decode.string)
        (Decode.field "description" Decode.string)
        (Decode.field "domain_id" Decode.string)
        (Decode.field "enabled" Decode.bool)


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
        |> Pipeline.required "user_id" Decode.string
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
