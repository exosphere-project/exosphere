module Rest.Nova exposing
    ( decodeFlavors
    , decodeKeypairs
    , decodeServer
    , decodeServerDetails
    , decodeServers
    , flavorDecoder
    , ipAddressOpenstackTypeDecoder
    , keypairDecoder
    , receiveConsoleUrl
    , receiveFlavors
    , receiveKeypairs
    , receiveServer
    , receiveServers
    , requestConsoleUrls
    , requestCreateKeypair
    , requestCreateServer
    , requestCreateServerImage
    , requestDeleteKeypair
    , requestDeleteServer
    , requestFlavors
    , requestKeypairs
    , requestServer
    , requestServers
    , requestSetServerMetadata
    , requestSetServerName
    , serverIpAddressDecoder
    , serverPowerStateDecoder
    )

import Array
import Base64
import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers as Helpers
import Helpers.RemoteDataPlusPlus as RDPP
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import OpenStack.ServerPassword as OSServerPassword
import OpenStack.Types as OSTypes
import RemoteData
import Rest.Helpers
    exposing
        ( expectJsonWithErrorBody
        , expectStringWithErrorBody
        , iso8601StringToPosixDecodeError
        , openstackCredentialedRequest
        , resultToMsgErrorBody
        )
import Types.Defaults as Defaults
import Types.Error exposing (ErrorContext, ErrorLevel(..), HttpErrorWithBody)
import Types.Guacamole as GuacTypes
import Types.Types
    exposing
        ( ExoServerProps
        , FloatingIpState(..)
        , HttpRequestMethod(..)
        , Model
        , Msg(..)
        , NewServerNetworkOptions(..)
        , Project
        , ProjectSpecificMsgConstructor(..)
        , ProjectViewConstructor(..)
        , Server
        , ServerOrigin(..)
        , ServerSpecificMsgConstructor(..)
        , ViewState(..)
        )



{- HTTP Requests -}


requestServers : Project -> Cmd Msg
requestServers project =
    let
        errorContext =
            ErrorContext
                ("get details of servers for project \"" ++ project.auth.project.name ++ "\"")
                ErrorCrit
                Nothing

        resultToMsg result =
            ProjectMsg
                project.auth.project.uuid
                (ReceiveServers errorContext result)
    in
    openstackCredentialedRequest
        project
        Get
        (Just "compute 2.27")
        (project.endpoints.nova ++ "/servers/detail")
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg
            decodeServers
        )


requestServer : Project -> OSTypes.ServerUuid -> Cmd Msg
requestServer project serverUuid =
    let
        errorContext =
            ErrorContext
                ("get details of server with UUID \"" ++ serverUuid ++ "\"")
                ErrorCrit
                Nothing

        resultToMsg result =
            ProjectMsg
                project.auth.project.uuid
                (ReceiveServer serverUuid errorContext result)

        requestServerCmd =
            openstackCredentialedRequest
                project
                Get
                (Just "compute 2.27")
                (project.endpoints.nova ++ "/servers/" ++ serverUuid)
                Http.emptyBody
                (expectJsonWithErrorBody
                    resultToMsg
                    (Decode.at [ "server" ] decodeServer)
                )
    in
    -- Get server events whenever we get a server. We may not want to do this forever, but let's try it out
    Cmd.batch
        [ requestServerCmd
        , requestServerEvents project serverUuid
        ]


requestServerEvents : Project -> OSTypes.ServerUuid -> Cmd Msg
requestServerEvents project serverUuid =
    let
        errorContext =
            ErrorContext
                ("get events for server with UUID \"" ++ serverUuid ++ "\"")
                ErrorCrit
                Nothing

        resultToMsg result =
            ProjectMsg project.auth.project.uuid <|
                ServerMsg serverUuid <|
                    ReceiveServerEvents errorContext result
    in
    openstackCredentialedRequest
        project
        Get
        (Just "compute 2.27")
        (project.endpoints.nova ++ "/servers/" ++ serverUuid ++ "/os-instance-actions")
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg
            (Decode.at [ "instanceActions" ] <| Decode.list decodeServerEvent)
        )


requestConsoleUrls : Project -> OSTypes.ServerUuid -> Cmd Msg
requestConsoleUrls project serverUuid =
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
                Post
                Nothing
                (project.endpoints.nova ++ "/servers/" ++ serverUuid ++ "/action")
                (Http.jsonBody reqBody)
                (expectJsonWithErrorBody
                    (\result ->
                        ProjectMsg project.auth.project.uuid <|
                            ServerMsg serverUuid <|
                                ReceiveConsoleUrl result
                    )
                    decodeConsoleUrl
                )
    in
    List.map buildReq reqParams
        |> Cmd.batch


requestFlavors : Project -> Cmd Msg
requestFlavors project =
    let
        errorContext =
            ErrorContext
                ("get details of flavors for project \"" ++ project.auth.project.name ++ "\"")
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\flavors -> ProjectMsg project.auth.project.uuid <| ReceiveFlavors flavors)
    in
    openstackCredentialedRequest
        project
        Get
        Nothing
        (project.endpoints.nova ++ "/flavors/detail")
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg_
            decodeFlavors
        )


requestKeypairs : Project -> Cmd Msg
requestKeypairs project =
    let
        errorContext =
            ErrorContext
                ("get details of keypairs for project \"" ++ project.auth.project.name ++ "\"")
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\keypairs -> ProjectMsg project.auth.project.uuid <| ReceiveKeypairs keypairs)
    in
    openstackCredentialedRequest
        project
        Get
        Nothing
        (project.endpoints.nova ++ "/os-keypairs")
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg_
            decodeKeypairs
        )


requestCreateKeypair : Project -> OSTypes.KeypairName -> OSTypes.PublicKey -> Cmd Msg
requestCreateKeypair project keypairName publicKey =
    let
        body =
            Encode.object
                [ ( "keypair"
                  , Encode.object
                        [ ( "name", Encode.string keypairName )
                        , ( "public_key", Encode.string publicKey )
                        ]
                  )
                ]

        errorContext =
            ErrorContext
                ("create keypair for project \"" ++ project.auth.project.name ++ "\"")
                ErrorCrit
                (Just "ensure that you are entering the entire public key with no extra line breaks or other characters.")

        resultToMsg_ =
            resultToMsgErrorBody errorContext
                (\keypair ->
                    ProjectMsg project.auth.project.uuid <|
                        ReceiveCreateKeypair keypair
                )
    in
    openstackCredentialedRequest
        project
        Post
        Nothing
        (project.endpoints.nova ++ "/os-keypairs")
        (Http.jsonBody body)
        (expectJsonWithErrorBody
            resultToMsg_
            keypairDecoder
        )


requestDeleteKeypair : Project -> OSTypes.KeypairName -> Cmd Msg
requestDeleteKeypair project keypairName =
    let
        errorContext =
            ErrorContext
                ("delete keypair with name \"" ++ keypairName ++ "\"")
                ErrorCrit
                Nothing
    in
    openstackCredentialedRequest
        project
        Delete
        Nothing
        (project.endpoints.nova ++ "/os-keypairs/" ++ keypairName)
        Http.emptyBody
        (Http.expectWhatever
            (\result -> ProjectMsg project.auth.project.uuid <| ReceiveDeleteKeypair errorContext keypairName result)
        )


requestCreateServer : Project -> OSTypes.CreateServerRequest -> Cmd Msg
requestCreateServer project createServerRequest =
    let
        instanceNumbers =
            List.range 1 createServerRequest.count

        generateServerName : String -> Int -> Int -> String
        generateServerName baseName serverCount index =
            if serverCount == 1 then
                baseName

            else
                baseName ++ " " ++ String.fromInt index ++ " of " ++ String.fromInt createServerRequest.count

        instanceNames =
            instanceNumbers
                |> List.map (generateServerName createServerRequest.name createServerRequest.count)

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
                , ( "user_data", Encode.string (Base64.encode createServerRequest.userData) )
                , ( "security_groups", Encode.array Encode.object (Array.fromList [ [ ( "name", Encode.string "exosphere" ) ] ]) )
                , ( "metadata"
                  , Encode.object createServerRequest.metadata
                  )
                ]

        buildRequestOuterJson props =
            Encode.object [ ( "server", Encode.object props ) ]

        buildRequestBody instanceName =
            case createServerRequest.volBackedSizeGb of
                Nothing ->
                    ( "imageRef", Encode.string createServerRequest.imageUuid )
                        :: baseServerProps createServerRequest instanceName
                        |> buildRequestOuterJson

                Just sizeGb ->
                    ( "block_device_mapping_v2"
                    , Encode.list Encode.object
                        [ [ ( "boot_index", Encode.string "0" )
                          , ( "uuid", Encode.string createServerRequest.imageUuid )
                          , ( "source_type", Encode.string "image" )
                          , ( "volume_size", Encode.string (String.fromInt sizeGb) )
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
                        1 ->
                            ""

                        _ ->
                            "s"
            in
            ErrorContext
                ("create " ++ String.fromInt createServerRequest.count ++ " server" ++ plural)
                ErrorCrit
                (Just <| "It's possible your quota is not large enough to launch the requested server" ++ plural)

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\serverUuid ->
                    ProjectMsg
                        project.auth.project.uuid
                        (ReceiveCreateServer serverUuid)
                )
    in
    Cmd.batch
        (requestBodies
            |> List.map
                (\requestBody ->
                    openstackCredentialedRequest
                        project
                        Post
                        Nothing
                        (project.endpoints.nova ++ "/servers")
                        (Http.jsonBody requestBody)
                        (expectJsonWithErrorBody
                            resultToMsg_
                            (Decode.field "server" serverUuidDecoder)
                        )
                )
        )


requestDeleteServer : Project -> Server -> Cmd Msg
requestDeleteServer project server =
    let
        getFloatingIp =
            server.osProps.details.ipAddresses
                |> GetterSetters.getServerFloatingIp

        errorContext =
            ErrorContext
                ("delete server with UUID " ++ server.osProps.uuid)
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\_ ->
                    ProjectMsg project.auth.project.uuid <|
                        ServerMsg server.osProps.uuid <|
                            ReceiveDeleteServer getFloatingIp
                )
    in
    openstackCredentialedRequest
        project
        Delete
        Nothing
        (project.endpoints.nova ++ "/servers/" ++ server.osProps.uuid)
        Http.emptyBody
        (expectStringWithErrorBody resultToMsg_)


requestConsoleUrlIfRequestable : Project -> Server -> Cmd Msg
requestConsoleUrlIfRequestable project server =
    case server.osProps.details.openstackStatus of
        OSTypes.ServerActive ->
            requestConsoleUrls project server.osProps.uuid

        _ ->
            Cmd.none


requestCreateServerImage : Project -> OSTypes.ServerUuid -> String -> Cmd Msg
requestCreateServerImage project serverUuid imageName =
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
        Post
        Nothing
        (project.endpoints.nova ++ "/servers/" ++ serverUuid ++ "/action")
        (Http.jsonBody body)
        (expectStringWithErrorBody
            (resultToMsgErrorBody errorContext (\_ -> NoOp))
        )


requestSetServerName : Project -> OSTypes.ServerUuid -> String -> Cmd Msg
requestSetServerName project serverUuid newServerName =
    let
        body =
            Encode.object
                [ ( "server"
                  , Encode.object
                        [ ( "name"
                          , Encode.string newServerName
                          )
                        ]
                  )
                ]

        errorContext =
            ErrorContext
                ("rename server with UUID " ++ serverUuid ++ " to " ++ newServerName)
                ErrorCrit
                Nothing

        resultToMsg result =
            ProjectMsg project.auth.project.uuid <|
                ServerMsg serverUuid <|
                    ReceiveSetServerName newServerName errorContext result
    in
    openstackCredentialedRequest
        project
        Put
        Nothing
        (project.endpoints.nova ++ "/servers/" ++ serverUuid)
        (Http.jsonBody body)
        (expectJsonWithErrorBody
            resultToMsg
            (Decode.at [ "server", "name" ] Decode.string)
        )


requestSetServerMetadata : Project -> OSTypes.ServerUuid -> OSTypes.MetadataItem -> Cmd Msg
requestSetServerMetadata project serverUuid metadataItem =
    let
        body =
            Encode.object
                [ ( "metadata"
                  , Encode.object [ ( metadataItem.key, Encode.string metadataItem.value ) ]
                  )
                ]

        errorContext =
            ErrorContext
                (String.concat
                    [ "set metadata with key \""
                    , metadataItem.key
                    , "\" and value \""
                    , metadataItem.value
                    , "for server with UUID "
                    , serverUuid
                    ]
                )
                ErrorCrit
                Nothing

        resultToMsg result =
            ProjectMsg project.auth.project.uuid <|
                ServerMsg serverUuid <|
                    ReceiveSetServerMetadata metadataItem errorContext result
    in
    openstackCredentialedRequest
        project
        Post
        Nothing
        (project.endpoints.nova ++ "/servers/" ++ serverUuid ++ "/metadata")
        (Http.jsonBody body)
        (expectJsonWithErrorBody
            resultToMsg
            (Decode.field "metadata" metadataDecoder)
        )



{- HTTP Response Handling -}


receiveServers : Model -> Project -> List OSTypes.Server -> ( Model, Cmd Msg )
receiveServers model project osServers =
    let
        ( newExoServers, cmds ) =
            osServers
                |> List.map (receiveServer_ project)
                |> List.unzip

        newExoServersClearSomeExoProps =
            let
                clearRecTime : Server -> Server
                clearRecTime s =
                    let
                        oldExoProps =
                            s.exoProps

                        newExoProps =
                            { oldExoProps
                                | receivedTime = Nothing
                                , loadingSeparately = False
                            }
                    in
                    { s | exoProps = newExoProps }
            in
            List.map clearRecTime newExoServers

        projectNoDeletedSvrs =
            -- Set RDPP ReceivedTime and remove recently deleted servers from existing project
            { project
                | servers =
                    RDPP.RemoteDataPlusPlus
                        (RDPP.DoHave
                            (List.filter
                                (\s -> List.member s.osProps.uuid (List.map .uuid osServers))
                                (RDPP.withDefault [] project.servers)
                            )
                            model.clientCurrentTime
                        )
                        (RDPP.NotLoading Nothing)
            }

        newProject =
            List.foldl
                (\s p -> GetterSetters.projectUpdateServer p s)
                projectNoDeletedSvrs
                newExoServersClearSomeExoProps
    in
    ( GetterSetters.modelUpdateProject model newProject
    , Cmd.batch cmds
    )


receiveServer : Model -> Project -> OSTypes.Server -> ( Model, Cmd Msg )
receiveServer model project osServer =
    let
        ( newServer, cmd ) =
            receiveServer_ project osServer

        newServerUpdatedSomeExoProps =
            let
                oldExoProps =
                    newServer.exoProps

                newExoProps =
                    { oldExoProps
                        | receivedTime = Just model.clientCurrentTime
                        , loadingSeparately = False
                    }
            in
            { newServer | exoProps = newExoProps }

        newProject =
            case project.servers.data of
                RDPP.DoHave _ _ ->
                    GetterSetters.projectUpdateServer project newServerUpdatedSomeExoProps

                RDPP.DontHave ->
                    let
                        newServersRDPP =
                            RDPP.RemoteDataPlusPlus
                                (RDPP.DoHave [ newServerUpdatedSomeExoProps ] model.clientCurrentTime)
                                (RDPP.NotLoading Nothing)
                    in
                    { project | servers = newServersRDPP }
    in
    ( GetterSetters.modelUpdateProject model newProject
    , cmd
    )


receiveServer_ : Project -> OSTypes.Server -> ( Server, Cmd Msg )
receiveServer_ project osServer =
    let
        newServer : Server
        newServer =
            case GetterSetters.serverLookup project osServer.uuid of
                Nothing ->
                    let
                        defaultExoProps =
                            ExoServerProps
                                Unknown
                                False
                                Nothing
                                (Helpers.serverOrigin osServer.details)
                                Nothing
                                False
                    in
                    Server osServer defaultExoProps RemoteData.NotAsked

                Just exoServer ->
                    let
                        floatingIpState_ =
                            Helpers.checkFloatingIpState
                                osServer.details
                                exoServer.exoProps.priorFloatingIpState

                        oldOSProps =
                            exoServer.osProps

                        oldExoProps =
                            exoServer.exoProps

                        newTargetOpenstackStatus =
                            case oldExoProps.targetOpenstackStatus of
                                Nothing ->
                                    Nothing

                                Just statuses ->
                                    if List.member osServer.details.openstackStatus statuses then
                                        Nothing

                                    else
                                        Just statuses

                        -- If server is not active, then forget Guacamole token
                        newServerOrigin =
                            let
                                guacPropsForgetToken : GuacTypes.LaunchedWithGuacProps -> GuacTypes.LaunchedWithGuacProps
                                guacPropsForgetToken oldGuacProps =
                                    { oldGuacProps | authToken = RDPP.empty }
                            in
                            case oldExoProps.serverOrigin of
                                ServerNotFromExo ->
                                    ServerNotFromExo

                                ServerFromExo exoOriginProps ->
                                    case exoOriginProps.guacamoleStatus of
                                        GuacTypes.NotLaunchedWithGuacamole ->
                                            oldExoProps.serverOrigin

                                        GuacTypes.LaunchedWithGuacamole guacProps ->
                                            case osServer.details.openstackStatus of
                                                OSTypes.ServerActive ->
                                                    oldExoProps.serverOrigin

                                                _ ->
                                                    let
                                                        newOriginProps =
                                                            { exoOriginProps
                                                                | guacamoleStatus =
                                                                    GuacTypes.LaunchedWithGuacamole
                                                                        (guacPropsForgetToken guacProps)
                                                            }
                                                    in
                                                    ServerFromExo newOriginProps
                    in
                    { exoServer
                        | osProps = { oldOSProps | details = osServer.details }
                        , exoProps =
                            { oldExoProps
                                | priorFloatingIpState = floatingIpState_
                                , targetOpenstackStatus = newTargetOpenstackStatus
                                , serverOrigin = newServerOrigin
                            }
                    }

        consoleUrlCmd =
            case newServer.osProps.consoleUrl of
                RemoteData.Success _ ->
                    Cmd.none

                _ ->
                    requestConsoleUrlIfRequestable project newServer

        passwordCmd =
            case newServer.exoProps.serverOrigin of
                ServerNotFromExo ->
                    Cmd.none

                ServerFromExo serverFromExoProps ->
                    case
                        ( serverFromExoProps.exoServerVersion >= 1
                        , GetterSetters.getServerExouserPassword newServer.osProps.details
                        , newServer.osProps.details.openstackStatus
                        )
                    of
                        ( True, Nothing, OSTypes.ServerActive ) ->
                            OSServerPassword.requestServerPassword project newServer.osProps.uuid

                        _ ->
                            Cmd.none

        allCmds =
            [ consoleUrlCmd, passwordCmd ]
                |> Cmd.batch
    in
    ( newServer, allCmds )


receiveConsoleUrl : Model -> Project -> Server -> Result HttpErrorWithBody OSTypes.ConsoleUrl -> ( Model, Cmd Msg )
receiveConsoleUrl model project server result =
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
                    GetterSetters.projectUpdateServer project newServer

                newModel =
                    GetterSetters.modelUpdateProject model newProject
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
                ProjectView _ _ projectViewConstructor ->
                    case projectViewConstructor of
                        CreateServer viewParams ->
                            if viewParams.flavorUuid == "" then
                                let
                                    maybeSmallestFlavor =
                                        GetterSetters.sortedFlavors flavors |> List.head
                                in
                                case maybeSmallestFlavor of
                                    Just smallestFlavor ->
                                        ProjectView
                                            project.auth.project.uuid
                                            Defaults.projectViewParams
                                            (CreateServer
                                                { viewParams
                                                    | flavorUuid = smallestFlavor.uuid
                                                }
                                            )

                                    Nothing ->
                                        model.viewState

                            else
                                model.viewState

                        _ ->
                            model.viewState

                _ ->
                    model.viewState

        newModel =
            GetterSetters.modelUpdateProject { model | viewState = viewState } newProject
    in
    ( newModel, Cmd.none )


receiveKeypairs : Model -> Project -> List OSTypes.Keypair -> ( Model, Cmd Msg )
receiveKeypairs model project keypairs =
    let
        newProject =
            { project | keypairs = RemoteData.Success keypairs }

        newModel =
            GetterSetters.modelUpdateProject model newProject
    in
    ( newModel, Cmd.none )



{- JSON Decoders -}


decodeServers : Decode.Decoder (List OSTypes.Server)
decodeServers =
    Decode.field "servers" (Decode.list decodeServer)


decodeServer : Decode.Decoder OSTypes.Server
decodeServer =
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
        |> Pipeline.required "tags" (Decode.list Decode.string)
        |> Pipeline.required "locked" serverLockStatusDecoder


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

        "deleted" ->
            Decode.succeed OSTypes.ServerDeleted

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


serverIpAddressDecoder : Decode.Decoder OSTypes.ServerIpAddress
serverIpAddressDecoder =
    Decode.map2 OSTypes.ServerIpAddress
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


serverLockStatusDecoder : Decode.Decoder OSTypes.ServerLockStatus
serverLockStatusDecoder =
    let
        boolToLockStatus b =
            if b then
                Decode.succeed OSTypes.ServerLocked

            else
                Decode.succeed OSTypes.ServerUnlocked
    in
    Decode.bool |> Decode.andThen boolToLockStatus


decodeServerEvent : Decode.Decoder OSTypes.ServerEvent
decodeServerEvent =
    Decode.map5 OSTypes.ServerEvent
        (Decode.field "action" Decode.string)
        (Decode.field "message" (Decode.nullable Decode.string))
        (Decode.field "request_id" Decode.string)
        (Decode.field "start_time" Decode.string
            |> Decode.andThen
                iso8601StringToPosixDecodeError
        )
        (Decode.field "user_id" Decode.string)


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
