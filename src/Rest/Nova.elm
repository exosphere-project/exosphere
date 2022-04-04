module Rest.Nova exposing
    ( receiveConsoleUrl
    , receiveFlavors
    , receiveKeypairs
    , receiveServer
    , receiveServers
    , requestCreateKeypair
    , requestCreateServer
    , requestCreateServerImage
    , requestDeleteKeypair
    , requestDeleteServer
    , requestFlavors
    , requestKeypairs
    , requestServer
    , requestServerEvents
    , requestServerResize
    , requestServers
    , requestSetServerMetadata
    , requestSetServerName
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
import Types.Error exposing (ErrorContext, ErrorLevel(..), HttpErrorWithBody)
import Types.Guacamole as GuacTypes
import Types.HelperTypes exposing (HttpRequestMethod(..), ProjectIdentifier, Url)
import Types.Project exposing (Project)
import Types.Server exposing (ExoServerProps, ExoSetupStatus(..), NewServerNetworkOptions(..), Server, ServerOrigin(..))
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), ServerSpecificMsgConstructor(..), SharedMsg(..))



{- HTTP Requests -}


requestServers : Project -> Cmd SharedMsg
requestServers project =
    let
        errorContext =
            ErrorContext
                ("get details of servers for project \"" ++ project.auth.project.name ++ "\"")
                ErrorCrit
                Nothing

        resultToMsg result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveServers errorContext result)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        (Just "compute 2.27")
        (project.endpoints.nova ++ "/servers/detail")
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg
            decodeServers
        )


requestServer : Project -> OSTypes.ServerUuid -> Cmd SharedMsg
requestServer project serverUuid =
    let
        errorContext =
            ErrorContext
                ("get details of server with UUID \"" ++ serverUuid ++ "\"")
                ErrorCrit
                Nothing

        resultToMsg result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveServer serverUuid errorContext result)

        requestServerCmd =
            openstackCredentialedRequest
                (GetterSetters.projectIdentifier project)
                Get
                (Just "compute 2.27")
                (project.endpoints.nova ++ "/servers/" ++ serverUuid)
                Http.emptyBody
                (expectJsonWithErrorBody
                    resultToMsg
                    (Decode.at [ "server" ] decodeServer)
                )
    in
    requestServerCmd


requestServerEvents : Project -> OSTypes.ServerUuid -> Cmd SharedMsg
requestServerEvents project serverUuid =
    let
        errorContext =
            ErrorContext
                ("get events for server with UUID \"" ++ serverUuid ++ "\"")
                ErrorCrit
                Nothing

        resultToMsg result =
            ProjectMsg (GetterSetters.projectIdentifier project) <|
                ServerMsg serverUuid <|
                    ReceiveServerEvents errorContext result
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        (Just "compute 2.27")
        (project.endpoints.nova ++ "/servers/" ++ serverUuid ++ "/os-instance-actions")
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg
            (Decode.at [ "instanceActions" ] <| Decode.list decodeServerEvent)
        )


requestConsoleUrls : Project -> OSTypes.ServerUuid -> Cmd SharedMsg
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
                (GetterSetters.projectIdentifier project)
                Post
                Nothing
                (project.endpoints.nova ++ "/servers/" ++ serverUuid ++ "/action")
                (Http.jsonBody reqBody)
                (expectJsonWithErrorBody
                    (\result ->
                        ProjectMsg (GetterSetters.projectIdentifier project) <|
                            ServerMsg serverUuid <|
                                ReceiveConsoleUrl result
                    )
                    decodeConsoleUrl
                )
    in
    List.map buildReq reqParams
        |> Cmd.batch


requestFlavors : Project -> Cmd SharedMsg
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
                (\flavors -> ProjectMsg (GetterSetters.projectIdentifier project) <| ReceiveFlavors flavors)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        (Just "compute 2.61")
        (project.endpoints.nova ++ "/flavors/detail")
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg_
            decodeFlavors
        )


requestKeypairs : Project -> Cmd SharedMsg
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
                (\keypairs -> ProjectMsg (GetterSetters.projectIdentifier project) <| ReceiveKeypairs keypairs)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        (project.endpoints.nova ++ "/os-keypairs")
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg_
            decodeKeypairs
        )


requestCreateKeypair : Project -> OSTypes.KeypairName -> OSTypes.PublicKey -> Cmd SharedMsg
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
                    ProjectMsg (GetterSetters.projectIdentifier project) <|
                        ReceiveCreateKeypair keypair
                )
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Post
        Nothing
        (project.endpoints.nova ++ "/os-keypairs")
        (Http.jsonBody body)
        (expectJsonWithErrorBody
            resultToMsg_
            keypairDecoder
        )


requestDeleteKeypair : Project -> OSTypes.KeypairIdentifier -> Cmd SharedMsg
requestDeleteKeypair project keypairId =
    let
        errorContext =
            ErrorContext
                ("delete keypair with name \"" ++ Tuple.first keypairId ++ "\"")
                ErrorCrit
                Nothing
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Delete
        Nothing
        (project.endpoints.nova ++ "/os-keypairs/" ++ Tuple.first keypairId)
        Http.emptyBody
        (Http.expectWhatever
            (\result -> ProjectMsg (GetterSetters.projectIdentifier project) <| ReceiveDeleteKeypair errorContext (Tuple.first keypairId) result)
        )


requestCreateServer : Project -> OSTypes.CreateServerRequest -> Cmd SharedMsg
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
                , ( "flavorRef", Encode.string innerCreateServerRequest.flavorId )
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

        resultToMsg result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveCreateServer errorContext result)
    in
    Cmd.batch
        (requestBodies
            |> List.map
                (\requestBody ->
                    openstackCredentialedRequest
                        (GetterSetters.projectIdentifier project)
                        Post
                        Nothing
                        (project.endpoints.nova ++ "/servers")
                        (Http.jsonBody requestBody)
                        (expectJsonWithErrorBody
                            resultToMsg
                            (Decode.field "server" serverUuidDecoder)
                        )
                )
        )


requestDeleteServer : ProjectIdentifier -> Url -> OSTypes.ServerUuid -> Cmd SharedMsg
requestDeleteServer projectId novaUrl serverId =
    let
        errorContext =
            ErrorContext
                ("delete server with UUID " ++ serverId)
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\_ ->
                    ProjectMsg projectId <|
                        ServerMsg serverId <|
                            ReceiveDeleteServer
                )
    in
    openstackCredentialedRequest
        projectId
        Delete
        Nothing
        (novaUrl ++ "/servers/" ++ serverId)
        Http.emptyBody
        (expectStringWithErrorBody resultToMsg_)


requestConsoleUrlIfRequestable : Project -> Server -> Cmd SharedMsg
requestConsoleUrlIfRequestable project server =
    case server.osProps.consoleUrl of
        RemoteData.Success _ ->
            Cmd.none

        _ ->
            if
                List.member server.osProps.details.openstackStatus
                    [ OSTypes.ServerActive, OSTypes.ServerPassword, OSTypes.ServerRescue, OSTypes.ServerVerifyResize ]
            then
                requestConsoleUrls project server.osProps.uuid

            else
                Cmd.none


requestPassphraseIfRequestable : Project -> Server -> Cmd SharedMsg
requestPassphraseIfRequestable project server =
    case server.exoProps.serverOrigin of
        ServerNotFromExo ->
            Cmd.none

        ServerFromExo serverFromExoProps ->
            let
                passphraseLikelySetAlready =
                    List.member
                        (RDPP.withDefault ( ExoSetupUnknown, Nothing ) serverFromExoProps.exoSetupStatus |> Tuple.first)
                        [ ExoSetupRunning, ExoSetupComplete ]
            in
            case
                ( GetterSetters.getServerExouserPassphrase server.osProps.details
                , server.osProps.details.openstackStatus
                , passphraseLikelySetAlready
                )
            of
                ( Nothing, OSTypes.ServerActive, True ) ->
                    OSServerPassword.requestServerPassword project server.osProps.uuid

                _ ->
                    Cmd.none


requestCreateServerImage : Project -> OSTypes.ServerUuid -> String -> Cmd SharedMsg
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
        (GetterSetters.projectIdentifier project)
        Post
        Nothing
        (project.endpoints.nova ++ "/servers/" ++ serverUuid ++ "/action")
        (Http.jsonBody body)
        (expectStringWithErrorBody
            (resultToMsgErrorBody errorContext (\_ -> NoOp))
        )


requestServerResize : Project -> OSTypes.ServerUuid -> OSTypes.FlavorId -> Cmd SharedMsg
requestServerResize project serverUuid flavorId =
    let
        body =
            Encode.object
                [ ( "resize"
                  , Encode.object
                        [ ( "flavorRef", Encode.string flavorId )
                        ]
                  )
                ]

        errorContext =
            ErrorContext
                ("resize server with UUID " ++ serverUuid ++ " to flavor ID " ++ flavorId)
                ErrorCrit
                Nothing
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Post
        Nothing
        (project.endpoints.nova ++ "/servers/" ++ serverUuid ++ "/action")
        (Http.jsonBody body)
        (expectStringWithErrorBody
            (resultToMsgErrorBody errorContext
                (\_ ->
                    ProjectMsg (GetterSetters.projectIdentifier project) <| ServerMsg serverUuid <| ReceiveServerAction
                )
            )
        )


requestSetServerName : Project -> OSTypes.ServerUuid -> String -> Cmd SharedMsg
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
            ProjectMsg (GetterSetters.projectIdentifier project) <|
                ServerMsg serverUuid <|
                    ReceiveSetServerName newServerName errorContext result
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Put
        Nothing
        (project.endpoints.nova ++ "/servers/" ++ serverUuid)
        (Http.jsonBody body)
        (expectJsonWithErrorBody
            resultToMsg
            (Decode.at [ "server", "name" ] Decode.string)
        )


requestSetServerMetadata : Project -> OSTypes.ServerUuid -> OSTypes.MetadataItem -> Cmd SharedMsg
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
            ProjectMsg (GetterSetters.projectIdentifier project) <|
                ServerMsg serverUuid <|
                    ReceiveSetServerMetadata metadataItem errorContext result
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Post
        Nothing
        (project.endpoints.nova ++ "/servers/" ++ serverUuid ++ "/metadata")
        (Http.jsonBody body)
        (expectJsonWithErrorBody
            resultToMsg
            (Decode.field "metadata" metadataDecoder)
        )


requestDeleteServerMetadata : Project -> OSTypes.ServerUuid -> OSTypes.MetadataKey -> Cmd SharedMsg
requestDeleteServerMetadata project serverUuid metadataKey =
    let
        errorContext =
            ErrorContext
                (String.concat
                    [ "delete metadata with key \""
                    , metadataKey
                    , "\" for server with UUID "
                    , serverUuid
                    ]
                )
                ErrorCrit
                Nothing

        resultToMsg result =
            ProjectMsg (GetterSetters.projectIdentifier project) <|
                ServerMsg serverUuid <|
                    ReceiveDeleteServerMetadata metadataKey errorContext result
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Delete
        Nothing
        (project.endpoints.nova ++ "/servers/" ++ serverUuid ++ "/metadata/" ++ metadataKey)
        Http.emptyBody
        (expectStringWithErrorBody resultToMsg)



{- HTTP Response Handling -}


receiveServers : SharedModel -> Project -> List OSTypes.Server -> ( SharedModel, Cmd SharedMsg )
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


receiveServer : SharedModel -> Project -> OSTypes.Server -> ( SharedModel, Cmd SharedMsg )
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


receiveServer_ : Project -> OSTypes.Server -> ( Server, Cmd SharedMsg )
receiveServer_ project osServer =
    let
        newServer : Server
        newServer =
            case GetterSetters.serverLookup project osServer.uuid of
                Nothing ->
                    let
                        defaultExoProps =
                            ExoServerProps
                                (Helpers.decodeFloatingIpOption osServer.details)
                                False
                                Nothing
                                (Helpers.serverOrigin osServer.details)
                                Nothing
                                False
                    in
                    Server osServer defaultExoProps RDPP.empty

                Just exoServer ->
                    let
                        floatingIpCreationOption =
                            Helpers.getNewFloatingIpOption
                                project
                                osServer
                                exoServer.exoProps.floatingIpCreationOption

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
                                | floatingIpCreationOption = floatingIpCreationOption
                                , targetOpenstackStatus = newTargetOpenstackStatus
                                , serverOrigin = newServerOrigin
                            }
                    }

        consoleUrlCmd =
            requestConsoleUrlIfRequestable project newServer

        passphraseCmd =
            requestPassphraseIfRequestable project newServer

        deleteFloatingIpMetadataOptionCmd =
            -- The exoCreateFloatingIp metadata property is only used temporarily so that Exosphere knows the user's
            -- choice of whether to create a floating IP address with a new server. Once it is stored in the model,
            -- we can delete the metadata property.
            let
                metadataKey =
                    "exoCreateFloatingIp"
            in
            case newServer.exoProps.serverOrigin of
                ServerFromExo _ ->
                    if
                        List.member metadataKey (List.map .key newServer.osProps.details.metadata)
                            && newServer.osProps.details.openstackStatus
                            == OSTypes.ServerActive
                    then
                        requestDeleteServerMetadata project newServer.osProps.uuid metadataKey

                    else
                        Cmd.none

                ServerNotFromExo ->
                    Cmd.none

        allCmds =
            [ consoleUrlCmd, passphraseCmd, deleteFloatingIpMetadataOptionCmd ]
                |> Cmd.batch
    in
    ( newServer, allCmds )


receiveConsoleUrl : SharedModel -> Project -> Server -> Result HttpErrorWithBody OSTypes.ConsoleUrl -> ( SharedModel, Cmd SharedMsg )
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


receiveFlavors : SharedModel -> Project -> List OSTypes.Flavor -> ( SharedModel, Cmd SharedMsg )
receiveFlavors model project flavors =
    let
        newProject =
            { project | flavors = flavors }

        newModel =
            GetterSetters.modelUpdateProject model newProject
    in
    ( newModel, Cmd.none )


receiveKeypairs : SharedModel -> Project -> List OSTypes.Keypair -> ( SharedModel, Cmd SharedMsg )
receiveKeypairs model project keypairs =
    let
        sortedKeypairs =
            List.sortBy .name keypairs

        newProject =
            { project | keypairs = RemoteData.Success sortedKeypairs }

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
    Decode.succeed OSTypes.ServerDetails
        |> Pipeline.required "status" (Decode.string |> Decode.andThen serverOpenstackStatusDecoder)
        |> Pipeline.required "created" (Decode.string |> Decode.andThen iso8601StringToPosixDecodeError)
        |> Pipeline.required "OS-EXT-STS:power_state" (Decode.int |> Decode.andThen serverPowerStateDecoder)
        |> Pipeline.optionalAt [ "image", "id" ] Decode.string ""
        |> Pipeline.requiredAt [ "flavor", "id" ] Decode.string
        |> Pipeline.optional "key_name" (Decode.string |> Decode.andThen (\s -> Decode.succeed <| Just s)) Nothing
        |> Pipeline.required "metadata" metadataDecoder
        |> Pipeline.required "user_id" Decode.string
        |> Pipeline.required "os-extended-volumes:volumes_attached" (Decode.list (Decode.at [ "id" ] Decode.string))
        |> Pipeline.required "tags" (Decode.list Decode.string)
        |> Pipeline.required "locked" serverLockStatusDecoder
        |> Pipeline.optional "fault" (serverFaultDecoder |> Decode.andThen (\f -> Decode.succeed <| Just f)) Nothing


serverOpenstackStatusDecoder : String -> Decode.Decoder OSTypes.ServerStatus
serverOpenstackStatusDecoder status =
    case String.toLower status of
        "active" ->
            Decode.succeed OSTypes.ServerActive

        "build" ->
            Decode.succeed OSTypes.ServerBuild

        "deleted" ->
            Decode.succeed OSTypes.ServerDeleted

        "error" ->
            Decode.succeed OSTypes.ServerError

        "hard_reboot" ->
            Decode.succeed OSTypes.ServerHardReboot

        "migrating" ->
            Decode.succeed OSTypes.ServerMigrating

        "password" ->
            Decode.succeed OSTypes.ServerPassword

        "paused" ->
            Decode.succeed OSTypes.ServerPaused

        "reboot" ->
            Decode.succeed OSTypes.ServerReboot

        "rebuild" ->
            Decode.succeed OSTypes.ServerRebuild

        "rescue" ->
            Decode.succeed OSTypes.ServerRescue

        "resize" ->
            Decode.succeed OSTypes.ServerResize

        "revert_resize" ->
            Decode.succeed OSTypes.ServerRevertResize

        "shelved" ->
            Decode.succeed OSTypes.ServerShelved

        "shelved_offloaded" ->
            Decode.succeed OSTypes.ServerShelvedOffloaded

        "shutoff" ->
            Decode.succeed OSTypes.ServerShutoff

        "soft_deleted" ->
            Decode.succeed OSTypes.ServerSoftDeleted

        "stopped" ->
            Decode.succeed OSTypes.ServerStopped

        "suspended" ->
            Decode.succeed OSTypes.ServerSuspended

        "unknown" ->
            Decode.succeed OSTypes.ServerUnknown

        "verify_resize" ->
            Decode.succeed OSTypes.ServerVerifyResize

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


serverFaultDecoder : Decode.Decoder OSTypes.ServerFault
serverFaultDecoder =
    Decode.map3 OSTypes.ServerFault
        (Decode.field "code" Decode.int)
        (Decode.field "created" (Decode.string |> Decode.andThen iso8601StringToPosixDecodeError))
        (Decode.field "message" Decode.string)


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
    Decode.map7 OSTypes.Flavor
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "vcpus" Decode.int)
        (Decode.field "ram" Decode.int)
        (Decode.field "disk" Decode.int)
        (Decode.field "OS-FLV-EXT-DATA:ephemeral" Decode.int)
        (Decode.oneOf
            [ Decode.field "extra_specs" metadataDecoder
            , Decode.succeed []
            ]
        )


decodeKeypairs : Decode.Decoder (List OSTypes.Keypair)
decodeKeypairs =
    Decode.field "keypairs" (Decode.list keypairDecoder)


keypairDecoder : Decode.Decoder OSTypes.Keypair
keypairDecoder =
    Decode.map3 OSTypes.Keypair
        (Decode.at [ "keypair", "name" ] Decode.string)
        (Decode.at [ "keypair", "public_key" ] Decode.string)
        (Decode.at [ "keypair", "fingerprint" ] Decode.string)
