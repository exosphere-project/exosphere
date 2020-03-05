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
    , receiveCreateServer
    , receiveFlavors
    , receiveKeypairs
    , receiveServer
    , receiveServers
    , requestConsoleUrls
    , requestCreateServer
    , requestCreateServerImage
    , requestDeleteServer
    , requestDeleteServers
    , requestFlavors
    , requestKeypairs
    , requestServer
    , requestServers
    , serverIpAddressDecoder
    , serverPowerStateDecoder
    )

import Array
import Base64
import Error exposing (ErrorContext, ErrorLevel(..))
import Helpers.Helpers as Helpers
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import OpenStack.ServerPassword as OSServerPassword
import OpenStack.Types as OSTypes
import RemoteData
import Rest.Cockpit exposing (requestCockpitIfRequestable)
import Rest.Helpers exposing (openstackCredentialedRequest, resultToMsg)
import Rest.Neutron exposing (getFloatingIpRequestPorts, requestNetworks)
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
        , ServerOrigin(..)
        , ViewState(..)
        , currentExoServerVersion
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
        Get
        (Just "compute 2.27")
        (project.endpoints.nova ++ "/servers/detail")
        Http.emptyBody
        (Http.expectJson
            resultToMsg_
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

        resultToMsg_ =
            resultToMsg
                errorContext
                (\server ->
                    ProjectMsg
                        (Helpers.getProjectId project)
                        (ReceiveServer server)
                )
    in
    openstackCredentialedRequest
        project
        Get
        (Just "compute 2.27")
        (project.endpoints.nova ++ "/servers/" ++ serverUuid)
        Http.emptyBody
        (Http.expectJson
            resultToMsg_
            (Decode.at [ "server" ] decodeServer)
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
                (Http.expectJson
                    (\result -> ProjectMsg (Helpers.getProjectId project) (ReceiveConsoleUrl serverUuid result))
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
            resultToMsg
                errorContext
                (\flavors -> ProjectMsg (Helpers.getProjectId project) <| ReceiveFlavors flavors)
    in
    openstackCredentialedRequest
        project
        Get
        Nothing
        (project.endpoints.nova ++ "/flavors/detail")
        Http.emptyBody
        (Http.expectJson
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
            resultToMsg
                errorContext
                (\keypairs -> ProjectMsg (Helpers.getProjectId project) <| ReceiveKeypairs keypairs)
    in
    openstackCredentialedRequest
        project
        Get
        Nothing
        (project.endpoints.nova ++ "/os-keypairs")
        Http.emptyBody
        (Http.expectJson
            resultToMsg_
            decodeKeypairs
        )


requestCreateServer : Project -> CreateServerRequest -> Cmd Msg
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

        renderedUserData =
            Helpers.renderUserDataTemplate project createServerRequest

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
                , ( "user_data", Encode.string (Base64.encode renderedUserData) )
                , ( "security_groups", Encode.array Encode.object (Array.fromList [ [ ( "name", Encode.string "exosphere" ) ] ]) )
                , ( "metadata"
                  , Encode.object
                        [ ( "exoServerVersion"
                          , Encode.string (String.fromInt currentExoServerVersion)
                          )
                        ]
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
                        Post
                        Nothing
                        (project.endpoints.nova ++ "/servers")
                        (Http.jsonBody requestBody)
                        (Http.expectJson
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
        Delete
        Nothing
        (project.endpoints.nova ++ "/servers/" ++ server.osProps.uuid)
        Http.emptyBody
        (Http.expectString
            resultToMsg_
        )


requestDeleteServers : Project -> List Server -> Cmd Msg
requestDeleteServers project serversToDelete =
    let
        deleteRequests =
            List.map (requestDeleteServer project) serversToDelete
    in
    Cmd.batch deleteRequests


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
        (Http.expectString
            (resultToMsg errorContext (\_ -> NoOp))
        )



{- HTTP Response Handling -}


receiveServers : Model -> Project -> List OSTypes.Server -> ( Model, Cmd Msg )
receiveServers model project osServers =
    let
        ( newExoServers, cmds ) =
            osServers
                |> List.map (receiveServer_ project)
                |> List.unzip

        projectNoDeletedSvrs =
            -- Remove recently deleted servers from existing project
            { project
                | servers =
                    RemoteData.Success <|
                        List.filter
                            (\s -> List.member s.osProps.uuid (List.map .uuid osServers))
                            (RemoteData.withDefault [] project.servers)
            }

        newProject =
            List.foldl
                (\s p -> Helpers.projectUpdateServer p s)
                projectNoDeletedSvrs
                newExoServers
    in
    ( Helpers.modelUpdateProject model newProject
    , Cmd.batch cmds
    )


receiveServer : Model -> Project -> OSTypes.Server -> ( Model, Cmd Msg )
receiveServer model project osServer =
    let
        ( newServer, cmd ) =
            receiveServer_ project osServer

        newProject =
            Helpers.projectUpdateServer project newServer
    in
    ( Helpers.modelUpdateProject model newProject
    , cmd
    )


receiveServer_ : Project -> OSTypes.Server -> ( Server, Cmd Msg )
receiveServer_ project osServer =
    let
        newServer =
            case Helpers.serverLookup project osServer.uuid of
                Nothing ->
                    let
                        defaultExoProps =
                            ExoServerProps
                                Unknown
                                False
                                False
                                Nothing
                                (Helpers.serverOrigin osServer.details)
                    in
                    Server osServer defaultExoProps

                Just exoServer ->
                    let
                        floatingIpState_ =
                            Helpers.checkFloatingIpState
                                osServer.details
                                exoServer.exoProps.floatingIpState

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
                    in
                    Server
                        { oldOSProps | details = osServer.details }
                        { oldExoProps
                            | floatingIpState = floatingIpState_
                            , targetOpenstackStatus = newTargetOpenstackStatus
                        }

        floatingIpCmd =
            case newServer.exoProps.floatingIpState of
                Requestable ->
                    [ getFloatingIpRequestPorts project newServer
                    , requestNetworks project
                    ]
                        |> Cmd.batch

                _ ->
                    Cmd.none

        consoleUrlCmd =
            requestConsoleUrlIfRequestable project newServer

        passwordCmd =
            case newServer.exoProps.serverOrigin of
                ServerNotFromExo ->
                    Cmd.none

                ServerFromExo serverFromExoProps ->
                    if serverFromExoProps.exoServerVersion < 1 then
                        Cmd.none

                    else
                        case Helpers.getServerExouserPassword newServer.osProps.details of
                            Nothing ->
                                OSServerPassword.requestServerPassword project newServer.osProps.uuid

                            Just _ ->
                                Cmd.none

        cockpitLoginCmd =
            requestCockpitIfRequestable project newServer

        allCmds =
            [ floatingIpCmd, consoleUrlCmd, passwordCmd, cockpitLoginCmd ]
                |> Cmd.batch
    in
    ( newServer, allCmds )


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
                ProjectView _ _ projectViewConstructor ->
                    case projectViewConstructor of
                        CreateServer createServerRequest ->
                            if createServerRequest.flavorUuid == "" then
                                let
                                    maybeSmallestFlavor =
                                        Helpers.sortedFlavors flavors |> List.head
                                in
                                case maybeSmallestFlavor of
                                    Just smallestFlavor ->
                                        ProjectView
                                            (Helpers.getProjectId project)
                                            { createPopup = False }
                                            (CreateServer
                                                { createServerRequest
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
                    ProjectView
                        (Helpers.getProjectId project)
                        { createPopup = False }
                    <|
                        ListProjectServers { onlyOwnServers = False } []
            }
    in
    ( newModel
    , [ requestServers
      , requestNetworks
      ]
        |> List.map (\x -> x project)
        |> Cmd.batch
    )



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
