module Rest exposing (addFloatingIpInServerDetails, createProvider, decodeAuthToken, decodeFlavors, decodeFloatingIpCreation, decodeImages, decodeKeypairs, decodeNetworks, decodePorts, decodeServerDetails, decodeServers, flavorDecoder, getFloatingIpRequestPorts, imageDecoder, imageStatusDecoder, ipAddressOpenstackTypeDecoder, keypairDecoder, networkDecoder, openstackEndpointDecoder, openstackEndpointInterfaceDecoder, openstackServiceDecoder, portDecoder, receiveAuthToken, receiveCreateServer, receiveFlavors, receiveFloatingIp, receiveImages, receiveKeypairs, receiveNetworks, receivePortsAndRequestFloatingIp, receiveServerDetail, receiveServers, requestAuthToken, requestCreateServer, requestDeleteServer, requestDeleteServers, requestFlavors, requestFloatingIp, requestFloatingIpIfRequestable, requestImages, requestKeypairs, requestNetworks, requestServerDetail, requestServers, serverDecoder, serverIpAddressDecoder, serverPowerStateDecoder)

import Base64
import Dict
import Helpers
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import RemoteData
import Types.OpenstackTypes as OpenstackTypes
import Types.Types exposing (..)



{- HTTP Requests -}


requestAuthToken : Model -> Cmd Msg
requestAuthToken model =
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
                                                [ ( "name", Encode.string model.creds.username )
                                                , ( "domain"
                                                  , Encode.object
                                                        [ ( "id", Encode.string model.creds.userDomain )
                                                        ]
                                                  )
                                                , ( "password", Encode.string model.creds.password )
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
                                        [ ( "name", Encode.string model.creds.projectName )
                                        , ( "domain"
                                          , Encode.object
                                                [ ( "id", Encode.string model.creds.projectDomain )
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
        , url = model.creds.authUrl
        , body = Http.jsonBody requestBody

        {- Todo handle no response? -}
        , expect = Http.expectStringResponse (\response -> Ok response)
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send ReceiveAuthToken


requestImages : Provider -> Cmd Msg
requestImages provider =
    let
        request =
            Http.request
                { method = "GET"
                , headers = [ Http.header "X-Auth-Token" provider.authToken ]
                , url = provider.endpoints.glance ++ "/v2/images?limit=999999"
                , body = Http.emptyBody
                , expect = Http.expectJson decodeImages
                , timeout = Nothing
                , withCredentials = False
                }

        resultMsg result =
            ProviderMsg provider.name (ReceiveImages result)
    in
    Http.send resultMsg request


requestServers : Provider -> Cmd Msg
requestServers provider =
    let
        request =
            Http.request
                { method = "GET"
                , headers = [ Http.header "X-Auth-Token" provider.authToken ]
                , url = provider.endpoints.nova ++ "/servers"
                , body = Http.emptyBody
                , expect = Http.expectJson decodeServers
                , timeout = Nothing
                , withCredentials = False
                }

        resultMsg result =
            ProviderMsg provider.name (ReceiveServers result)
    in
    Http.send resultMsg request


requestServerDetail : Provider -> ServerUuid -> Cmd Msg
requestServerDetail provider serverUuid =
    let
        request =
            Http.request
                { method = "GET"
                , headers = [ Http.header "X-Auth-Token" provider.authToken ]
                , url = provider.endpoints.nova ++ "/servers/" ++ serverUuid
                , body = Http.emptyBody
                , expect = Http.expectJson decodeServerDetails
                , timeout = Nothing
                , withCredentials = False
                }

        resultMsg result =
            ProviderMsg provider.name (ReceiveServerDetail serverUuid result)
    in
    Http.send resultMsg request


requestFlavors : Provider -> Cmd Msg
requestFlavors provider =
    let
        request =
            Http.request
                { method = "GET"
                , headers = [ Http.header "X-Auth-Token" provider.authToken ]
                , url = provider.endpoints.nova ++ "/flavors/detail"
                , body = Http.emptyBody
                , expect = Http.expectJson decodeFlavors
                , timeout = Nothing
                , withCredentials = False
                }

        resultMsg result =
            ProviderMsg provider.name (ReceiveFlavors result)
    in
    Http.send resultMsg request


requestKeypairs : Provider -> Cmd Msg
requestKeypairs provider =
    let
        request =
            Http.request
                { method = "GET"
                , headers = [ Http.header "X-Auth-Token" provider.authToken ]
                , url = provider.endpoints.nova ++ "/os-keypairs"
                , body = Http.emptyBody
                , expect = Http.expectJson decodeKeypairs
                , timeout = Nothing
                , withCredentials = False
                }

        resultMsg result =
            ProviderMsg provider.name (ReceiveKeypairs result)
    in
    Http.send resultMsg request


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
            [ ( "name", Encode.string instanceName )
            , ( "flavorRef", Encode.string innerCreateServerRequest.flavorUuid )
            , ( "key_name", Encode.string innerCreateServerRequest.keypairName )
            , ( "networks", Encode.string "auto" )
            , ( "user_data", Encode.string (Base64.encode innerCreateServerRequest.userData) )
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

        resultMsg result =
            ProviderMsg provider.name (ReceiveCreateServer result)
    in
    Cmd.batch
        (requestBodies
            |> List.map
                (\requestBody ->
                    Http.request
                        { method = "POST"
                        , headers =
                            [ Http.header "X-Auth-Token" provider.authToken

                            -- Microversion needed for automatic network provisioning
                            , Http.header "OpenStack-API-Version" "compute 2.38"
                            ]
                        , url = provider.endpoints.nova ++ "/servers"
                        , body = Http.jsonBody requestBody
                        , expect = Http.expectJson (Decode.field "server" serverDecoder)
                        , timeout = Nothing
                        , withCredentials = False
                        }
                        |> Http.send resultMsg
                )
        )


requestDeleteServer : Provider -> Server -> Cmd Msg
requestDeleteServer provider server =
    let
        request =
            Http.request
                { method = "DELETE"
                , headers = [ Http.header "X-Auth-Token" provider.authToken ]
                , url = provider.endpoints.nova ++ "/servers/" ++ server.uuid
                , body = Http.emptyBody
                , expect = Http.expectString
                , timeout = Nothing
                , withCredentials = False
                }

        resultMsg result =
            ProviderMsg provider.name (ReceiveDeleteServer result)
    in
    Http.send resultMsg request


requestDeleteServers : Provider -> List Server -> Cmd Msg
requestDeleteServers provider serversToDelete =
    let
        deleteRequests =
            List.map (requestDeleteServer provider) serversToDelete
    in
    Cmd.batch deleteRequests


requestNetworks : Provider -> Cmd Msg
requestNetworks provider =
    let
        request =
            Http.request
                { method = "GET"
                , headers = [ Http.header "X-Auth-Token" provider.authToken ]
                , url = provider.endpoints.neutron ++ "/v2.0/networks"
                , body = Http.emptyBody
                , expect = Http.expectJson decodeNetworks
                , timeout = Nothing
                , withCredentials = False
                }

        resultMsg result =
            ProviderMsg provider.name (ReceiveNetworks result)
    in
    Http.send resultMsg request


getFloatingIpRequestPorts : Provider -> Server -> Cmd Msg
getFloatingIpRequestPorts provider server =
    let
        request =
            Http.request
                { method = "GET"
                , headers = [ Http.header "X-Auth-Token" provider.authToken ]
                , url = provider.endpoints.neutron ++ "/v2.0/ports"
                , body = Http.emptyBody
                , expect = Http.expectJson decodePorts
                , timeout = Nothing
                , withCredentials = False
                }

        resultMsg result =
            ProviderMsg provider.name (GetFloatingIpReceivePorts server.uuid result)
    in
    Http.send resultMsg request


requestFloatingIpIfRequestable : Model -> Provider -> Network -> Port -> ServerUuid -> ( Model, Cmd Msg )
requestFloatingIpIfRequestable model provider network port_ serverUuid =
    let
        maybeServer =
            List.filter (\s -> s.uuid == serverUuid) (RemoteData.withDefault [] provider.servers)
                |> List.head
    in
    case maybeServer of
        Nothing ->
            Helpers.processError model "We should have a server here but we don't"

        Just server ->
            case server.floatingIpState of
                Requestable ->
                    requestFloatingIp model provider network port_ server

                _ ->
                    ( model, Cmd.none )


requestFloatingIp : Model -> Provider -> Network -> Port -> Server -> ( Model, Cmd Msg )
requestFloatingIp model provider network port_ server =
    let
        newServer =
            { server | floatingIpState = RequestedWaiting }

        otherServers =
            List.filter (\s -> s.uuid /= newServer.uuid) (RemoteData.withDefault [] provider.servers)

        newServers =
            newServer :: otherServers

        newProvider =
            { provider | servers = RemoteData.Success newServers }

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

        request =
            Http.request
                { method = "POST"
                , headers = [ Http.header "X-Auth-Token" provider.authToken ]
                , url = provider.endpoints.neutron ++ "/v2.0/floatingips"
                , body = Http.jsonBody requestBody
                , expect = Http.expectJson decodeFloatingIpCreation
                , timeout = Nothing
                , withCredentials = False
                }

        resultMsg result =
            ProviderMsg provider.name (ReceiveFloatingIp server.uuid result)

        cmd =
            Http.send resultMsg request
    in
    ( newModel, cmd )



{- HTTP Response Handling -}


receiveAuthToken : Model -> Result Http.Error (Http.Response String) -> ( Model, Cmd Msg )
receiveAuthToken model responseResult =
    case responseResult of
        Err error ->
            Helpers.processError model error

        Ok response ->
            createProvider model response


createProvider : Model -> Http.Response String -> ( Model, Cmd Msg )
createProvider model response =
    case Decode.decodeString decodeAuthToken response.body of
        Err error ->
            Helpers.processError model error

        Ok serviceCatalog ->
            let
                authToken =
                    Maybe.withDefault "" (Dict.get "X-Subject-Token" response.headers)

                endpoints =
                    Helpers.serviceCatalogToEndpoints serviceCatalog

                newProvider =
                    { name = Helpers.providerNameFromUrl model.creds.authUrl
                    , authToken = authToken
                    , endpoints = endpoints
                    , images = []
                    , servers = RemoteData.NotAsked
                    , flavors = []
                    , keypairs = []
                    , networks = []
                    , ports = []
                    }

                newProviders =
                    newProvider :: model.providers

                newModel =
                    { model
                        | providers = newProviders
                        , viewState = ProviderView newProvider.name ListProviderServers
                    }
            in
            ( newModel, requestServers newProvider )


receiveImages : Model -> Provider -> Result Http.Error (List Image) -> ( Model, Cmd Msg )
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


receiveServers : Model -> Provider -> Result Http.Error (List Server) -> ( Model, Cmd Msg )
receiveServers model provider result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok newServers ->
            -- Instead of just overwriting model.servers, copy `details`, `floatingIpState` and `selected` from
            -- existing, matching servers in model.servers (assuming there are corresponding ones)
            let
                serverUuidOfServer : Server -> ServerUuid
                serverUuidOfServer server =
                    server.uuid

                serverIsInListOfServers : List Server -> Server -> Bool
                serverIsInListOfServers servers server =
                    let
                        serverUuids =
                            List.map serverUuidOfServer servers

                        serverUuid =
                            serverUuidOfServer server
                    in
                    List.member serverUuid serverUuids

                existingServers =
                    RemoteData.withDefault [] provider.servers

                ( newServersInExistingServers, newServersNotInExistingServers ) =
                    List.partition (serverIsInListOfServers existingServers) newServers

                ( existingServersInNewServers, existingServersNotInNewServers ) =
                    List.partition (serverIsInListOfServers newServers) existingServers

                existingServersInNewServersSorted =
                    List.sortBy .uuid existingServersInNewServers

                newServersInExistingServersSorted =
                    List.sortBy .uuid newServersInExistingServers

                getCombinedMatchingNewAndExistingServers =
                    List.map2 (\a b -> ( a, b )) newServersInExistingServersSorted existingServersInNewServersSorted

                enrichNewServerWithExistingDetails : ( Server, Server ) -> Server
                enrichNewServerWithExistingDetails ( newServer, existingServer ) =
                    { existingServer | name = newServer.name }

                enrichNewServersWithExistingDetails : List ( Server, Server ) -> List Server
                enrichNewServersWithExistingDetails combinedMatchingNewAndExistingServers =
                    List.map enrichNewServerWithExistingDetails combinedMatchingNewAndExistingServers

                enrichedNewServers =
                    enrichNewServersWithExistingDetails getCombinedMatchingNewAndExistingServers

                newServersWithExistingMatchesAndWithout =
                    List.append newServersNotInExistingServers enrichedNewServers

                newServersWithExistingMatchesAndWithoutSorted =
                    List.sortBy .name newServersWithExistingMatchesAndWithout

                newProvider =
                    { provider | servers = RemoteData.Success newServersWithExistingMatchesAndWithoutSorted }

                newModel =
                    Helpers.modelUpdateProvider model newProvider
            in
            ( newModel, Cmd.none )


receiveServerDetail : Model -> Provider -> ServerUuid -> Result Http.Error ServerDetails -> ( Model, Cmd Msg )
receiveServerDetail model provider serverUuid result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok serverDetails ->
            let
                maybeServer =
                    List.filter
                        (\s -> s.uuid == serverUuid)
                        (RemoteData.withDefault [] provider.servers)
                        |> List.head
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
                                server.floatingIpState

                        newServer =
                            { server
                                | details = Just serverDetails
                                , floatingIpState = floatingIpState
                            }

                        otherServers =
                            List.filter
                                (\s -> s.uuid /= newServer.uuid)
                                (RemoteData.withDefault [] provider.servers)

                        newServers =
                            newServer :: otherServers

                        newServersSorted =
                            List.sortBy .name newServers

                        newProvider =
                            { provider | servers = RemoteData.Success newServersSorted }

                        newModel =
                            Helpers.modelUpdateProvider model newProvider
                    in
                    case floatingIpState of
                        Requestable ->
                            ( newModel
                            , Cmd.batch
                                [ getFloatingIpRequestPorts
                                    newProvider
                                    newServer
                                , requestNetworks
                                    newProvider
                                ]
                            )

                        _ ->
                            ( newModel, Cmd.none )


receiveFlavors : Model -> Provider -> Result Http.Error (List Flavor) -> ( Model, Cmd Msg )
receiveFlavors model provider result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok flavors ->
            let
                newProvider =
                    { provider | flavors = flavors }

                newModel =
                    Helpers.modelUpdateProvider model newProvider
            in
            ( newModel, Cmd.none )


receiveKeypairs : Model -> Provider -> Result Http.Error (List Keypair) -> ( Model, Cmd Msg )
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


receiveCreateServer : Model -> Provider -> Result Http.Error Server -> ( Model, Cmd Msg )
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


receiveNetworks : Model -> Provider -> Result Http.Error (List Network) -> ( Model, Cmd Msg )
receiveNetworks model provider result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok networks ->
            let
                newProvider =
                    { provider | networks = networks }

                newModel =
                    Helpers.modelUpdateProvider model newProvider
            in
            ( newModel, Cmd.none )


receivePortsAndRequestFloatingIp : Model -> Provider -> ServerUuid -> Result Http.Error (List Port) -> ( Model, Cmd Msg )
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
                            requestFloatingIpIfRequestable
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


receiveFloatingIp : Model -> Provider -> ServerUuid -> Result Http.Error IpAddress -> ( Model, Cmd Msg )
receiveFloatingIp model provider serverUuid result =
    let
        maybeServer =
            List.filter (\s -> s.uuid == serverUuid) (RemoteData.withDefault [] provider.servers)
                |> List.head
    in
    case maybeServer of
        Nothing ->
            Helpers.processError
                model
                "We should have a server here but we don't"

        Just server ->
            case result of
                Err error ->
                    let
                        newServer =
                            { server | floatingIpState = Failed }

                        otherServers =
                            List.filter (\s -> s.uuid /= newServer.uuid) (RemoteData.withDefault [] provider.servers)

                        newServers =
                            newServer :: otherServers

                        newProvider =
                            { provider | servers = RemoteData.Success newServers }

                        newModel =
                            Helpers.modelUpdateProvider model newProvider
                    in
                    Helpers.processError newModel error

                Ok ipAddress ->
                    let
                        newServer =
                            { server
                                | floatingIpState = Success
                                , details =
                                    addFloatingIpInServerDetails
                                        server.details
                                        ipAddress
                            }

                        otherServers =
                            List.filter
                                (\s -> s.uuid /= newServer.uuid)
                                (RemoteData.withDefault [] provider.servers)

                        newServers =
                            newServer :: otherServers

                        newProvider =
                            { provider | servers = RemoteData.Success newServers }

                        newModel =
                            Helpers.modelUpdateProvider model newProvider
                    in
                    ( newModel, Cmd.none )


addFloatingIpInServerDetails : Maybe ServerDetails -> IpAddress -> Maybe ServerDetails
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



{- JSON Decoders -}


decodeAuthToken : Decode.Decoder OpenstackTypes.ServiceCatalog
decodeAuthToken =
    Decode.at [ "token", "catalog" ] (Decode.list openstackServiceDecoder)


openstackServiceDecoder : Decode.Decoder OpenstackTypes.Service
openstackServiceDecoder =
    Decode.map3 OpenstackTypes.Service
        (Decode.field "name" Decode.string)
        (Decode.field "type" Decode.string)
        (Decode.field "endpoints" (Decode.list openstackEndpointDecoder))


openstackEndpointDecoder : Decode.Decoder OpenstackTypes.Endpoint
openstackEndpointDecoder =
    Decode.map2 OpenstackTypes.Endpoint
        (Decode.field "interface" Decode.string
            |> Decode.andThen openstackEndpointInterfaceDecoder
        )
        (Decode.field "url" Decode.string)


openstackEndpointInterfaceDecoder : String -> Decode.Decoder OpenstackTypes.EndpointInterface
openstackEndpointInterfaceDecoder interface =
    case interface of
        "public" ->
            Decode.succeed OpenstackTypes.Public

        "admin" ->
            Decode.succeed OpenstackTypes.Admin

        "internal" ->
            Decode.succeed OpenstackTypes.Internal

        _ ->
            Decode.fail "unrecognized interface type"


decodeImages : Decode.Decoder (List Image)
decodeImages =
    Decode.field "images" (Decode.list imageDecoder)


imageDecoder : Decode.Decoder Image
imageDecoder =
    Decode.map8 Image
        (Decode.field "name" Decode.string)
        (Decode.field "status" Decode.string |> Decode.andThen imageStatusDecoder)
        (Decode.field "id" Decode.string)
        (Decode.field "size" (Decode.nullable Decode.int))
        (Decode.field "checksum" (Decode.nullable Decode.string))
        (Decode.field "disk_format" (Decode.nullable Decode.string))
        (Decode.field "container_format" (Decode.nullable Decode.string))
        (Decode.field "tags" (Decode.list Decode.string))


imageStatusDecoder : String -> Decode.Decoder ImageStatus
imageStatusDecoder status =
    case status of
        "queued" ->
            Decode.succeed Queued

        "saving" ->
            Decode.succeed Saving

        "active" ->
            Decode.succeed Active

        "killed" ->
            Decode.succeed Killed

        "deleted" ->
            Decode.succeed Deleted

        "pending_delete" ->
            Decode.succeed PendingDelete

        "deactivated" ->
            Decode.succeed Deactivated

        _ ->
            Decode.fail "Unrecognized image status"


decodeServers : Decode.Decoder (List Server)
decodeServers =
    Decode.field "servers" (Decode.list serverDecoder)


serverDecoder : Decode.Decoder Server
serverDecoder =
    Decode.map6 Server
        (Decode.oneOf
            [ Decode.field "name" Decode.string
            , Decode.succeed ""
            ]
        )
        (Decode.field "id" Decode.string)
        (Decode.succeed Nothing)
        (Decode.succeed Unknown)
        (Decode.succeed False)
        (Decode.succeed False)


decodeServerDetails : Decode.Decoder ServerDetails
decodeServerDetails =
    Decode.map7 ServerDetails
        (Decode.at [ "server", "status" ] Decode.string)
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
        (Decode.at [ "server", "key_name" ] Decode.string)
        (Decode.oneOf
            [ Decode.at [ "server", "addresses", "auto_allocated_network" ] (Decode.list serverIpAddressDecoder)
            , Decode.succeed []
            ]
        )


serverPowerStateDecoder : Int -> Decode.Decoder ServerPowerState
serverPowerStateDecoder int =
    case int of
        0 ->
            Decode.succeed NoState

        1 ->
            Decode.succeed Running

        3 ->
            Decode.succeed Paused

        4 ->
            Decode.succeed Shutdown

        6 ->
            Decode.succeed Crashed

        7 ->
            Decode.succeed Suspended

        _ ->
            Decode.fail "Ooooooops, unrecognised server power state"


serverIpAddressDecoder : Decode.Decoder IpAddress
serverIpAddressDecoder =
    Decode.map2 IpAddress
        (Decode.field "addr" Decode.string)
        (Decode.field "OS-EXT-IPS:type" Decode.string
            |> Decode.andThen ipAddressOpenstackTypeDecoder
        )


ipAddressOpenstackTypeDecoder : String -> Decode.Decoder IpAddressOpenstackType
ipAddressOpenstackTypeDecoder string =
    case string of
        "fixed" ->
            Decode.succeed Fixed

        "floating" ->
            Decode.succeed Floating

        _ ->
            Decode.fail "oooooooops, unrecognised IP address type"


decodeFlavors : Decode.Decoder (List Flavor)
decodeFlavors =
    Decode.field "flavors" (Decode.list flavorDecoder)


flavorDecoder : Decode.Decoder Flavor
flavorDecoder =
    Decode.map6 Flavor
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "vcpus" Decode.int)
        (Decode.field "ram" Decode.int)
        (Decode.field "disk" Decode.int)
        (Decode.field "OS-FLV-EXT-DATA:ephemeral" Decode.int)


decodeKeypairs : Decode.Decoder (List Keypair)
decodeKeypairs =
    Decode.field "keypairs" (Decode.list keypairDecoder)


keypairDecoder : Decode.Decoder Keypair
keypairDecoder =
    Decode.map3 Keypair
        (Decode.at [ "keypair", "name" ] Decode.string)
        (Decode.at [ "keypair", "public_key" ] Decode.string)
        (Decode.at [ "keypair", "fingerprint" ] Decode.string)


decodeNetworks : Decode.Decoder (List Network)
decodeNetworks =
    Decode.field "networks" (Decode.list networkDecoder)


networkDecoder : Decode.Decoder Network
networkDecoder =
    Decode.map5 Network
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "admin_state_up" Decode.bool)
        (Decode.field "status" Decode.string)
        (Decode.field "router:external" Decode.bool)


decodePorts : Decode.Decoder (List Port)
decodePorts =
    Decode.field "ports" (Decode.list portDecoder)


portDecoder : Decode.Decoder Port
portDecoder =
    Decode.map4 Port
        (Decode.field "id" Decode.string)
        (Decode.field "device_id" Decode.string)
        (Decode.field "admin_state_up" Decode.bool)
        (Decode.field "status" Decode.string)


decodeFloatingIpCreation : Decode.Decoder IpAddress
decodeFloatingIpCreation =
    Decode.map2 IpAddress
        (Decode.at [ "floatingip", "floating_ip_address" ] Decode.string)
        (Decode.succeed Floating)
