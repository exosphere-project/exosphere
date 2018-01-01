module Rest exposing (..)

import Dict
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Base64
import Helpers
import Types exposing (..)


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
                                [ ( "methods", Encode.list [ Encode.string "password" ] )
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
            , url = model.creds.authURL
            , body = Http.jsonBody requestBody {- Todo handle no response? -}
            , expect = Http.expectStringResponse (\response -> Ok response)
            , timeout = Nothing
            , withCredentials = True
            }
            |> Http.send ReceiveAuth


requestImages : Model -> Cmd Msg
requestImages model =
    Http.request
        { method = "GET"
        , headers = [ Http.header "X-Auth-Token" model.authToken ]
        , url = model.endpoints.glance ++ "/v1/images"
        , body = Http.emptyBody
        , expect = Http.expectJson decodeImages
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send ReceiveImages


requestServers : Model -> Cmd Msg
requestServers model =
    Http.request
        { method = "GET"
        , headers = [ Http.header "X-Auth-Token" model.authToken ]
        , url = model.endpoints.nova ++ "/v2.1/servers"
        , body = Http.emptyBody
        , expect = Http.expectJson decodeServers
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send ReceiveServers


requestServerDetail : Model -> ServerUuid -> Cmd Msg
requestServerDetail model serverUuid =
    Http.request
        { method = "GET"
        , headers = [ Http.header "X-Auth-Token" model.authToken ]
        , url = model.endpoints.nova ++ "/v2.1/servers/" ++ serverUuid
        , body = Http.emptyBody
        , expect = Http.expectJson decodeServerDetails
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send (ReceiveServerDetail serverUuid)


requestFlavors : Model -> Cmd Msg
requestFlavors model =
    Http.request
        { method = "GET"
        , headers = [ Http.header "X-Auth-Token" model.authToken ]
        , url = model.endpoints.nova ++ "/v2.1/flavors"
        , body = Http.emptyBody
        , expect = Http.expectJson decodeFlavors
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send ReceiveFlavors


requestKeypairs : Model -> Cmd Msg
requestKeypairs model =
    Http.request
        { method = "GET"
        , headers = [ Http.header "X-Auth-Token" model.authToken ]
        , url = model.endpoints.nova ++ "/v2.1/os-keypairs"
        , body = Http.emptyBody
        , expect = Http.expectJson decodeKeypairs
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send ReceiveKeypairs


requestCreateServer : Model -> CreateServerRequest -> Cmd Msg
requestCreateServer model createServerRequest =
    let
        serverCount =
            Result.withDefault 1 (String.toInt createServerRequest.count)

        instanceNumbers =
            List.range 1 serverCount

        generateServerName : String -> Int -> Int -> String
        generateServerName baseName serverCount index =
            if serverCount == 1 then
                baseName
            else
                baseName ++ " " ++ Basics.toString index ++ " of " ++ Basics.toString serverCount

        instanceNames =
            instanceNumbers
                |> List.map (generateServerName createServerRequest.name serverCount)

        requestBodies =
            instanceNames
                |> List.map
                    (\x ->
                        Encode.object
                            [ ( "server"
                              , Encode.object
                                    [ ( "name", Encode.string x )
                                    , ( "flavorRef", Encode.string createServerRequest.flavorUuid )
                                    , ( "imageRef", Encode.string createServerRequest.imageUuid )
                                    , ( "key_name", Encode.string createServerRequest.keypairName )
                                    , ( "networks", Encode.string "auto" )
                                    , ( "user_data", Encode.string (Base64.encode createServerRequest.userData) )
                                    ]
                              )
                            ]
                    )
    in
        Cmd.batch
            (requestBodies
                |> List.map
                    (\requestBody ->
                        (Http.request
                            { method = "POST"
                            , headers =
                                [ Http.header "X-Auth-Token" model.authToken
                                  -- Microversion needed for automatic network provisioning
                                , Http.header "OpenStack-API-Version" "compute 2.38"
                                ]
                            , url = model.endpoints.nova ++ "/v2.1/servers"
                            , body = Http.jsonBody requestBody
                            , expect = Http.expectJson (Decode.field "server" serverDecoder)
                            , timeout = Nothing
                            , withCredentials = True
                            }
                            |> Http.send ReceiveCreateServer
                        )
                    )
            )


requestDeleteServer : Model -> Server -> Cmd Msg
requestDeleteServer model server =
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "X-Auth-Token" model.authToken ]
        , url = model.endpoints.nova ++ "/v2.1/servers/" ++ server.uuid
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send ReceiveDeleteServer


requestNetworks : Model -> Cmd Msg
requestNetworks model =
    Http.request
        { method = "GET"
        , headers = [ Http.header "X-Auth-Token" model.authToken ]
        , url = model.endpoints.neutron ++ "/v2.0/networks"
        , body = Http.emptyBody
        , expect = Http.expectJson decodeNetworks
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send ReceiveNetworks


getFloatingIpRequestPorts : Model -> Server -> Cmd Msg
getFloatingIpRequestPorts model server =
    Http.request
        { method = "GET"
        , headers = [ Http.header "X-Auth-Token" model.authToken ]
        , url = model.endpoints.neutron ++ "/v2.0/ports"
        , body = Http.emptyBody
        , expect = Http.expectJson decodePorts
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send (GetFloatingIpReceivePorts server.uuid)


requestFloatingIpIfRequestable : Model -> Network -> Port -> ServerUuid -> ( Model, Cmd Msg )
requestFloatingIpIfRequestable model network port_ serverUuid =
    let
        maybeServer =
            List.filter (\s -> s.uuid == serverUuid) model.servers
                |> List.head
    in
        case maybeServer of
            Nothing ->
                Helpers.processError model "We should have a server here but we don't"

            Just server ->
                case server.floatingIpState of
                    Requestable ->
                        requestFloatingIp model network port_ server

                    _ ->
                        ( model, Cmd.none )


requestFloatingIp : Model -> Network -> Port -> Server -> ( Model, Cmd Msg )
requestFloatingIp model network port_ server =
    let
        newServer =
            { server | floatingIpState = RequestedWaiting }

        otherServers =
            List.filter (\s -> s.uuid /= newServer.uuid) model.servers

        newServers =
            newServer :: otherServers

        newModel =
            { model | servers = newServers }

        requestBody =
            Encode.object
                [ ( "floatingip"
                  , Encode.object
                        [ ( "floating_network_id", Encode.string network.uuid )
                        , ( "port_id", Encode.string port_.uuid )
                        ]
                  )
                ]

        cmd =
            Http.request
                { method = "POST"
                , headers = [ Http.header "X-Auth-Token" model.authToken ]
                , url = model.endpoints.neutron ++ "/v2.0/floatingips"
                , body = Http.jsonBody requestBody
                , expect = Http.expectJson decodeFloatingIpCreation
                , timeout = Nothing
                , withCredentials = True
                }
                |> Http.send (ReceiveFloatingIp server.uuid)
    in
        ( newModel, cmd )



{- HTTP Response Handling -}


receiveAuth : Model -> Result Http.Error (Http.Response String) -> ( Model, Cmd Msg )
receiveAuth model responseResult =
    case responseResult of
        Err error ->
            Helpers.processError model error

        Ok response ->
            let
                authToken =
                    Maybe.withDefault "" (Dict.get "X-Subject-Token" response.headers)

                newModel =
                    { model | authToken = authToken, viewState = ListUserServers }
            in
                ( newModel, Cmd.none )


receiveImages : Model -> Result Http.Error (List Image) -> ( Model, Cmd Msg )
receiveImages model result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok images ->
            ( { model | images = images }, Cmd.none )


receiveServers : Model -> Result Http.Error (List Server) -> ( Model, Cmd Msg )
receiveServers model result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok servers ->
            ( { model | servers = servers }, Cmd.none )


receiveServerDetail : Model -> ServerUuid -> Result Http.Error ServerDetails -> ( Model, Cmd Msg )
receiveServerDetail model serverUuid result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok serverDetails ->
            let
                maybeServer =
                    List.filter (\s -> s.uuid == serverUuid) model.servers
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
                                List.filter (\s -> s.uuid /= newServer.uuid) model.servers

                            newServers =
                                newServer :: otherServers

                            newModel =
                                { model
                                    | servers = newServers
                                    , {- TODO take this out? -} viewState = ServerDetail newServer.uuid
                                }
                        in
                            case floatingIpState of
                                Requestable ->
                                    ( newModel
                                    , Cmd.batch
                                        [ getFloatingIpRequestPorts newModel newServer
                                        , requestNetworks newModel
                                        ]
                                    )

                                _ ->
                                    ( newModel, Cmd.none )


receiveFlavors : Model -> Result Http.Error (List Flavor) -> ( Model, Cmd Msg )
receiveFlavors model result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok flavors ->
            ( { model | flavors = flavors }, Cmd.none )


receiveKeypairs : Model -> Result Http.Error (List Keypair) -> ( Model, Cmd Msg )
receiveKeypairs model result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok keypairs ->
            ( { model | keypairs = keypairs }, Cmd.none )


receiveCreateServer : Model -> Result Http.Error Server -> ( Model, Cmd Msg )
receiveCreateServer model result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok _ ->
            let
                newModel =
                    { model | viewState = ListUserServers }
            in
                ( newModel
                , Cmd.batch
                    [ requestServers model
                    , requestNetworks model
                    ]
                )


receiveNetworks : Model -> Result Http.Error (List Network) -> ( Model, Cmd Msg )
receiveNetworks model result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok networks ->
            ( { model | networks = networks }, Cmd.none )


receivePortsAndRequestFloatingIp : Model -> ServerUuid -> Result Http.Error (List Port) -> ( Model, Cmd Msg )
receivePortsAndRequestFloatingIp model serverUuid result =
    case result of
        Err error ->
            Helpers.processError model error

        Ok ports ->
            let
                newModel =
                    { model | ports = ports }

                maybeExtNet =
                    Helpers.getExternalNetwork model

                maybePortForServer =
                    List.filter (\port_ -> port_.deviceUuid == serverUuid) ports
                        |> List.head
            in
                case maybeExtNet of
                    Just extNet ->
                        case maybePortForServer of
                            Just port_ ->
                                requestFloatingIpIfRequestable newModel extNet port_ serverUuid

                            Nothing ->
                                Helpers.processError
                                    model
                                    "We should have a port here but we don't!?"

                    Nothing ->
                        Helpers.processError
                            model
                            "We should have an external network here but we don't"


receiveFloatingIp : Model -> ServerUuid -> Result Http.Error IpAddress -> ( Model, Cmd Msg )
receiveFloatingIp model serverUuid result =
    let
        maybeServer =
            List.filter (\s -> s.uuid == serverUuid) model.servers
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
                                List.filter (\s -> s.uuid /= newServer.uuid) model.servers

                            newServers =
                                newServer :: otherServers

                            newModel =
                                { model | servers = newServers }
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
                                List.filter (\s -> s.uuid /= newServer.uuid) model.servers

                            newServers =
                                newServer :: otherServers

                            newModel =
                                { model | servers = newServers }
                        in
                            ( newModel, Cmd.none )


addFloatingIpInServerDetails : Maybe ServerDetails -> IpAddress -> Maybe ServerDetails
addFloatingIpInServerDetails serverDetails ipAddress =
    case serverDetails of
        Nothing ->
            Nothing

        Just serverDetails ->
            let
                newIps =
                    ipAddress :: serverDetails.ipAddresses
            in
                Just { serverDetails | ipAddresses = newIps }



{- JSON Decoders -}


decodeImages : Decode.Decoder (List Image)
decodeImages =
    Decode.field "images" (Decode.list imageDecoder)


imageDecoder : Decode.Decoder Image
imageDecoder =
    Decode.map6 Image
        (Decode.field "name" Decode.string)
        (Decode.field "id" Decode.string)
        (Decode.field "size" Decode.int)
        (Decode.field "checksum" Decode.string)
        (Decode.field "disk_format" Decode.string)
        (Decode.field "container_format" Decode.string)


decodeServers : Decode.Decoder (List Server)
decodeServers =
    Decode.field "servers" (Decode.list serverDecoder)


serverDecoder : Decode.Decoder Server
serverDecoder =
    Decode.map4 Server
        (Decode.oneOf
            [ Decode.field "name" Decode.string
            , Decode.succeed ""
            ]
        )
        (Decode.field "id" Decode.string)
        (Decode.succeed Nothing)
        (Decode.succeed Unknown)


decodeServerDetails : Decode.Decoder ServerDetails
decodeServerDetails =
    Decode.map5 ServerDetails
        (Decode.at [ "server", "status" ] Decode.string)
        (Decode.at [ "server", "created" ] Decode.string)
        (Decode.at [ "server", "OS-EXT-STS:power_state" ] Decode.int)
        (Decode.at [ "server", "key_name" ] Decode.string)
        (Decode.oneOf
            [ Decode.at [ "server", "addresses", "auto_allocated_network" ] (Decode.list serverIpAddressDecoder)
            , Decode.succeed []
            ]
        )


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
    Decode.map2 Flavor
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)


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
