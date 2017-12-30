module Main exposing (main)

import Dict
import Html exposing (Html, button, div, fieldset, h2, input, label, p, strong, table, td, text, textarea, th, tr)
import Html.Attributes exposing (cols, placeholder, rows, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Filesize exposing (format)
import Base64


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )



{- Todo remove default creds once storing this in local storage -}


init =
    ( { authToken = "" {- Todo remove the following hard coding and decode JSON in auth token response -}
      , endpoints =
            { glance = "https://tombstone-cloud.cyverse.org:9292"
            , nova = "https://tombstone-cloud.cyverse.org:8774"
            , neutron = "https://tombstone-cloud.cyverse.org:9696"
            }
      , creds =
            Creds
                "https://tombstone-cloud.cyverse.org:8000/v3/auth/tokens"
                "default"
                "demo"
                "default"
                "demo"
                ""
            {- password -}
      , messages = []
      , images = []
      , servers = []
      , viewState = Login
      , flavors = []
      , keypairs = []
      , networks = []
      , ports = []
      }
    , Cmd.none
    )


type alias Model =
    { authToken : String
    , endpoints : Endpoints
    , creds : Creds
    , messages : List String
    , images : List Image
    , servers : List Server
    , viewState : ViewState
    , flavors : List Flavor
    , keypairs : List Keypair
    , networks : List Network
    , ports : List Port
    }


type ViewState
    = Login
    | Home
    | ListImages
    | ListUserServers
    | ServerDetail Server
    | CreateServer CreateServerRequest


type alias Creds =
    { authURL : String
    , projectDomain : String
    , projectName : String
    , userDomain : String
    , username : String
    , password : String
    }


type alias Endpoints =
    { glance : String
    , nova : String
    , neutron : String
    }


type alias Image =
    { name : String
    , uuid : ImageUuid
    , size : Int
    , checksum : String
    , diskFormat : String
    , containerFormat : String
    }


type alias Server =
    { name : String
    , uuid : ServerUuid
    , details : Maybe ServerDetails
    , floatingIpState : FloatingIpState
    }


type FloatingIpState
    = Unknown
    | NotRequestable
    | Requestable
    | RequestedWaiting
    | Success
    | Failed



{- Todo add to ServerDetail:
   - Flavor
   - Image
   - Metadata
   - Volumes
   - Security Groups
   - Etc

   Also, make status and powerState union types, keypairName a key type, created a real date/time, etc
-}


type alias ServerDetails =
    { status : String
    , created : String
    , powerState : Int
    , keypairName : String
    , ipAddresses : List IpAddress
    }


type alias CreateServerRequest =
    { name : String
    , imageUuid : ImageUuid
    , flavorUuid : FlavorUuid
    , keypairName : String
    , userData : String
    }


type alias Flavor =
    { uuid : FlavorUuid
    , name : String
    }


type alias Keypair =
    { name : String
    , publicKey : String
    , fingerprint : String
    }


type alias IpAddress =
    { address : String
    , openstackType : IpAddressOpenstackType
    }


type IpAddressOpenstackType
    = Fixed
    | Floating


type alias Network =
    { uuid : NetworkUuid
    , name : String
    , adminStateUp : Bool
    , status : String
    , isExternal : Bool
    }


type alias Port =
    { uuid : PortUuid
    , deviceUuid : ServerUuid
    , adminStateUp : Bool
    , status : String
    }


type alias ServerUuid =
    Uuid


type alias ImageUuid =
    Uuid


type alias NetworkUuid =
    Uuid


type alias PortUuid =
    Uuid


type alias FlavorUuid =
    Uuid


type alias Uuid =
    String


type Msg
    = ChangeViewState ViewState
    | RequestAuth
    | RequestCreateServer CreateServerRequest
    | RequestDeleteServer Server
    | ReceiveAuth (Result Http.Error (Http.Response String))
    | ReceiveImages (Result Http.Error (List Image))
    | ReceiveServers (Result Http.Error (List Server))
    | ReceiveServerDetail Server (Result Http.Error ServerDetails)
    | ReceiveCreateServer (Result Http.Error Server)
    | ReceiveDeleteServer (Result Http.Error String)
    | ReceiveFlavors (Result Http.Error (List Flavor))
    | ReceiveKeypairs (Result Http.Error (List Keypair))
    | ReceiveNetworks (Result Http.Error (List Network))
    | GetFloatingIpReceivePorts Server (Result Http.Error (List Port))
    | ReceiveFloatingIp Server (Result Http.Error String)
    | InputAuthURL String
    | InputProjectDomain String
    | InputProjectName String
    | InputUserDomain String
    | InputUsername String
    | InputPassword String
    | InputCreateServerName CreateServerRequest String
    | InputCreateServerImage CreateServerRequest String
    | InputCreateServerUserData CreateServerRequest String
    | InputCreateServerSize CreateServerRequest String
    | InputCreateServerKeypairName CreateServerRequest String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeViewState state ->
            let
                newModel =
                    { model | viewState = state }
            in
                case state of
                    Login ->
                        ( newModel, Cmd.none )

                    Home ->
                        ( newModel, Cmd.none )

                    ListImages ->
                        ( newModel, requestImages newModel )

                    ListUserServers ->
                        ( newModel, requestServers newModel )

                    ServerDetail server ->
                        ( newModel, requestServerDetail newModel server )

                    CreateServer _ ->
                        ( newModel, Cmd.batch [ requestFlavors newModel, requestKeypairs newModel ] )

        RequestAuth ->
            ( model, requestAuthToken model )

        RequestCreateServer createServerRequest ->
            ( model, requestCreateServer model createServerRequest )

        RequestDeleteServer server ->
            ( { model | servers = List.filter (\s -> s /= server) model.servers }
            , requestDeleteServer model server
            )

        ReceiveAuth response ->
            receiveAuth model response

        ReceiveImages result ->
            receiveImages model result

        ReceiveServers result ->
            receiveServers model result

        ReceiveServerDetail server result ->
            receiveServerDetail model server result

        ReceiveFlavors result ->
            receiveFlavors model result

        ReceiveKeypairs result ->
            receiveKeypairs model result

        ReceiveCreateServer result ->
            receiveCreateServer model result

        ReceiveDeleteServer _ ->
            {- Todo this ignores the result of server deletion API call, we should display errors to user -}
            update (ChangeViewState Home) model

        ReceiveNetworks result ->
            receiveNetworks model result

        GetFloatingIpReceivePorts server result ->
            receivePortsAndRequestFloatingIp model server result

        ReceiveFloatingIp server result ->
            receiveFloatingIp model server result

        {- Form inputs -}
        InputAuthURL authURL ->
            let
                creds =
                    model.creds
            in
                ( { model | creds = { creds | authURL = authURL } }, Cmd.none )

        InputProjectDomain projectDomain ->
            let
                creds =
                    model.creds
            in
                ( { model | creds = { creds | projectDomain = projectDomain } }, Cmd.none )

        InputProjectName projectName ->
            let
                creds =
                    model.creds
            in
                ( { model | creds = { creds | projectName = projectName } }, Cmd.none )

        InputUserDomain userDomain ->
            let
                creds =
                    model.creds
            in
                ( { model | creds = { creds | userDomain = userDomain } }, Cmd.none )

        InputUsername username ->
            let
                creds =
                    model.creds
            in
                ( { model | creds = { creds | username = username } }, Cmd.none )

        InputPassword password ->
            let
                creds =
                    model.creds
            in
                ( { model | creds = { creds | password = password } }, Cmd.none )

        InputCreateServerName createServerRequest name ->
            let
                viewState =
                    CreateServer { createServerRequest | name = name }
            in
                ( { model | viewState = viewState }, Cmd.none )

        InputCreateServerImage createServerRequest imageUuid ->
            let
                viewState =
                    CreateServer { createServerRequest | imageUuid = imageUuid }
            in
                ( { model | viewState = viewState }, Cmd.none )

        InputCreateServerUserData createServerRequest userData ->
            let
                viewState =
                    CreateServer { createServerRequest | userData = userData }
            in
                ( { model | viewState = viewState }, Cmd.none )

        InputCreateServerSize createServerRequest flavorUuid ->
            let
                viewState =
                    CreateServer { createServerRequest | flavorUuid = flavorUuid }
            in
                ( { model | viewState = viewState }, Cmd.none )

        InputCreateServerKeypairName createServerRequest keypairName ->
            let
                viewState =
                    CreateServer { createServerRequest | keypairName = keypairName }
            in
                ( { model | viewState = viewState }, Cmd.none )



{- HTTP and JSON -}


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


receiveAuth : Model -> Result Http.Error (Http.Response String) -> ( Model, Cmd Msg )
receiveAuth model responseResult =
    case responseResult of
        Err error ->
            processError model error

        Ok response ->
            let
                authToken =
                    Maybe.withDefault "" (Dict.get "X-Subject-Token" response.headers)

                newModel =
                    { model | authToken = authToken, viewState = Home }
            in
                ( newModel, requestNetworks newModel )


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


receiveImages : Model -> Result Http.Error (List Image) -> ( Model, Cmd Msg )
receiveImages model result =
    case result of
        Err error ->
            processError model error

        Ok images ->
            ( { model | images = images }, Cmd.none )


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


receiveServers : Model -> Result Http.Error (List Server) -> ( Model, Cmd Msg )
receiveServers model result =
    case result of
        Err error ->
            processError model error

        Ok servers ->
            ( { model | servers = servers }, Cmd.none )


requestServerDetail : Model -> Server -> Cmd Msg
requestServerDetail model server =
    Http.request
        { method = "GET"
        , headers = [ Http.header "X-Auth-Token" model.authToken ]
        , url = model.endpoints.nova ++ "/v2.1/servers/" ++ server.uuid
        , body = Http.emptyBody
        , expect = Http.expectJson decodeServerDetails
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send (ReceiveServerDetail server)


receiveServerDetail : Model -> Server -> Result Http.Error ServerDetails -> ( Model, Cmd Msg )
receiveServerDetail model server result =
    case result of
        Err error ->
            processError model error

        Ok serverDetails ->
            let
                floatingIpState =
                    checkFloatingIpState serverDetails server.floatingIpState

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
                        , {- TODO take this out? -} viewState = ServerDetail newServer
                    }
            in
                case floatingIpState of
                    Requestable ->
                        ( newModel, getFloatingIpRequestPorts newModel newServer )

                    _ ->
                        ( newModel, Cmd.none )


checkFloatingIpState : ServerDetails -> FloatingIpState -> FloatingIpState
checkFloatingIpState serverDetails floatingIpState =
    let
        hasFixedIp =
            List.filter (\a -> a.openstackType == Fixed) serverDetails.ipAddresses
                |> List.isEmpty
                |> not

        hasFloatingIp =
            List.filter (\a -> a.openstackType == Floating) serverDetails.ipAddresses
                |> List.isEmpty
                |> not

        isActive =
            serverDetails.status == "ACTIVE"
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



{- Todo for now this only handles IPs on the auto-allocated network -}


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


decodeFlavors : Decode.Decoder (List Flavor)
decodeFlavors =
    Decode.field "flavors" (Decode.list flavorDecoder)


flavorDecoder : Decode.Decoder Flavor
flavorDecoder =
    Decode.map2 Flavor
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)


receiveFlavors : Model -> Result Http.Error (List Flavor) -> ( Model, Cmd Msg )
receiveFlavors model result =
    case result of
        Err error ->
            processError model error

        Ok flavors ->
            ( { model | flavors = flavors }, Cmd.none )


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


decodeKeypairs : Decode.Decoder (List Keypair)
decodeKeypairs =
    Decode.field "keypairs" (Decode.list keypairDecoder)


keypairDecoder : Decode.Decoder Keypair
keypairDecoder =
    Decode.map3 Keypair
        (Decode.at [ "keypair", "name" ] Decode.string)
        (Decode.at [ "keypair", "public_key" ] Decode.string)
        (Decode.at [ "keypair", "fingerprint" ] Decode.string)


receiveKeypairs : Model -> Result Http.Error (List Keypair) -> ( Model, Cmd Msg )
receiveKeypairs model result =
    case result of
        Err error ->
            processError model error

        Ok keypairs ->
            ( { model | keypairs = keypairs }, Cmd.none )


requestCreateServer : Model -> CreateServerRequest -> Cmd Msg
requestCreateServer model createServerRequest =
    let
        requestBody =
            Encode.object
                [ ( "server"
                  , Encode.object
                        [ ( "name", Encode.string createServerRequest.name )
                        , ( "flavorRef", Encode.string createServerRequest.flavorUuid )
                        , ( "imageRef", Encode.string createServerRequest.imageUuid )
                        , ( "key_name", Encode.string createServerRequest.keypairName )
                        , ( "networks", Encode.string "auto" )
                        , ( "user_data", Encode.string (Base64.encode createServerRequest.userData) )
                        ]
                  )
                ]
    in
        Http.request
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


receiveCreateServer : Model -> Result Http.Error Server -> ( Model, Cmd Msg )
receiveCreateServer model result =
    case result of
        Err error ->
            (processError model error)

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


receiveNetworks : Model -> Result Http.Error (List Network) -> ( Model, Cmd Msg )
receiveNetworks model result =
    case result of
        Err error ->
            processError model error

        Ok networks ->
            ( { model | networks = networks }, Cmd.none )


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
        |> Http.send (GetFloatingIpReceivePorts server)


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


receivePortsAndRequestFloatingIp : Model -> Server -> Result Http.Error (List Port) -> ( Model, Cmd Msg )
receivePortsAndRequestFloatingIp model server result =
    case result of
        Err error ->
            processError model error

        Ok ports ->
            let
                newServer =
                    { server | floatingIpState = RequestedWaiting }

                otherServers =
                    List.filter (\s -> s.uuid /= newServer.uuid) model.servers

                newServers =
                    newServer :: otherServers

                newModel =
                    { model | ports = ports, servers = newServers }

                maybeExtNet =
                    getExternalNetwork model

                maybePortForServer =
                    List.filter (\port_ -> port_.deviceUuid == server.uuid) ports
                        |> List.head
            in
                case maybeExtNet of
                    Just extNet ->
                        case maybePortForServer of
                            Just port_ ->
                                ( newModel, requestFloatingIp newModel extNet port_ newServer )

                            Nothing ->
                                processError model "We should have a port here but we don't!?"

                    Nothing ->
                        processError model "We should have an external network here but we don't"



{- Tech debt: This doesn't handle the case where there is >1 external network -}


getExternalNetwork : Model -> Maybe Network
getExternalNetwork model =
    List.filter (\n -> n.isExternal) model.networks |> List.head


requestFloatingIp : Model -> Network -> Port -> Server -> Cmd Msg
requestFloatingIp model network port_ server =
    let
        requestBody =
            Encode.object
                [ ( "floatingip"
                  , Encode.object
                        [ ( "floating_network_id", Encode.string network.uuid )
                        , ( "port_id", Encode.string port_.uuid )
                        ]
                  )
                ]
    in
        Http.request
            { method = "POST"
            , headers = [ Http.header "X-Auth-Token" model.authToken ]
            , url = model.endpoints.neutron ++ "/v2.0/floatingips"
            , body = Http.jsonBody requestBody
            , expect = Http.expectString
            , timeout = Nothing
            , withCredentials = True
            }
            |> Http.send (ReceiveFloatingIp server)


receiveFloatingIp : Model -> Server -> Result Http.Error String -> ( Model, Cmd Msg )
receiveFloatingIp model server result =
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
                processError newModel error

        Ok _ ->
            let
                newServer =
                    { server | floatingIpState = Success }

                otherServers =
                    List.filter (\s -> s.uuid /= newServer.uuid) model.servers

                newServers =
                    newServer :: otherServers

                newModel =
                    { model | servers = newServers }
            in
                ( newModel, Cmd.none )


processError : Model -> a -> ( Model, Cmd Msg )
processError model error =
    let
        newMsgs =
            toString error :: model.messages
    in
        ( { model | messages = newMsgs }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ viewMessages model
        , case model.viewState of
            Login ->
                div [] []

            _ ->
                viewNav model
        , case model.viewState of
            Login ->
                viewLogin model

            Home ->
                div [] []

            ListImages ->
                viewImages model

            ListUserServers ->
                viewServers model

            ServerDetail server ->
                viewServerDetail server

            CreateServer createServerRequest ->
                viewCreateServer model createServerRequest
        ]


renderMessage : String -> Html Msg
renderMessage message =
    p [] [ text message ]


viewMessages : Model -> Html Msg
viewMessages model =
    div [] (List.map renderMessage model.messages)


viewLogin : Model -> Html Msg
viewLogin model =
    div []
        [ h2 [] [ text "Please log in" ]
        , table []
            [ tr []
                [ td [] [ text "Keystone auth URL" ]
                , td []
                    [ input
                        [ type_ "text"
                        , value model.creds.authURL
                        , placeholder "Auth URL e.g. https://mycloud.net:5000/v3"
                        , onInput InputAuthURL
                        ]
                        []
                    ]
                ]
            , tr []
                [ td [] [ text "Project Domain" ]
                , td []
                    [ input
                        [ type_ "text"
                        , value model.creds.projectDomain
                        , onInput InputProjectDomain
                        ]
                        []
                    ]
                ]
            , tr []
                [ td [] [ text "Project Name" ]
                , td []
                    [ input
                        [ type_ "text"
                        , value model.creds.projectName
                        , onInput InputProjectName
                        ]
                        []
                    ]
                ]
            , tr []
                [ td [] [ text "User Domain" ]
                , td []
                    [ input
                        [ type_ "text"
                        , value model.creds.userDomain
                        , onInput InputUserDomain
                        ]
                        []
                    ]
                ]
            , tr []
                [ td [] [ text "User Name" ]
                , td []
                    [ input
                        [ type_ "text"
                        , value model.creds.username
                        , onInput InputUsername
                        ]
                        []
                    ]
                ]
            , tr []
                [ td [] [ text "Password" ]
                , td []
                    [ input
                        [ type_ "text"
                        , value model.creds.password
                        , onInput InputPassword
                        ]
                        []
                    ]
                ]
            ]
        , button [ onClick RequestAuth ] [ text "Log in" ]
        ]


viewImages : Model -> Html Msg
viewImages model =
    div [] (List.map renderImage model.images)


renderImage : Image -> Html Msg
renderImage image =
    div []
        [ p [] [ strong [] [ text image.name ] ]
        , button [ onClick (ChangeViewState (CreateServer (CreateServerRequest "" image.uuid "" "" ""))) ] [ text "Launch" ]
        , table []
            [ tr []
                [ th [] [ text "Property" ]
                , th [] [ text "Value" ]
                ]
            , tr []
                [ td [] [ text "Size" ]
                , td [] [ text (format image.size) ]
                ]
            , tr []
                [ td [] [ text "Checksum" ]
                , td [] [ text image.checksum ]
                ]
            , tr []
                [ td [] [ text "Disk format" ]
                , td [] [ text image.diskFormat ]
                ]
            , tr []
                [ td [] [ text "Container format" ]
                , td [] [ text image.containerFormat ]
                ]
            , tr []
                [ td [] [ text "UUID" ]
                , td [] [ text image.uuid ]
                ]
            ]
        ]


viewServers : Model -> Html Msg
viewServers model =
    div [] (List.map renderServer model.servers)


renderServer : Server -> Html Msg
renderServer server =
    div []
        [ p [] [ strong [] [ text server.name ] ]
        , text ("UUID: " ++ server.uuid)
        , button [ onClick (ChangeViewState (ServerDetail server)) ] [ text "Details" ]
        , button [ onClick (RequestDeleteServer server) ] [ text "Delete" ]
        ]


viewNav : Model -> Html Msg
viewNav _ =
    div []
        [ h2 [] [ text "Navigation" ]
        , button [ onClick (ChangeViewState Home) ] [ text "Home" ]
        , button [ onClick (ChangeViewState ListImages) ] [ text "Images" ]
        , button [ onClick (ChangeViewState ListUserServers) ] [ text "My Servers" ]
        ]


viewServerDetail : Server -> Html Msg
viewServerDetail server =
    case server.details of
        Nothing ->
            text "Retrieving details??"

        Just details ->
            div []
                [ table []
                    [ tr []
                        [ th [] [ text "Property" ]
                        , th [] [ text "Value" ]
                        ]
                    , tr []
                        [ td [] [ text "Name" ]
                        , td [] [ text server.name ]
                        ]
                    , tr []
                        [ td [] [ text "UUID" ]
                        , td [] [ text server.uuid ]
                        ]
                    , tr []
                        [ td [] [ text "Created on" ]
                        , td [] [ text details.created ]
                        ]
                    , tr []
                        [ td [] [ text "Status" ]
                        , td [] [ text details.status ]
                        ]
                    , tr []
                        [ td [] [ text "Power state" ]
                        , td [] [ text (toString details.powerState) ]
                        ]
                    , tr []
                        [ td [] [ text "SSH Key Name" ]
                        , td [] [ text details.keypairName ]
                        ]
                    , tr []
                        [ td [] [ text "IP addresses" ]
                        , td [] [ renderIpAddresses details.ipAddresses ]
                        ]
                    ]
                ]


renderIpAddresses : List IpAddress -> Html Msg
renderIpAddresses ipAddresses =
    div [] (List.map renderIpAddress ipAddresses)


renderIpAddress : IpAddress -> Html Msg
renderIpAddress ipAddress =
    p []
        [ text (toString ipAddress.openstackType ++ ": " ++ ipAddress.address)
        ]


viewCreateServer : Model -> CreateServerRequest -> Html Msg
viewCreateServer model createServerRequest =
    div []
        [ h2 [] [ text "Create Server" ]
        , table []
            [ tr []
                [ th [] [ text "Property" ]
                , th [] [ text "Value" ]
                ]
            , tr []
                [ td [] [ text "Server Name" ]
                , td []
                    [ input
                        [ type_ "text"
                        , placeholder "My Server"
                        , value createServerRequest.name
                        , onInput (InputCreateServerName createServerRequest)
                        ]
                        []
                    ]
                ]
            , tr []
                [ td [] [ text "Image" ]
                , td []
                    [ input
                        [ type_ "text"
                        , value createServerRequest.imageUuid
                        , onInput (InputCreateServerImage createServerRequest)
                        ]
                        []
                    ]
                ]
            , tr []
                [ td [] [ text "Size" ]
                , td []
                    [ viewFlavorPicker model.flavors createServerRequest
                    ]
                ]
            , tr []
                [ td [] [ text "SSH Keypair" ]
                , td []
                    [ viewKeypairPicker model.keypairs createServerRequest
                    ]
                ]
            , tr []
                [ td [] [ text "User Data" ]
                , td []
                    [ div []
                        [ textarea
                            [ value createServerRequest.userData
                            , rows 20
                            , cols 80
                            , onInput (InputCreateServerUserData createServerRequest)
                            ]
                            []
                        ]
                    , div [] [ text (getEffectiveUserDataSize createServerRequest) ]
                    ]
                ]
            ]
        , button [ onClick (RequestCreateServer createServerRequest) ] [ text "Create" ]
        ]


getEffectiveUserDataSize : CreateServerRequest -> String
getEffectiveUserDataSize createServerRequest =
    let
        rawLength =
            String.length createServerRequest.userData

        base64Value =
            Base64.encode createServerRequest.userData

        base64Length =
            String.length base64Value
    in
        Basics.toString rawLength
            ++ " characters,  "
            ++ Basics.toString base64Length
            ++ "/16384 allowed bytes (Base64 encoded)"


viewFlavorPicker : List Flavor -> CreateServerRequest -> Html Msg
viewFlavorPicker flavors createServerRequest =
    let
        viewFlavorPickerLabel flavor =
            label []
                [ input [ type_ "radio", onClick (InputCreateServerSize createServerRequest flavor.uuid) ] []
                , text flavor.name
                ]
    in
        fieldset [] (List.map viewFlavorPickerLabel flavors)


viewKeypairPicker : List Keypair -> CreateServerRequest -> Html Msg
viewKeypairPicker keypairs createServerRequest =
    let
        viewKeypairPickerLabel keypair =
            label []
                [ input [ type_ "radio", onClick (InputCreateServerKeypairName createServerRequest keypair.name) ] []
                , text keypair.name
                ]
    in
        fieldset [] (List.map viewKeypairPickerLabel keypairs)
