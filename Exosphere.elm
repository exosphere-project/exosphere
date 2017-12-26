module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Filesize exposing (format)


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
            , nova = "https://tombstone-cloud.cyverse.org:8774/v2.1"
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
    }


type ViewState
    = Login
    | Home
    | ListImages
    | ListUserServers
    | ServerDetail Server


type alias Creds =
    { authURL : String
    , projectDomain : String
    , projectName : String
    , userDomain : String
    , username : String
    , password : String
    }


type alias Endpoints =
    { glance : String, nova : String }


type alias Image =
    { name : String
    , uuid : String
    , size : Int
    , checksum : String
    , diskFormat : String
    , containerFormat : String
    }


type alias Server =
    { name : String
    , uuid : String
    , details : Maybe ServerDetails
    }



{- Todo add to ServerDetails:
   - IP addresses
   - Flavor
   - Image
   - Metadata
   - Volumes
   - Security Groups
   - Etc

   Also, make status and powerState union types, keyName a key type, created a real date/time, etc
-}


type alias ServerDetails =
    { status : String
    , created : String
    , powerState : Int
    , keyName : String
    }


type Msg
    = InputAuthURL String
    | InputProjectDomain String
    | InputProjectName String
    | InputUserDomain String
    | InputUsername String
    | InputPassword String
    | RequestAuth
    | ReceiveAuth (Result Http.Error (Http.Response String))
    | RequestImages
    | ReceiveImages (Result Http.Error (List Image))
    | RequestServers
    | ReceiveServers (Result Http.Error (List Server))
    | LaunchImage Image
    | ChangeViewState ViewState
    | RequestServerDetails Server
    | ReceiveServerDetails Server (Result Http.Error ServerDetails)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        RequestAuth ->
            ( model, requestAuthToken model )

        ReceiveAuth response ->
            receiveAuth model response

        RequestImages ->
            ( model, requestImages model )

        ReceiveImages result ->
            receiveImages model result

        LaunchImage image ->
            ( model, Cmd.none )

        RequestServers ->
            ( model, requestServers model )

        ReceiveServers result ->
            receiveServers model result

        ChangeViewState state ->
            ( { model | viewState = state }, Cmd.none )

        RequestServerDetails server ->
            ( model, requestServerDetails model server )

        ReceiveServerDetails server result ->
            receiveServerDetails model server result


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
        Err _ ->
            {- Todo something reasonable here -}
            ( model, Cmd.none )

        Ok response ->
            let
                authToken =
                    Maybe.withDefault "" (Dict.get "X-Subject-Token" response.headers)
            in
                ( { model | authToken = authToken, viewState = Home }, Cmd.none )


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
            let
                newMsgs =
                    (toString error) :: model.messages
            in
                ( { model | messages = newMsgs }, Cmd.none )

        Ok images ->
            ( { model | images = images }, Cmd.none )


requestServers : Model -> Cmd Msg
requestServers model =
    Http.request
        { method = "GET"
        , headers = [ Http.header "X-Auth-Token" model.authToken ]
        , url = model.endpoints.nova ++ "/servers"
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
    Decode.map3 Server
        (Decode.field "name" Decode.string)
        (Decode.field "id" Decode.string)
        (Decode.succeed Nothing)


receiveServers : Model -> Result Http.Error (List Server) -> ( Model, Cmd Msg )
receiveServers model result =
    case result of
        Err error ->
            let
                newMsgs =
                    (toString error) :: model.messages
            in
                ( { model | messages = newMsgs }, Cmd.none )

        Ok servers ->
            ( { model | servers = servers }, Cmd.none )


requestServerDetails model server =
    Http.request
        { method = "GET"
        , headers = [ Http.header "X-Auth-Token" model.authToken ]
        , url = model.endpoints.nova ++ "/servers/" ++ server.uuid
        , body = Http.emptyBody
        , expect = Http.expectJson decodeServerDetails
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send (ReceiveServerDetails server)


receiveServerDetails : Model -> Server -> Result Http.Error ServerDetails -> ( Model, Cmd Msg )
receiveServerDetails model server result =
    case result of
        Err _ ->
            ( model, Cmd.none )

        Ok serverDetails ->
            let
                newServer =
                    { server | details = Just serverDetails }

                otherServers =
                    List.filter (\s -> s.uuid /= newServer.uuid) model.servers

                newServers =
                    newServer :: otherServers
            in
                ( { model | servers = newServers, viewState = ServerDetail newServer }, Cmd.none )


decodeServerDetails : Decode.Decoder ServerDetails
decodeServerDetails =
    Decode.map4 ServerDetails
        (Decode.at [ "server", "status" ] Decode.string)
        (Decode.at [ "server", "created" ] Decode.string)
        (Decode.at [ "server", "OS-EXT-STS:power_state" ] Decode.int)
        (Decode.at [ "server", "key_name" ] Decode.string)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ viewMessages model
        , case model.viewState of
            Login ->
                viewLogin model

            Home ->
                div []
                    [ viewNav model
                    ]

            ListImages ->
                div []
                    [ viewNav model
                    , viewImages model
                    ]

            ListUserServers ->
                div []
                    [ viewNav model
                    , viewServers model
                    ]

            ServerDetail server ->
                div []
                    [ viewNav model
                    , viewServerDetails server
                    ]
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
    div []
        (button [ onClick RequestImages ] [ text "Get Images" ]
            :: (List.map renderImage model.images)
        )


renderImage : Image -> Html Msg
renderImage image =
    div []
        [ p [] [ strong [] [ text image.name ] ]
        , button [ onClick (LaunchImage image) ] [ text "Launch" ]
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
    div []
        (button [ onClick RequestServers ] [ text "Get Servers" ]
            :: (List.map renderServer model.servers)
        )


renderServer : Server -> Html Msg
renderServer server =
    div []
        [ p [] [ strong [] [ text server.name ] ]
        , text ("UUID: " ++ server.uuid)
        , button [ onClick (ChangeViewState (ServerDetail server)) ] [ text "Details" ]
        ]


viewNav : Model -> Html Msg
viewNav model =
    div []
        [ h2 [] [ text "Navigation" ]
        , button [ onClick (ChangeViewState Home) ] [ text "Home" ]
        , button [ onClick (ChangeViewState ListImages) ] [ text "Images" ]
        , button [ onClick (ChangeViewState ListUserServers) ] [ text "My Servers" ]
        ]


viewServerDetails : Server -> Html Msg
viewServerDetails server =
    case server.details of
        Nothing ->
            div []
                [ button [ onClick (RequestServerDetails server) ] [ text "Get server details" ]
                ]

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
                        , td [] [ text details.keyName ]
                        ]
                    ]
                ]
