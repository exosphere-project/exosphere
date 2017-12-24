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
    ( { authToken = Nothing {- Todo remove this hard coding and decode JSON in auth token response -}
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
      , images = Nothing
      , servers = Nothing
      }
    , Cmd.none
    )


type alias Model =
    { authToken : Maybe String
    , endpoints : Endpoints
    , creds : Creds
    , messages : List String
    , images : Maybe (List Image)
    , servers : Maybe (List Server)
    }


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
    , id : String
    , size : Int
    , checksum : String
    , diskFormat : String
    , containerFormat : String
    }


type alias Server =
    { name : String
    , id : String
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
                    Dict.get "X-Subject-Token" response.headers
            in
                ( { model | authToken = authToken }, Cmd.none )


requestImages : Model -> Cmd Msg
requestImages model =
    Http.request
        { method = "GET"
        , headers = [ Http.header "X-Auth-Token" (Maybe.withDefault "TODO handle this maybe better" model.authToken) ]
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
        Err _ ->
            ( model, Cmd.none )

        Ok images ->
            ( { model | images = Just images }, Cmd.none )


requestServers : Model -> Cmd Msg
requestServers model =
    Http.request
        { method = "GET"
        , headers = [ Http.header "X-Auth-Token" (Maybe.withDefault "TODO handle this better" model.authToken) ]
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
    Decode.map2 Server
        (Decode.field "name" Decode.string)
        (Decode.field "id" Decode.string)


receiveServers : Model -> Result Http.Error (List Server) -> ( Model, Cmd Msg )
receiveServers model result =
    case result of
        Err _ ->
            ( model, Cmd.none )

        Ok servers ->
            ( { model | servers = Just servers }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ viewMessages model
        , case model.authToken of
            Nothing ->
                viewCollectCreds model

            Just authToken ->
                div []
                    [ h2 [] [ text "Available Images" ]
                    , viewGlanceImages model
                    , h2 [] [ text "Your Servers" ]
                    , viewServers model
                    ]
        ]


renderMessage : String -> Html Msg
renderMessage message =
    p [] [ text message ]


viewMessages : Model -> Html Msg
viewMessages model =
    div [] (List.map renderMessage model.messages)


viewCollectCreds : Model -> Html Msg
viewCollectCreds model =
    div []
        [ div [] [ text "Please log in" ]
        , input
            [ type_ "text"
            , value model.creds.authURL
            , placeholder "Auth URL e.g. https://mycloud.net:5000/v3"
            , onInput InputAuthURL
            ]
            []
        , input
            [ type_ "text"
            , value model.creds.projectDomain
            , placeholder "Project domain"
            , onInput InputProjectDomain
            ]
            []
        , input
            [ type_ "text"
            , value model.creds.projectName
            , placeholder "Project name"
            , onInput InputProjectName
            ]
            []
        , input
            [ type_ "text"
            , value model.creds.userDomain
            , placeholder "User domain"
            , onInput InputUserDomain
            ]
            []
        , input
            [ type_ "text"
            , value model.creds.username
            , placeholder "Username"
            , onInput InputUsername
            ]
            []
        , input
            [ type_ "text"
            , value model.creds.password
            , placeholder "Password"
            , onInput InputPassword
            ]
            []
        , button [ onClick RequestAuth ] [ text "Log in" ]
        ]


viewGlanceImages : Model -> Html Msg
viewGlanceImages model =
    case model.images of
        Nothing ->
            div []
                [ button [ onClick RequestImages ] [ text "Get Images" ]
                ]

        Just images ->
            div [] (List.map renderImage images)


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
                , td [] [ text image.id ]
                ]
            ]
        ]


viewServers : Model -> Html Msg
viewServers model =
    case model.servers of
        Nothing ->
            div []
                [ button [ onClick RequestServers ] [ text "Get Servers" ]
                ]

        Just servers ->
            div [] (List.map renderServer servers)


renderServer : Server -> Html Msg
renderServer server =
    div []
        [ p [] [ strong [] [ text server.name ] ]
        , text server.id
        ]
