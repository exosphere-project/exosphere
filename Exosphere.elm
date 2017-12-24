module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Json.Encode as Encode


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
      }
    , Cmd.none
    )


type alias Model =
    { authToken : Maybe String
    , endpoints : Endpoints
    , creds : Creds
    , messages : List String
    , images : Maybe (List Image)
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

        ReceiveImages images ->
            receiveImages model images


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
    Decode.map2 Image
        (Decode.field "name" Decode.string)
        (Decode.field "id" Decode.string)


receiveImages : Model -> Result Http.Error (List Image) -> ( Model, Cmd Msg )
receiveImages model result =
    case result of
        Err _ ->
            ( model, Cmd.none )

        Ok images ->
            ( { model | images = Just images }, Cmd.none )


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
                viewGlanceImages model
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
            div []
                (List.map text <| List.map toString images)
