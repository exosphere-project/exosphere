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

init : ( Model, Cmd Msg)
{- Todo remove default creds once storing this in local storage -}
init = ( { authToken = Nothing, creds = Creds "https://tombstone-cloud.cyverse.org:5000/v3/auth/tokens" "default" "demo" "default" "demo" "", messages = [] } , Cmd.none )

type alias Model =
  { authToken : Maybe String
  , creds : Creds
  , messages : List String
  }

type alias Creds =
  { authURL : String
  , projectDomain : String
  , projectName : String
  , userDomain : String
  , username : String
  , password : String
  }

type alias ScopedAuthToken =
  {
  }

type Msg
  = InputAuthURL String
  | InputProjectDomain String
  | InputProjectName String
  | InputUserDomain String
  | InputUsername String
  | InputPassword String
  | RequestToken
  | ReceiveAuth (Result Http.Error (Http.Response String))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    InputAuthURL authURL ->
      let creds = model.creds in
      ( { model | creds = { creds | authURL = authURL } } , Cmd.none )
    InputProjectDomain projectDomain ->
      let creds = model.creds in
      ( { model | creds = { creds | projectDomain = projectDomain } } , Cmd.none )
    InputProjectName projectName ->
      let creds = model.creds in
      ( { model | creds = { creds | projectName = projectName } } , Cmd.none )
    InputUserDomain userDomain ->
      let creds = model.creds in
      ( { model | creds = { creds | userDomain = userDomain } } , Cmd.none )
    InputUsername username ->
      let creds = model.creds in
      ( { model | creds = { creds | username = username } } , Cmd.none )
    InputPassword password ->
      let creds = model.creds in
      ( { model | creds = { creds | password = password } } , Cmd.none )
    RequestToken -> ( model, requestAuthToken model )
    ReceiveAuth response -> receiveAuth model response

{-
    ReceiveToken (Err error) ->
      let
        errorStr = toString error
        newErrors = ("Error obtaining auth token: " ++ errorStr) :: model.errors
      in
        ( { model | errors = newErrors }, Cmd.none )
-}

requestAuthToken : Model -> Cmd Msg
requestAuthToken model =
  let
    requestBody =
      Encode.object
        [ ("auth", Encode.object
          [ ("identity", Encode.object
            [ ("methods", Encode.list [Encode.string "password"])
            , ("password", Encode.object
              [ ("user", Encode.object
                [ ("name", Encode.string model.creds.username)
                , ("domain", Encode.object
                  [ ("id", Encode.string model.creds.userDomain)
                  ] )
                , ("password", Encode.string model.creds.password)
                ] )
              ] )
            ] )
          , ("scope", Encode.object
            [ ("project", Encode.object
              [ ("name", Encode.string model.creds.projectName)
              , ("domain", Encode.object
                [ ("id", Encode.string model.creds.projectDomain)
                ] )
              ] )
            ] )
          ] )
        ]

  in
    Http.request
      { method = "POST"
      , headers = []
      , url = model.creds.authURL
      , body = Http.jsonBody requestBody
      {- Todo handle no response? -}
      , expect = Http.expectStringResponse (\response -> Ok response)
      , timeout = Nothing
      , withCredentials = True
    } |> Http.send ReceiveAuth

{-
processTokenResponse : Http.Response String -> Result String String
processTokenResponse response =
  let
    token =
      Dict.get "X-Subject-Token" response.headers
        |> Result.fromMaybe ("Auth token header not found")
  in
  Ok (response)
-}

receiveAuth : Model -> Result Http.Error (Http.Response String) -> (Model, Cmd Msg)
receiveAuth model responseResult =
  case responseResult of
    Err _ ->
      {- Todo something reasonable here -}
      ( model, Cmd.none )
    Ok response ->
      let
        {- Todo handle error here -}
        authToken = Dict.get "X-Subject-Token" response.headers
      in
        ( { model | authToken = authToken }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

view : Model -> Html Msg
view model =
  div []
  [ viewMessages model
  , case model.authToken of
     Nothing -> viewCollectCreds model
     Just authToken -> text ("Token is " ++ authToken)
  ]

renderMessage : String -> Html Msg
renderMessage message = p [] [ text message ]

viewMessages : Model -> Html Msg
viewMessages model =
  div [] (List.map renderMessage model.messages)


viewCollectCreds : Model -> ( Html Msg )
viewCollectCreds model =
  div []
    [ div [] [ text "Please log in" ]
    , input
      [ type_ "text"
      , value model.creds.authURL
      , placeholder "Auth URL e.g. https://mycloud.net:5000/v3"
      , onInput InputAuthURL
      ] []
    , input
      [ type_ "text"
      , value model.creds.projectDomain
      , placeholder "Project domain"
      , onInput InputProjectDomain
      ] []
    , input
      [ type_ "text"
      , value model.creds.projectName
      , placeholder "Project name"
      , onInput InputProjectName
      ] []
    , input
      [ type_ "text"
      , value model.creds.userDomain
      , placeholder "User domain"
      , onInput InputUserDomain
      ] []
    , input
      [ type_ "text"
      , value model.creds.username
      , placeholder "Username"
      , onInput InputUsername
      ] []
    , input
      [ type_ "text"
      , value model.creds.password
      , placeholder "Password"
      , onInput InputPassword
      ] []
    , button [ onClick RequestToken ] [ text "Log in" ]
    ]
