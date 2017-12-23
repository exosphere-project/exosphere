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
init = ( { authToken = Nothing, creds = Creds "" "" "" "" "" "", errors = [] } , Cmd.none )

type alias Model =
  { authToken : Maybe String
  , creds : Creds
  , errors : List String
  }

type alias Creds =
  { authURL : String
  , projectDomain : String
  , projectName : String
  , userDomain : String
  , username : String
  , password : String
  }

type Msg
  = InputAuthURL String
  | InputProjectDomain String
  | InputProjectName String
  | InputUserDomain String
  | InputUsername String
  | InputPassword String
  | RequestToken
  | ReceiveToken (Result Http.Error String)

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
    ReceiveToken (Ok tokenResponse) -> ( model, Cmd.none )
    ReceiveToken (Err error) ->
      let
        errorStr = toString error
        newErrors = ("Error obtaining auth token: " ++ errorStr) :: model.errors
      in
        ( { model | errors = newErrors }, Cmd.none )

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
    Http.post model.creds.authURL (Http.jsonBody requestBody) Decode.string
      |> Http.send ReceiveToken

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

view : Model -> Html Msg
view model =
  div []
  [ viewErrors model
  , case model.authToken of
     Nothing -> viewCollectCreds model
     Just authToken -> text ("Token is " ++ authToken)
  ]

viewErrors : Model -> Html Msg
viewErrors model =
  div [] (List.map text model.errors)

viewCollectCreds : Model -> ( Html Msg )
viewCollectCreds model =
  div []
    [ input [ type_ "text", placeholder "Auth URL e.g. https://mycloud.net:5000/v3", onInput InputAuthURL ] []
    , input [ type_ "text", placeholder "Project domain", onInput InputProjectDomain ] []
    , input [ type_ "text", placeholder "Project name", onInput InputProjectName ] []
    , input [ type_ "text", placeholder "User domain", onInput InputUserDomain ] []
    , input [ type_ "text", placeholder "Username", onInput InputUsername ] []
    , input [ type_ "text", placeholder "Password", onInput InputPassword ] []
    , button [ onClick RequestToken ] [ text "Log in" ]
    ]
