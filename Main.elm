module Main exposing (main)

import Json.Encode as Encode
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http
import IdentityTypes exposing (decodeAuth, Auth, AuthIntro)


-- NOTE: If served from a website (as opposed to an Electron ap) then this
-- code needs the API server to add CORS headers like:
-- "Access-Control-Allow-Origin: *"
-- "Access-Control-Allow-Headers: content-type"
-- And change response status: "405 Method Not Allowed" to "200 OK"
-- I use Charles to rewrite the response for now.
-- A reverse proxy/gateway using Nginx could also be an option.


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( { authIntro = Nothing, auth = Nothing }
    , Cmd.none
    )



-- MODEL


type alias Model =
    { authIntro : Maybe AuthIntro
    , auth : Maybe Auth
    }


type Msg
    = GetAuthIntro
    | ReceiveAuthIntro (Result Http.Error AuthIntro)
    | PostAuthAuthToken
    | ReceiveAuthAuthToken (Result Http.Error Auth)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetAuthIntro ->
            ( model
            , getAuthIntro
            )

        ReceiveAuthIntro (Ok authIntro) ->
            ( { model | authIntro = Just authIntro }
            , Cmd.none
            )

        -- Try setting the port of auth to 8901 below to get this error
        ReceiveAuthIntro (Err _) ->
            ( { model | authIntro = Just "Oops - something wrong with Auth intro" }
            , Cmd.none
            )

        PostAuthAuthToken ->
            ( model
            , postAuthAuthToken
            )

        ReceiveAuthAuthToken (Ok auth) ->
            ( { model | auth = Just auth }
            , Cmd.none
            )

        ReceiveAuthAuthToken (Err _) ->
            ( { model | authIntro = Just "Oops - something wrong with Auth token" }
            , Cmd.none
            )


getAuthIntro : Cmd Msg
getAuthIntro =
    Http.getString "http://localhost:8900/"
        |> Http.send ReceiveAuthIntro


postAuthAuthToken : Cmd Msg
postAuthAuthToken =
    let
        apiKeyCredentials =
            Encode.object
                [ "username" => Encode.string "some_user"
                , "apiKey" => Encode.string "12345"
                ]

        auth =
            Encode.object
                [ "RAX-KSKEY:apiKeyCredentials" => apiKeyCredentials ]

        body =
            Encode.object [ "auth" => auth ]
                |> Http.jsonBody
    in
        Http.post "http://localhost:8900/identity/v2.0/tokens" body decodeAuth
            |> Http.send ReceiveAuthAuthToken



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text (toString model) ]
        , button [ onClick GetAuthIntro ] [ text "GetAuthIntro" ]
        , button [ onClick PostAuthAuthToken ] [ text "PostAuthAuthToken" ]
        ]
