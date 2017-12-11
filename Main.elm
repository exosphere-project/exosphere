port module Main exposing (main)

import Json.Encode as Encode
import Html exposing (Html, button, div, text, ul, span, h3, h4)
import Html.Lazy exposing (lazy)
import Html.Events exposing (onClick)
import Http
import Types exposing (decodeAuth, AuthResponse, AuthIntro, Provider, Service, EndPoint, CallApiEndPointResponse)


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


main : Program (Maybe Model) Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = updateWithStorage
        , view = view
        , subscriptions = \_ -> Sub.none
        }


emptyModel : Model
emptyModel =
    { provider = Nothing
    , authIntro = Nothing
    , authResponse = Nothing
    }


init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
    Maybe.withDefault emptyModel savedModel ! []



-- MODEL


type alias Model =
    { provider : Maybe Provider
    , authIntro : Maybe AuthIntro
    , authResponse : Maybe AuthResponse
    }


type Msg
    = GetAuthIntro
    | ReceiveAuthIntro (Result Http.Error AuthIntro)
    | PostAuthToken
    | ReceiveAuthToken (Result Http.Error AuthResponse)
    | CallApiEndPoint EndPoint
    | ReceiveCallApiEndPoint (Result Http.Error CallApiEndPointResponse)



-- UPDATE


port setStorage : Model -> Cmd msg


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        ( newModel
        , Cmd.batch [ setStorage newModel, cmds ]
        )


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
            ( { model | authIntro = Just "Oops - something wrong with auth intro" }
            , Cmd.none
            )

        PostAuthToken ->
            ( model
            , postAuthToken
            )

        ReceiveAuthToken (Ok authResponse) ->
            ( { model | authResponse = Just authResponse }
            , Cmd.none
            )

        ReceiveAuthToken (Err _) ->
            ( { model | authIntro = Just "Oops - something wrong with auth token" }
            , Cmd.none
            )

        CallApiEndPoint endPoint ->
            ( model
            , callApiEndPoint endPoint
            )

        ReceiveCallApiEndPoint (Ok callApiEndPointResponse) ->
            ( model
            , Cmd.none
            )

        ReceiveCallApiEndPoint (Err _) ->
            ( { model | authIntro = Just "Oops - something wrong with endPoint" }
            , Cmd.none
            )


callApiEndPoint : EndPoint -> Cmd Msg
callApiEndPoint endPoint =
    Http.getString endPoint.publicURL
        |> Http.send ReceiveCallApiEndPoint


getAuthIntro : Cmd Msg
getAuthIntro =
    Http.getString "http://localhost:8900/"
        |> Http.send ReceiveAuthIntro


postAuthToken : Cmd Msg
postAuthToken =
    let
        apiKeyCredentials =
            Encode.object
                [ "username" => Encode.string "some_user"
                , "apiKey" => Encode.string "12345"
                ]

        authResponse =
            Encode.object
                [ "RAX-KSKEY:apiKeyCredentials" => apiKeyCredentials ]

        body =
            Encode.object [ "auth" => authResponse ]
                |> Http.jsonBody
    in
        Http.post "http://localhost:8900/identity/v2.0/tokens" body decodeAuth
            |> Http.send ReceiveAuthToken



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick GetAuthIntro ] [ text "GetAuthIntro" ]
        , button [ onClick PostAuthToken ] [ text "PostAuthToken" ]
        , authResponseView model.authResponse

        --, div [] [ text (toString model) ]
        ]


authResponseView : Maybe AuthResponse -> Html Msg
authResponseView authResponse =
    case authResponse of
        Nothing ->
            div [] [ text "No service catalog loaded yet" ]

        Just x ->
            lazy servicesView x.access.serviceCatalog


servicesView : List Service -> Html Msg
servicesView services =
    --div [] [ text (toString services) ]
    ul [] <|
        List.map serviceView services


serviceView : Service -> Html Msg
serviceView service =
    div []
        [ h3 [] [ text service.serviceName ]
        , h4 [] [ text service.serviceType ]
        , ul [] <|
            List.map endPointView service.endPoints
        ]


endPointView : EndPoint -> Html Msg
endPointView endPoint =
    div []
        [ span [] [ text endPoint.region ]
        , span [] [ text " - " ]
        , span [] [ text endPoint.publicURL ]
        , button [ onClick (CallApiEndPoint endPoint) ] [ text "Get" ]
        ]
