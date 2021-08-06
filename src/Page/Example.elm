module Page.Example exposing (Model, Msg(..), init, update, view)

import Element
import Element.Events as Events
import Element.Input as Input
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg exposing (SharedMsg(..))


type alias Model =
    { contents : String }


type Msg
    = NewContents String
    | ButtonClicked


init : Model
init =
    { contents = "hi" }


view : Model -> Element.Element Msg
view model =
    Element.column []
        [ Input.text []
            { onChange = NewContents
            , text = model.contents
            , placeholder = Nothing
            , label = Input.labelAbove [] (Element.text "type in the text box")
            }
        , Element.text ("What you typed in the box: " ++ model.contents)
        , Element.el
            [ Events.onClick ButtonClicked ]
            (Element.text "click here to go elsewhere")
        ]


update : Msg -> SharedModel -> Model -> ( Model, Cmd Msg, SharedMsg )
update msg _ model =
    case msg of
        NewContents s ->
            ( { contents = s }, Cmd.none, NoOp )

        ButtonClicked ->
            ( model, Cmd.none, OpenNewWindow "https://ipcow.com" )
