module View.Nested exposing (Model, Msg(..), init, update, view)

import Element
import Element.Input as Input
import Types.Types


type alias Model =
    { contents : String }


type Msg
    = NewContents String


init : Model
init =
    { contents = "hi" }


view : Model -> Element.Element Msg
view model =
    Input.text []
        { onChange = NewContents
        , text = model.contents
        , placeholder = Nothing
        , label = Input.labelAbove [] (Element.text "type in the text box")
        }


update : Msg -> ( Model, Cmd Msg )
update msg =
    case msg of
        NewContents s ->
            ( { contents = s }, Cmd.none )
