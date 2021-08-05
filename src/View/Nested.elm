module View.Nested exposing (Model, Msg(..), init, update, view)

import Element
import Element.Input as Input
import Types.Types exposing (SharedModel)


type alias Model =
    { contents : String }


type Msg
    = NewContents String


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
        ]


update : Msg -> SharedModel -> Model -> ( SharedModel, Model, Cmd Msg )
update msg sharedModel _ =
    case msg of
        NewContents s ->
            ( sharedModel, { contents = s }, Cmd.none )
