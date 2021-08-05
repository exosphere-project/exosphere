module View.Nested exposing (Model, Msg(..), update, view)

import Element


type alias Model =
    { contents : String }


type Msg
    = NewContents String


init : ( Model, Cmd Msg )
init =
    ( { contents = "hi" }, Cmd.none )


view : Model -> Element.Element Msg
view model =
    Element.text model.contents


update : Msg -> ( Model, Cmd Msg )
update msg =
    case msg of
        NewContents s ->
            ( { contents = s }, Cmd.none )
