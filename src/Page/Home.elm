module Page.Home exposing (Model, Msg, init, update, view)

import Element
import Types.SharedModel as SharedModel
import Types.SharedMsg as SharedMsg
import View.Types


type alias Model =
    ()


type Msg
    = NoOp


init : Model
init =
    ()


update : Msg -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg model =
    ( model, Cmd.none, SharedMsg.NoOp )


view : View.Types.Context -> SharedModel.SharedModel -> Model -> Element.Element Msg
view context sharedModel model =
    Element.text "TODO home page"
