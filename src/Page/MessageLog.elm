module Page.MessageLog exposing (Model, Msg(..), init, update, view)

import Element
import Element.Input as Input
import Route
import Style.Helpers as SH
import Style.Widgets.Icon as Icon
import Types.Error exposing (ErrorLevel(..))
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types


type alias Model =
    { showDebugMsgs : Bool
    }


type Msg
    = GotShowDebugMsgs Bool


init : Maybe Bool -> Model
init maybeShowDebugMsgs =
    { showDebugMsgs = Maybe.withDefault False maybeShowDebugMsgs }


update : Msg -> SharedModel -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg { viewContext } model =
    case msg of
        GotShowDebugMsgs new ->
            ( { model | showDebugMsgs = new }
            , Route.replaceUrl viewContext (Route.MessageLog new)
            , SharedMsg.NoOp
            )


view : View.Types.Context -> SharedModel -> Model -> Element.Element Msg
view context sharedModel model =
    let
        shownMessages =
            let
                filter =
                    if model.showDebugMsgs then
                        \_ -> True

                    else
                        \message -> message.context.level /= ErrorDebug
            in
            sharedModel.logMessages
                |> List.filter filter
    in
    Element.column
        (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ Element.row
            (VH.heading2 context.palette ++ [ Element.spacing 12 ])
            [ Icon.bell (SH.toElementColor context.palette.on.background) 20
            , Element.text "Recent Messages"
            ]
        , Input.checkbox
            []
            { label = Input.labelRight [] (Element.text "Show low-level debug messages")
            , icon = Input.defaultCheckbox
            , checked = model.showDebugMsgs
            , onChange = GotShowDebugMsgs
            }
        , if List.isEmpty shownMessages then
            Element.text "(No Messages)"

          else
            Element.column VH.contentContainer (List.map (VH.renderMessageAsElement context) shownMessages)
        ]
