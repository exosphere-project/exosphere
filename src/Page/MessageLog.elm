module Page.MessageLog exposing (Model, Msg(..), headerView, init, update, view)

import Element
import Element.Input as Input
import Html.Attributes
import Route
import Style.Helpers as SH
import Style.Widgets.Text as Text
import Types.Error exposing (ErrorLevel(..))
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types
import Widget


type alias Model =
    { showDebugMsgs : Bool
    }


type Msg
    = GotShowDebugMsgs Bool
    | NoOp


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

        NoOp ->
            ( model, Cmd.none, SharedMsg.NoOp )


headerView : View.Types.Context -> Element.Element msg
headerView context =
    Text.heading context.palette
        VH.headerHeadingAttributes
        Element.none
        "Recent Messages"


view : View.Types.Context -> SharedModel -> Model -> Element.Element Msg
view context sharedModel model =
    let
        columnSpacing =
            36

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

        allMessagesStr =
            List.map VH.renderMessageAsString shownMessages
                |> List.intersperse "\n"
                |> String.concat

        copyMessagesBtn =
            -- to make ClipboardJS work, we have to keep this "copy-button" class element always in DOM, even when we are not showing the button
            Element.el
                [ Element.htmlAttribute <| Html.Attributes.class "copy-button"
                , Element.htmlAttribute <|
                    Html.Attributes.attribute "data-clipboard-text" allMessagesStr
                , Element.alignRight
                ]
            <|
                if List.isEmpty shownMessages then
                    Element.none

                else
                    Widget.textButton
                        (SH.materialStyle context.palette).textButton
                        { onPress = Just NoOp
                        , text = "Copy all messages"
                        }
    in
    Element.column
        (VH.contentContainer ++ [ Element.spacing columnSpacing ])
        [ Element.row [ Element.width Element.fill ]
            [ Element.el [] <|
                Input.checkbox []
                    { label = Input.labelRight [] (Element.text "Show low-level debug messages")
                    , icon = Input.defaultCheckbox
                    , checked = model.showDebugMsgs
                    , onChange = GotShowDebugMsgs
                    }
            , copyMessagesBtn
            ]
        , if List.isEmpty shownMessages then
            Element.text "(No Messages)"

          else
            Element.column
                [ Element.spacing columnSpacing
                , Element.width Element.fill
                ]
                (List.map (VH.renderMessageAsElement context) shownMessages)
        ]
