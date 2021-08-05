module View.Messages exposing (messageLog)

import Element
import Element.Input as Input
import Style.Helpers as SH
import Style.Widgets.Icon as Icon
import Types.Error exposing (ErrorLevel(..))
import Types.Msg exposing (SharedMsg(..))
import Types.Types exposing (LogMessage)
import Types.View exposing (NonProjectViewConstructor(..))
import View.Helpers as VH
import View.Types


messageLog : View.Types.Context -> List LogMessage -> Bool -> Element.Element SharedMsg
messageLog context logMessages showDebugMsgs =
    let
        shownMessages =
            let
                filter =
                    if showDebugMsgs then
                        \_ -> True

                    else
                        \message -> message.context.level /= ErrorDebug
            in
            logMessages
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
            , checked = showDebugMsgs
            , onChange = \new -> SetNonProjectView <| MessageLog new
            }
        , if List.isEmpty shownMessages then
            Element.text "(No Messages)"

          else
            Element.column VH.contentContainer (List.map (VH.renderMessageAsElement context) shownMessages)
        ]
