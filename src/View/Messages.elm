module View.Messages exposing (messageLog)

import Element
import Types.Types exposing (LogMessage, Msg(..))
import View.Helpers as VH
import View.Types


messageLog : View.Types.Context -> List LogMessage -> Element.Element Msg
messageLog context logMessages =
    Element.column
        VH.exoColumnAttributes
        [ Element.el
            VH.heading2
            (Element.text "Recent Messages")
        , if List.isEmpty logMessages then
            Element.text "(No Messages)"

          else
            Element.column VH.exoColumnAttributes (List.map (VH.renderMessageAsElement context) logMessages)
        ]
