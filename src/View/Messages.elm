module View.Messages exposing (messageLog)

import Element
import Types.Types exposing (Model, Msg(..))
import View.Helpers as VH


messageLog : Model -> Element.Element Msg
messageLog model =
    Element.column
        VH.exoColumnAttributes
        [ Element.el
            VH.heading2
            (Element.text "Recent Messages")
        , if List.isEmpty model.logMessages then
            Element.text "(No Messages)"

          else
            Element.column VH.exoColumnAttributes (List.map (VH.renderMessage model.style) model.logMessages)
        ]
