module View.Messages exposing (viewMessageLog)

import Element
import Types.Types exposing (..)
import View.Helpers as VH


viewMessageLog : Model -> Element.Element Msg
viewMessageLog model =
    Element.column
        VH.exoColumnAttributes
        [ Element.el
            VH.heading2
            (Element.text "Messages")
        , if List.isEmpty model.messages then
            Element.text "(No Messages)"

          else
            Element.column VH.exoColumnAttributes (List.map VH.renderMessage model.messages)
        ]
