module Style.Widgets.DataList exposing (dataList)

import Element
import Element.Border as Border
import Element.Font as Font


dataList :
    (dataRecord -> Element.Element msg)
    -> List dataRecord
    -> Element.Element msg
dataList view data =
    let
        rowStyle =
            [ Element.padding 24
            , Border.widthEach { top = 0, bottom = 1, left = 0, right = 0 }
            , Border.color <| Element.rgba255 0 0 0 0.2
            , Font.size 16
            , Element.width Element.fill
            ]

        rowView i dataRecord =
            if i == 0 then
                Element.row
                    (rowStyle ++ [ Border.widthEach { top = 1, bottom = 1, left = 0, right = 0 } ])
                    [ view dataRecord ]

            else
                Element.row rowStyle [ view dataRecord ]
    in
    Element.column [ Element.width (Element.maximum 900 Element.fill) ]
        (List.indexedMap rowView data)
