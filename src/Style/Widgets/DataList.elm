module Style.Widgets.DataList exposing (dataList)

import Element
import Element.Border as Border


dataList :
    List (Element.Attribute msg)
    -> (dataRecord -> Element.Element msg)
    -> List dataRecord
    -> Element.Element msg
dataList styleAttrs view data =
    let
        rowStyle =
            [ Element.padding 24
            , Border.widthEach { top = 0, bottom = 1, left = 0, right = 0 }
            , Border.color <| Element.rgba255 0 0 0 0.16
            , Element.width Element.fill
            ]

        rowView i dataRecord =
            if i == List.length data - 1 then
                -- Don't show divider (bottom border) for last row
                Element.row
                    (rowStyle ++ [ Border.width 0 ])
                    [ view dataRecord ]

            else
                Element.row rowStyle [ view dataRecord ]
    in
    Element.column
        ([ Element.width Element.fill
         , Border.width 1
         , Border.color (Element.rgba255 0 0 0 0.1)
         , Border.rounded 4
         ]
            -- Add or override default style with passed style attributes
            ++ styleAttrs
        )
        (List.indexedMap rowView data)
