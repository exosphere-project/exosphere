module Style.Widgets.DataList exposing (init, view)

import Element
import Element.Border as Border


type alias DataListModel =
    { selectedRowIndices : List Int
    }


init : DataListModel
init =
    { selectedRowIndices = [] }


view :
    List (Element.Attribute msg)
    -> (dataRecord -> Element.Element msg)
    -> List dataRecord
    -> DataListModel
    -> Element.Element msg
view styleAttrs listItemView data model =
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
                    [ listItemView dataRecord ]

            else
                Element.row rowStyle [ listItemView dataRecord ]

        toolbar =
            Element.row rowStyle
                [ --- Some action button that corresponds model-view-update handling by user
                  Element.text
                    (String.fromInt (List.length model.selectedRowIndices)
                        ++ " row(s) selected"
                    )
                ]
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
        (toolbar :: List.indexedMap rowView data)
