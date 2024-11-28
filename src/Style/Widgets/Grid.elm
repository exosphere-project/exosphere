module Style.Widgets.Grid exposing (GridCell(..), GridRow(..), grid, scrollableCell)

import Element
import Style.Widgets.Spacer exposing (spacer)


type GridCell msg
    = GridCell (List (Element.Attribute msg)) (Element.Element msg)


type GridRow msg
    = GridRow (List (Element.Attribute msg)) (List (GridCell msg))


gridCell : GridCell msg -> Element.Element msg
gridCell (GridCell attrs child) =
    Element.el
        ([ Element.width <| Element.fillPortion 1
         , Element.padding spacer.px4
         ]
            ++ attrs
        )
    <|
        child


gridRow : GridRow msg -> Element.Element msg
gridRow (GridRow attrs row) =
    Element.row ((Element.width <| Element.fill) :: attrs)
        (List.map
            gridCell
            row
        )


scrollableCell : List (Element.Attribute msg) -> Element.Element msg -> Element.Element msg
scrollableCell attrs msg =
    Element.el
        ([ Element.scrollbarX, Element.clipY ]
            ++ attrs
        )
        (Element.el
            [ -- HACK: A width needs to be set so that the cell expands responsively while having a horizontal scrollbar to contain overflow.
              Element.width (Element.px 0)
            ]
            msg
        )


grid :
    List (Element.Attribute msg)
    -> List (GridRow msg)
    -> Element.Element msg
grid attributes rows =
    Element.column
        (Element.width Element.fill :: attributes)
        (List.map
            gridRow
            rows
        )
