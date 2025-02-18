module Style.Widgets.Grid exposing (GridCell(..), GridRow(..), grid, notes, scrollableCell)

import Element
import Style.Widgets.Spacer exposing (spacer)


notes : String
notes =
    """
## Usage

A grid widget lays out its children in rows of equally weighted cells.

```elm
Grid.grid
    []
    [ GridRow []
        [ GridCell [] (Text.body "Cell 1")
        , GridCell [] (Text.body "Cell 2")
        ]
    ]
```

The grid itself, each row & each cell can have attributes applied to them.

By default, each cell has `Element.fillPortion 1` applied.

To create merged cells, set the `Element.width` attribute to `Element.fillPortion n` where `n` is the number of cells to merge.

(Or override the width attribute with any you prefer.)

### Scrollable cells

For cells that overflow their widths, use the `scrollableCell` widget to make them scrollable in the x-axis.

### Alternatives

If you are using a consistent row data with a fixed number of columns, consider using `Element.table`.
"""


type GridCell msg
    = GridCell (List (Element.Attribute msg)) (Element.Element msg)


type GridRow msg
    = GridRow (List (Element.Attribute msg)) (List (GridCell msg))


{-| A cell for a grid row.
-}
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


{-| A row of cells for a grid.
-}
gridRow : GridRow msg -> Element.Element msg
gridRow (GridRow attrs row) =
    Element.row ((Element.width <| Element.fill) :: attrs)
        (List.map
            gridCell
            row
        )


{-| A cell that is scrollable in the x-axis.
-}
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


{-| A grid organised in rows of equally weighted cells.
-}
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
