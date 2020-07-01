module Style.Theme exposing (Style, materialStyle)

import Element
import Widget.Style exposing (ButtonStyle, ColumnStyle, RowStyle, SortTableStyle, TextInputStyle)
import Widget.Style.Material as Material


type alias Style style msg =
    { style
        | textInput : TextInputStyle msg
        , column : ColumnStyle msg
        , sortTable : SortTableStyle msg
        , cardColumn : ColumnStyle msg
        , primaryButton : ButtonStyle msg
        , button : ButtonStyle msg
        , chipButton : ButtonStyle msg
        , row : RowStyle msg
    }


materialStyle : Style {} msg
materialStyle =
    { textInput = Material.textInput Material.defaultPalette
    , column = Material.column
    , sortTable =
        { containerTable = [ Element.spacingXY 5 20 ]
        , headerButton = Material.textButton Material.defaultPalette
        , ascIcon =
            Material.expansionPanel Material.defaultPalette
                |> .collapseIcon
        , descIcon =
            Material.expansionPanel Material.defaultPalette
                |> .expandIcon
        , defaultIcon = Element.none
        }
    , cardColumn = Material.cardColumn Material.defaultPalette
    , primaryButton = Material.containedButton Material.defaultPalette
    , button = Material.outlinedButton Material.defaultPalette
    , chipButton = Material.chip Material.defaultPalette
    , row = Material.row
    }
