module Style.Theme exposing (Style, materialStyle)

import Widget.Style exposing (ButtonStyle, ColumnStyle, RowStyle, TextInputStyle)
import Widget.Style.Material as Material


type alias Style style msg =
    { style
        | textInput : TextInputStyle msg
        , column : ColumnStyle msg
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
    , cardColumn = Material.cardColumn Material.defaultPalette
    , primaryButton = Material.containedButton Material.defaultPalette
    , button = Material.outlinedButton Material.defaultPalette
    , chipButton = Material.chip Material.defaultPalette
    , row = Material.row
    }
