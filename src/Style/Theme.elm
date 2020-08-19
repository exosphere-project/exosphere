module Style.Theme exposing (Style, exoPalette, materialStyle)

import Color
import Widget.Style exposing (ButtonStyle, ColumnStyle, ProgressIndicatorStyle, RowStyle, TextInputStyle)
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
        , progressIndicator : ProgressIndicatorStyle msg
    }


materialStyle : Style {} msg
materialStyle =
    { textInput = Material.textInput exoPalette
    , column = Material.column
    , cardColumn = Material.cardColumn exoPalette
    , primaryButton = Material.containedButton exoPalette
    , button = Material.outlinedButton exoPalette
    , chipButton = Material.chip exoPalette
    , row = Material.row
    , progressIndicator = Material.progressIndicator exoPalette
    }


exoPalette : Material.Palette
exoPalette =
    { primary = Color.rgb255 0 108 163

    -- I (cmart) don't believe secondary gets used right now, but at some point we'll want to pick a secondary color?
    , secondary = Color.rgb255 96 239 255
    , background = Color.rgb255 255 255 255
    , surface = Color.rgb255 255 255 255
    , error = Color.rgb255 176 0 32
    , on =
        { primary = Color.rgb255 255 255 255
        , secondary = Color.rgb255 0 0 0
        , background = Color.rgb255 0 0 0
        , surface = Color.rgb255 0 0 0
        , error = Color.rgb255 255 255 255
        }
    }
