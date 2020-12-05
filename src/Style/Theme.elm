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


materialStyle : Material.Palette -> Style {} msg
materialStyle palette =
    { textInput = Material.textInput palette
    , column = Material.column
    , cardColumn = Material.cardColumn palette
    , primaryButton = Material.containedButton palette
    , button = Material.outlinedButton palette
    , chipButton = Material.chip palette
    , row = Material.row
    , progressIndicator = Material.progressIndicator palette
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
