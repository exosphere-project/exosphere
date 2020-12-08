module Style.Types exposing (ExoPalette, Style, darkPalette, defaultPalette)

import Color
import Widget.Style
    exposing
        ( ButtonStyle
        , ColumnStyle
        , ProgressIndicatorStyle
        , RowStyle
        , TextInputStyle
        )


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


type alias ExoPalette =
    { primary : Color.Color
    , secondary : Color.Color
    , background : Color.Color
    , surface : Color.Color
    , error : Color.Color
    , on :
        { primary : Color.Color
        , secondary : Color.Color
        , background : Color.Color
        , surface : Color.Color
        , error : Color.Color
        }
    , warn : Color.Color
    , readyGood : Color.Color
    , muted : Color.Color
    , menu :
        { secondary : Color.Color
        , background : Color.Color
        , surface : Color.Color
        , on :
            { background : Color.Color
            , surface : Color.Color
            }
        }
    }


defaultPalette : ExoPalette
defaultPalette =
    { primary = Color.rgb255 0 108 163

    -- I (cmart) don't believe secondary gets used right now, but at some point we'll want to pick a secondary color?
    , secondary = Color.rgb255 96 239 255
    , background = Color.rgb255 255 255 255
    , surface = Color.rgb255 242 242 242
    , error = Color.rgb255 204 0 0
    , on =
        { primary = Color.rgb255 255 255 255
        , secondary = Color.rgb255 0 0 0
        , background = Color.rgb255 0 0 0
        , surface = Color.rgb255 0 0 0
        , error = Color.rgb255 255 255 255
        }
    , warn = Color.rgb255 252 175 62
    , readyGood = Color.rgb255 35 209 96
    , muted = Color.rgb255 122 122 122
    , menu =
        { secondary = Color.rgb255 29 29 29
        , background = Color.rgb255 36 36 36
        , surface = Color.rgb255 51 51 51
        , on =
            { background = Color.rgb255 181 181 181
            , surface = Color.rgb255 255 255 255
            }
        }
    }


darkPalette : ExoPalette
darkPalette =
    { primary = Color.rgb255 0 165 255
    , secondary = Color.rgb255 96 239 255
    , background = Color.rgb255 36 36 36
    , surface = Color.rgb255 51 51 51
    , error = Color.rgb255 204 0 0
    , on =
        { primary = Color.rgb255 255 255 255
        , secondary = Color.rgb255 0 0 0
        , background = Color.rgb255 181 181 181
        , surface = Color.rgb255 255 255 255
        , error = Color.rgb255 255 255 255
        }
    , warn = Color.rgb255 252 175 62
    , readyGood = Color.rgb255 35 209 96
    , muted = Color.rgb255 122 122 122
    , menu =
        { secondary = Color.rgb255 29 29 29
        , background = Color.rgb255 36 36 36
        , surface = Color.rgb255 51 51 51
        , on =
            { background = Color.rgb255 181 181 181
            , surface = Color.rgb255 255 255 255
            }
        }
    }
