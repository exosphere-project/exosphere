module Style.Types exposing
    ( ElmUiWidgetStyle
    , ExoPalette
    , StyleMode(..)
    , defaultPrimaryColor
    , defaultSecondaryColor
    )

import Color
import Widget.Style
    exposing
        ( ButtonStyle
        , ColumnStyle
        , ProgressIndicatorStyle
        , RowStyle
        , TextInputStyle
        )


type alias ElmUiWidgetStyle style msg =
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


type StyleMode
    = LightMode
    | DarkMode


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
        , warn : Color.Color
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


defaultPrimaryColor : Color.Color
defaultPrimaryColor =
    Color.rgb255 0 108 163


defaultSecondaryColor : Color.Color
defaultSecondaryColor =
    Color.rgb255 96 239 255
