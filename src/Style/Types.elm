module Style.Types exposing
    ( DeployerColorThemes
    , DeployerColors
    , ElmUiWidgetStyle
    , ExoPalette
    , StyleMode
    , Theme(..)
    , ThemeChoice(..)
    , defaultColors
    )

import Color
import Widget.Style
    exposing
        ( ButtonStyle
        , ColumnStyle
        , ProgressIndicatorStyle
        , RowStyle
        , TabStyle
        , TextInputStyle
        )


type alias ElmUiWidgetStyle style msg =
    { style
        | textInput : TextInputStyle msg
        , column : ColumnStyle msg
        , cardColumn : ColumnStyle msg
        , primaryButton : ButtonStyle msg
        , button : ButtonStyle msg
        , warningButton : ButtonStyle msg
        , dangerButton : ButtonStyle msg
        , chipButton : ButtonStyle msg
        , iconButton : ButtonStyle msg
        , textButton : ButtonStyle msg
        , row : RowStyle msg
        , progressIndicator : ProgressIndicatorStyle msg
        , tab : TabStyle msg
    }


type Theme
    = Light
    | Dark


type ThemeChoice
    = Override Theme
    | System


type alias StyleMode =
    { theme : ThemeChoice
    , systemPreference : Maybe Theme
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
        , warn : Color.Color
        , readyGood : Color.Color
        , muted : Color.Color
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


type alias DeployerColorThemes =
    { light : DeployerColors
    , dark : DeployerColors
    }


type alias DeployerColors =
    { primary : Color.Color
    , secondary : Color.Color
    }


defaultColors : DeployerColorThemes
defaultColors =
    { light =
        { primary = Color.rgb255 0 108 163
        , secondary = Color.rgb255 96 239 255
        }
    , dark =
        { primary = Color.rgb255 83 183 226
        , secondary = Color.rgb255 96 239 255
        }
    }
