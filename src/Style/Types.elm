module Style.Types exposing
    ( AllColorsPalette
    , ColorShades9
    , DeployerColorThemes
    , DeployerColors
    , ElmUiWidgetStyle
    , ExoPalette
    , PopoverPosition(..)
    , StyleMode
    , Theme(..)
    , ThemeChoice(..)
    , UIStateColors
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
        , dangerButtonSecondary : ButtonStyle msg
        , chipButton : ButtonStyle msg
        , iconButton : ButtonStyle msg
        , textButton : ButtonStyle msg
        , row : RowStyle msg
        , progressIndicator : ProgressIndicatorStyle msg
        , tab : TabStyle msg
    }


type alias ColorShades9 =
    { lightest : Color.Color
    , lighter : Color.Color
    , light : Color.Color
    , semiLight : Color.Color
    , base : Color.Color
    , semiDark : Color.Color
    , dark : Color.Color
    , darker : Color.Color
    , darkest : Color.Color
    }


type alias AllColorsPalette =
    { gray : ColorShades9
    , blue : ColorShades9
    , green : ColorShades9
    , yellow : ColorShades9
    , red : ColorShades9
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


type alias UIStateColors =
    { default : Color.Color
    , background : Color.Color
    , border : Color.Color
    , textOnNeutralBG : Color.Color -- on black/gray/white colored background
    , textOnColoredBG : Color.Color -- on `background` field colored background
    }


type alias ExoPalette =
    { primary : Color.Color
    , secondary : Color.Color
    , background : Color.Color
    , surface : Color.Color

    -- TODO: give usecase-based names and integrate with previous fields
    , on :
        { primary : Color.Color
        , secondary : Color.Color
        , background : Color.Color
        , surface : Color.Color
        }
    , info : UIStateColors
    , success : UIStateColors
    , warning : UIStateColors
    , danger : UIStateColors
    , muted : UIStateColors
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


{-| Based on position prop in <https://www.patternfly.org/v4/components/popover#popover>
See interactive demo at <https://ant.design/components/popover/#components-popover-demo-placement>
-}
type PopoverPosition
    = PositionTopLeft
    | PositionTop
    | PositionTopRight
    | PositionRightTop
    | PositionRight
    | PositionRightBottom
    | PositionBottomRight
    | PositionBottom
    | PositionBottomLeft
    | PositionLeftBottom
    | PositionLeft
    | PositionLeftTop


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
