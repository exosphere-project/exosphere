module Style.Types exposing
    ( ColorPalette
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


type alias ColorShades5 =
    { lightest : Color.Color
    , light : Color.Color
    , base : Color.Color
    , dark : Color.Color
    , darkest : Color.Color
    }


type alias ColorPalette =
    { gray : ColorShades5
    , blue : ColorShades5
    , green : ColorShades5
    , yellow : ColorShades5
    , red : ColorShades5
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
    { background : Color.Color
    , text : Color.Color
    , border : Color.Color
    , default : Color.Color
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
