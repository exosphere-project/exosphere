module Style.Widgets.Tag exposing (tag, tagNeutral, tagPositive, tagWithColor)

import Color
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Style.Helpers as SH
import Style.Types
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text


tagWithColor : Color.Color -> String -> Element.Element msg
tagWithColor color text =
    Element.el
        [ Background.color (SH.toElementColorWithOpacity color 0.1)
        , Border.width 1
        , Border.color (SH.toElementColorWithOpacity color 0.7)
        , Text.fontSize Text.Tiny
        , Font.color (SH.toElementColor color)
        , Border.rounded 20
        , Element.paddingXY spacer.px8 spacer.px4
        ]
        (Element.text text)


tag : Style.Types.ExoPalette -> String -> Element.Element msg
tag palette text =
    tagWithColor palette.primary text


tagPositive : Style.Types.ExoPalette -> String -> Element.Element msg
tagPositive palette text =
    tagWithColor palette.success.textOnNeutralBG text


tagNeutral : Style.Types.ExoPalette -> String -> Element.Element msg
tagNeutral palette text =
    tagWithColor palette.muted.textOnNeutralBG text
