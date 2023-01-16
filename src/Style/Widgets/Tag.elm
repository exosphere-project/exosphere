module Style.Widgets.Tag exposing (tag)

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Style.Helpers as SH
import Style.Types
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text


tag : Style.Types.ExoPalette -> String -> Element.Element msg
tag palette text =
    Element.el
        [ Background.color (SH.toElementColorWithOpacity palette.primary 0.1)
        , Border.width 1
        , Border.color (SH.toElementColorWithOpacity palette.primary 0.7)
        , Text.fontSize Text.Tiny
        , Font.color (SH.toElementColor palette.primary)
        , Border.rounded 20
        , Element.paddingXY spacer.px8 spacer.px4
        ]
        (Element.text text)
