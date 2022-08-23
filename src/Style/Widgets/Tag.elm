module Style.Widgets.Tag exposing (tag)

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Style.Helpers as SH
import Style.Types


tag : Style.Types.ExoPalette -> String -> Element.Element msg
tag palette text =
    Element.el
        [ Background.color (SH.toElementColorWithOpacity palette.primary 0.1)
        , Border.width 1
        , Border.color (SH.toElementColorWithOpacity palette.primary 0.7)
        , Font.size 12
        , Font.color (SH.toElementColor palette.primary)
        , Border.rounded 20
        , Element.paddingEach { top = 4, bottom = 4, left = 8, right = 8 }
        ]
        (Element.text text)
