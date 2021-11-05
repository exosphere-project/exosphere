module Style.Widgets.Meter exposing (meter)

import Element
import Element.Background as Background
import Element.Font as Font
import Html.Attributes as HtmlA
import Style.Helpers as SH
import Style.Types exposing (ExoPalette)


meter : ExoPalette -> String -> String -> Int -> Int -> Element.Element msg
meter palette title subtitle value maximum =
    Element.column
        [ Element.spacing 5
        , Font.size 14
        ]
        [ Element.row
            [ Element.width Element.fill ]
            [ Element.text title
            , Element.el [ Element.alignRight ] (Element.text subtitle)
            ]
        , Element.row
            [ Element.width (Element.px 262)
            , Element.height (Element.px 25)
            , Background.color (SH.toElementColorWithOpacity palette.primary 0.15)
            , Element.htmlAttribute (HtmlA.attribute "role" "meter")
            ]
            [ Element.el
                [ Element.width (Element.fillPortion value)
                , Element.height Element.fill
                , Background.color (SH.toElementColor palette.primary)
                ]
                Element.none
            , Element.el
                [ Element.width (Element.fillPortion (maximum - value))
                , Element.height Element.fill
                ]
                Element.none
            ]
        ]
