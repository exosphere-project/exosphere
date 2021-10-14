module Style.Widgets.Meter exposing (meter)

import Css
import Css.Global
import Element
import Element.Font as Font
import Html.Styled as Html
import Html.Styled.Attributes as HtmlA
import Style.Helpers as SH
import Style.Types exposing (ExoPalette)



-- Most of the CSS came from https://css-tricks.com/html5-meter-element/


meter : ExoPalette -> String -> String -> Int -> Int -> Element.Element msg
meter palette title subtitle value maximum =
    let
        primaryColor =
            SH.toCssColor palette.primary
    in
    Element.column
        [ Element.spacing 5
        , Font.size 14
        ]
        [ Element.row
            [ Element.width Element.fill ]
            [ Element.text title
            , Element.el [ Element.alignRight ] (Element.text subtitle)
            ]
        , Html.div []
            [ Css.Global.global
                [ Css.Global.selector "meter::-moz-meter-bar"
                    [ Css.boxShadow6 Css.inset (Css.px 0) (Css.px 5) (Css.px 5) (Css.px -5) (Css.hex "#999")
                    , Css.backgroundSize2 (Css.pct 100) (Css.pct 100)
                    , Css.backgroundImage
                        (Css.linearGradient2
                            (Css.deg 90)
                            -- TODO use primary color
                            (Css.stop2 primaryColor (Css.pct 0))
                            (Css.stop2 primaryColor (Css.pct 100))
                            []
                        )
                    ]
                ]
            , Css.Global.global
                [ Css.Global.selector "meter::-webkit-meter-bar"
                    [ Css.property "background" "none"
                    , Css.backgroundColor (Css.hex "#F5F5F5")
                    , Css.boxShadow6 Css.inset (Css.px 0) (Css.px 5) (Css.px 5) (Css.px -5) (Css.hex "#333")
                    ]
                , Css.Global.selector "meter::-webkit-meter-optimum-value"
                    [ Css.boxShadow6 Css.inset (Css.px 0) (Css.px 5) (Css.px 5) (Css.px -5) (Css.hex "#999")
                    , Css.backgroundImage
                        (Css.linearGradient2
                            (Css.deg 90)
                            -- TODO use primary color
                            (Css.stop2 primaryColor (Css.pct 0))
                            (Css.stop2 primaryColor (Css.pct 100))
                            []
                        )
                    , Css.backgroundSize2 (Css.pct 100) (Css.pct 100)
                    ]
                ]
            , Html.meter
                [ HtmlA.css
                    [ Css.width (Css.px 260)
                    , Css.height (Css.px 25)
                    , Css.property "background" "none"
                    , Css.backgroundColor (Css.hex "#F5F5F5")
                    , Css.border3 (Css.px 1) Css.solid (Css.hex "#ccc")
                    , Css.borderRadius (Css.px 3)
                    , Css.boxShadow6 Css.inset (Css.px 0) (Css.px 5) (Css.px 5) (Css.px -5) (Css.hex "#999")

                    --, Css.property "-webkit-appearance" "none"
                    , Css.property "-moz-appearance" "none"
                    ]
                , HtmlA.value (String.fromInt value)
                , HtmlA.max (String.fromInt maximum)
                ]
                []
            ]
            |> Html.toUnstyled
            |> Element.html
            |> Element.el []
        ]
