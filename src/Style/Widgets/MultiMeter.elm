module Style.Widgets.MultiMeter exposing (multiMeter)

import Element
import Element.Background as Background
import Html.Attributes as HtmlA
import Style.Helpers as SH
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import View.Types


{-| Draws a proportional meter given multiple sub-values.

This is like the Style.Widgets.Meter but renders multiple values.
The sum of all the values must not exceed the `max` or it will render incorrectly.

Tooltips appear when hovering over the meter segments to clarify what they measure.

    -- Show the breakdown of server usage
    multiMeter
        context
        "Server Usage"
        "42 of 1337 Servers"
        -- Max width in absolute units
        1337
        -- Label, Width in absolute units, attributes to add to meter segment (e.g. for coloring)
        [ ( "Busy", 17, [ Background.color (Element.rgb 1 0 0) ] )
        , ( "Idle", 22, [ Background.color (Element.rgb 0 0 1) ] )
        , ( "Booting", 3, [ Background.color (Element.rgb 1 1 0) ] )
        ]

-}
multiMeter : View.Types.Context -> String -> String -> Int -> List ( String, Int, List (Element.Attribute msg) ) -> Element.Element msg
multiMeter { palette } title subtitle max values =
    let
        backgroundColor =
            SH.toElementColorWithOpacity palette.primary 0.15

        unaccountedWidth =
            List.foldl (\( _, w, _ ) leftOver -> leftOver - w) max values

        tooltipLink s =
            ("multiMeter-" ++ title ++ "-" ++ s) |> String.replace " " "_"
    in
    Element.column
        [ Element.spacing spacer.px4
        , Text.fontSize Text.Small
        ]
        [ Element.row
            [ Element.width Element.fill ]
            [ Element.text title
            , Element.el [ Element.alignRight ] (Element.text subtitle)
            ]
        , Element.row
            [ Element.width (Element.px 262)
            , Element.height (Element.px 25)
            , Background.color backgroundColor
            , Element.htmlAttribute (HtmlA.attribute "role" "meter")
            ]
            (List.append
                (values
                    |> List.map
                        (\( label, w, extraAttributes ) ->
                            Element.el
                                (List.append extraAttributes
                                    [ Element.width (Element.fillPortion w)
                                    , Element.height Element.fill
                                    , Element.htmlAttribute (HtmlA.attribute "aria-describedby" (tooltipLink label))
                                    , tooltip Element.below label (tooltipLink label)
                                    ]
                                )
                                Element.none
                        )
                )
                [ Element.el
                    [ Element.width (Element.fillPortion unaccountedWidth)
                    , Element.height Element.fill
                    ]
                    Element.none
                ]
            )
        ]


tooltipLabel : String -> Element.Element msg
tooltipLabel label =
    Element.el
        [ Element.paddingEach { top = 4, right = 0, bottom = 0, left = 0 } ]
        (Element.text label)



{-
   @cite: https://ellie-app.com/7R2VHTzHJYYa1
-}


tooltip : (Element.Element msg -> Element.Attribute msg) -> String -> String -> Element.Attribute msg
tooltip position label tooltipLink =
    Element.inFront <|
        Element.el
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.transparent True
            , Element.mouseOver [ Element.transparent False ]
            , Element.htmlAttribute (HtmlA.attribute "role" "tooltip")
            , Element.htmlAttribute (HtmlA.id tooltipLink)
            , (position << Element.map never) <|
                Element.el [ Element.htmlAttribute (HtmlA.style "pointerEvents" "none") ]
                    (tooltipLabel label)
            ]
            Element.none
