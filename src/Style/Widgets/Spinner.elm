module Style.Widgets.Spinner exposing (Size(..), circularProgressIndicator, medium, notes, small, withSize)

import Color
import Element exposing (Attribute, Element)
import Html.Attributes exposing (attribute)
import Style.Types exposing (ExoPalette)
import Style.Widgets.Spacer exposing (spacer)
import Svg
import Svg.Attributes


notes : String
notes =
    """
## Usage

A circular indeterminate spinner with predefined sizes used to indicate loading or processing states.

    Spinner.medium context.palette

(Based on the `circularProgressIndicator` from [elm-ui-widgets](https://package.elm-lang.org/packages/Orasund/elm-ui-widgets/latest/Widget#ProgressIndicator) but with flexible width and height.)
"""


type Size
    = Small
    | Medium


{-| A small circular indeterminate activity indicator.
-}
small : ExoPalette -> Element msg
small palette =
    withSize Small palette.primary


{-| A medium-sized circular indeterminate activity indicator.
-}
medium : ExoPalette -> Element msg
medium palette =
    withSize Medium palette.primary


{-| A circular indeterminate activity indicator with a predefined size.
-}
withSize : Size -> Color.Color -> Element msg
withSize size color =
    let
        label =
            "Loading"

        accessibility =
            [ Element.htmlAttribute (attribute "aria-label" label)
            , Element.htmlAttribute (attribute "title" label)
            ]
    in
    circularProgressIndicator
        (case size of
            Small ->
                spacer.px32

            Medium ->
                spacer.px48
        )
        color
        accessibility


{-| A circular indeterminate activity indicator.

        circularProgressIndicator 30 color [ Element.padding 16 ]

    Based on https://github.com/Orasund/elm-ui-widgets.
    Originally based on https://codepen.io/FezVrasta/pen/oXrgdR.

-}
circularProgressIndicator : Int -> Color.Color -> List (Attribute msg) -> Element msg
circularProgressIndicator sizePx color attribs =
    let
        px =
            String.fromInt sizePx ++ "px"
    in
    Svg.svg
        [ Svg.Attributes.height px
        , Svg.Attributes.width px
        , Svg.Attributes.viewBox "0 0 66 66"
        , Svg.Attributes.xmlSpace "http://www.w3.org/2000/svg"
        ]
        [ Svg.g []
            [ Svg.animateTransform
                [ Svg.Attributes.attributeName "transform"
                , Svg.Attributes.type_ "rotate"
                , Svg.Attributes.values "0 33 33;270 33 33"
                , Svg.Attributes.begin "0s"
                , Svg.Attributes.dur "1.4s"
                , Svg.Attributes.fill "freeze"
                , Svg.Attributes.repeatCount "indefinite"
                ]
                []
            , Svg.circle
                [ Svg.Attributes.fill "none"
                , Svg.Attributes.stroke (Color.toCssString color)
                , Svg.Attributes.strokeWidth "5"
                , Svg.Attributes.strokeLinecap "square"
                , Svg.Attributes.cx "33"
                , Svg.Attributes.cy "33"
                , Svg.Attributes.r "30"
                , Svg.Attributes.strokeDasharray "187"
                , Svg.Attributes.strokeDashoffset "610"
                ]
                [ Svg.animateTransform
                    [ Svg.Attributes.attributeName "transform"
                    , Svg.Attributes.type_ "rotate"
                    , Svg.Attributes.values "0 33 33;135 33 33;450 33 33"
                    , Svg.Attributes.begin "0s"
                    , Svg.Attributes.dur "1.4s"
                    , Svg.Attributes.fill "freeze"
                    , Svg.Attributes.repeatCount "indefinite"
                    ]
                    []
                , Svg.animate
                    [ Svg.Attributes.attributeName "stroke-dashoffset"
                    , Svg.Attributes.values "187;46.75;187"
                    , Svg.Attributes.begin "0s"
                    , Svg.Attributes.dur "1.4s"
                    , Svg.Attributes.fill "freeze"
                    , Svg.Attributes.repeatCount "indefinite"
                    ]
                    []
                ]
            ]
        ]
        |> Element.html
        |> Element.el attribs
