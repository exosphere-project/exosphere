module Style.Widgets.Alert exposing (inlineAlert)

import Color
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import FeatherIcons
import Style.Helpers as SH
import Style.Types
import View.Helpers exposing (edges)


inlineAlert : Style.Types.ExoPalette -> Element msg -> Element msg -> Element msg -> Element msg
inlineAlert palette title subTitle content =
    let
        backgroundColor =
            Color.rgb255 250 234 232
                |> SH.toElementColor
    in
    Element.column
        [ Background.color backgroundColor
        , Element.padding 16
        , Element.spacing 10
        , Border.widthEach { edges | top = 3 }
        , Border.color (palette.error |> SH.toElementColor)
        , Element.width Element.fill
        ]
    <|
        List.append
            [ Element.row
                [ Element.width Element.fill
                , Element.spacingXY 10 0
                , Font.size 16
                , Font.color (palette.error |> SH.toElementColor)
                ]
                [ Element.el []
                    (FeatherIcons.alertCircle
                        |> FeatherIcons.toHtml []
                        |> Element.html
                    )
                , Element.el
                    [ Font.bold
                    ]
                    title
                , Element.el [ Element.alignRight ] subTitle
                ]
            ]
            (if content == Element.none then
                []

             else
                [ content ]
            )
