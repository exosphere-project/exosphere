module Style.Widgets.IconButton exposing (chip, goToButton)

import Element exposing (Element)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Style.Helpers as SH
import Style.Types exposing (ExoPalette)
import Style.Widgets.Icon exposing (timesCircle)


chip : ExoPalette -> Maybe msg -> Element msg -> Element msg
chip palette onPress label =
    Element.row
        [ Element.spacing 8
        , Element.padding 8
        , Border.width 1
        , Font.size 12
        , Border.color <| SH.toElementColor palette.muted
        , Border.rounded 3
        ]
        [ label
        , Input.button
            [ Element.mouseOver
                [ Border.color <| SH.toElementColor palette.on.background
                ]
            ]
            { onPress = onPress
            , label = timesCircle (SH.toElementColor palette.on.background) 12
            }
        ]


goToButton : ExoPalette -> Maybe msg -> Element msg
goToButton palette onPress =
    Input.button
        [ Border.width 1
        , Border.rounded 6
        , Border.color (SH.toElementColor palette.muted)
        , Element.padding 3
        ]
        { onPress = onPress
        , label =
            FeatherIcons.chevronRight
                |> FeatherIcons.withSize 14
                |> FeatherIcons.toHtml []
                |> Element.html
                |> Element.el []
        }
