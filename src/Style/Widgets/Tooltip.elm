module Style.Widgets.Tooltip exposing (tooltip)

import Element
import Element.Background as Background
import Element.Border as Border
import Style.Helpers as SH
import Style.Types


tooltip : Style.Types.ExoPalette -> Element.Element msg -> Element.Element msg -> Bool -> (Bool -> msg) -> Element.Element msg
tooltip palette anchorContent tooltipContent shown showHideTooltipMsg =
    Element.el
        (List.concat
            [ if shown then
                [ Element.above <|
                    Element.el
                        [ Element.width Element.shrink
                        , Element.centerX
                        , Element.padding 5
                        , Background.color (SH.toElementColor palette.surface)
                        , Border.width 1
                        , Border.rounded 5
                        , Border.color (SH.toElementColor palette.muted)
                        ]
                        tooltipContent
                ]

              else
                []
            ]
        )
        anchorContent
