module Style.Widgets.Tooltip exposing (tooltip)

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Style.Helpers as SH
import Style.Types


tooltip : Style.Types.ExoPalette -> Element.Element msg -> Element.Element msg -> Bool -> (Bool -> msg) -> Element.Element msg
tooltip palette anchorContent tooltipContent shown toShowHideTooltipMsg =
    Element.el
        (List.concat
            [ if shown then
                [ Element.above <|
                    Element.el
                        [ Element.centerX
                        , Element.paddingEach { bottom = 8, top = 0, left = 0, right = 0 }
                        ]
                    <|
                        Element.el
                            [ Element.width Element.shrink
                            , Element.centerX
                            , Element.padding 5
                            , Background.color (SH.toElementColor palette.surface)
                            , Border.width 1
                            , Border.rounded 5
                            , Border.color (SH.toElementColor palette.muted)
                            , Border.shadow
                                { offset = ( 0, 3 ), blur = 6, size = 0, color = Element.rgba 0 0 0 0.32 }
                            ]
                            tooltipContent
                ]

              else
                []
            , [ Events.onClick (not shown |> toShowHideTooltipMsg)
              , Element.pointer
              ]
            ]
        )
        anchorContent
