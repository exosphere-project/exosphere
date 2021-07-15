module Style.Widgets.ToggleTip exposing (toggleTip)

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import FeatherIcons
import Html.Attributes
import Style.Helpers as SH
import Style.Types


toggleTip : Style.Types.ExoPalette -> String -> Bool -> (Bool -> msg) -> Element.Element msg
toggleTip palette tipText shown toShowHideTipMsg =
    let
        whileShownAttributes =
            [ Element.above <|
                Element.el
                    [ Element.centerX
                    , Element.paddingEach { bottom = 8, top = 0, left = 0, right = 0 }
                    ]
                <|
                    Element.el
                        [ Element.htmlAttribute (Html.Attributes.style "pointerEvents" "none")
                        , Element.width Element.shrink
                        , Element.centerX
                        , Element.padding 5
                        , Background.color (SH.toElementColor palette.surface)
                        , Border.width 1
                        , Border.rounded 5
                        , Border.color (SH.toElementColor palette.muted)
                        , Border.shadow
                            { offset = ( 0, 3 ), blur = 6, size = 0, color = Element.rgba 0 0 0 0.32 }
                        , Font.color (palette.on.surface |> SH.toElementColor)
                        ]
                        (Element.text tipText)
            ]
    in
    FeatherIcons.info
        |> FeatherIcons.withSize 20
        |> FeatherIcons.toHtml []
        |> Element.html
        |> Element.el
            (List.concat
                [ [ Element.paddingXY 5 0
                  , Events.onClick (not shown |> toShowHideTipMsg)
                  , Element.pointer
                  , Font.color (palette.muted |> SH.toElementColor)
                  ]
                , if shown then
                    whileShownAttributes

                  else
                    []
                ]
            )
