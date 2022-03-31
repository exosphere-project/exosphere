module Style.Widgets.ToggleTip exposing (toggleTip)

import Element
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import FeatherIcons
import Html.Attributes
import Style.Helpers as SH
import Style.Types


tipPopover : Style.Types.ExoPalette -> Element.Element msg -> Element.Element msg
tipPopover palette content =
    Element.el
        (SH.popoverStyleDefaults palette
            ++ [ Element.htmlAttribute (Html.Attributes.style "pointerEvents" "none")
               , Border.rounded 4
               , Font.color (SH.toElementColorWithOpacity palette.on.surface 0.8)
               , Font.size 15
               ]
        )
        content


toggleTip : Style.Types.ExoPalette -> Element.Element msg -> Style.Types.PopoverPosition -> Bool -> msg -> Element.Element msg
toggleTip palette content position shown showHideTipMsg =
    let
        clickOrHoverStyle =
            [ -- darken the icon color
              Font.color (palette.on.background |> SH.toElementColor)
            ]
    in
    FeatherIcons.info
        |> FeatherIcons.withSize 20
        |> FeatherIcons.toHtml []
        |> Element.html
        |> Element.el
            (List.concat
                [ [ Element.paddingXY 5 0
                  , Events.onClick showHideTipMsg
                  , Element.pointer
                  , Font.color (palette.muted |> SH.toElementColor)
                  , Element.mouseOver clickOrHoverStyle
                  ]
                , if shown then
                    SH.popoverAttribs (tipPopover palette content) position Nothing
                        ++ clickOrHoverStyle

                  else
                    []
                ]
            )
