module Style.Widgets.ToggleTip exposing (toggleTip, toggleTip2)

import Element
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import FeatherIcons
import Html.Attributes
import Style.Helpers as SH
import Style.Types
import Style.Widgets.Popover exposing (popover)
import Types.SharedMsg
import View.Types


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


toggleTip2 :
    View.Types.Context
    -> (Types.SharedMsg.SharedMsg -> msg)
    -> View.Types.PopoverId
    -> Element.Element msg
    -> Style.Types.PopoverPosition
    -> Element.Element msg
toggleTip2 context sharedMsgMapper id content position =
    let
        tipStyle =
            [ Element.htmlAttribute (Html.Attributes.style "pointerEvents" "none")
            , Border.rounded 4
            , Font.color (SH.toElementColorWithOpacity context.palette.on.surface 0.8)
            , Font.size 15
            ]

        btnClickOrHoverStyle =
            [ -- darken the icon color
              Font.color (context.palette.on.background |> SH.toElementColor)
            ]

        tipIconBtn toggleMsg tipIsShown =
            FeatherIcons.info
                |> FeatherIcons.withSize 20
                |> FeatherIcons.toHtml []
                |> Element.html
                |> Element.el
                    ([ Element.paddingXY 5 0
                     , Events.onClick toggleMsg
                     , Element.pointer
                     , Font.color (context.palette.muted |> SH.toElementColor)
                     , Element.mouseOver btnClickOrHoverStyle
                     ]
                        ++ (if tipIsShown then
                                btnClickOrHoverStyle

                            else
                                []
                           )
                    )
    in
    popover context
        sharedMsgMapper
        { id = id
        , styleAttrs = tipStyle
        , content = \_ -> content
        , position = position
        , distance = Nothing
        , target = tipIconBtn
        }
