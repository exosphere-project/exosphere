module Style.Widgets.ToggleTip exposing (toggleTip, toggleTipWithIcon, warningToggleTip)

import Element
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import FeatherIcons as Icons
import Set
import Style.Helpers as SH
import Style.Types exposing (ExoPalette)
import Style.Widgets.Icon exposing (featherIcon)
import Style.Widgets.Popover.Popover exposing (popover)
import Style.Widgets.Popover.Types exposing (PopoverId)
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text


{-| Shows an info icon button which displays a popover when clicked.
-}
toggleTip :
    { viewContext | palette : ExoPalette, showPopovers : Set.Set PopoverId }
    -> (PopoverId -> msg)
    -> PopoverId
    -> Element.Element msg
    -> Style.Types.PopoverPosition
    -> Element.Element msg
toggleTip context msgMapper id content position =
    toggleTipWithIcon
        context
        msgMapper
        id
        content
        position
        Icons.info
        (context.palette.neutral.icon |> SH.toElementColor)
        (context.palette.neutral.text.default |> SH.toElementColor)


{-| Shows a warning icon button which displays a popover when clicked.
-}
warningToggleTip :
    { viewContext | palette : ExoPalette, showPopovers : Set.Set PopoverId }
    -> (PopoverId -> msg)
    -> PopoverId
    -> Element.Element msg
    -> Style.Types.PopoverPosition
    -> Element.Element msg
warningToggleTip context msgMapper id content position =
    toggleTipWithIcon
        context
        msgMapper
        id
        content
        position
        Icons.alertTriangle
        -- FIXME: Palette's warning `default` is difficult to read on a neutral bg so `textOnNeutralBG` is better; but the focus color must be darker & `textOnColoredBG` is a bit too dark.
        (context.palette.warning.textOnNeutralBG |> SH.toElementColor)
        (context.palette.warning.textOnColoredBG |> SH.toElementColor)


{-| Shows a customisable icon button which displays a popover when clicked.
-}
toggleTipWithIcon :
    { viewContext | palette : ExoPalette, showPopovers : Set.Set PopoverId }
    -> (PopoverId -> msg)
    -> PopoverId
    -> Element.Element msg
    -> Style.Types.PopoverPosition
    -> Icons.Icon
    -> Element.Color
    -> Element.Color
    -> Element.Element msg
toggleTipWithIcon context msgMapper id content position icon color hoverColor =
    let
        tipStyle =
            [ Border.rounded 4
            , Font.color (context.palette.neutral.text.subdued |> SH.toElementColor)
            , Text.fontSize Text.Small
            ]

        btnClickOrHoverStyle =
            [ -- darken the icon color
              Font.color hoverColor
            ]

        tipIconBtn toggleMsg tipIsShown =
            (icon |> Icons.withSize 20)
                |> featherIcon
                    ([ Element.paddingXY spacer.px4 0
                     , Events.onClick toggleMsg
                     , Element.pointer
                     , Font.color color
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
        msgMapper
        { id = id
        , content = \_ -> content
        , contentStyleAttrs = tipStyle
        , position = position
        , distanceToTarget = Nothing
        , target = tipIconBtn
        , targetStyleAttrs = []
        }
