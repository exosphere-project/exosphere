module Style.Widgets.ToggleTip exposing (toggleTip, toggleTipWithIcon, warningToggleTip)

import Element
import Element.Border as Border
import Element.Font as Font
import FeatherIcons as Icons
import Set
import Style.Helpers as SH
import Style.Types exposing (ExoPalette)
import Style.Widgets.IconButton exposing (clickableIcon)
import Style.Widgets.Popover.Popover exposing (popover)
import Style.Widgets.Popover.Types exposing (PopoverId)
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
        "info"
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
        "warning"
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
    -> String
    -> Element.Color
    -> Element.Color
    -> Element.Element msg
toggleTipWithIcon context msgMapper id content position icon accessibilityLabel color hoverColor =
    let
        tipStyle =
            [ Border.rounded 4
            , Font.color (context.palette.neutral.text.subdued |> SH.toElementColor)
            , Text.fontSize Text.Small
            ]

        tipIconBtn toggleMsg tipIsShown =
            clickableIcon
                -- persist the hover style if the tooltip is shown
                (if tipIsShown then
                    [ Font.color hoverColor ]

                 else
                    []
                )
                { icon = icon
                , accessibilityLabel = accessibilityLabel
                , onClick = Just toggleMsg
                , color = color
                , hoverColor = hoverColor
                }
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
