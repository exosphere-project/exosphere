module Style.Widgets.StatusBadge exposing (StatusBadgeState(..), statusBadge, toColors)

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Style.Helpers as SH
import Style.Types


type StatusBadgeState
    = ReadyGood
    | Muted
    | Warning
    | Error


statusBadge : Style.Types.ExoPalette -> StatusBadgeState -> Element.Element msg -> Element.Element msg
statusBadge palette state status =
    let
        stateColor =
            toColors palette state
    in
    Element.el
        [ Element.paddingXY 12 6
        , Border.rounded 24
        , Border.width 1
        , Border.color <| SH.toElementColor stateColor.border
        , Background.color <| SH.toElementColor stateColor.background
        , Font.color <| SH.toElementColor stateColor.text
        , Font.size 16
        ]
        status


toColors : Style.Types.ExoPalette -> StatusBadgeState -> Style.Types.UIStateColors
toColors palette state =
    case state of
        ReadyGood ->
            palette.success

        Muted ->
            palette.muted

        Warning ->
            palette.warning

        Error ->
            palette.danger
