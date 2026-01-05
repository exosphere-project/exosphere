module Style.Widgets.StatusBadge exposing (StatusBadgeSize(..), StatusBadgeState(..), statusBadge, statusBadgeWithSize, toColors)

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Style.Helpers as SH
import Style.Types
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text


type StatusBadgeState
    = ReadyGood
    | Muted
    | Warning
    | Error


type StatusBadgeSize
    = Small
    | Normal


statusBadge : Style.Types.ExoPalette -> StatusBadgeState -> Element.Element msg -> Element.Element msg
statusBadge palette state status =
    statusBadgeWithSize palette Normal state status


statusBadgeWithSize : Style.Types.ExoPalette -> StatusBadgeSize -> StatusBadgeState -> Element.Element msg -> Element.Element msg
statusBadgeWithSize palette size state status =
    let
        stateColor =
            toColors palette state

        textVariant =
            case size of
                Small ->
                    Text.Small

                Normal ->
                    Text.Body

        ( x, y ) =
            case size of
                Small ->
                    ( spacer.px8, 5 )

                Normal ->
                    ( spacer.px12, 6 )
    in
    Element.el
        [ Element.paddingXY x y
        , Border.rounded 24
        , Border.width 1
        , Border.color <| SH.toElementColor stateColor.border
        , Background.color <| SH.toElementColor stateColor.background
        , Font.color <| SH.toElementColor stateColor.textOnColoredBG
        , Text.fontSize textVariant
        , Font.medium
        , Font.center
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
