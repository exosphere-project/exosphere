module Style.Widgets.StatusBadge exposing (StatusBadgeState(..), statusBadge)

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


statusBadge : Style.Types.ExoPalette -> StatusBadgeState -> String -> Element.Element msg
statusBadge palette state statusStr =
    let
        ( backgroundColor, textColor ) =
            toColors palette state
    in
    Element.el
        [ Element.paddingXY 10 6
        , Border.rounded 4
        , Border.shadow
            { offset = ( 0, 3 ), blur = 6, size = 0, color = Element.rgba 0 0 0 0.32 }
        , Background.color backgroundColor
        , Font.color textColor
        , Font.bold
        ]
        (Element.text statusStr)


toColors : Style.Types.ExoPalette -> StatusBadgeState -> ( Element.Color, Element.Color )
toColors palette state =
    let
        ( backgroundAvh4Color, textAvh4Color ) =
            case state of
                ReadyGood ->
                    ( palette.readyGood, palette.on.readyGood )

                Muted ->
                    ( palette.muted, palette.on.muted )

                Warning ->
                    ( palette.warn, palette.on.warn )

                Error ->
                    ( palette.error, palette.on.error )
    in
    ( backgroundAvh4Color, textAvh4Color )
        |> Tuple.mapBoth SH.toElementColor SH.toElementColor
