module Page.ServerResourceUsageAlerts exposing (view)

import Dict
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import FeatherIcons
import Helpers.ServerResourceUsage exposing (timeSeriesRecentDataPoints)
import Style.Helpers as SH
import Time
import Tuple
import Types.ServerResourceUsage exposing (Alert, AlertLevel(..), DataPoint, TimeSeries)
import View.Types


view : View.Types.Context -> Time.Posix -> TimeSeries -> Element.Element msg
view context currentTime timeSeries =
    let
        -- Get most recent data point, it must be <60 minutes old
        maybeNewestDataPoint =
            let
                sixtyMinMillis =
                    3600000

                recentDataPoints =
                    timeSeriesRecentDataPoints timeSeries currentTime sixtyMinMillis
            in
            Dict.toList recentDataPoints
                |> List.sortBy Tuple.first
                -- Sort time series chronologically, oldest to newest
                |> List.reverse
                -- Order by newest first
                |> List.head
                |> Maybe.map Tuple.second
    in
    case maybeNewestDataPoint of
        Just newestDataPoint ->
            dataPointToAlerts context newestDataPoint
                |> List.map (renderAlert context)
                |> Element.column [ Element.paddingXY 0 5, Element.spacing 8 ]

        Nothing ->
            Element.none


dataPointToAlerts : View.Types.Context -> DataPoint -> List Alert
dataPointToAlerts context dataPoint =
    let
        diskAlerts =
            if dataPoint.rootfsPctUsed > 95 then
                [ Alert Crit
                    ("Root disk is full! Please free some space now, else "
                        ++ context.localization.virtualComputer
                        ++ " will stop working."
                    )
                ]

            else if dataPoint.rootfsPctUsed > 90 then
                [ Alert Warn "Root disk is nearly full. Be careful not to use up all the space." ]

            else
                []

        memAlerts =
            if dataPoint.memPctUsed > 95 then
                [ Alert Warn "Available memory (RAM) is nearly exhausted." ]

            else
                []

        cpuAlerts =
            if dataPoint.cpuPctUsed > 95 then
                [ Alert Info "CPU usage is high." ]

            else
                []
    in
    List.concat [ diskAlerts, memAlerts, cpuAlerts ]


renderAlert : View.Types.Context -> Alert -> Element.Element msg
renderAlert context alert =
    let
        ( icon, color, onColor ) =
            case alert.level of
                Info ->
                    ( FeatherIcons.info, context.palette.primary, context.palette.on.primary )

                Warn ->
                    ( FeatherIcons.alertTriangle, context.palette.warn, context.palette.on.warn )

                Crit ->
                    ( FeatherIcons.alertOctagon, context.palette.error, context.palette.on.error )
    in
    Element.row
        [ Element.padding 0, Element.spacing 8 ]
        [ Element.el
            [ Element.padding 3
            , Border.rounded 4
            , Background.color (SH.toElementColor color)
            , Font.color (SH.toElementColor onColor)
            ]
            (icon
                |> FeatherIcons.toHtml []
                |> Element.html
            )
        , Element.text alert.text
        ]
