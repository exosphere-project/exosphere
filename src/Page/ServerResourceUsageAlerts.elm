module Page.ServerResourceUsageAlerts exposing (view)

import Dict
import Element
import Helpers.ServerResourceUsage exposing (timeSeriesRecentDataPoints)
import Style.Helpers exposing (spacer)
import Style.Widgets.Alert as Alert
import Time
import Tuple
import Types.ServerResourceUsage exposing (DataPoint, TimeSeries)
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
                |> Element.column
                    [ Element.spacing spacer.px8 ]

        Nothing ->
            Element.none


dataPointToAlerts : View.Types.Context -> DataPoint -> List ( Alert.AlertState, String )
dataPointToAlerts context dataPoint =
    let
        diskAlerts =
            if dataPoint.rootfsPctUsed > 95 then
                [ ( Alert.Danger
                  , "Root disk is full! Please free some space now, else "
                        ++ context.localization.virtualComputer
                        ++ " will stop working."
                  )
                ]

            else if dataPoint.rootfsPctUsed > 90 then
                [ ( Alert.Warning, "Root disk is nearly full. Be careful not to use up all the space." ) ]

            else
                []

        memAlerts =
            if dataPoint.memPctUsed > 95 then
                [ ( Alert.Warning, "Available memory (RAM) is nearly exhausted." ) ]

            else
                []

        cpuAlerts =
            if dataPoint.cpuPctUsed > 95 then
                [ ( Alert.Info, "CPU usage is high." ) ]

            else
                []
    in
    List.concat [ diskAlerts, memAlerts, cpuAlerts ]


renderAlert : View.Types.Context -> ( Alert.AlertState, String ) -> Element.Element msg
renderAlert context ( alertState, alertText ) =
    Alert.alert [ Element.padding spacer.px4 ]
        context.palette
        { state = alertState
        , showIcon = True
        , showContainer = False
        , content = Element.paragraph [] [ Element.text alertText ]
        }
