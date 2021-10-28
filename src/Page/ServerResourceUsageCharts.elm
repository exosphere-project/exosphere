module Page.ServerResourceUsageCharts exposing (view)

import Dict
import Element
import Helpers.ServerResourceUsage exposing (timeSeriesRecentDataPoints)
import LineChart
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Axis.Line as AxisLine
import LineChart.Axis.Range as Range
import LineChart.Axis.Tick as Tick
import LineChart.Axis.Ticks as Ticks
import LineChart.Axis.Title as Title
import LineChart.Colors as Colors
import LineChart.Container as Container
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk
import LineChart.Legends as Legends
import LineChart.Line as Line
import Time
import Tuple
import Types.ServerResourceUsage exposing (AlertLevel(..), DataPoint, TimeSeries)
import Types.SharedMsg exposing (SharedMsg(..))
import View.Helpers as VH
import View.Types


view : View.Types.Context -> Int -> ( Time.Posix, Time.Zone ) -> TimeSeries -> Element.Element SharedMsg
view context widthPx ( currentTime, timeZone ) timeSeriesDict =
    let
        timeSeriesListLast30m =
            let
                thirtyMinMillis =
                    1800000
            in
            timeSeriesRecentDataPoints timeSeriesDict currentTime thirtyMinMillis

        getTime : ( Int, DataPoint ) -> Float
        getTime dataPointTuple =
            Tuple.first dataPointTuple
                |> toFloat

        getMetricUsedPct : (DataPoint -> Int) -> ( Int, DataPoint ) -> Float
        getMetricUsedPct accessor dataPointTuple =
            Tuple.second dataPointTuple
                |> accessor
                |> toFloat

        percentRange getDataFunc =
            let
                tick percent =
                    let
                        label =
                            Junk.label context.palette.on.background (String.fromInt percent ++ "%")
                    in
                    Tick.custom
                        { position = toFloat percent
                        , color = context.palette.on.background
                        , width = 2
                        , length = 2
                        , grid = True
                        , direction = Tick.negative
                        , label = Just label
                        }

                ticks =
                    Ticks.custom <|
                        \_ _ ->
                            [ tick 0
                            , tick 25
                            , tick 50
                            , tick 75
                            , tick 100
                            ]
            in
            Axis.custom
                { title = Title.default ""
                , variable = Just << getDataFunc
                , pixels = 220
                , range = Range.window 0 100
                , axisLine = AxisLine.full context.palette.on.background
                , ticks = ticks
                }

        customTick : Int -> Tick.Config msg
        customTick number =
            let
                label =
                    Junk.label context.palette.on.background (millisecond_to_str number)

                even =
                    modBy 20 number == 0
            in
            Tick.custom
                { position = toFloat number
                , color = context.palette.on.background
                , width = 2
                , length = 2
                , grid = even
                , direction = Tick.negative
                , label = Just label
                }

        timeRange getDataFunc =
            Axis.custom
                { title = Title.default ""
                , variable = Just << getDataFunc
                , pixels = widthPx
                , range = Range.default
                , axisLine = AxisLine.full context.palette.on.background
                , ticks = Ticks.intCustom 4 customTick
                }

        -- function call for proper formatting of times
        millisecond_to_str milli_time =
            let
                posix_time =
                    Time.millisToPosix milli_time

                hours =
                    Time.toHour timeZone posix_time

                minutes =
                    Time.toMinute timeZone posix_time

                am_pm =
                    if hours < 12 then
                        "AM"

                    else
                        "PM"

                string_and_pad time =
                    String.padLeft 2 '0' <| String.fromInt time
            in
            string_and_pad (modBy 12 hours) ++ ":" ++ string_and_pad minutes ++ " " ++ am_pm

        chartConfig : (( Int, DataPoint ) -> Float) -> LineChart.Config ( Int, DataPoint ) msg
        chartConfig getYData =
            { y = percentRange getYData
            , x = timeRange getTime
            , container = Container.spaced "line-chart-1" 40 20 40 70
            , interpolation = Interpolation.monotone
            , intersection = Intersection.default
            , legends = Legends.none
            , events = Events.default
            , junk = Junk.default
            , grid = Grid.default
            , area = Area.default
            , line = Line.default
            , dots = Dots.default
            }

        series : List (LineChart.Series ( Int, DataPoint ))
        series =
            [ LineChart.line
                context.palette.primary
                Dots.circle
                ""
                (Dict.toList timeSeriesListLast30m)
            ]
    in
    Element.column VH.exoColumnAttributes
        [ Element.el VH.heading4 (Element.text "CPU Usage")
        , Element.html <|
            LineChart.viewCustom (chartConfig (getMetricUsedPct .cpuPctUsed)) series
        , Element.el VH.heading4 (Element.text "Memory Usage")
        , Element.html <|
            LineChart.viewCustom (chartConfig (getMetricUsedPct .memPctUsed)) series
        , Element.el VH.heading4 (Element.text "Root Filesystem Usage")
        , Element.html <|
            LineChart.viewCustom (chartConfig (getMetricUsedPct .rootfsPctUsed)) series
        ]
