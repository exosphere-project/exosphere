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
import LineChart.Container as Container
import LineChart.Coordinate as Coordinate
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk
import LineChart.Legends as Legends
import LineChart.Line as Line
import Svg
import Time
import Tuple
import Types.ServerResourceUsage exposing (AlertLevel(..), DataPoint, TimeSeries)
import Types.SharedMsg exposing (SharedMsg(..))
import View.Types


view : View.Types.Context -> Int -> ( Time.Posix, Time.Zone ) -> TimeSeries -> Element.Element SharedMsg
view context widthPx ( currentTime, timeZone ) timeSeriesDict =
    let
        thirtyMinMillis =
            30 * 60 * 1000

        timeSeriesListLast30m =
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

        percentRangeCustomTick : Int -> Tick.Config msg
        percentRangeCustomTick percent =
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

        percentRangeCustomTicks : Ticks.Config msg
        percentRangeCustomTicks =
            Ticks.custom <|
                \_ _ ->
                    [ percentRangeCustomTick 0
                    , percentRangeCustomTick 25
                    , percentRangeCustomTick 50
                    , percentRangeCustomTick 75
                    , percentRangeCustomTick 100
                    ]

        percentRange : (( Int, DataPoint ) -> Float) -> Axis.Config ( Int, DataPoint ) msg
        percentRange getDataFunc =
            Axis.custom
                { title = Title.default ""
                , variable = Just << getDataFunc
                , pixels = 220
                , range = Range.window 0 100
                , axisLine = AxisLine.full context.palette.on.background
                , ticks = percentRangeCustomTicks
                }

        timeRangeCustomTick : Int -> Tick.Config msg
        timeRangeCustomTick time =
            let
                label =
                    Svg.g [ Junk.transform [ Junk.offset 0 3 ] ] [ Junk.label context.palette.on.background (millisecond_to_str time) ]
            in
            Tick.custom
                { position = toFloat time
                , color = context.palette.on.background
                , width = 2
                , length = 2
                , grid = True
                , direction = Tick.negative
                , label = Just label
                }

        timeRangeCustomTicks : Coordinate.Range -> Coordinate.Range -> List (Tick.Config msg)
        timeRangeCustomTicks _ axisRange =
            let
                min =
                    axisRange.min

                max =
                    axisRange.max

                increment =
                    thirtyMinMillis / 3
            in
            [ min
            , min + increment
            , min + 2 * increment
            , max
            ]
                |> List.map round
                |> List.map timeRangeCustomTick

        timeRange : (( Int, DataPoint ) -> Float) -> Axis.Config ( Int, DataPoint ) msg
        timeRange getDataFunc =
            Axis.custom
                { title = Title.default ""
                , variable = Just << getDataFunc
                , pixels = widthPx // 3
                , range =
                    Range.window
                        (Time.posixToMillis currentTime - thirtyMinMillis |> toFloat)
                        (Time.posixToMillis currentTime |> toFloat)
                , axisLine = AxisLine.full context.palette.on.background
                , ticks = Ticks.custom timeRangeCustomTicks
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

                string_and_pad time =
                    String.padLeft 2 '0' <| String.fromInt time
            in
            (modBy 12 hours |> String.fromInt) ++ ":" ++ string_and_pad minutes

        junk : String -> Junk.Config ( Int, DataPoint ) msg
        junk title =
            Junk.custom
                (\system ->
                    { below = []
                    , above =
                        [ Junk.placed
                            system
                            system.x.min
                            system.y.max
                            0
                            -15
                            [ Junk.label context.palette.on.background title ]
                        ]
                    , html = []
                    }
                )

        chartConfig : (( Int, DataPoint ) -> Float) -> String -> LineChart.Config ( Int, DataPoint ) msg
        chartConfig getYData title =
            { y = percentRange getYData
            , x = timeRange getTime
            , container = Container.spaced "line-chart-1" 35 25 25 50
            , interpolation = Interpolation.monotone
            , intersection = Intersection.default
            , legends = Legends.none
            , events = Events.default
            , junk = junk title
            , grid = Grid.default
            , area = Area.default
            , line = Line.wider 2
            , dots = Dots.default
            }

        series : List (LineChart.Series ( Int, DataPoint ))
        series =
            [ LineChart.line
                context.palette.primary
                Dots.none
                ""
                (Dict.toList timeSeriesListLast30m)
            ]
    in
    Element.row
        [ Element.width Element.fill
        , Element.scrollbarX

        -- This is needed to avoid showing a vertical scroll bar
        , Element.paddingXY 0 1
        ]
        ([ LineChart.viewCustom
            (chartConfig (getMetricUsedPct .cpuPctUsed) "CPU")
            series
         , LineChart.viewCustom
            (chartConfig (getMetricUsedPct .memPctUsed) "Memory")
            series
         , LineChart.viewCustom
            (chartConfig (getMetricUsedPct .rootfsPctUsed) "Root Disk")
            series
         ]
            |> List.map Element.html
            |> List.map (\e -> Element.el [] e)
        )
