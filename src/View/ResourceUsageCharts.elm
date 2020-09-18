module View.ResourceUsageCharts exposing (charts)

import Color
import Dict
import Element
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
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk
import LineChart.Legends as Legends
import LineChart.Line as Line
import Time
import Tuple
import Types.ServerResourceUsage exposing (DataPoint, TimeSeries)


charts : Time.Posix -> TimeSeries -> Element.Element msg
charts currentTime timeSeriesDict =
    -- TODO need times on X-axis to look sane, either "minutes before present" or local time
    -- TODO label most recent value? With a custom tick or something?
    let
        timeSeriesList =
            Dict.toList timeSeriesDict

        timeSeriesListLast30m =
            let
                thirtyMinMillis =
                    1800000

                thirtyMinAgo =
                    Time.posixToMillis currentTime - thirtyMinMillis
            in
            List.filter (\t -> Tuple.first t > thirtyMinAgo) timeSeriesList

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
                ticks =
                    Ticks.custom <|
                        \_ _ ->
                            [ Tick.int 0
                            , Tick.int 25
                            , Tick.int 50
                            , Tick.int 75
                            , Tick.int 100
                            ]
            in
            Axis.custom
                { title = Title.default "Percent"
                , variable = Just << getDataFunc
                , pixels = 200
                , range = Range.window 0 100
                , axisLine = AxisLine.full Color.black
                , ticks = ticks
                }

        -- TODO implement this
        timeRangeNoLabel =
            timeRange

        timeRange getDataFunc =
            let
                ticks =
                    Ticks.time Time.utc 6
            in
            Axis.custom
                { title = Title.default ""
                , variable = Just << getDataFunc
                , pixels = 700
                , range = Range.default
                , axisLine = AxisLine.full Color.black
                , ticks = ticks
                }

        chartConfig : (( Int, DataPoint ) -> Float) -> Bool -> LineChart.Config ( Int, DataPoint ) msg
        chartConfig getYData isBottom =
            { y = percentRange getYData
            , x =
                if isBottom then
                    timeRange getTime

                else
                    timeRangeNoLabel getTime
            , container = Container.default "line-chart-1"
            , interpolation = Interpolation.monotone
            , intersection = Intersection.default
            , legends = Legends.default
            , events = Events.default
            , junk = Junk.default
            , grid = Grid.default
            , area = Area.default
            , line = Line.default
            , dots = Dots.default
            }

        series : String -> LineChart.Series ( Int, DataPoint )
        series label =
            LineChart.line
                (Color.rgb255 0 108 163)
                Dots.circle
                label
                timeSeriesListLast30m
    in
    Element.column [] <|
        List.map
            (\x -> Element.html <| LineChart.viewCustom (Tuple.first x) (Tuple.second x))
            [ ( chartConfig (getMetricUsedPct .cpuPctUsed) False
              , [ series "CPU" ]
              )
            , ( chartConfig (getMetricUsedPct .memPctUsed) False
              , [ series "Memory" ]
              )
            , ( chartConfig (getMetricUsedPct .rootfsPctUsed) True
              , [ series "Root Filesystem" ]
              )
            ]
