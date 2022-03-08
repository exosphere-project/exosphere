module Page.ServerResourceUsageCharts exposing (view)

import Dict
import Element
import Element.Font as Font
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
import Style.Helpers as SH
import Style.Widgets.Icon as Icon
import Svg
import Time
import Tuple
import Types.HelperTypes as HelperTypes
import Types.ServerResourceUsage exposing (AlertLevel(..), DataPoint, TimeSeries)
import View.Types


type alias TimeAndSingleMetric =
    ( Int, Int )


view :
    View.Types.Context
    -> Int
    -> ( Time.Posix, Time.Zone )
    -> Maybe HelperTypes.ServerResourceQtys
    -> TimeSeries
    -> Element.Element msg
view context widthPx ( currentTime, timeZone ) maybeServerResourceQtys timeSeriesDict =
    let
        thirtyMinMillis =
            30 * 60 * 1000

        timeSeriesListLast30m =
            timeSeriesRecentDataPoints timeSeriesDict currentTime thirtyMinMillis

        haveGpuData =
            -- See if we have _any_ GPU usage data
            timeSeriesListLast30m
                |> Dict.toList
                |> List.any
                    (\( _, dataPoint ) ->
                        case dataPoint.gpuPctUsed of
                            Just _ ->
                                True

                            Nothing ->
                                False
                    )

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

        percentRange : (TimeAndSingleMetric -> Int) -> Axis.Config TimeAndSingleMetric msg
        percentRange getDataFunc =
            Axis.custom
                { title = Title.default ""
                , variable = Just << toFloat << getDataFunc
                , pixels = 210
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

        timeRange : (TimeAndSingleMetric -> Int) -> Axis.Config TimeAndSingleMetric msg
        timeRange getDataFunc =
            Axis.custom
                { title = Title.default ""
                , variable = Just << toFloat << getDataFunc
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

        chartConfig : LineChart.Config TimeAndSingleMetric msg
        chartConfig =
            { y = percentRange Tuple.second
            , x = timeRange Tuple.first
            , container = Container.spaced "line-chart-1" 20 25 25 50
            , interpolation = Interpolation.monotone
            , intersection = Intersection.default
            , legends = Legends.none
            , events = Events.default
            , junk = Junk.default
            , grid = Grid.default
            , area = Area.default
            , line = Line.wider 2
            , dots = Dots.default
            }

        series : (DataPoint -> Int) -> Maybe (DataPoint -> Int) -> List (LineChart.Series TimeAndSingleMetric)
        series getPrimaryData maybeGetSecondaryData =
            List.concat
                [ [ LineChart.line
                        context.palette.primary
                        Dots.none
                        ""
                        (timeSeriesListLast30m
                            |> Dict.toList
                            |> List.map (Tuple.mapSecond getPrimaryData)
                        )
                  ]
                , case maybeGetSecondaryData of
                    Just getSecondaryData ->
                        [ LineChart.line
                            context.palette.warn
                            Dots.none
                            ""
                            (timeSeriesListLast30m
                                |> Dict.toList
                                |> List.map (Tuple.mapSecond getSecondaryData)
                            )
                        ]

                    Nothing ->
                        []
                ]
    in
    Element.row
        [ Element.width Element.fill
        , Element.height (Element.minimum 229 Element.shrink)
        , Element.scrollbarX

        -- This is needed to avoid showing a vertical scroll bar
        , Element.paddingXY 0 1
        ]
        [ Element.column []
            [ toCpuHeading context maybeServerResourceQtys haveGpuData
            , LineChart.viewCustom
                chartConfig
                (series
                    .cpuPctUsed
                    (if haveGpuData then
                        Just (\datapoint -> Maybe.withDefault 0 datapoint.gpuPctUsed)

                     else
                        Nothing
                    )
                )
                |> Element.html
            ]
        , Element.column []
            [ toMemHeading context maybeServerResourceQtys
            , LineChart.viewCustom
                chartConfig
                (series
                    .memPctUsed
                    Nothing
                )
                |> Element.html
            ]
        , Element.column []
            [ toDiskHeading context maybeServerResourceQtys
            , LineChart.viewCustom
                chartConfig
                (series
                    .rootfsPctUsed
                    Nothing
                )
                |> Element.html
            ]
        ]


toChartHeading : View.Types.Context -> Element.Element msg -> String -> Element.Element msg
toChartHeading context title subtitle =
    Element.row
        [ Element.width Element.fill, Element.paddingEach { top = 0, bottom = 0, left = 0, right = 25 } ]
        [ Element.el [ Font.bold ] title
        , Element.el
            [ Font.color (context.palette.muted |> SH.toElementColor)
            , Element.alignRight
            ]
            (Element.text subtitle)
        ]


toCpuHeading : View.Types.Context -> Maybe HelperTypes.ServerResourceQtys -> Bool -> Element.Element msg
toCpuHeading context maybeServerResourceQtys haveGpuData =
    toChartHeading
        context
        (if haveGpuData then
            Element.row [ Element.spacing 5 ]
                [ Element.text "CPU"
                , Icon.roundRect (context.palette.primary |> SH.toElementColor) 16
                , Element.text "and GPU"
                , Icon.roundRect (context.palette.warn |> SH.toElementColor) 16
                ]

         else
            Element.text "CPU"
        )
        (maybeServerResourceQtys
            |> Maybe.map .cores
            |> Maybe.map
                (\x ->
                    String.join " "
                        [ "of"
                        , String.fromInt x
                        , "total"
                        , if x == 1 then
                            "core"

                          else
                            "cores"
                        ]
                )
            |> Maybe.withDefault ""
        )


toMemHeading : View.Types.Context -> Maybe HelperTypes.ServerResourceQtys -> Element.Element msg
toMemHeading context maybeServerResourceQtys =
    toChartHeading
        context
        (Element.text "RAM")
        (maybeServerResourceQtys
            |> Maybe.map .ramGb
            |> Maybe.map
                (\x ->
                    String.join " "
                        [ "of"
                        , String.fromInt x
                        , "total GB"
                        ]
                )
            |> Maybe.withDefault ""
        )


toDiskHeading : View.Types.Context -> Maybe HelperTypes.ServerResourceQtys -> Element.Element msg
toDiskHeading context maybeServerResourceQtys =
    toChartHeading
        context
        (Element.text "Root Disk")
        (maybeServerResourceQtys
            |> Maybe.andThen .rootDiskGb
            |> Maybe.map
                (\x ->
                    String.join " "
                        [ "of"
                        , String.fromInt x
                        , "total GB"
                        ]
                )
            |> Maybe.withDefault ""
        )
