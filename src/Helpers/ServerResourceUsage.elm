module Helpers.ServerResourceUsage exposing
    ( parseConsoleLog
    , timeSeriesRecentDataPoints
    )

import Dict
import Helpers.Helpers as Helpers
import Json.Decode
import Time
import Types.ServerResourceUsage exposing (DataPoint, History, TimeSeries)



{- Parses console log for server resource usage -}


parseConsoleLog : String -> History -> History
parseConsoleLog consoleLog prevHistory =
    let
        loglines =
            String.split "\n" consoleLog

        decodedData =
            loglines
                |> List.map Helpers.stripTimeSinceBootFromLogLine
                |> List.filterMap
                    (\l -> Json.Decode.decodeString logLineDecoder l |> Result.toMaybe)

        newTimeSeries =
            List.foldl
                (\( k, v ) -> Dict.insert k v)
                prevHistory.timeSeries
                decodedData

        newStrikes =
            if newTimeSeries == prevHistory.timeSeries then
                prevHistory.pollingStrikes + 1

            else
                0
    in
    History newTimeSeries newStrikes


logLineDecoder : Json.Decode.Decoder ( Int, DataPoint )
logLineDecoder =
    Json.Decode.map2
        Tuple.pair
        (Json.Decode.field "epoch" Json.Decode.int
            -- This gets us milliseconds
            |> Json.Decode.map (\epoch -> epoch * 1000)
        )
        (Json.Decode.map4
            DataPoint
            (Json.Decode.field "cpuPctUsed" Json.Decode.int)
            (Json.Decode.field "memPctUsed" Json.Decode.int)
            (Json.Decode.field "rootfsPctUsed" Json.Decode.int)
            (Json.Decode.oneOf
                [ Json.Decode.field "gpuPctUsed" Json.Decode.int
                    |> Json.Decode.map Just
                , Json.Decode.succeed Nothing
                ]
            )
        )


timeSeriesRecentDataPoints : TimeSeries -> Time.Posix -> Int -> Dict.Dict Int DataPoint
timeSeriesRecentDataPoints timeSeries currentTime timeIntervalDurationMillis =
    let
        timeSeriesList =
            Dict.toList timeSeries

        durationAgo =
            Time.posixToMillis currentTime - timeIntervalDurationMillis

        recentDataPoints =
            List.filter (\t -> Tuple.first t > durationAgo) timeSeriesList
    in
    Dict.fromList recentDataPoints
