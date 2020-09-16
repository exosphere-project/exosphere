module Helpers.ServerResourceUsage exposing (getMostRecentDataPoint, parseConsoleLog)

import Dict
import Json.Decode
import Types.ServerResourceUsage exposing (DataPoint, History)



{- Parses console log for server resource usage -}


parseConsoleLog : String -> History -> History
parseConsoleLog consoleLog prevHistory =
    let
        loglines =
            --- Removing the backslash from double quotes so that JSON decoding works.
            String.split "\n" consoleLog

        decodedData =
            List.filterMap
                (\l -> Json.Decode.decodeString decodeLogLine l |> Result.toMaybe)
                loglines

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


getMostRecentDataPoint : Dict.Dict Int DataPoint -> Maybe ( Int, DataPoint )
getMostRecentDataPoint timeSeries =
    timeSeries
        |> Dict.toList
        |> List.sortBy Tuple.first
        |> List.reverse
        |> List.head


decodeLogLine : Json.Decode.Decoder ( Int, DataPoint )
decodeLogLine =
    Json.Decode.map2
        Tuple.pair
        (Json.Decode.field "epoch" Json.Decode.int
            |> Json.Decode.map (\epoch -> epoch * 1000)
         -- This gets us milliseconds
        )
        (Json.Decode.map3
            DataPoint
            (Json.Decode.field "cpuPctUsed" Json.Decode.int)
            (Json.Decode.field "memPctUsed" Json.Decode.int)
            (Json.Decode.field "rootfsPctUsed" Json.Decode.int)
        )
