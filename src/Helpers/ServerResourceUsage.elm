module Helpers.ServerResourceUsage exposing (getMostRecentDataPoint, parseConsoleLog)

import Dict
import Time
import Types.ServerResourceUsage exposing (DataPoint, History)



{- Parses console log for server resource usage -}


parseConsoleLog : String -> History -> History
parseConsoleLog consoleLog prevHistory =
    -- TODO implement me!
    -- TODO No results in console log should result in a blank resource usage history
    -- TODO Increment pollingStrikes if no new log entries received
    -- TODO when new log entries received, reset pollingStrikes to 0
    prevHistory


getMostRecentDataPoint : Dict.Dict Time.Posix DataPoint -> Maybe ( Time.Posix, DataPoint )
getMostRecentDataPoint timeSeries =
    timeSeries
        |> Dict.toList
        |> List.sortBy
            (\i ->
                i
                    |> Tuple.first
                    |> Time.posixToMillis
            )
        |> List.reverse
        |> List.head
