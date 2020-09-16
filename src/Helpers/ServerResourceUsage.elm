module Helpers.ServerResourceUsage exposing (getMostRecentDataPoint, parseConsoleLog)

import Dict
import Time
import Types.ServerResourceUsage exposing (DataPoint, History)



{- Parses console log for server resource usage -}


parseConsoleLog : String -> History -> History
parseConsoleLog consoleLog resUsgHist =
    -- TODO implement me!
    -- No results in console log should result in a blank resource usage history
    resUsgHist


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
