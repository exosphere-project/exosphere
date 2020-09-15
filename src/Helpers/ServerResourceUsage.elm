module Helpers.ServerResourceUsage exposing (parseConsoleLog)

import Types.ServerResourceUsage exposing (ResourceUsageHistory, ResourceUsageRecord)



{- Parses console log for server resource usage -}


parseConsoleLog : String -> ResourceUsageHistory -> ResourceUsageHistory
parseConsoleLog consoleLog resUsgHist =
    -- TODO implement me!
    -- No results in console log should result in a blank resource usage history
    resUsgHist
