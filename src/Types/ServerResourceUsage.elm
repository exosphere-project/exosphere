module Types.ServerResourceUsage exposing (DataPoint, History, TimeSeries, emptyResourceUsageHistory)

import Dict
import Time


type alias History =
    { timeSeries : TimeSeries

    -- pollingStrikes indicate when we have polled the console log but not received any new JSON.
    -- This way we can eventually give up on polling if server is not logging resource usage.
    , pollingStrikes : Int
    }


emptyResourceUsageHistory : History
emptyResourceUsageHistory =
    History Dict.empty 0


type alias TimeSeries =
    Dict.Dict Time.Posix DataPoint


type alias DataPoint =
    { cpuPctUsed : Int
    , memPctUsed : Int
    , rootfsPctUsed : Int
    }
