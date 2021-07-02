module Types.ServerResourceUsage exposing
    ( Alert
    , AlertLevel(..)
    , DataPoint
    , History
    , TimeSeries
    , emptyResourceUsageHistory
    )

import Dict


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
    -- Here the Int is milliseconds since the epoch (what you would get from Time.posixToMillis)
    Dict.Dict Int DataPoint


type alias DataPoint =
    { cpuPctUsed : Int
    , memPctUsed : Int
    , rootfsPctUsed : Int
    }


type alias Alert =
    { level : AlertLevel
    , text : String
    }


type AlertLevel
    = Info
    | Warn
    | Crit
