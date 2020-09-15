module Types.ServerResourceUsage exposing (DataPoint, History, emptyResourceUsageHistory)

import Dict
import Time


type alias History =
    Dict.Dict Time.Posix DataPoint


emptyResourceUsageHistory : History
emptyResourceUsageHistory =
    Dict.empty


type alias DataPoint =
    { cpuPctUsed : Int
    , memPctUsed : Int
    , rootfsPctUsed : Int
    }
