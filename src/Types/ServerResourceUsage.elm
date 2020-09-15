module Types.ServerResourceUsage exposing (ResourceUsageHistory, ResourceUsageRecord, emptyResourceUsageHistory)

import Dict
import Time


type alias ResourceUsageHistory =
    Dict.Dict Time.Posix ResourceUsageRecord


emptyResourceUsageHistory : ResourceUsageHistory
emptyResourceUsageHistory =
    Dict.empty


type alias ResourceUsageRecord =
    { cpuPctUsed : Int
    , memPctUsed : Int
    , rootfsPctUsed : Int
    }
