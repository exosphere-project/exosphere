module Types.Jetstream2Accounting exposing (Allocation, Resource, resourceFromStr, resourceToStr)

import Time


type alias Allocation =
    { description : String
    , abstract : String
    , serviceUnitsAllocated : Float
    , serviceUnitsUsed : Maybe Float
    , startDate : Time.Posix
    , endDate : Time.Posix
    , resource : Resource
    }


type Resource
    = CPU
    | GPU
    | LargeMemory
    | Storage


resourceFromStr : String -> Maybe Resource
resourceFromStr str =
    case str of
        "jetstream2.indiana.xsede.org" ->
            Just CPU

        "jetstream2-gpu.indiana.xsede.org" ->
            Just GPU

        "jetstream2-lm.indiana.xsede.org" ->
            Just LargeMemory

        "jetstream2-storage.indiana.xsede.org" ->
            Just Storage

        _ ->
            Nothing


resourceToStr : String -> Resource -> String
resourceToStr instanceWord resource =
    case resource of
        CPU ->
            "CPU " ++ instanceWord

        GPU ->
            "GPU " ++ instanceWord

        LargeMemory ->
            "Lrg Mem " ++ instanceWord

        Storage ->
            "Storage"
