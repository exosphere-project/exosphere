module Types.Jetstream2Accounting exposing
    ( Allocation
    , AllocationStatus(..)
    , Resource
    , resourceFromStr
    , resourceToStr
    , shownAndSortedAllocations
    )

import Time



-- Types


type alias Allocation =
    { description : String
    , abstract : String
    , serviceUnitsAllocated : Float
    , serviceUnitsUsed : Maybe Float
    , startDate : Time.Posix
    , endDate : Time.Posix
    , resource : Resource
    , status : AllocationStatus
    }


type AllocationStatus
    = Active
    | Inactive


type Resource
    = CPU
    | GPU
    | LargeMemory
    | Storage



-- Converting types to/from strings


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



-- Helper functions to display allocations


shownAndSortedAllocations : Time.Posix -> List Allocation -> List Allocation
shownAndSortedAllocations currentTime allocations =
    -- Not showing storage allocation right now, per
    -- https://jetstream-cloud.slack.com/archives/G01GD9MUUHF/p1664901636186179?thread_ts=1664844400.359949&cid=G01GD9MUUHF
    [ CPU, GPU, LargeMemory ]
        |> List.filterMap (shownAllocationForResource currentTime allocations)


shownAllocationForResource : Time.Posix -> List Allocation -> Resource -> Maybe Allocation
shownAllocationForResource currentTime allocations resource =
    let
        allocationsForResource =
            List.filter (\a -> a.resource == resource) allocations

        currentActiveAllocation =
            allocationsForResource
                |> List.filter (\a -> a.status == Active)
                |> List.filter (allocationIsCurrent currentTime)
                |> List.head

        latestEndingAllocation =
            allocationsForResource
                |> List.sortBy (\a -> Time.posixToMillis a.endDate)
                |> List.reverse
                |> List.head

        firstAllocation =
            allocationsForResource |> List.head
    in
    case currentActiveAllocation of
        Just a ->
            Just a

        Nothing ->
            case latestEndingAllocation of
                Just a ->
                    Just a

                Nothing ->
                    firstAllocation


allocationIsCurrent : Time.Posix -> Allocation -> Bool
allocationIsCurrent currentTime allocation =
    (Time.posixToMillis allocation.startDate < Time.posixToMillis currentTime)
        && (Time.posixToMillis currentTime < Time.posixToMillis allocation.endDate)
