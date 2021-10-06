module Helpers.List exposing (multiSortBy)

{-| Sorts a list with multi-level comparison

    multiSortBy [ .isOpen, .rating ] restaurants

-}


multiSortBy : List (a -> comparable) -> List a -> List a
multiSortBy sorters list =
    List.sortWith (multiSortCompare sorters) list


{-| Compare two items with multi-level comparison

    multiSortCompare [ .isOpen, .rating ] restaurantA restuarantB

This function performs a multi-level comparison between two
items given a priority-ordered list of comparators/sorters.

When a given level of comparison returns equality, check with
the next level to try and form an ordering between the elements.
Try each new level on equality until either the ordering is
resolved or we run out of ways to compare the elements.

@see multiSortBy
@internal

-}
multiSortCompare : List (a -> comparable) -> a -> a -> Order
multiSortCompare sorters a b =
    case sorters of
        -- if there are no remaining sorters, it implies that
        -- we have no further way to distinguish the elements
        -- they are, as far as this function cares, equivalent
        [] ->
            EQ

        -- otherwise let's compare them with the next sorter
        -- in the sequence and try to resolve the ordering
        sorter :: remaining ->
            case compare (sorter a) (sorter b) of
                -- recurse if they are still prooving equivalent
                -- at this level of comparison
                EQ ->
                    multiSortCompare remaining a b

                order ->
                    order
