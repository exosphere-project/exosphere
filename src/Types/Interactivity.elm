module Types.Interactivity exposing
    ( InteractionLevel(..)
    , compare
    , interactionIsWanted
    , maximum
    , minimum
    )

{-| Assumed levels of interaction we can expect for a given resource
-}


type InteractionLevel
    = NoInteraction
    | LowInteraction
    | HighInteraction


{-| Find the maximum of two interaction levels

    maximum NoInteraction LowInteraction
    --> LowInteraction

-}
maximum : InteractionLevel -> InteractionLevel -> InteractionLevel
maximum a b =
    if compare a b == Basics.LT then
        b

    else
        a


{-| Find the minimum of two interaction levels

    minimum NoInteraction LowInteraction
    --> NoInteraction

-}
minimum : InteractionLevel -> InteractionLevel -> InteractionLevel
minimum a b =
    if compare a b == Basics.GT then
        b

    else
        a


{-| Determine if an interaction is wanted for a given level

    interactionIsWanted LowInteraction NoInteraction
    --> False

    interactionIsWanted LowInteraction HighInteraction
    --> True

-}
interactionIsWanted : InteractionLevel -> InteractionLevel -> Bool
interactionIsWanted required current =
    compare current required /= Basics.LT


{-| Custom comparator function for InteractionLevels
-}
compare : InteractionLevel -> InteractionLevel -> Basics.Order
compare left right =
    let
        leftVal =
            toInt left

        rightVal =
            toInt right
    in
    Basics.compare leftVal rightVal


{-| Convert an InteractionLevel to an Int for use in comparisons, because
newtypes cannot be compared directly
-}
toInt : InteractionLevel -> Int
toInt level =
    case level of
        NoInteraction ->
            0

        LowInteraction ->
            1

        HighInteraction ->
            2
