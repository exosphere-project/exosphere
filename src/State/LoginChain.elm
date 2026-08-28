module State.LoginChain exposing (popNext, shouldAbandon)


popNext : List intent -> ( Maybe intent, List intent )
popNext intents =
    case intents of
        first :: remaining ->
            ( Just first, remaining )

        [] ->
            ( Nothing, [] )


shouldAbandon : Bool -> Bool -> Bool
shouldAbandon wasSelectingRegions willSelectRegions =
    wasSelectingRegions && not willSelectRegions
