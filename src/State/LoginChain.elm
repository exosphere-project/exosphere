module State.LoginChain exposing (acceptsResponse, addNotice, nextGeneration, popNext, shouldAbandon)


nextGeneration : Int -> Int
nextGeneration generation =
    generation + 1


acceptsResponse : Int -> Int -> Bool
acceptsResponse currentGeneration responseGeneration =
    currentGeneration == responseGeneration


addNotice : String -> List String -> List String
addNotice projectName projectNames =
    if List.member projectName projectNames then
        projectNames

    else
        projectNames ++ [ projectName ]


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
