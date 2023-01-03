module Rest.Naming exposing (generateServerName)


generateServerName : String -> Int -> Int -> String
generateServerName baseName serverCount index =
    if serverCount == 1 then
        baseName

    else
        baseName ++ " " ++ String.fromInt index ++ " of " ++ String.fromInt serverCount
