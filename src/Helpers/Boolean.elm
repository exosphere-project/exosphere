module Helpers.Boolean exposing (toMaybe)


toMaybe : a -> Bool -> Maybe a
toMaybe a condition =
    if condition then
        Just a

    else
        Nothing
