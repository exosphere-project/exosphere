module Helpers.Random exposing (generateServerName)

import Helpers.PetNames as PetNames
import Random
import Random.List as RandomList


randomWord : List String -> String -> Random.Generator String
randomWord wordlist default =
    Random.map
        (Tuple.first >> Maybe.withDefault default)
        (RandomList.choose wordlist)


serverNameGenerator : Random.Generator String
serverNameGenerator =
    let
        randomAdverb =
            randomWord PetNames.adverbs "foo"

        randomAdjective =
            randomWord PetNames.adjectives "bar"

        randomName =
            randomWord PetNames.names "baz"
    in
    Random.map3
        (\adverb adjective name ->
            adverb ++ "_" ++ adjective ++ "_" ++ name
        )
        randomAdverb
        randomAdjective
        randomName


generateServerName : (String -> msg) -> Cmd msg
generateServerName toMsg =
    Random.generate toMsg <|
        serverNameGenerator
