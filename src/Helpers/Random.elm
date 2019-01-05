module Helpers.Random exposing (generatePassword, generateServerName)

import Helpers.PetNames as PetNames
import Random
import Random.Char as RandomChar
import Random.Extra as RandomExtra
import Random.List as RandomList
import Random.String as RandomString


randomWord : List String -> String -> Random.Generator String
randomWord wordlist default =
    Random.map
        (Tuple.first >> Maybe.withDefault default)
        (RandomList.choose wordlist)


generatePassword : (String -> msg) -> Cmd msg
generatePassword toMsg =
    let
        passwordGenerator =
            Random.map5
                (\foo bar baz qux quux ->
                    foo ++ "-" ++ bar ++ "-" ++ baz ++ "-" ++ qux ++ "-" ++ quux
                )
                (randomWord PetNames.mediumNames "foo")
                (randomWord PetNames.mediumNames "bar")
                (randomWord PetNames.mediumNames "baz")
                (randomWord PetNames.mediumNames "qux")
                (randomWord PetNames.names "quux")
    in
    Random.generate toMsg passwordGenerator


generateServerName : (String -> msg) -> Cmd msg
generateServerName toMsg =
    let
        randomAdverb =
            randomWord PetNames.adverbs "foo"

        randomAdjective =
            randomWord PetNames.adjectives "bar"

        randomName =
            randomWord PetNames.names "baz"

        nameGenerator =
            Random.map3
                (\adverb adjective name ->
                    adverb ++ "_" ++ adjective ++ "_" ++ name
                )
                randomAdverb
                randomAdjective
                randomName
    in
    Random.generate toMsg nameGenerator
