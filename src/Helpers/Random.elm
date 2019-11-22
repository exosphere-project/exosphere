module Helpers.Random exposing (generatePasswordAndServerName)

import Helpers.PetNames as PetNames
import Random
import Random.List as RandomList


randomWord : List String -> String -> Random.Generator String
randomWord wordlist default =
    Random.map
        (Tuple.first >> Maybe.withDefault default)
        (RandomList.choose wordlist)


randomPhrase : List String -> List String -> List String -> Random.Generator String
randomPhrase adverbs adjectives names =
    Random.map3
        (\adverb adjective name ->
            adverb ++ "-" ++ adjective ++ "-" ++ name
        )
        (randomWord adverbs "foo")
        (randomWord adjectives "bar")
        (randomWord names "baz")


randomMediumPhrase : Random.Generator String
randomMediumPhrase =
    randomPhrase PetNames.mediumAdverbs PetNames.mediumAdjectives PetNames.mediumNames


passwordGenerator : Random.Generator String
passwordGenerator =
    Random.map2
        (\phrase1 phrase2 ->
            phrase1 ++ "-" ++ phrase2
        )
        randomMediumPhrase
        randomMediumPhrase


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


generatePasswordAndServerName : (( String, String ) -> msg) -> Cmd msg
generatePasswordAndServerName toMsg =
    Random.generate toMsg <|
        Random.map2
            (\password serverName -> ( password, serverName ))
            passwordGenerator
            serverNameGenerator
