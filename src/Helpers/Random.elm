module Helpers.Random exposing (generatePassword, generateServerName)

import Helpers.PetNames as PetNames
import Random
import Random.Char as RandomChar
import Random.Extra as RandomExtra
import Random.List as RandomList
import Random.String as RandomString
import Types.Types exposing (..)


generatePassword : (String -> Msg) -> Cmd Msg
generatePassword toMsg =
    Random.generate toMsg (RandomString.string 16 RandomChar.english)


generateServerName : (String -> Msg) -> Cmd Msg
generateServerName toMsg =
    let
        randomWord wordlist default =
            Random.map
                (Tuple.first >> Maybe.withDefault default)
                (RandomList.choose wordlist)

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
