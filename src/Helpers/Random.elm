module Helpers.Random exposing (generatePassword)

import Random
import Random.Char as RandomChar
import Random.Extra as RandomExtra
import Random.String as RandomString
import Types.Types exposing (..)


generatePassword provider =
    Random.generate (RandomPassword provider) (RandomString.string 16 RandomChar.english)
