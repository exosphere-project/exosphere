module Helpers.Json exposing (resultToDecoder)

import Json.Decode as Decode


resultToDecoder : Result String a -> Decode.Decoder a
resultToDecoder result =
    case result of
        Result.Ok value ->
            Decode.succeed value

        Result.Err message ->
            Decode.fail message
