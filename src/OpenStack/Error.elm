module OpenStack.Error exposing (decodeSynchronousErrorJson)

import Dict
import Json.Decode as Decode
import OpenStack.Types as OSTypes


decodeSynchronousErrorJson : Decode.Decoder OSTypes.SynchronousAPIError
decodeSynchronousErrorJson =
    let
        decodeMessageAndCode : Decode.Decoder OSTypes.SynchronousAPIError
        decodeMessageAndCode =
            Decode.map2 OSTypes.SynchronousAPIError
                (Decode.field "message" Decode.string)
                (Decode.field "code" Decode.int)
    in
    Decode.dict decodeMessageAndCode
        |> Decode.map Dict.values
        |> Decode.map List.head
        |> Decode.andThen
            (\maybeSynchronousAPIError ->
                case maybeSynchronousAPIError of
                    Just e ->
                        Decode.succeed e

                    Nothing ->
                        Decode.fail "Could not find an error message and code in JSON response"
            )
