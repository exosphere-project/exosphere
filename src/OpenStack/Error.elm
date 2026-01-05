module OpenStack.Error exposing (ErrorDomain(..), fieldForErrorDomain, synchronousErrorJsonDecoder)

import Dict
import Json.Decode as Decode
import OpenStack.Types as OSTypes


type ErrorDomain
    = NeutronError


fieldForErrorDomain : ErrorDomain -> String
fieldForErrorDomain errorDomain =
    case errorDomain of
        NeutronError ->
            "NeutronError"


synchronousErrorJsonDecoder : Decode.Decoder OSTypes.SynchronousAPIError
synchronousErrorJsonDecoder =
    let
        messageAndCodeDecoder : Decode.Decoder OSTypes.SynchronousAPIError
        messageAndCodeDecoder =
            Decode.map2 OSTypes.SynchronousAPIError
                (Decode.field "message" Decode.string)
                (Decode.field "code" Decode.int)
    in
    Decode.dict messageAndCodeDecoder
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
