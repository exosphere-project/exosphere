module Rest.Guacamole exposing (requestLoginToken)

import Http
import Json.Decode as Decode
import Types.HelperTypes exposing (Url)
import Types.SharedMsg exposing (SharedMsg)


requestLoginToken : Url -> String -> String -> (Result Http.Error String -> SharedMsg) -> Cmd SharedMsg
requestLoginToken url username passphrase resultToMsg =
    Http.request
        { method = "POST"
        , headers = []
        , url = url
        , body = Http.stringBody "application/x-www-form-urlencoded" <| "username=" ++ username ++ "&password=" ++ passphrase
        , expect = Http.expectJson resultToMsg loginTokenDecoder
        , timeout = Just 10000
        , tracker = Nothing
        }


loginTokenDecoder : Decode.Decoder String
loginTokenDecoder =
    Decode.field "authToken" Decode.string
