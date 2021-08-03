module Rest.Guacamole exposing (requestLoginToken)

import Http
import Json.Decode as Decode
import Types.HelperTypes exposing (Url)
import Types.Msg exposing (Msg)


requestLoginToken : Url -> String -> String -> (Result Http.Error String -> Msg) -> Cmd Msg
requestLoginToken url username password resultToMsg =
    Http.request
        { method = "POST"
        , headers = []
        , url = url
        , body = Http.stringBody "text/plain" <| "username=" ++ username ++ "&password=" ++ password
        , expect = Http.expectJson resultToMsg decodeLoginToken
        , timeout = Just 10000
        , tracker = Nothing
        }


decodeLoginToken : Decode.Decoder String
decodeLoginToken =
    Decode.field "authToken" Decode.string
