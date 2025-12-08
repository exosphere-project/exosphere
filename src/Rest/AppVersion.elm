module Rest.AppVersion exposing (requestAppVersion)

import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Rest.Helpers exposing (expectJsonWithErrorBody)
import Time
import Types.AppVersion exposing (AppVersion)
import Types.Error exposing (ErrorContext, ErrorLevel(..), HttpErrorWithBody)
import Types.SharedMsg exposing (SharedMsg)
import Url.Builder as UB
import View.Types exposing (Context)


errorContext : ErrorContext
errorContext =
    ErrorContext
        "receive app version"
        ErrorInfo
        (Just "Check the contents of your version.json")


buildVersionUrl : Context -> Time.Posix -> String
buildVersionUrl context currentTime =
    let
        file =
            "version.json"

        cacheBustingQueryParam =
            UB.int "t" (Time.posixToMillis currentTime)

        urlNoPathPrefix =
            UB.absolute [ file ] [ cacheBustingQueryParam ]
    in
    case context.urlPathPrefix of
        Nothing ->
            urlNoPathPrefix

        Just "" ->
            urlNoPathPrefix

        Just urlPathPrefix ->
            UB.absolute [ urlPathPrefix, file ] [ cacheBustingQueryParam ]


requestAppVersion : (ErrorContext -> Result HttpErrorWithBody AppVersion -> SharedMsg) -> Context -> Time.Posix -> Cmd SharedMsg
requestAppVersion tagger context currentTime =
    Http.request
        { method = "GET"
        , headers =
            [ -- Use a no-store request directive for good measure.
              -- https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Headers/Cache-Control#request_directives
              Http.header "Cache-Control" "no-store"
            ]
        , url = buildVersionUrl context currentTime
        , body = Http.emptyBody
        , expect =
            expectJsonWithErrorBody
                (tagger errorContext)
                decodeVersion
        , timeout = Nothing
        , tracker = Nothing
        }


decodeVersion : Decode.Decoder AppVersion
decodeVersion =
    Decode.succeed AppVersion
        |> Pipeline.optional "version" (Decode.nullable Decode.string) Nothing
