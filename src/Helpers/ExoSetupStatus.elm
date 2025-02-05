module Helpers.ExoSetupStatus exposing
    ( decodeExoSetupJson
    , encodeMetadataItem
    , exoSetupDecoder
    , exoSetupStatusToStr
    , parseConsoleLogExoSetupStatus
    )

import Json.Decode
import Json.Encode
import Time
import Types.Server exposing (ExoSetupStatus(..))


parseConsoleLogExoSetupStatus : ( ExoSetupStatus, Maybe Time.Posix ) -> String -> Time.Posix -> Time.Posix -> ( ExoSetupStatus, Maybe Time.Posix )
parseConsoleLogExoSetupStatus ( oldExoSetupStatus, oldTimestamp ) consoleLog serverCreatedTime currentTime =
    let
        logLines =
            String.split "\n" consoleLog

        decodedData =
            -- TODO this may do a lot of work and it can easily be made more performant
            logLines
                -- Throw out anything before the start of a JSON object on a given line
                |> List.map
                    (\line ->
                        String.indexes "{" line
                            |> List.head
                            |> Maybe.map (\index -> String.dropLeft index line)
                            |> Maybe.withDefault line
                    )
                -- Throw out anything after the end of a JSON object on a given line
                |> List.map
                    (\line ->
                        String.indexes "}" line
                            |> List.reverse
                            |> List.head
                            |> Maybe.map (\index -> String.left (index + 1) line)
                            |> Maybe.withDefault line
                    )
                |> List.map (Json.Decode.decodeString exoSetupDecoder)
                |> List.filterMap Result.toMaybe

        ( latestStatus, latestTimestamp ) =
            decodedData
                |> List.reverse
                |> List.head
                |> Maybe.withDefault ( oldExoSetupStatus, oldTimestamp )

        nonTerminalStatuses =
            [ ExoSetupWaiting, ExoSetupStarting, ExoSetupRunning ]

        statusIsNonTerminal =
            List.member latestStatus nonTerminalStatuses

        serverTooOldForNonTerminalStatus =
            Time.posixToMillis serverCreatedTime
                -- 2 hours
                + 7200000
                < Time.posixToMillis currentTime
    in
    if statusIsNonTerminal && serverTooOldForNonTerminalStatus then
        ( ExoSetupTimeout, Just currentTime )

    else
        ( latestStatus, latestTimestamp )


exoSetupStatusToStr : ExoSetupStatus -> String
exoSetupStatusToStr status =
    case status of
        ExoSetupWaiting ->
            "waiting"

        ExoSetupStarting ->
            "starting"

        ExoSetupRunning ->
            "running"

        ExoSetupComplete ->
            "complete"

        ExoSetupTimeout ->
            "timeout"

        ExoSetupError ->
            "error"

        ExoSetupUnknown ->
            "unknown"


decodeExoSetupJson : String -> ( ExoSetupStatus, Maybe Time.Posix )
decodeExoSetupJson jsonValue =
    Json.Decode.decodeString exoSetupDecoder jsonValue
        |> Result.withDefault ( strtoExoSetupStatus jsonValue, Nothing )


strtoExoSetupStatus : String -> ExoSetupStatus
strtoExoSetupStatus str =
    case str of
        "waiting" ->
            ExoSetupWaiting

        "starting" ->
            ExoSetupStarting

        "running" ->
            ExoSetupRunning

        "complete" ->
            ExoSetupComplete

        "timeout" ->
            ExoSetupTimeout

        "error" ->
            ExoSetupError

        "unknown" ->
            ExoSetupUnknown

        _ ->
            ExoSetupUnknown


exoSetupDecoder : Json.Decode.Decoder ( ExoSetupStatus, Maybe Time.Posix )
exoSetupDecoder =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.oneOf
            [ -- Field name previously used in console log
              Json.Decode.field "exoSetup" Json.Decode.string
            , -- Field name currently used in console log
              Json.Decode.field "status" Json.Decode.string
            ]
            |> Json.Decode.map (\str -> strtoExoSetupStatus str)
        )
        (Json.Decode.oneOf
            [ Json.Decode.field "epoch" Json.Decode.int
                |> Json.Decode.map Time.millisToPosix
                |> Json.Decode.map Just
            , Json.Decode.succeed Nothing
            ]
        )


encodeMetadataItem : ExoSetupStatus -> Maybe Time.Posix -> String
encodeMetadataItem status epoch =
    Json.Encode.object
        [ ( "status"
          , exoSetupStatusToStr status
                |> Json.Encode.string
          )
        , ( "epoch"
          , epoch
                |> Maybe.map Time.posixToMillis
                |> Maybe.map Json.Encode.int
                |> Maybe.withDefault Json.Encode.null
          )
        ]
        |> Json.Encode.encode 0
