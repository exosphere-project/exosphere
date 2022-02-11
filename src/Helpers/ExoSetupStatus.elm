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
                |> List.map Result.toMaybe
                |> List.filterMap identity

        ( latestStatus, latestTimestamp ) =
            decodedData
                |> List.reverse
                |> List.head
                |> Maybe.withDefault ( oldExoSetupStatus, oldTimestamp )

        nonTerminalStatuses =
            [ ExoSetupWaiting, ExoSetupRunning ]

        statusIsNonTerminal =
            List.member latestStatus nonTerminalStatuses

        serverTooOldForNonTerminalStatus =
            Time.posixToMillis serverCreatedTime
                -- 2 hours
                + 7200000
                < Time.posixToMillis currentTime

        finalStatus =
            if statusIsNonTerminal && serverTooOldForNonTerminalStatus then
                ( ExoSetupTimeout, Just currentTime )

            else
                ( latestStatus, latestTimestamp )
    in
    finalStatus


exoSetupStatusToStr : ExoSetupStatus -> String
exoSetupStatusToStr status =
    case status of
        ExoSetupWaiting ->
            "waiting"

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
        |> Result.withDefault ( ExoSetupUnknown, Nothing )


exoSetupDecoder : Json.Decode.Decoder ( ExoSetupStatus, Maybe Time.Posix )
exoSetupDecoder =
    let
        strtoExoSetupStatus str =
            case str of
                "waiting" ->
                    Json.Decode.succeed ExoSetupWaiting

                "running" ->
                    Json.Decode.succeed ExoSetupRunning

                "complete" ->
                    Json.Decode.succeed ExoSetupComplete

                "timeout" ->
                    Json.Decode.succeed ExoSetupTimeout

                "error" ->
                    Json.Decode.succeed ExoSetupError

                "unknown" ->
                    Json.Decode.succeed ExoSetupUnknown

                _ ->
                    Json.Decode.fail "no matching string"
    in
    Json.Decode.oneOf
        [ -- as currently used in both server console log and server metadata
          Json.Decode.map2 Tuple.pair
            (Json.Decode.field "exoSetup" Json.Decode.string
                |> Json.Decode.andThen strtoExoSetupStatus
            )
            (Json.Decode.oneOf
                [ Json.Decode.field "epoch" Json.Decode.int
                    |> Json.Decode.map Time.millisToPosix
                    |> Json.Decode.map Just
                , Json.Decode.succeed Nothing
                ]
            )
        , -- as previously used in server metadata only
          Json.Decode.string
            |> Json.Decode.andThen strtoExoSetupStatus
            |> Json.Decode.map (\status -> ( status, Nothing ))
        ]


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
