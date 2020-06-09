module Helpers.Time exposing (humanReadableTime, iso8601StringToPosix)

import ISO8601
import Time exposing (..)


humanReadableTime : Posix -> String
humanReadableTime posix =
    let
        monthToStr month =
            case month of
                Jan ->
                    "01"

                Feb ->
                    "02"

                Mar ->
                    "03"

                Apr ->
                    "04"

                May ->
                    "05"

                Jun ->
                    "06"

                Jul ->
                    "07"

                Aug ->
                    "08"

                Sep ->
                    "09"

                Oct ->
                    "10"

                Nov ->
                    "11"

                Dec ->
                    "12"
    in
    [ toYear utc posix |> String.fromInt
    , "-"
    , toMonth utc posix |> monthToStr
    , "-"
    , toDay utc posix |> String.fromInt |> String.padLeft 2 '0'
    , " "
    , toHour utc posix |> String.fromInt |> String.padLeft 2 '0'
    , ":"
    , toMinute utc posix |> String.fromInt |> String.padLeft 2 '0'
    , ":"
    , toSecond utc posix |> String.fromInt |> String.padLeft 2 '0'
    , "."
    , toMillis utc posix |> String.fromInt |> String.padLeft 3 '0'
    , " UTC"
    ]
        |> String.concat


iso8601StringToPosix : String -> Result String Posix
iso8601StringToPosix str =
    ISO8601.fromString str
        |> Result.map ISO8601.toPosix
