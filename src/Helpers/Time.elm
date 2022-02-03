module Helpers.Time exposing (humanReadableDateAndTime, iso8601StringToPosix)

import ISO8601
import Time


humanReadableDateAndTime : Time.Posix -> String
humanReadableDateAndTime posix =
    let
        monthToStr month =
            case month of
                Time.Jan ->
                    "01"

                Time.Feb ->
                    "02"

                Time.Mar ->
                    "03"

                Time.Apr ->
                    "04"

                Time.May ->
                    "05"

                Time.Jun ->
                    "06"

                Time.Jul ->
                    "07"

                Time.Aug ->
                    "08"

                Time.Sep ->
                    "09"

                Time.Oct ->
                    "10"

                Time.Nov ->
                    "11"

                Time.Dec ->
                    "12"
    in
    [ Time.toYear Time.utc posix |> String.fromInt
    , "-"
    , Time.toMonth Time.utc posix |> monthToStr
    , "-"
    , Time.toDay Time.utc posix |> String.fromInt |> String.padLeft 2 '0'
    , " "
    , Time.toHour Time.utc posix |> String.fromInt |> String.padLeft 2 '0'
    , ":"
    , Time.toMinute Time.utc posix |> String.fromInt |> String.padLeft 2 '0'
    , ":"
    , Time.toSecond Time.utc posix |> String.fromInt |> String.padLeft 2 '0'
    , " UTC"
    ]
        |> String.concat


iso8601StringToPosix : String -> Result String Time.Posix
iso8601StringToPosix str =
    ISO8601.fromString str
        |> Result.map ISO8601.toPosix
