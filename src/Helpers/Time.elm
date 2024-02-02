module Helpers.Time exposing (humanReadableDate, humanReadableDateAndTime, iso8601StringToPosix, makeIso8601StringToPosixDecoder, relativeTimeNoAffixes)

import DateFormat.Relative
import ISO8601
import Json.Decode exposing (Decoder, fail, succeed)
import Time


humanReadableDateAndTime : Time.Posix -> String
humanReadableDateAndTime posix =
    [ humanReadableDate posix
    , " "
    , Time.toHour Time.utc posix |> String.fromInt |> String.padLeft 2 '0'
    , ":"
    , Time.toMinute Time.utc posix |> String.fromInt |> String.padLeft 2 '0'
    , ":"
    , Time.toSecond Time.utc posix |> String.fromInt |> String.padLeft 2 '0'
    , " UTC"
    ]
        |> String.concat


humanReadableDate : Time.Posix -> String
humanReadableDate posix =
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
    ]
        |> String.concat


iso8601StringToPosix : String -> Result String Time.Posix
iso8601StringToPosix str =
    ISO8601.fromString str
        |> Result.map ISO8601.toPosix


relativeTimeNoAffixes : Time.Posix -> Time.Posix -> String
relativeTimeNoAffixes start end =
    let
        relativeTimeStr =
            DateFormat.Relative.relativeTime start end

        isNotAffixWord str =
            [ "in", "ago" ]
                |> List.member str
                |> not
    in
    relativeTimeStr
        |> String.words
        |> List.filter isNotAffixWord
        |> String.join " "


makeIso8601StringToPosixDecoder : String -> Decoder Time.Posix
makeIso8601StringToPosixDecoder str =
    case iso8601StringToPosix str of
        Ok posix ->
            succeed posix

        Err error ->
            fail error
