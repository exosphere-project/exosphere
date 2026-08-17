module Helpers.Time exposing
    ( hoursToMillis
    , humanReadableDate
    , humanReadableDateAndTime
    , humanReadableDateAndTimeCompact
    , iso8601StringToPosix
    , makeIso8601StringToPosixDecoder
    , minutesToMillis
    , relativeTimeNoAffixes
    )

import DateFormat.Relative
import ISO8601
import Json.Decode exposing (Decoder, fail, succeed)
import Time


minutesToMillis : Int -> Int
minutesToMillis =
    (*) 60000


hoursToMillis : Int -> Int
hoursToMillis =
    (*) 3600000


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


{-| A compact, humanized local date-and-time for at-a-glance reading, e.g.
"Jul 20, 2026 · 10:52 PM". Uses the given zone (so it reads in the viewer's local time) and a
12-hour clock. Distinct from `humanReadableDateAndTime`, which is UTC and ISO-ish for precision.
-}
humanReadableDateAndTimeCompact : Time.Zone -> Time.Posix -> String
humanReadableDateAndTimeCompact zone posix =
    let
        hour24 =
            Time.toHour zone posix

        hour12 =
            case modBy 12 hour24 of
                0 ->
                    12

                h ->
                    h

        meridiem =
            if hour24 < 12 then
                "AM"

            else
                "PM"
    in
    String.concat
        [ monthAbbreviation (Time.toMonth zone posix)
        , " "
        , Time.toDay zone posix |> String.fromInt
        , ", "
        , Time.toYear zone posix |> String.fromInt
        , " · "
        , String.fromInt hour12
        , ":"
        , Time.toMinute zone posix |> String.fromInt |> String.padLeft 2 '0'
        , " "
        , meridiem
        ]


monthAbbreviation : Time.Month -> String
monthAbbreviation month =
    case month of
        Time.Jan ->
            "Jan"

        Time.Feb ->
            "Feb"

        Time.Mar ->
            "Mar"

        Time.Apr ->
            "Apr"

        Time.May ->
            "May"

        Time.Jun ->
            "Jun"

        Time.Jul ->
            "Jul"

        Time.Aug ->
            "Aug"

        Time.Sep ->
            "Sep"

        Time.Oct ->
            "Oct"

        Time.Nov ->
            "Nov"

        Time.Dec ->
            "Dec"


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
