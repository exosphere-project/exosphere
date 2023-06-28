module Helpers.Formatting exposing (Unit(..), humanBytes, humanCount, humanNumber, humanRatio, usageComparison, usageLabel, usageLabels)

import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), Locale)


type Unit
    = Bytes
    | Count
    | CinderGB
    | GibiBytes
    | MebiBytes


humanBytes : Locale -> Int -> ( String, String )
humanBytes locale byteCount =
    let
        n =
            toFloat byteCount

        ( count, unit ) =
            if n <= 1024 then
                ( n, "B" )

            else if n <= 1024 * 1024 then
                ( n / 1.0e3, "KB" )

            else if n <= 1024 * 1024 * 1024 then
                ( n / 1.0e6, "MB" )

            else if n <= 1024 * 1024 * 1024 * 1024 then
                ( n / 1.0e9, "GB" )

            else if n < 1024 * 1024 * 1024 * 1024 * 1024 then
                ( n / 1.0e12, "TB" )

            else
                ( n / 1.0e15, "PB" )
    in
    ( format { locale | decimals = Max 1 } count, unit )


humanCount : Locale -> Int -> String
humanCount locale n =
    format { locale | decimals = Exact 0 } (toFloat n)


humanRatio : Locale -> Float -> String
humanRatio locale n =
    format { locale | decimals = Exact 2 } n


{-| Transform a number for clearer human comprehension when reading.

The byte conversions here are wrong. They were introduced to
harmonize the UI display of storage quotas with the values
reported by Horizon and should be fixed or removed with a
real solution to the problem of "GB" ambiguity.

@TODO: Add input and output units to this function and handle the
conversion properly once and for all.

-}
humanNumber : Locale -> Unit -> Int -> ( String, String )
humanNumber locale unit n =
    case unit of
        Bytes ->
            humanBytes locale n

        Count ->
            ( humanCount locale n, "total" )

        CinderGB ->
            humanBytes locale (n * 1000000000)

        GibiBytes ->
            humanBytes locale (n * 1000 * 1000 * 1000)

        MebiBytes ->
            humanBytes locale (n * 1000 * 1000)


usageLabels : Locale -> Unit -> Int -> ( String, String )
usageLabels locale unit usage =
    humanNumber { locale | decimals = Exact 0 } unit usage


usageLabel : Locale -> Unit -> Int -> String
usageLabel locale unit usage =
    let
        ( count, label ) =
            usageLabels locale unit usage
    in
    count ++ " " ++ label


usageComparison : Locale -> Unit -> Int -> Int -> String
usageComparison locale unit used total =
    let
        ( usedCount, usedLabel ) =
            usageLabels locale unit used

        ( totalCount, totalLabel ) =
            usageLabels locale unit total
    in
    -- No need to display the units on both numbers if they are the same.
    String.join " "
        (if usedLabel == totalLabel then
            -- 1.3 of 2.0 TB
            [ usedCount, "of", totalCount, totalLabel ]

         else
            -- 743 GB of 2.0 TB
            [ usedCount, usedLabel, "of", totalCount, totalLabel ]
        )
