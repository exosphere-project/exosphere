module Helpers.Formatting exposing (Unit(..), humanBytes, humanCount, humanNumber)

import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), Locale)


type Unit
    = Bytes
    | Count
    | GibiBytes
    | MebiBytes


humanBytes : Locale -> Int -> ( String, String )
humanBytes locale nInt =
    let
        n =
            toFloat nInt

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
    ( format
        (if n <= 1.0e9 then
            { locale | decimals = Exact 0 }

         else
            locale
        )
        count
    , unit
    )


humanCount : Locale -> Int -> String
humanCount locale n =
    format { locale | decimals = Exact 0 } (toFloat n)


humanNumber : Locale -> Unit -> Int -> ( String, String )
humanNumber locale unit n =
    case unit of
        Bytes ->
            humanBytes locale n

        Count ->
            ( humanCount locale n, "total" )

        GibiBytes ->
            humanBytes locale (n * 1024 * 1024 * 1024)

        MebiBytes ->
            humanBytes locale (n * 1024 * 1024)
