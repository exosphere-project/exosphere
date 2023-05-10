module Helpers.Units exposing (bytesToGiB)

{-| One GiB equals 1024^3 bytes
-}


bytesToGiB : Int -> Int
bytesToGiB bytes =
    bytes // (1024 ^ 3)
