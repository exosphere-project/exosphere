module Tests.Helpers.Formatting exposing (unitsSuite)

-- Test related Modules
-- Exosphere Modules Under Test

import Expect
import FormatNumber.Locales
import Helpers.Formatting
import Helpers.Units
import Test exposing (Test, describe, test)


unitsSuite : Test
unitsSuite =
    let
        locale =
            FormatNumber.Locales.base
    in
    describe "Units"
        [ test "bytesToGiB" <|
            \_ -> Expect.equal (Helpers.Units.bytesToGiB 21474836480) 20
        , test "humanBytes 99 B" <|
            \_ -> Expect.equal (Helpers.Formatting.humanBytes locale (99 * (1024 ^ 0))) ( "99", "B" )
        , test "humanBytes 99 KB" <|
            \_ -> Expect.equal (Helpers.Formatting.humanBytes locale (99 * (1024 ^ 1))) ( "99", "KB" )
        , test "humanBytes 99 MB" <|
            \_ -> Expect.equal (Helpers.Formatting.humanBytes locale (99 * (1024 ^ 2))) ( "99", "MB" )
        , test "humanBytes 99 GB" <|
            \_ -> Expect.equal (Helpers.Formatting.humanBytes locale (99 * (1024 ^ 3))) ( "99", "GB" )
        , test "humanBytes 99 TB" <|
            \_ -> Expect.equal (Helpers.Formatting.humanBytes locale (99 * (1024 ^ 4))) ( "99", "TB" )
        , test "humanBytes 99 PB" <|
            \_ -> Expect.equal (Helpers.Formatting.humanBytes locale (99 * (1024 ^ 5))) ( "99", "PB" )
        , test "humanBytes 99 EB" <|
            \_ -> Expect.equal (Helpers.Formatting.humanBytes locale (99 * (1024 ^ 6))) ( "101376", "PB" )
        , test "humanBytes decimals" <|
            \_ -> Expect.equal (Helpers.Formatting.humanBytes locale 910398476) ( "868.2", "MB" )
        ]
