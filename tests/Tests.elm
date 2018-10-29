module Tests exposing (emptyCreds, processOpenRcSuite, stringIsUuidOrDefaultSuite)

-- Test related Modules
-- Exosphere Modules Under Test

import Expect exposing (Expectation)
import Helpers.Helpers as Helpers
import Test exposing (..)
import TestData
import Types.Types exposing (Creds)


emptyCreds : Creds
emptyCreds =
    Creds "" "" "" "" "" ""


stringIsUuidOrDefaultSuite : Test
stringIsUuidOrDefaultSuite =
    describe "The Helpers.stringIsUuidOrDefault function"
        [ test "accepts a valid UUID" <|
            \_ ->
                Expect.equal True (Helpers.stringIsUuidOrDefault "deadbeef-dead-dead-dead-beefbeefbeef")
        , test "accepts a valid UUID with no hyphens" <|
            \_ ->
                Expect.equal True (Helpers.stringIsUuidOrDefault "deadbeefdeaddeaddeadbeefbeefbeef")
        , test "accepts a UUID but with too many hyphens (we are forgiving here?)" <|
            \_ ->
                Expect.equal True (Helpers.stringIsUuidOrDefault "deadbe-ef-dead-dead-dead-beefbeef-bee-f")
        , test "rejects a UUID that is too short" <|
            \_ ->
                Expect.equal False (Helpers.stringIsUuidOrDefault "deadbeef-dead-dead-dead-beefbeefbee")
        , test "rejects a UUID with invalid characters" <|
            \_ ->
                Expect.equal False (Helpers.stringIsUuidOrDefault "deadbeef-dead-dead-dead-beefbeefbees")
        , test "rejects a non-uuid" <|
            \_ ->
                Expect.equal False (Helpers.stringIsUuidOrDefault "gesnodulator")
        , test "Accepts \"default\"" <|
            \_ ->
                Expect.equal True (Helpers.stringIsUuidOrDefault "default")
        , test "Accepts \"Default\" (note upper case)" <|
            \_ ->
                Expect.equal True (Helpers.stringIsUuidOrDefault "Default")
        ]


processOpenRcSuite : Test
processOpenRcSuite =
    describe "end result of processing imported openrc files"
        [ test "ensure an empty file is unmatched" <|
            \() ->
                ""
                    |> Helpers.processOpenRc emptyCreds
                    |> Expect.equal emptyCreds
        , test "that $OS_PASSWORD_INPUT is *not* processed" <|
            \() ->
                """
                export OS_PASSWORD=$OS_PASSWORD_INPUT
                """
                    |> Helpers.processOpenRc emptyCreds
                    |> .password
                    |> Expect.equal ""
        , test "that double quotes are not included in a processed match" <|
            \() ->
                """
                export OS_AUTH_URL="https://cell.alliance.rebel:5000/v3"
                """
                    |> Helpers.processOpenRc emptyCreds
                    |> .authUrl
                    |> Expect.equal "https://cell.alliance.rebel:5000/v3"
        , test "that double quotes are optional" <|
            \() ->
                """
                export OS_AUTH_URL=https://cell.alliance.rebel:5000/v3
                """
                    |> Helpers.processOpenRc emptyCreds
                    |> .authUrl
                    |> Expect.equal "https://cell.alliance.rebel:5000/v3"
        , test "that project domain name is still matched" <|
            \() ->
                """
                # newer OpenStack release seem to use _ID suffix
                export OS_PROJECT_DOMAIN_NAME="super-specific"
                """
                    |> Helpers.processOpenRc emptyCreds
                    |> .projectDomain
                    |> Expect.equal "super-specific"
        , test "that project domain ID is still matched" <|
            \() ->
                """
                # newer OpenStack release seem to use _ID suffix
                export OS_PROJECT_DOMAIN_ID="DEFAULT"
                """
                    |> Helpers.processOpenRc emptyCreds
                    |> .projectDomain
                    |> Expect.equal "DEFAULT"
        , test "ensure pre-'API Version 3' can be processed " <|
            \() ->
                TestData.openrcPreV3
                    |> Helpers.processOpenRc emptyCreds
                    |> Expect.equal
                        (Creds
                            "https://cell.alliance.rebel:35357/v3"
                            "default"
                            "cloud-riders"
                            "default"
                            "enfysnest"
                            ""
                        )
        , test "ensure an 'API Version 3' open with comments works" <|
            \() ->
                TestData.openrcV3withComments
                    |> Helpers.processOpenRc emptyCreds
                    |> Expect.equal
                        (Creds
                            "https://cell.alliance.rebel:5000/v3"
                            "default"
                            "cloud-riders"
                            "Default"
                            "enfysnest"
                            ""
                        )
        , test "ensure an 'API Version 3' open _without_ comments works" <|
            \() ->
                TestData.openrcV3
                    |> Helpers.processOpenRc emptyCreds
                    |> Expect.equal
                        (Creds
                            "https://cell.alliance.rebel:5000/v3"
                            "default"
                            "cloud-riders"
                            "Default"
                            "enfysnest"
                            ""
                        )
        ]
