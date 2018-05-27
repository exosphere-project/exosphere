module Tests exposing (..)

-- Test related Modules

import Expect exposing (Expectation)
import Test exposing (..)
import TestData


-- Exosphere Modules Under Test

import Helpers
import Types.Types exposing (Creds)


emptyCreds : Creds
emptyCreds =
    Creds "" "" "" "" "" ""


processOpenRcSuite : Test
processOpenRcSuite =
    describe "end result of processing imported openrc files"
        [ test "ensure an empty file is unmatched" <|
            \() ->
                ""
                    |> Helpers.processOpenRc emptyCreds
                    |> Expect.equal (emptyCreds)
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



{-
   type alias Creds =
       { authUrl : String
       , projectDomain : String
       , projectName : String
       , userDomain : String
       , username : String
       , password : String
       }
-}
