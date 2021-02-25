module Tests exposing
    ( computeQuotasAndLimitsSuite
    , decodeSynchronousOpenStackAPIErrorSuite
    , glanceImageAtmoIncludeSuite
    , processOpenRcSuite
    , stringIsUuidOrDefaultSuite
    , volumeQuotasAndLimitsSuite
    )

-- Test related Modules
-- Exosphere Modules Under Test

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Helpers.Helpers as Helpers
import Json.Decode as Decode
import OpenStack.Error as OSError
import OpenStack.Quotas
    exposing
        ( computeQuotaDecoder
        , volumeQuotaDecoder
        )
import OpenStack.Types as OSTypes
    exposing
        ( ComputeQuota
        , OpenstackLogin
        , VolumeQuota
        )
import State.Auth
import Test exposing (..)
import TestData
import Types.Defaults as Defaults


computeQuotasAndLimitsSuite : Test
computeQuotasAndLimitsSuite =
    describe "Decoding compute quotas and limits"
        [ test "compute limits" <|
            \_ ->
                Expect.equal
                    (Decode.decodeString
                        (Decode.field "limits" computeQuotaDecoder)
                        TestData.novaLimits
                    )
                    (Ok
                        { cores =
                            { inUse = 1
                            , limit = Just 48
                            }
                        , instances =
                            { inUse = 1
                            , limit = Just 10
                            }
                        , ram =
                            { inUse = 1024
                            , limit = Just 999999
                            }
                        }
                    )
        ]


volumeQuotasAndLimitsSuite : Test
volumeQuotasAndLimitsSuite =
    describe "Decoding volume quotas and limits"
        [ test "volume limits" <|
            \_ ->
                Expect.equal
                    (Decode.decodeString
                        (Decode.field "limits" volumeQuotaDecoder)
                        TestData.cinderLimits
                    )
                    (Ok
                        { volumes =
                            { inUse = 5
                            , limit = Just 10
                            }
                        , gigabytes =
                            { inUse = 82
                            , limit = Just 1000
                            }
                        }
                    )
        ]


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
        , test "Rejects \"Default\" (note upper case)" <|
            \_ ->
                Expect.equal False (Helpers.stringIsUuidOrDefault "Default")
        ]


processOpenRcSuite : Test
processOpenRcSuite =
    describe "end result of processing imported openrc files"
        [ test "ensure an empty file is unmatched" <|
            \() ->
                ""
                    |> State.Auth.processOpenRc Defaults.openstackCreds
                    |> Expect.equal Defaults.openstackCreds
        , test "that $OS_PASSWORD_INPUT is *not* processed" <|
            \() ->
                """
                export OS_PASSWORD=$OS_PASSWORD_INPUT
                """
                    |> State.Auth.processOpenRc Defaults.openstackCreds
                    |> .password
                    |> Expect.equal ""
        , test "that double quotes are not included in a processed match" <|
            \() ->
                """
                export OS_AUTH_URL="https://cell.alliance.rebel:5000/v3"
                """
                    |> State.Auth.processOpenRc Defaults.openstackCreds
                    |> .authUrl
                    |> Expect.equal "https://cell.alliance.rebel:5000/v3"
        , test "that double quotes are optional" <|
            \() ->
                """
                export OS_AUTH_URL=https://cell.alliance.rebel:5000/v3
                """
                    |> State.Auth.processOpenRc Defaults.openstackCreds
                    |> .authUrl
                    |> Expect.equal "https://cell.alliance.rebel:5000/v3"
        , test "that project domain name is still matched" <|
            \() ->
                """
                # newer OpenStack release seem to use _ID suffix
                export OS_PROJECT_DOMAIN_NAME="super-specific"
                """
                    |> State.Auth.processOpenRc Defaults.openstackCreds
                    |> .projectDomain
                    |> Expect.equal "super-specific"
        , test "that project domain ID is still matched" <|
            \() ->
                """
                # newer OpenStack release seem to use _ID suffix
                export OS_PROJECT_DOMAIN_ID="DEFAULT"
                """
                    |> State.Auth.processOpenRc Defaults.openstackCreds
                    |> .projectDomain
                    |> Expect.equal "DEFAULT"
        , test "ensure pre-'API Version 3' can be processed " <|
            \() ->
                TestData.openrcPreV3
                    |> State.Auth.processOpenRc Defaults.openstackCreds
                    |> Expect.equal
                        (OpenstackLogin
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
                    |> State.Auth.processOpenRc Defaults.openstackCreds
                    |> Expect.equal
                        (OpenstackLogin
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
                    |> State.Auth.processOpenRc Defaults.openstackCreds
                    |> Expect.equal
                        (OpenstackLogin
                            "https://cell.alliance.rebel:5000/v3"
                            "default"
                            "cloud-riders"
                            "Default"
                            "enfysnest"
                            ""
                        )
        ]


decodeSynchronousOpenStackAPIErrorSuite : Test
decodeSynchronousOpenStackAPIErrorSuite =
    describe "Try decoding JSON body of error messages from OpenStack API"
        [ test "Decode invalid API microversion" <|
            \_ ->
                Expect.equal
                    (Ok <|
                        OSTypes.SynchronousAPIError
                            "Version 4.87 is not supported by the API. Minimum is 2.1 and maximum is 2.65."
                            406
                    )
                    (Decode.decodeString
                        OSError.decodeSynchronousErrorJson
                        """{
                           "computeFault": {
                             "message": "Version 4.87 is not supported by the API. Minimum is 2.1 and maximum is 2.65.",
                             "code": 406
                           }
                         }"""
                    )
        , test "Decode invalid Nova URL" <|
            \_ ->
                Expect.equal
                    (Ok <|
                        OSTypes.SynchronousAPIError
                            "Instance detailFOOBARBAZ could not be found."
                            404
                    )
                    (Decode.decodeString
                        OSError.decodeSynchronousErrorJson
                        """{
                                        "itemNotFound": {
                                          "message": "Instance detailFOOBARBAZ could not be found.",
                                          "code": 404
                                        }
                                      }"""
                    )
        ]


glanceImageAtmoIncludeSuite : Test
glanceImageAtmoIncludeSuite =
    describe "Decoding Atmosphere image records"
        [ test "atmosphere image, key-value pairs" <|
            \_ ->
                Expect.equal
                    (Decode.decodeString
                        (Decode.keyValuePairs Decode.string)
                        TestData.glanceImageAtmoIncludeBare
                    )
                    (Ok
                        [ ( "atmo_image_exclude", "false" )
                        , ( "atmo_image_include", "true" )
                        ]
                    )
        , test "atmosphere image, dict" <|
            \_ ->
                Expect.equal
                    (Decode.decodeString
                        (Decode.dict Decode.string)
                        TestData.glanceImageAtmoIncludeBare
                    )
                    (Ok
                        (Dict.fromList
                            [ ( "atmo_image_exclude", "false" )
                            , ( "atmo_image_include", "true" )
                            ]
                        )
                    )
        , test "atmosphere image, dict, maybe" <|
            \_ ->
                Expect.equal
                    (Decode.decodeString
                        (Decode.dict Decode.string)
                        TestData.glanceImageAtmoIncludeBare
                        |> Result.map (Dict.get "atmo_image_exclude")
                    )
                    (Ok
                        (Just "false")
                    )
        , test "atmosphere image, dict, decode bool, first iteration" <|
            \_ ->
                let
                    justTrue : Dict String String -> Bool
                    justTrue someDict =
                        True

                    excludeDec : Decode.Decoder Bool
                    excludeDec =
                        Decode.map justTrue (Decode.dict Decode.string)
                in
                Expect.equal
                    (Decode.decodeString
                        excludeDec
                        TestData.glanceImageAtmoIncludeBare
                    )
                    (Ok
                        True
                    )
        , test "atmosphere image, dict, decode bool, second iteration" <|
            \_ ->
                let
                    maybeTrue : Dict String String -> Bool
                    maybeTrue someDict =
                        Dict.get "atmo_image_exclude" someDict
                            |> Maybe.map (\x -> x == "false")
                            |> Maybe.withDefault False

                    excludeDec : Decode.Decoder Bool
                    excludeDec =
                        Decode.map maybeTrue (Decode.dict Decode.string)
                in
                Expect.equal
                    (Decode.decodeString
                        excludeDec
                        TestData.glanceImageAtmoIncludeBare
                    )
                    (Ok
                        True
                    )
        , test "atmosphere image, dict, decode bool, third iteration" <|
            \_ ->
                let
                    maybeTrue : String -> String -> Dict String String -> Bool
                    maybeTrue expectedKey expectedValue someDict =
                        Dict.get expectedKey someDict
                            |> Maybe.map (\x -> x == expectedValue)
                            |> Maybe.withDefault False

                    excludeDec : Decode.Decoder Bool
                    excludeDec =
                        Decode.map (maybeTrue "atmo_image_exclude" "false") (Decode.dict Decode.string)
                in
                Expect.equal
                    (Decode.decodeString
                        excludeDec
                        TestData.glanceImageAtmoIncludeBare
                    )
                    (Ok
                        True
                    )
        , test "atmosphere image, dict, decode bool, forth iteration" <|
            \_ ->
                let
                    trueIfKeyExistsAndValueAsExpected : String -> String -> Dict String String -> Bool
                    trueIfKeyExistsAndValueAsExpected expectedKey expectedValue someDict =
                        Dict.get expectedKey someDict
                            |> Maybe.map (\x -> x == expectedValue)
                            |> Maybe.withDefault False

                    filterOutImageBasedOnAttribute : Decode.Decoder Bool
                    filterOutImageBasedOnAttribute =
                        Decode.dict Decode.string
                            |> Decode.map (trueIfKeyExistsAndValueAsExpected "atmo_image_exclude" "false")
                in
                Expect.equal
                    (Decode.decodeString
                        filterOutImageBasedOnAttribute
                        TestData.glanceImageAtmoIncludeBare
                    )
                    (Ok
                        True
                    )
        , test "atmosphere image, dict, decode bool, fifth iteration" <|
            \_ ->
                let
                    trueIfKeyExistsAndValueAsExpected : String -> String -> Dict String String -> Bool
                    trueIfKeyExistsAndValueAsExpected expectedKey expectedValue someDict =
                        Dict.get expectedKey someDict
                            |> Maybe.map (\x -> x == expectedValue)
                            |> Maybe.withDefault False

                    dropItemBasedOnAttribute : String -> String -> Decode.Decoder Bool
                    dropItemBasedOnAttribute expectedKey expectedValue =
                        Decode.dict Decode.string
                            |> Decode.map
                                (\someDict ->
                                    Dict.get expectedKey someDict
                                        |> Maybe.map (\x -> x == expectedValue)
                                        |> Maybe.withDefault False
                                )
                in
                Expect.equal
                    (Decode.decodeString
                        (dropItemBasedOnAttribute "atmo_image_exclude" "false")
                        TestData.glanceImageAtmoIncludeBare
                    )
                    (Ok
                        True
                    )
        ]
