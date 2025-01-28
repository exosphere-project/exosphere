module Tests exposing
    ( cidrSuite
    , computeQuotasAndLimitsSuite
    , decodeSynchronousOpenStackAPIErrorSuite
    , hostnameSuite
    , imageOSSuite
    , indefiniteArticlesSuite
    , manilaQuotasAndLimitsSuite
    , processOpenRcSuite
    , sshKeySuite
    , stringIsUuidOrDefaultSuite
    , unitsSuite
    , volumeQuotasAndLimitsSuite
    , volumeSnapshotsSuite
    )

-- Test related Modules
-- Exosphere Modules Under Test

import Expect
import FormatNumber.Locales
import Helpers.Cidr exposing (expandIPv6, isValidCidr)
import Helpers.Formatting
import Helpers.Helpers as Helpers
import Helpers.Image
import Helpers.SshKeyTypeGuesser
import Helpers.String
import Helpers.Units
import Json.Decode as Decode
import OpenStack.Error as OSError
import OpenStack.OpenRc
import OpenStack.Quotas
    exposing
        ( computeQuotaDecoder
        , shareQuotaDecoder
        , volumeQuotaDecoder
        )
import OpenStack.SecurityGroupRule exposing (SecurityGroupRuleEthertype(..))
import OpenStack.Types as OSTypes exposing (OpenstackLogin)
import OpenStack.VolumeSnapshots exposing (Status(..), volumeSnapshotDecoder)
import Page.LoginOpenstack
import Test exposing (Test, describe, test)
import TestData
import Time exposing (millisToPosix)


indefiniteArticlesSuite : Test
indefiniteArticlesSuite =
    let
        testData =
            [ { expectedArticle = "an"
              , phrase = "orange"
              , description = "simple word starting with vowel"
              }
            , { expectedArticle = "a"
              , phrase = "fruit"
              , description = "simple word starting with consonant"
              }
            , { expectedArticle = "a"
              , phrase = "TV antenna"
              , description = "acronym starting with consonant sound"
              }
            , { expectedArticle = "an"
              , phrase = "HIV patient"
              , description = "acronym starting with vowel sound"
              }
            , { expectedArticle = "an"
              , phrase = "mRNA vaccine"
              , description = "acronym beginning with a lower-case letter"
              }
            , { expectedArticle = "an"
              , phrase = "R value"
              , description = "first word is just a letter that starts with vowel sound"
              }
            , { expectedArticle = "a"
              , phrase = "U boat"
              , description = "first word is just a letter that starts with consonant sound"
              }
            , { expectedArticle = "a"
              , phrase = "U-boat"
              , description = "hyphenated prefix to first word with consonanty sound"
              }
            , { expectedArticle = "an"
              , phrase = "I-beam"
              , description = "hyphenated prefix to first word with vowely sound"
              }
            , { expectedArticle = "an"
              , phrase = "E.B. White"
              , description = "vowely-sounding initials"
              }
            , { expectedArticle = "a"
              , phrase = "C. Mart"
              , description = "consonanty-sounding initials"
              }
            ]
    in
    describe "Correct indefinite article for various phrases"
        (List.map
            (\item ->
                test item.description <|
                    \_ ->
                        Expect.equal
                            item.expectedArticle
                            (Helpers.String.indefiniteArticle item.phrase)
            )
            testData
        )


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
                            , limit = OSTypes.Limit 48
                            }
                        , instances =
                            { inUse = 1
                            , limit = OSTypes.Limit 10
                            }
                        , ram =
                            { inUse = 1024
                            , limit = OSTypes.Limit 999999
                            }
                        , keypairsLimit = 100
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
                            , limit = OSTypes.Limit 10
                            }
                        , gigabytes =
                            { inUse = 82
                            , limit = OSTypes.Limit 1000
                            }
                        }
                    )
        ]


manilaQuotasAndLimitsSuite : Test
manilaQuotasAndLimitsSuite =
    describe "Decoding share quotas and limits"
        [ test "quota limits" <|
            \_ ->
                Expect.equal
                    (Decode.decodeString shareQuotaDecoder TestData.manilaLimits)
                    (Ok
                        { gigabytes = { inUse = 122, limit = OSTypes.Limit 1000 }
                        , snapshots = { inUse = 0, limit = OSTypes.Limit 50 }
                        , shares = { inUse = 5, limit = OSTypes.Limit 50 }
                        , snapshotGigabytes = { inUse = 0, limit = OSTypes.Limit 1000 }
                        , shareNetworks = Just { inUse = 0, limit = OSTypes.Limit 10 }
                        , shareReplicas = Nothing
                        , shareReplicaGigabytes = Nothing
                        , shareGroups = Nothing
                        , shareGroupSnapshots = Nothing
                        , perShareGigabytes = Nothing
                        }
                    )
        ]


volumeSnapshotsSuite : Test
volumeSnapshotsSuite =
    describe "Decoding volume snapshots"
        [ test "volume snapshots" <|
            \_ ->
                Expect.equal
                    (Decode.decodeString
                        (Decode.field "volume_snapshots" (Decode.list volumeSnapshotDecoder))
                        TestData.cinderVolumeSnapshots
                    )
                    (Ok
                        [ { uuid = "a7b6d2c6-2a1b-4d1d-9b3a-0f3b3d3f0e7d"
                          , name = Just "snapshot-001-with-description"
                          , description = Just "This is a snapshot with a description"
                          , volumeId = "f5d3c3b3-3a1b-4d1d-9b3a-0f3b3d3f0e7d"
                          , sizeInGiB = 1
                          , createdAt = millisToPosix 1425393000000
                          , status = Available
                          }
                        , { uuid = "b7b6d2c6-2a1b-4d1d-9b3a-0f3b3d3f0e7d"
                          , name = Just "snapshot-002-no-description"
                          , description = Nothing
                          , volumeId = "f5d3c3b3-3a1b-4d1d-9b3a-0f3b3d3f0e7d"
                          , sizeInGiB = 1
                          , createdAt = millisToPosix 1425393000000
                          , status = Available
                          }
                        ]
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
        , test "Accepts 'default'" <|
            \_ ->
                Expect.equal True (Helpers.stringIsUuidOrDefault "default")
        , test "Rejects 'Default' (note upper case)" <|
            \_ ->
                Expect.equal False (Helpers.stringIsUuidOrDefault "Default")
        ]


processOpenRcSuite : Test
processOpenRcSuite =
    describe "end result of processing imported openrc files"
        [ test "ensure an empty file is unmatched" <|
            \() ->
                ""
                    |> OpenStack.OpenRc.processOpenRc Page.LoginOpenstack.defaultCreds
                    |> Expect.equal Page.LoginOpenstack.defaultCreds
        , test "that $OS_PASSWORD_INPUT is *not* processed" <|
            \() ->
                """
                export OS_PASSWORD=$OS_PASSWORD_INPUT
                """
                    |> OpenStack.OpenRc.processOpenRc Page.LoginOpenstack.defaultCreds
                    |> .password
                    |> Expect.equal ""
        , test "that double quotes are not included in a processed match" <|
            \() ->
                """
                export OS_AUTH_URL="https://cell.alliance.rebel:5000/v3"
                """
                    |> OpenStack.OpenRc.processOpenRc Page.LoginOpenstack.defaultCreds
                    |> .authUrl
                    |> Expect.equal "https://cell.alliance.rebel:5000/v3"
        , test "that single quotes are accepted but not included in a processed match" <|
            \() ->
                """
                export OS_AUTH_URL='https://cell.alliance.rebel:5000/v3'
                """
                    |> OpenStack.OpenRc.processOpenRc Page.LoginOpenstack.defaultCreds
                    |> .authUrl
                    |> Expect.equal "https://cell.alliance.rebel:5000/v3"
        , test "that quotes are optional" <|
            \() ->
                """
                export OS_AUTH_URL=https://cell.alliance.rebel:5000/v3
                """
                    |> OpenStack.OpenRc.processOpenRc Page.LoginOpenstack.defaultCreds
                    |> .authUrl
                    |> Expect.equal "https://cell.alliance.rebel:5000/v3"
        , test "that mismatched quotes fail to parse" <|
            \() ->
                """
                export OS_AUTH_URL='https://cell.alliance.rebel:5000/v3"
                """
                    |> OpenStack.OpenRc.processOpenRc Page.LoginOpenstack.defaultCreds
                    |> .authUrl
                    |> Expect.equal ""
        , test "ensure pre-'API Version 3' can be processed " <|
            \() ->
                TestData.openrcPreV3
                    |> OpenStack.OpenRc.processOpenRc Page.LoginOpenstack.defaultCreds
                    |> Expect.equal
                        (OpenstackLogin
                            "https://cell.alliance.rebel:35357/v3"
                            "default"
                            "enfysnest"
                            ""
                        )
        , test "ensure an 'API Version 3' open with comments works" <|
            \() ->
                TestData.openrcV3withComments
                    |> OpenStack.OpenRc.processOpenRc Page.LoginOpenstack.defaultCreds
                    |> Expect.equal
                        (OpenstackLogin
                            "https://cell.alliance.rebel:5000/v3"
                            "Default"
                            "enfysnest"
                            ""
                        )
        , test "ensure an 'API Version 3' open _without_ comments works" <|
            \() ->
                TestData.openrcV3
                    |> OpenStack.OpenRc.processOpenRc Page.LoginOpenstack.defaultCreds
                    |> Expect.equal
                        (OpenstackLogin
                            "https://cell.alliance.rebel:5000/v3"
                            "Default"
                            "enfysnest"
                            ""
                        )
        , test "ensure that export keyword is optional" <|
            \() ->
                TestData.openrcNoExportKeyword
                    |> OpenStack.OpenRc.processOpenRc Page.LoginOpenstack.defaultCreds
                    |> Expect.equal
                        (OpenstackLogin
                            "https://mycloud.whatever:5000/v3/"
                            "Default"
                            "redactedusername"
                            "redactedpassword"
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
                        OSError.synchronousErrorJsonDecoder
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
                        OSError.synchronousErrorJsonDecoder
                        """{
                                        "itemNotFound": {
                                          "message": "Instance detailFOOBARBAZ could not be found.",
                                          "code": 404
                                        }
                                      }"""
                    )
        ]


sshKeySuite : Test
sshKeySuite =
    describe "Test SSH key parsing"
        [ test "private key" <|
            \_ ->
                Expect.equal Helpers.SshKeyTypeGuesser.PrivateKey
                    (Helpers.SshKeyTypeGuesser.guessKeyType """-----BEGIN OPENSSH PRIVATE KEY-----
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
-----END OPENSSH PRIVATE KEY-----""")
        , test "public key" <|
            \_ ->
                Expect.equal Helpers.SshKeyTypeGuesser.PublicKey
                    (Helpers.SshKeyTypeGuesser.guessKeyType """ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDUUNdSxvLPTfmpcJoODn6rNshA7ykz6Z8Qlycr3oRo7uVIZKr1GDB5KKh6D3Vvksi2X1uHO1kNZHZBtQxdBnuOa3F9zXN5NOBdOEc1/2WmmCGGbw7a0wRAgkji1nSxpFRPHaxFyKdjDzQgsziInE47c/8/CN8hQeCLx3u/TITAmJmqU2x5SAuurb5SIt81HEqonPi9cYqt1fN12EZPMRbIgfAQc+t0UZiouG8y/gqfVF03iXh+7xGuqDiAtJHzx8eL/EuTFGvNUpIoyEd+k9BNHCtRsFe3DxxANRlckEFWbdHnq+J5OsIAG1LiPwyFLd39BvGBvixCcZeq1JUbEKa70f7Yuhy0YCM+KdhtL/prnX39x7NWqkEhmT+mSWylO7cAPDltp00oj9smkC5Sd/A8OLlJfdGlBvT4O9G/kHzjWPwCnZWsv2+Wgz4B2ifVhm72j91lODluWEaZXaA6udJ6cx4UcS7LSalnFoyvPF8/ITqvMxwMcZFca9f8lEfAYRU= cmart@foobar""")
        , test "public key with the word 'private' in it" <|
            \_ ->
                Expect.equal Helpers.SshKeyTypeGuesser.PublicKey
                    (Helpers.SshKeyTypeGuesser.guessKeyType """ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDUUNdSxvLPTfmpcJoODn6rNshA7ykz6Z8Qlycr3oRo7uVIZKr1GDB5KKh6D3Vvksi2X1uHO1kNZHZBtQxdBnuOa3F9zXN5NOBdOEc1/2WmmCGGbw7a0wRAgkji1nSxpFRPHaxFyKdjDzQgsziInE47c/8/CN8hQeCLx3u/TITAmJmqU2x5SprivateIt81HEqonPi9cYqt1fN12EZPMRbIgfAQc+t0UZiouG8y/gqfVF03iXh+7xGuqDiAtJHzx8eL/EuTFGvNUpIoyEd+k9BNHCtRsFe3DxxANRlckEFWbdHnq+J5OsIAG1LiPwyFLd39BvGBvixCcZeq1JUbEKa70f7Yuhy0YCM+KdhtL/prnX39x7NWqkEhmT+mSWylO7cAPDltp00oj9smkC5Sd/A8OLlJfdGlBvT4O9G/kHzjWPwCnZWsv2+Wgz4B2ifVhm72j91lODluWEaZXaA6udJ6cx4UcS7LSalnFoyvPF8/ITqvMxwMcZFca9f8lEfAYRU= cmart@privateer""")
        , test "not an SSH key at all" <|
            \_ ->
                Expect.equal Helpers.SshKeyTypeGuesser.Unknown
                    (Helpers.SshKeyTypeGuesser.guessKeyType """this is not an ssh key""")
        ]


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


{-| A port of OpenStack Nova's utils.sanitize\_hostname test suite
-}
hostnameSuite : Test
hostnameSuite =
    let
        testCases : List ( String, Maybe String )
        testCases =
            [ ( "的myamazinghostname", Just "myamazinghostname" )
            , ( "....test.example.com...", Just "test-example-com" )
            , ( "----my-amazing-hostname---", Just "my-amazing-hostname" )
            , ( " a b c ", Just "a-b-c" )
            , ( "的hello", Just "hello" )
            , ( "(#@&$!(@*--#&91)(__=+--test-host.example!!.com-0+"
              , Just "91----test-host-example-com-0"
              )
            , ( "<}\u{001F}h\u{0010}e\u{0008}l\u{0002}l\u{0005}o\u{0012}!{>"
              , Just "hello"
              )
            , ( "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
              , Just "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
              )
            , ( "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-a"
              , Just "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
              )
            , ( "的", Nothing )
            , ( "---...", Nothing )
            ]
    in
    describe "Sanitizing hostnames should match Nova's utils.sanitize_hostname" <|
        List.map
            (\( hostname, expect ) ->
                test
                    (hostname ++ " should result in " ++ Maybe.withDefault "Nothing" expect)
                    (\_ -> Expect.equal (Helpers.sanitizeHostname hostname) expect)
            )
            testCases


cidrSuite : Test
cidrSuite =
    describe "CIDR Tests"
        [ let
            testCases =
                [ { description = "Expands address with :: abbreviation"
                  , input = "2001:db8::ff00:42:8329"
                  , expected = Just "2001:0db8:0000:0000:0000:ff00:0042:8329"
                  }
                , { description = "Expands loopback address"
                  , input = "::1"
                  , expected = Just "0000:0000:0000:0000:0000:0000:0000:0001"
                  }
                , { description = "Expands address without leading zeroes"
                  , input = "2001:db8:0:0:0:ff00:42:8329"
                  , expected = Just "2001:0db8:0000:0000:0000:ff00:0042:8329"
                  }
                , { description = "Returns Nothing for too many groups"
                  , input = "2001:db8::ff00:42:8329:1234:5678:90ab"
                  , expected = Nothing
                  }
                , { description = "Returns Nothing for multiple ::"
                  , input = "2001:db8::ff00::8329"
                  , expected = Nothing
                  }
                , { description = "Expands addresses with leading zeros"
                  , input = "2001:0db8::"
                  , expected = Just "2001:0db8:0000:0000:0000:0000:0000:0000"
                  }
                , { description = "Expands address with all zeros"
                  , input = "::"
                  , expected = Just "0000:0000:0000:0000:0000:0000:0000:0000"
                  }
                , { description = "Returns Nothing for junk addresses"
                  , input = "zzzz!"
                  , expected = Nothing
                  }
                ]
          in
          describe "expandIPv6"
            (List.map
                (\{ description, input, expected } ->
                    test description <|
                        \_ ->
                            Expect.equal (expandIPv6 input) expected
                )
                testCases
            )
        , let
            testCases =
                [ { description = "Valid CIDR"
                  , input = "192.168.1.0/24"
                  , expected = True
                  }
                , { description = "Invalid prefix length"
                  , input = "192.168.1.0/33"
                  , expected = False
                  }
                , { description = "Invalid address range"
                  , input = "256.256.256.256/24"
                  , expected = False
                  }
                , { description = "Non-numeric octet"
                  , input = "192.168.one.1/24"
                  , expected = False
                  }
                , { description = "Missing prefix length"
                  , input = "192.168.1.0"
                  , expected = False
                  }
                , { description = "Too many octets"
                  , input = "192.168.1.0.1/24"
                  , expected = False
                  }
                , { description = "Valid any IP"
                  , input = "0.0.0.0/0"
                  , expected = True
                  }
                ]
          in
          describe "isValidCidr for IPv4"
            (List.map
                (\{ description, input, expected } ->
                    test description <|
                        \_ ->
                            Expect.equal (isValidCidr Ipv4 input) expected
                )
                testCases
            )
        , let
            testCases =
                [ { description = "Valid with :: abbreviation"
                  , input = "2001:db8::ff00:42:8329/64"
                  , expected = True
                  }
                , { description = "Valid without :: abbreviation"
                  , input = "2001:0db8:0000:0000:0000:ff00:0042:8329/64"
                  , expected = True
                  }
                , { description = "Invalid prefix length"
                  , input = "2001:db8::ff00:42:8329/129"
                  , expected = False
                  }
                , { description = "Bad hex group"
                  , input = "2001:db8::gggg:42:8329/64"
                  , expected = False
                  }
                , { description = "Invalid with multiple ::"
                  , input = "2001:db8::ff00::8329/64"
                  , expected = False
                  }
                , { description = "Missing prefix length"
                  , input = "2001:db8::ff00:42:8329"
                  , expected = False
                  }
                , { description = "Valid loopback address"
                  , input = "::1/128"
                  , expected = True
                  }
                , { description = "Too many groups"
                  , input = "2001:db8:0:0:0:0:2:1:0/64"
                  , expected = False
                  }
                , { description = "Valid any IP"
                  , input = "::/0"
                  , expected = True
                  }
                , { description = "Invalid trailing :"
                  , input = "2001:0db8:0000:0000:0000:ff00:0042:/64"
                  , expected = False
                  }
                ]
          in
          describe "isValidCidr for IPv6"
            (List.map
                (\{ description, input, expected } ->
                    test description <|
                        \_ ->
                            Expect.equal (isValidCidr Ipv6 input) expected
                )
                testCases
            )
        , test "isValidCidr for Unsupported Ether Types" <|
            \_ -> Expect.equal (isValidCidr (UnsupportedEthertype "IPv7") "192.0.0.127.0.0.1.1") False
        ]


imageOSDetectionSuite : Test
imageOSDetectionSuite =
    let
        dIOS =
            Helpers.Image.detectImageOperatingSystem
    in
    describe "Image operating system detection"
        [ test "helpful name, no metadata" <|
            \_ ->
                -- Is `supported = Nothing` the behavior we want? We definitely support Ubuntu but not sure of the version.
                Expect.equal
                    (dIOS "Featured-Ubuntu24" Nothing Nothing)
                    (Just { distribution = "Ubuntu", version = Nothing, supported = Nothing })
        , test "helpful name and metadata" <|
            \_ ->
                Expect.equal
                    (dIOS "Featured-Ubuntu24" (Just "ubuntu") (Just "24.04"))
                    (Just { distribution = "Ubuntu", version = Just "24.04", supported = Just True })
        , test "JS2 Windows image, no metadata" <|
            \_ ->
                Expect.equal
                    (dIOS "Windows-Server-2022-JS2-Beta" Nothing Nothing)
                    (Just { distribution = "Windows", version = Nothing, supported = Just False })
        , test "explicitly unsupported image with metadata" <|
            \_ ->
                Expect.equal
                    (dIOS "any-image-name-here" (Just "windows") (Just "3.1"))
                    (Just { distribution = "Windows", version = Just "3.1", supported = Just False })
        , test "unknown operating system with metadata" <|
            \_ ->
                Expect.equal
                    (dIOS "any-image-name-here" (Just "NeXTSTEP") (Just "3.3"))
                    (Just { distribution = "NeXTSTEP", version = Just "3.3", supported = Nothing })
        , test "conflicting name and metadata, we should use metadata first" <|
            \_ ->
                Expect.equal
                    (dIOS "Windows-Server-2022-JS2-Beta" (Just "ubuntu") (Just "24.04"))
                    (Just { distribution = "Ubuntu", version = Just "24.04", supported = Just True })
        , test "conflicting name and metadata, other way around" <|
            \_ ->
                Expect.equal
                    (dIOS "Featured-Ubuntu24" (Just "Windows") (Just "95"))
                    (Just { distribution = "Windows", version = Just "95", supported = Just False })
        , test "unhelpful name but with metadata" <|
            \_ ->
                Expect.equal
                    (dIOS "unhelpful-image-name" (Just "ubuntu") (Just "24.04"))
                    (Just { distribution = "Ubuntu", version = Just "24.04", supported = Just True })
        , test "a complete enigma" <|
            \_ ->
                Expect.equal
                    (dIOS "unhelpful-image-name" Nothing Nothing)
                    Nothing

        -- , test "sneaky edge case, will not pass" <|
        --     \_ ->
        --         Expect.equal
        --             (dIOS "not-windows-actually-ubuntu" Nothing Nothing)
        --             (Just { distribution = "Ubuntu", version = Nothing, supported = Nothing })
        ]
