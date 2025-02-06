module Tests.Helpers.Helpers exposing (hostnameSuite, stringIsUuidOrDefaultSuite)

-- Test related Modules
-- Exosphere Modules Under Test

import Expect
import Helpers.Helpers as Helpers
import Test exposing (Test, describe, test)


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
