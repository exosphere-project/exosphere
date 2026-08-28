module Tests.Helpers.Helpers exposing (exoGuacMetadataSuite, hostnameSuite, stringIsUuidOrDefaultSuite)

import Expect
import Helpers.Helpers as Helpers
import OpenStack.Types as OSTypes
import Test exposing (Test, describe, test)
import Time
import Types.Guacamole as GuacTypes
import Types.Server exposing (ServerOrigin(..))


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


{-| The `exoGuac` server metadata item gained a `tls` field in version 2. Instances launched at
version 1 must keep decoding, with `tlsSupported` false, so that turning direct Guacamole on for a
cloud does not strand instances that were launched before it.
-}
exoGuacMetadataSuite : Test
exoGuacMetadataSuite =
    let
        serverDetailsWithMetadata metadata =
            { openstackStatus = OSTypes.ServerActive
            , created = Time.millisToPosix 0
            , powerState = OSTypes.PowerRunning
            , imageUuid = "8f1c4b8f-3ba9-4a2f-9c3a-1a9f3b6c5d40"
            , flavorId = "1"
            , keypairName = Nothing
            , metadata = metadata
            , userUuid = "b0e9f3f0-2a1c-4c17-9a3a-2c8d9f0a1b23"
            , volumesAttached = []
            , tags = []
            , lockStatus = OSTypes.ServerUnlocked
            , fault = Nothing
            }

        guacamoleStatusOf exoGuacValue =
            serverDetailsWithMetadata
                [ { key = "exoServerVersion", value = "8" }
                , { key = "exoGuac", value = exoGuacValue }
                ]
                |> Helpers.serverOrigin
                |> (\origin ->
                        case origin of
                            ServerFromExo exoOriginProps ->
                                Just exoOriginProps.guacamoleStatus

                            ServerNotFromExo ->
                                Nothing
                   )

        supportFlagsOf exoGuacValue =
            case guacamoleStatusOf exoGuacValue of
                Just (GuacTypes.LaunchedWithGuacamole props) ->
                    Just ( props.sshSupported, props.vncSupported, props.tlsSupported )

                _ ->
                    Nothing
    in
    describe "Decoding the exoGuac server metadata item"
        [ test "reads TLS support from a version 2 item" <|
            \_ ->
                supportFlagsOf """{"v":2,"ssh":true,"vnc":true,"tls":true}"""
                    |> Expect.equal (Just ( True, True, True ))
        , test "reads a version 2 item that was launched without TLS" <|
            \_ ->
                supportFlagsOf """{"v":2,"ssh":true,"vnc":false,"tls":false}"""
                    |> Expect.equal (Just ( True, False, False ))
        , test "treats a version 1 item, which has no tls field, as not supporting TLS" <|
            \_ ->
                supportFlagsOf """{"v":1,"ssh":true,"vnc":true}"""
                    |> Expect.equal (Just ( True, True, False ))
        , test "ignores an item it cannot decode" <|
            \_ ->
                guacamoleStatusOf """{"v":2}"""
                    |> Expect.equal (Just GuacTypes.NotLaunchedWithGuacamole)
        ]
