module Tests.Helpers.Helpers exposing (automaticFloatingIpSuite, exoGuacMetadataSuite, hostnameSuite, stringIsUuidOrDefaultSuite)

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
version 1 must keep decoding, with `tlsSupported` false, so that instances launched before
Exosphere served Guacamole from the instance keep going through the user application proxy.
-}
serverDetailsWithMetadata : List OSTypes.MetadataItem -> OSTypes.ServerDetails
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


exoGuacMetadataSuite : Test
exoGuacMetadataSuite =
    let
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

        decodedPropsOf exoGuacValue =
            case guacamoleStatusOf exoGuacValue of
                Just (GuacTypes.LaunchedWithGuacamole props) ->
                    Just
                        { sshSupported = props.sshSupported
                        , vncSupported = props.vncSupported
                        , tlsSupported = props.tlsSupported
                        , consecutiveIpv6NetworkErrors = props.consecutiveIpv6NetworkErrors
                        }

                _ ->
                    Nothing
    in
    describe "Decoding the exoGuac server metadata item"
        [ test "reads TLS support from a version 2 item" <|
            \_ ->
                decodedPropsOf """{"v":2,"ssh":true,"vnc":true,"tls":true}"""
                    |> Expect.equal
                        (Just
                            { sshSupported = True
                            , vncSupported = True
                            , tlsSupported = True
                            , consecutiveIpv6NetworkErrors = 0
                            }
                        )
        , test "reads a version 2 item that was launched without TLS" <|
            \_ ->
                decodedPropsOf """{"v":2,"ssh":true,"vnc":false,"tls":false}"""
                    |> Expect.equal
                        (Just
                            { sshSupported = True
                            , vncSupported = False
                            , tlsSupported = False
                            , consecutiveIpv6NetworkErrors = 0
                            }
                        )
        , test "treats a version 1 item, which has no tls field, as not supporting TLS" <|
            \_ ->
                decodedPropsOf """{"v":1,"ssh":true,"vnc":true}"""
                    |> Expect.equal
                        (Just
                            { sshSupported = True
                            , vncSupported = True
                            , tlsSupported = False
                            , consecutiveIpv6NetworkErrors = 0
                            }
                        )
        , test "ignores an item it cannot decode" <|
            \_ ->
                guacamoleStatusOf """{"v":2}"""
                    |> Expect.equal (Just GuacTypes.NotLaunchedWithGuacamole)
        ]


{-| The `Automatic` floating IP option spends one only when the instance cannot be reached without
it. A globally routable IPv6 address counts only on an instance that serves Guacamole itself, since
that is what Exosphere opens over IPv6.
-}
automaticFloatingIpSuite : Test
automaticFloatingIpSuite =
    let
        withGuacamoleOverTls =
            serverDetailsWithMetadata
                [ { key = "exoServerVersion", value = "8" }
                , { key = "exoGuac", value = """{"v":2,"ssh":true,"vnc":true,"tls":true}""" }
                ]

        withoutGuacamoleOverTls =
            serverDetailsWithMetadata
                [ { key = "exoServerVersion", value = "8" }
                , { key = "exoGuac", value = """{"v":1,"ssh":true,"vnc":true}""" }
                ]
    in
    describe "Deciding whether an automatic floating IP address is needed"
        [ test "skips it for a public IPv4 address" <|
            \_ ->
                Helpers.automaticSkipsFloatingIp withoutGuacamoleOverTls [ "128.0.0.5" ]
                    |> Expect.equal True
        , test "skips it for a routable IPv6 address on an instance serving Guacamole itself" <|
            \_ ->
                Helpers.automaticSkipsFloatingIp withGuacamoleOverTls [ "192.168.1.20", "2001:db8::1" ]
                    |> Expect.equal True
        , test "spends it for a routable IPv6 address on an instance that goes through the proxy" <|
            \_ ->
                Helpers.automaticSkipsFloatingIp withoutGuacamoleOverTls [ "192.168.1.20", "2001:db8::1" ]
                    |> Expect.equal False
        , test "spends it when the only IPv6 address is link-local" <|
            \_ ->
                Helpers.automaticSkipsFloatingIp withGuacamoleOverTls [ "192.168.1.20", "fe80::f816:3eff:fe1c:2b0a" ]
                    |> Expect.equal False
        , test "spends it when the instance has only a private IPv4 address" <|
            \_ ->
                Helpers.automaticSkipsFloatingIp withGuacamoleOverTls [ "192.168.1.20" ]
                    |> Expect.equal False
        ]
