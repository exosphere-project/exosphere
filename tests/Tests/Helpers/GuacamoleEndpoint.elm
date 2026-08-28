module Tests.Helpers.GuacamoleEndpoint exposing (guacamoleEndpointSuite)

import Expect
import Helpers.GuacamoleEndpoint exposing (Endpoint(..), Instance, buildUrl, guacUpstreamPort, resolve)
import Helpers.Url exposing (buildProxyUrl)
import Test exposing (Test, describe, test)
import Url
import Url.Builder


{-| A cloud with a user application proxy and nothing else turned on, and an instance on it with a
floating IP address. Every resolve case below is this with some fields changed, so that each test
reads as the one thing it is about.
-}
proxiedInstance : Instance
proxiedInstance =
    { directGuacamole = False
    , tlsSupported = False
    , userAppProxyHostname = Just "proxy.example.com"
    , floatingIpAddress = Just "10.0.0.5"
    , fixedIpAddresses = [ "192.168.1.20" ]
    }


{-| The same instance on a cloud that has opted into direct connections, launched after it did so.
-}
directInstance : Instance
directInstance =
    { proxiedInstance | directGuacamole = True, tlsSupported = True }


guacamoleEndpointSuite : Test
guacamoleEndpointSuite =
    describe "Guacamole Endpoint Tests"
        [ describe "resolve on a cloud without direct connections"
            [ test "Uses the user application proxy when both the proxy hostname and the floating IP address are known" <|
                \() ->
                    resolve proxiedInstance
                        |> Expect.equal (Just (ViaUserAppProxy "proxy.example.com" "10.0.0.5"))
            , test "Returns nothing without a proxy hostname" <|
                \() ->
                    resolve { proxiedInstance | userAppProxyHostname = Nothing }
                        |> Expect.equal Nothing
            , test "Returns nothing without a floating IP address" <|
                \() ->
                    resolve { proxiedInstance | floatingIpAddress = Nothing }
                        |> Expect.equal Nothing
            , test "Returns nothing without either" <|
                \() ->
                    resolve { proxiedInstance | userAppProxyHostname = Nothing, floatingIpAddress = Nothing }
                        |> Expect.equal Nothing
            , test "Ignores a routable IPv6 address, because the cloud has not opted in" <|
                \() ->
                    resolve
                        { proxiedInstance
                            | userAppProxyHostname = Nothing
                            , floatingIpAddress = Nothing
                            , fixedIpAddresses = [ "2001:db8::1" ]
                        }
                        |> Expect.equal Nothing
            ]
        , describe "resolve on a cloud with direct connections"
            [ test "Goes straight to the floating IP address" <|
                \() ->
                    resolve directInstance
                        |> Expect.equal (Just (Direct "10.0.0.5"))
            , test "Prefers the floating IP address over a routable IPv6 address, because every project has IPv4" <|
                \() ->
                    resolve { directInstance | fixedIpAddresses = [ "2001:db8::1" ] }
                        |> Expect.equal (Just (Direct "10.0.0.5"))
            , test "Falls back to a routable IPv6 address when there is no floating IP address" <|
                \() ->
                    resolve
                        { directInstance
                            | floatingIpAddress = Nothing
                            , fixedIpAddresses = [ "2001:db8::1" ]
                        }
                        |> Expect.equal (Just (Direct "2001:db8::1"))
            , test "Takes the routable IPv6 address in preference to the user application proxy" <|
                \() ->
                    resolve
                        { directInstance
                            | floatingIpAddress = Nothing
                            , fixedIpAddresses = [ "2001:db8::1" ]
                            , userAppProxyHostname = Just "proxy.example.com"
                        }
                        |> Expect.equal (Just (Direct "2001:db8::1"))
            , test "Skips the fixed IPv4 addresses, which a browser cannot reach" <|
                \() ->
                    resolve
                        { directInstance
                            | floatingIpAddress = Nothing
                            , fixedIpAddresses = [ "192.168.1.20" ]
                            , userAppProxyHostname = Nothing
                        }
                        |> Expect.equal Nothing
            , test "Skips a link-local IPv6 address" <|
                \() ->
                    resolve
                        { directInstance
                            | floatingIpAddress = Nothing
                            , fixedIpAddresses = [ "fe80::f816:3eff:fe1c:2b0a" ]
                            , userAppProxyHostname = Nothing
                        }
                        |> Expect.equal Nothing
            , test "Skips a unique-local IPv6 address" <|
                \() ->
                    resolve
                        { directInstance
                            | floatingIpAddress = Nothing
                            , fixedIpAddresses = [ "fd00:1234::5" ]
                            , userAppProxyHostname = Nothing
                        }
                        |> Expect.equal Nothing
            , test "Picks the routable IPv6 address out of a mixed list" <|
                \() ->
                    resolve
                        { directInstance
                            | floatingIpAddress = Nothing
                            , fixedIpAddresses = [ "192.168.1.20", "fe80::f816:3eff:fe1c:2b0a", "2001:db8::1" ]
                        }
                        |> Expect.equal (Just (Direct "2001:db8::1"))
            , test "Returns nothing when the instance has no public address and the cloud has no proxy" <|
                \() ->
                    resolve
                        { directInstance
                            | floatingIpAddress = Nothing
                            , userAppProxyHostname = Nothing
                        }
                        |> Expect.equal Nothing
            , test "Returns nothing when the instance has no public address, proxy or not, because the proxy needs a floating IP address too" <|
                \() ->
                    resolve { directInstance | floatingIpAddress = Nothing }
                        |> Expect.equal Nothing
            ]
        , describe "resolve for an instance launched before the cloud opted in"
            [ test "Uses the user application proxy even though the cloud now allows direct connections" <|
                \() ->
                    resolve { directInstance | tlsSupported = False }
                        |> Expect.equal (Just (ViaUserAppProxy "proxy.example.com" "10.0.0.5"))
            , test "Ignores its routable IPv6 address, because it is not serving TLS" <|
                \() ->
                    resolve
                        { directInstance
                            | tlsSupported = False
                            , floatingIpAddress = Nothing
                            , fixedIpAddresses = [ "2001:db8::1" ]
                        }
                        |> Expect.equal Nothing
            ]
        , describe "buildUrl"
            [ test "Builds the same URL as a direct call to buildProxyUrl" <|
                \() ->
                    let
                        path =
                            [ "guacamole", "#", "client", "c2hlbGwAYwBkZWZhdWx0" ]

                        params =
                            [ Url.Builder.string "token" "abc123" ]
                    in
                    buildUrl (ViaUserAppProxy "proxy.example.com" "10.0.0.5") path params
                        |> Expect.equal
                            (buildProxyUrl "proxy.example.com" "10.0.0.5" guacUpstreamPort Url.Http path params)
            , test "Sends the user application proxy endpoint over http to the Guacamole upstream port" <|
                \() ->
                    buildUrl (ViaUserAppProxy "proxy.example.com" "10.0.0.5") [ "guacamole", "api", "tokens" ] []
                        |> Expect.equal "https://http-10-0-0-5-49528.proxy.example.com/guacamole/api/tokens"
            , test "Sends the direct endpoint to the instance over HTTPS on port 443" <|
                \() ->
                    buildUrl (Direct "2001:db8::1") [ "guacamole", "api", "tokens" ] []
                        |> Expect.equal "https://[2001:db8::1]/guacamole/api/tokens"
            , test "Sends the direct endpoint to an IPv4 address without brackets" <|
                \() ->
                    buildUrl (Direct "10.0.0.5") [ "guacamole", "api", "tokens" ] []
                        |> Expect.equal "https://10.0.0.5/guacamole/api/tokens"
            ]
        , test "Guacamole listens on port 49528" <|
            \() ->
                guacUpstreamPort
                    |> Expect.equal 49528
        ]
