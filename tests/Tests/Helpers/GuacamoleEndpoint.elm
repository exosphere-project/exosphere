module Tests.Helpers.GuacamoleEndpoint exposing (guacamoleEndpointSuite)

import Expect
import Helpers.GuacamoleEndpoint exposing (Endpoint(..), Instance, buildUrl, guacUpstreamPort, nextEndpointAfterError, resolve)
import Helpers.Url exposing (buildProxyUrl)
import Http
import Test exposing (Test, describe, test)
import Types.Ipv6Reachability exposing (Ipv6Reachability(..))
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
    , ipv6Reachability = Unknown
    }


{-| The same instance on a cloud that has opted into direct connections, launched after it did so.
-}
directInstance : Instance
directInstance =
    { proxiedInstance | directGuacamole = True, tlsSupported = True }


{-| The same instance again, with a routable IPv6 address as well as its floating IP address, which
is the case where the two addresses compete.
-}
dualStackInstance : Instance
dualStackInstance =
    { directInstance | fixedIpAddresses = [ "192.168.1.20", "2001:db8::1" ] }


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
            , test "Keeps using the proxy whatever this browser has learned about IPv6" <|
                \() ->
                    resolve { proxiedInstance | ipv6Reachability = Unreachable }
                        |> Expect.equal (Just (ViaUserAppProxy "proxy.example.com" "10.0.0.5"))
            ]
        , describe "resolve on a cloud with direct connections"
            [ test "Prefers a routable IPv6 address over the floating IP address, which is the scarce one" <|
                \() ->
                    resolve dualStackInstance
                        |> Expect.equal (Just (Direct "2001:db8::1"))
            , test "Goes to the floating IP address when the instance has no routable IPv6 address" <|
                \() ->
                    resolve directInstance
                        |> Expect.equal (Just (Direct "10.0.0.5"))
            , test "Goes to a routable IPv6 address when there is no floating IP address" <|
                \() ->
                    resolve
                        { directInstance
                            | floatingIpAddress = Nothing
                            , fixedIpAddresses = [ "2001:db8::1" ]
                        }
                        |> Expect.equal (Just (Direct "2001:db8::1"))
            , test "Never falls back to the user application proxy" <|
                \() ->
                    resolve
                        { directInstance
                            | floatingIpAddress = Nothing
                            , fixedIpAddresses = [ "192.168.1.20" ]
                            , userAppProxyHostname = Just "proxy.example.com"
                        }
                        |> Expect.equal Nothing
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
            , test "Returns nothing when the instance has no public address of its own" <|
                \() ->
                    resolve { directInstance | floatingIpAddress = Nothing }
                        |> Expect.equal Nothing
            ]
        , describe "resolve once this browser has been found to have no IPv6"
            [ test "Takes the floating IP address instead of the IPv6 address" <|
                \() ->
                    resolve { dualStackInstance | ipv6Reachability = Unreachable }
                        |> Expect.equal (Just (Direct "10.0.0.5"))
            , test "Still tries IPv6 when that is the only address the instance has" <|
                \() ->
                    resolve
                        { dualStackInstance
                            | ipv6Reachability = Unreachable
                            , floatingIpAddress = Nothing
                        }
                        |> Expect.equal (Just (Direct "2001:db8::1"))
            , test "Keeps preferring IPv6 while reachability is still unknown" <|
                \() ->
                    resolve { dualStackInstance | ipv6Reachability = Unknown }
                        |> Expect.equal (Just (Direct "2001:db8::1"))
            , test "Keeps preferring IPv6 once it is known to work" <|
                \() ->
                    resolve { dualStackInstance | ipv6Reachability = Reachable }
                        |> Expect.equal (Just (Direct "2001:db8::1"))
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
        , describe "nextEndpointAfterError"
            [ test "Retries at the floating IP address when the browser could not open a connection over IPv6" <|
                \() ->
                    nextEndpointAfterError Http.NetworkError (Direct "2001:db8::1") (Just "10.0.0.5")
                        |> Expect.equal (Just (Direct "10.0.0.5"))
            , test "Keeps retrying over IPv6 when the instance has no floating IP address to try" <|
                \() ->
                    nextEndpointAfterError Http.NetworkError (Direct "2001:db8::1") Nothing
                        |> Expect.equal Nothing
            , test "Stays put when the instance answered with an error, which says nothing about the address" <|
                \() ->
                    nextEndpointAfterError (Http.BadStatus 403) (Direct "2001:db8::1") (Just "10.0.0.5")
                        |> Expect.equal Nothing
            , test "Stays put on a timeout, which the floating IP address would not fix" <|
                \() ->
                    nextEndpointAfterError Http.Timeout (Direct "2001:db8::1") (Just "10.0.0.5")
                        |> Expect.equal Nothing
            , test "Stays put when the failing endpoint is already the floating IP address" <|
                \() ->
                    nextEndpointAfterError Http.NetworkError (Direct "10.0.0.5") (Just "10.0.0.5")
                        |> Expect.equal Nothing
            , test "Stays put when the request went through the user application proxy" <|
                \() ->
                    nextEndpointAfterError Http.NetworkError (ViaUserAppProxy "proxy.example.com" "10.0.0.5") (Just "10.0.0.5")
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
