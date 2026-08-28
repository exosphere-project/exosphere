module Tests.Helpers.GuacamoleEndpoint exposing (guacamoleEndpointSuite)

import Expect
import Helpers.GuacamoleEndpoint exposing (GuacEndpoint(..), buildUrl, guacUpstreamPort, resolve)
import Helpers.Url exposing (buildDirectUrl, buildProxyUrl)
import Test exposing (Test, describe, test)
import Url
import Url.Builder


guacamoleEndpointSuite : Test
guacamoleEndpointSuite =
    describe "Guacamole Endpoint Tests"
        [ describe "resolve"
            [ test "Returns a user application proxy endpoint when both the proxy hostname and the IP address are known" <|
                \() ->
                    resolve (Just "proxy.example.com") (Just "10.0.0.5")
                        |> Expect.equal (Just (ViaUserAppProxy "proxy.example.com" "10.0.0.5"))
            , test "Returns nothing without a proxy hostname" <|
                \() ->
                    resolve Nothing (Just "10.0.0.5")
                        |> Expect.equal Nothing
            , test "Returns nothing without an IP address" <|
                \() ->
                    resolve (Just "proxy.example.com") Nothing
                        |> Expect.equal Nothing
            , test "Returns nothing without either" <|
                \() ->
                    resolve Nothing Nothing
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
                    let
                        path =
                            [ "guacamole", "api", "tokens" ]
                    in
                    buildUrl (Direct "2001:db8::1") path []
                        |> Expect.equal (buildDirectUrl "2001:db8::1" 443 path [])
            ]
        , test "Guacamole listens on port 49528" <|
            \() ->
                guacUpstreamPort
                    |> Expect.equal 49528
        ]
