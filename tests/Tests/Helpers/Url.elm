module Tests.Helpers.Url exposing (urlSuite)

import Expect
import Helpers.Url exposing (buildDirectUrl)
import Test exposing (Test, describe, test)
import Url.Builder


urlSuite : Test
urlSuite =
    describe "URL Tests"
        [ let
            testCases =
                [ { description = "Leaves an IPv4 address unbracketed"
                  , destinationIp = "10.0.0.5"
                  , port_ = 443
                  , path = [ "guacamole" ]
                  , params = []
                  , expected = "https://10.0.0.5/guacamole"
                  }
                , { description = "Leaves a hostname unbracketed"
                  , destinationIp = "instance.example.com"
                  , port_ = 443
                  , path = [ "guacamole" ]
                  , params = []
                  , expected = "https://instance.example.com/guacamole"
                  }
                , { description = "Brackets an abbreviated IPv6 address"
                  , destinationIp = "2001:db8::1"
                  , port_ = 443
                  , path = [ "guacamole" ]
                  , params = []
                  , expected = "https://[2001:db8::1]/guacamole"
                  }
                , { description = "Brackets a fully expanded IPv6 address"
                  , destinationIp = "2001:0db8:0000:0000:0000:ff00:0042:8329"
                  , port_ = 443
                  , path = [ "guacamole" ]
                  , params = []
                  , expected = "https://[2001:0db8:0000:0000:0000:ff00:0042:8329]/guacamole"
                  }
                , { description = "Omits port 443"
                  , destinationIp = "10.0.0.5"
                  , port_ = 443
                  , path = [ "guacamole", "api", "tokens" ]
                  , params = []
                  , expected = "https://10.0.0.5/guacamole/api/tokens"
                  }
                , { description = "Includes a non-default port for IPv4"
                  , destinationIp = "10.0.0.5"
                  , port_ = 8443
                  , path = [ "guacamole" ]
                  , params = []
                  , expected = "https://10.0.0.5:8443/guacamole"
                  }
                , { description = "Includes a non-default port after the IPv6 brackets"
                  , destinationIp = "2001:db8::1"
                  , port_ = 8443
                  , path = [ "guacamole" ]
                  , params = []
                  , expected = "https://[2001:db8::1]:8443/guacamole"
                  }
                , { description = "Appends query parameters"
                  , destinationIp = "2001:db8::1"
                  , port_ = 443
                  , path = [ "guacamole" ]
                  , params = [ Url.Builder.string "token" "abc123" ]
                  , expected = "https://[2001:db8::1]/guacamole?token=abc123"
                  }
                , { description = "Passes path segments through untouched, so a Guacamole client fragment survives"
                  , destinationIp = "10.0.0.5"
                  , port_ = 443
                  , path = [ "guacamole", "#", "client", "c2hlbGwAYwBkZWZhdWx0" ]
                  , params = []
                  , expected = "https://10.0.0.5/guacamole/#/client/c2hlbGwAYwBkZWZhdWx0"
                  }
                ]
          in
          describe "buildDirectUrl" <|
            List.map
                (\testCase ->
                    test testCase.description <|
                        \() ->
                            buildDirectUrl testCase.destinationIp testCase.port_ testCase.path testCase.params
                                |> Expect.equal testCase.expected
                )
                testCases
        ]
