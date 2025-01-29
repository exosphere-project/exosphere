module Tests.Helpers.Cidr exposing (cidrSuite)

-- Test related Modules
-- Exosphere Modules Under Test

import Expect
import Helpers.Cidr exposing (expandIPv6, isValidCidr)
import OpenStack.SecurityGroupRule exposing (SecurityGroupRuleEthertype(..))
import Test exposing (Test, describe, test)


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
