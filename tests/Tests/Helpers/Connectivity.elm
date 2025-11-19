module Tests.Helpers.Connectivity exposing (connectivitySuite)

import Expect
import Helpers.Connectivity exposing (ConnectionEtherType(..), ConnectionPorts(..), ConnectionRemote(..), isConnectionPermitted)
import OpenStack.SecurityGroupRule exposing (SecurityGroupRuleDirection(..), SecurityGroupRuleEthertype(..), SecurityGroupRuleProtocol(..), buildRuleAllowAllOutgoingIPv4, buildRuleSSH, securityGroupRuleTemplateToRule)
import Test exposing (Test, describe, test)


connectivitySuite : Test
connectivitySuite =
    describe "Connectivity Tests"
        [ let
            prototype =
                { ethertype = SomeEtherType
                , direction = Egress
                , protocol = Just ProtocolTcp
                , ports = PortRange 443 443
                , remote = SomeRemote
                , description = Just "Allow HTTPS"
                }

            testCases =
                [ { description = "Outgoing connections are permitted to any remote"
                  , connection = prototype
                  , rules = [ buildRuleAllowAllOutgoingIPv4 |> securityGroupRuleTemplateToRule ]
                  , expected = True
                  }
                , { description = "Outgoing connections are permitted to a specific remote"
                  , connection = prototype
                  , rules =
                        let
                            rule =
                                buildRuleAllowAllOutgoingIPv4 |> securityGroupRuleTemplateToRule
                        in
                        [ { rule | remoteIpPrefix = Just "192.168.1.0/24" } ]
                  , expected = True
                  }
                , { description = "Outgoing HTTPS connections are permitted to a specific remote"
                  , connection = { prototype | ethertype = SpecificEtherType Ipv4 }
                  , rules =
                        let
                            rule =
                                buildRuleAllowAllOutgoingIPv4 |> securityGroupRuleTemplateToRule
                        in
                        [ { rule | remoteIpPrefix = Just "192.168.1.0/24", portRangeMin = Just 443, portRangeMax = Just 443 } ]
                  , expected = True
                  }
                , { description = "Incoming SSH connections are permitted"
                  , connection = { prototype | direction = Ingress, ports = PortRange 22 22 }
                  , rules = [ buildRuleSSH |> securityGroupRuleTemplateToRule ]
                  , expected = True
                  }
                , { description = "Incoming HTTP connections are not permitted"
                  , connection = { prototype | direction = Ingress, ports = PortRange 80 80 }
                  , rules = [ buildRuleSSH |> securityGroupRuleTemplateToRule ]
                  , expected = False
                  }
                , { description = "Outgoing TCP connections are permitted"
                  , connection = { prototype | direction = Egress, ports = AllPorts }
                  , rules = [ buildRuleAllowAllOutgoingIPv4 |> securityGroupRuleTemplateToRule ]
                  , expected = True
                  }
                , { description = "Outgoing TCP connections are required for all remotes"
                  , connection = { prototype | direction = Egress, remote = AllRemotes }
                  , rules = [ buildRuleAllowAllOutgoingIPv4 |> securityGroupRuleTemplateToRule ]
                  , expected = True
                  }
                ]
          in
          describe "isConnectionPermitted"
            (List.map
                (\{ description, connection, rules, expected } ->
                    test description <|
                        \_ ->
                            Expect.equal (isConnectionPermitted connection rules) expected
                )
                testCases
            )
        ]
