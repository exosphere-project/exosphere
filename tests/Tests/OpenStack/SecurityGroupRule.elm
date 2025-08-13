module Tests.OpenStack.SecurityGroupRule exposing (securityGroupRuleSuite)

import Expect
import OpenStack.SecurityGroupRule exposing (SecurityGroupRuleDirection(..), SecurityGroupRuleEthertype(..), SecurityGroupRuleProtocol(..), buildRuleAllowAllOutgoingIPv4, isRuleShadowed, matchRuleAndDescription, remoteSubsumedBy, securityGroupRuleTemplateToRule)
import Test exposing (Test, describe, test)


securityGroupRuleSuite : Test
securityGroupRuleSuite =
    describe "Security Group Rule Tests"
        [ let
            prototype =
                { uuid = ""
                , ethertype = Ipv4
                , direction = Ingress
                , protocol = Just ProtocolTcp
                , portRangeMin = Just 22
                , portRangeMax = Just 22
                , remoteIpPrefix = Nothing
                , remoteGroupUuid = Nothing
                , description = Nothing
                }

            testCases =
                [ { description = "Blank descriptions do not impact rule matching"
                  , ruleA = { prototype | description = Just "" }
                  , ruleB = { prototype | description = Nothing }
                  , expected = True
                  }
                , { description = "The content of descriptions must match"
                  , ruleA = { prototype | description = Just "SSH" }
                  , ruleB = { prototype | description = Just "ssh" }
                  , expected = False
                  }
                , { description = "Description comparison ignores extra spaces"
                  , ruleA = { prototype | description = Just "SSH   " }
                  , ruleB = { prototype | description = Just "   SSH" }
                  , expected = True
                  }
                ]
          in
          describe "matchRuleAndDescription"
            (List.map
                (\{ description, ruleA, ruleB, expected } ->
                    test description <|
                        \_ ->
                            Expect.equal (matchRuleAndDescription ruleA ruleB) expected
                )
                testCases
            )
        , let
            prototype =
                buildRuleAllowAllOutgoingIPv4 |> securityGroupRuleTemplateToRule

            testCases =
                [ { description = "Matching remotes subsume each other"
                  , ruleA = prototype
                  , ruleB = prototype
                  , expected = True
                  }
                , { description = "A more specific remote is subsumed by a broader one"
                  , ruleA = { prototype | remoteIpPrefix = Just "192.168.1.0/24" }
                  , ruleB = prototype
                  , expected = True
                  }
                , { description = "A broader remote is not subsumed by a more specific one"
                  , ruleA = prototype
                  , ruleB = { prototype | remoteIpPrefix = Just "192.168.1.0/24" }
                  , expected = False
                  }
                ]
          in
          describe "remoteSubsumedBy"
            (List.map
                (\{ description, ruleA, ruleB, expected } ->
                    test description <|
                        \_ ->
                            Expect.equal (remoteSubsumedBy ruleA ruleB) expected
                )
                testCases
            )
        , let
            prototype =
                buildRuleAllowAllOutgoingIPv4 |> securityGroupRuleTemplateToRule

            testCases =
                [ { description = "A rule cannot shadow itself"
                  , rule = prototype
                  , rules = [ prototype ]
                  , expected = False
                  }
                , { description = "A more specific remote rule is shadowed by a broader one"
                  , rule = { prototype | remoteIpPrefix = Just "192.168.1.0/24", uuid = "689e7ceb-d402-4cca-849c-4d3b921d4a9d" }
                  , rules = [ prototype ]
                  , expected = True
                  }
                , { description = "A broader remote rule is not shadowed by a more specific one"
                  , rule = prototype
                  , rules = [ { prototype | remoteIpPrefix = Just "192.168.1.0/24", uuid = "689e7ceb-d402-4cca-849c-4d3b921d4a9d" } ]
                  , expected = False
                  }
                ]
          in
          describe "isRuleShadowed"
            (List.map
                (\{ description, rule, rules, expected } ->
                    test description <|
                        \_ ->
                            Expect.equal (isRuleShadowed rule rules) expected
                )
                testCases
            )
        ]
