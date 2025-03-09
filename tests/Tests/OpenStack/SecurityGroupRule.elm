module Tests.OpenStack.SecurityGroupRule exposing (securityGroupRuleSuite)

-- Test related Modules
-- Exosphere Modules Under Test

import Expect
import OpenStack.SecurityGroupRule exposing (SecurityGroupRuleDirection(..), SecurityGroupRuleEthertype(..), SecurityGroupRuleProtocol(..), matchRuleAndDescription)
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
        ]
