module Tests.Helpers.UnifiedLimits exposing (unifiedLimitsSuite)

import Expect
import Helpers.UnifiedLimits as UnifiedLimits
import OpenStack.Types as OSTypes
import Test exposing (Test, describe, test)


unifiedLimitsSuite : Test
unifiedLimitsSuite =
    let
        registeredLimits =
            [ { id = "reg-vcpu"
              , regionId = Just "RegionOne"
              , resourceName = OSTypes.LimitResourceName "class:VCPU"
              , defaultLimit = 8
              , description = Nothing
              }
            , { id = "reg-a100x-10c"
              , regionId = Just "RegionOne"
              , resourceName = OSTypes.LimitResourceName "class:CUSTOM_A100X_10C"
              , defaultLimit = 1
              , description = Nothing
              }
            , { id = "reg-a100x-20c"
              , regionId = Just "RegionOne"
              , resourceName = OSTypes.LimitResourceName "class:CUSTOM_A100X_20C"
              , defaultLimit = -1
              , description = Nothing
              }
            , { id = "reg-servers"
              , regionId = Just "RegionOne"
              , resourceName = OSTypes.LimitResourceName "servers"
              , defaultLimit = 10
              , description = Nothing
              }
            ]

        projectLimits =
            [ { id = "proj-vcpu"
              , projectId = "project-id"
              , regionId = Just "RegionOne"
              , resourceName = OSTypes.LimitResourceName "class:VCPU"
              , resourceLimit = 6
              , description = Nothing
              }
            , { id = "proj-a100x-10c"
              , projectId = "project-id"
              , regionId = Just "RegionOne"
              , resourceName = OSTypes.LimitResourceName "class:CUSTOM_A100X_10C"
              , resourceLimit = 2
              , description = Nothing
              }
            ]

        projectUsages =
            [ { resourceName = OSTypes.UsageResourceName "VCPU"
              , resourceUsage = 3
              }
            , { resourceName = OSTypes.UsageResourceName "CUSTOM_A100X_10C"
              , resourceUsage = 1
              }
            ]

        quotas =
            UnifiedLimits.quotasFromUnifiedLimits registeredLimits projectLimits projectUsages
    in
    describe "Helpers.UnifiedLimits"
        [ describe "comparableStringForLimitResourceName"
            [ test "strips the class prefix" <|
                \_ ->
                    Expect.equal
                        (UnifiedLimits.comparableStringForLimitResourceName (OSTypes.LimitResourceName "class:CUSTOM_A100X_10C"))
                        "CUSTOM_A100X_10C"
            , test "leaves non-class resources unchanged" <|
                \_ ->
                    Expect.equal
                        (UnifiedLimits.comparableStringForLimitResourceName (OSTypes.LimitResourceName "servers"))
                        "servers"
            ]
        , describe "quotasFromUnifiedLimits"
            [ test "returns resource names normalized for custom resource config matching" <|
                \_ ->
                    quotas
                        |> List.map .resourceName
                        |> Expect.equal [ "CUSTOM_A100X_10C", "CUSTOM_A100X_20C", "VCPU", "servers" ]
            , test "prefers project limits over registered defaults" <|
                \_ ->
                    quotas
                        |> List.filter (\q -> q.resourceName == "CUSTOM_A100X_10C")
                        |> List.head
                        |> Expect.equal
                            (Just
                                { resourceName = "CUSTOM_A100X_10C"
                                , quota =
                                    { inUse = 1
                                    , limit = OSTypes.Limit 2
                                    }
                                }
                            )
            , test "falls back to registered defaults when no project limit exists" <|
                \_ ->
                    quotas
                        |> List.filter (\q -> q.resourceName == "servers")
                        |> List.head
                        |> Expect.equal
                            (Just
                                { resourceName = "servers"
                                , quota =
                                    { inUse = 0
                                    , limit = OSTypes.Limit 10
                                    }
                                }
                            )
            , test "converts -1 to Unlimited" <|
                \_ ->
                    quotas
                        |> List.filter (\q -> q.resourceName == "CUSTOM_A100X_20C")
                        |> List.head
                        |> Expect.equal
                            (Just
                                { resourceName = "CUSTOM_A100X_20C"
                                , quota =
                                    { inUse = 0
                                    , limit = OSTypes.Unlimited
                                    }
                                }
                            )
            , test "prefers a region-scoped registered limit over a global fallback for the same resource" <|
                \_ ->
                    UnifiedLimits.quotasFromUnifiedLimits
                        [ { id = "global-vcpu"
                          , regionId = Nothing
                          , resourceName = OSTypes.LimitResourceName "class:VCPU"
                          , defaultLimit = 4
                          , description = Nothing
                          }
                        , { id = "regional-vcpu"
                          , regionId = Just "RegionOne"
                          , resourceName = OSTypes.LimitResourceName "class:VCPU"
                          , defaultLimit = 8
                          , description = Nothing
                          }
                        ]
                        []
                        []
                        |> Expect.equal
                            [ { resourceName = "VCPU"
                              , quota =
                                    { inUse = 0
                                    , limit = OSTypes.Limit 8
                                    }
                              }
                            ]
            ]
        ]
