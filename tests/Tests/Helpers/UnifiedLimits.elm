module Tests.Helpers.UnifiedLimits exposing (unifiedLimitsSuite)

import Dict
import Expect
import Helpers.UnifiedLimits as UnifiedLimits
import OpenStack.Types as OSTypes
import Test exposing (Test, describe, test)
import Types.HelperTypes as HelperTypes


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
        , describe "flavor custom resource requirements"
            [ test "parses multiple trimmed alias requirements" <|
                \_ ->
                    flavorWithAliasSpec " A100 : 1, NVMe:2 "
                        |> UnifiedLimits.parseFlavorCustomResourceRequirements
                        |> Expect.equal
                            [ Ok { alias = "A100", count = 1 }
                            , Ok { alias = "NVMe", count = 2 }
                            ]
            , test "keeps malformed requirements unresolved" <|
                \_ ->
                    flavorWithAliasSpec "A100:0, :2, malformed, H100:two"
                        |> UnifiedLimits.parseFlavorCustomResourceRequirements
                        |> List.map Result.toMaybe
                        |> Expect.equal
                            [ Nothing, Nothing, Nothing, Nothing ]
            , test "is case insensitive when matching aliases" <|
                \_ ->
                    flavorWithAliasSpec "A100:1,nvme:1,a100:2,NVME:2"
                        |> UnifiedLimits.customResourceRequirementsForFlavor
                            [ customResourceA100
                            , customResourceNVMe
                            ]
                        |> Expect.equal
                            [ Ok { resource = customResourceA100, count = 3 }
                            , Ok { resource = customResourceNVMe, count = 3 }
                            ]
            , test "aggregates requirements that resolve to the same resource" <|
                \_ ->
                    [ Ok { resource = customResourceA100, count = 1 }
                    , Ok { resource = customResourceNVMe, count = 2 }
                    , Ok { resource = customResourceA100, count = 3 }
                    , Err "unresolved"
                    ]
                        |> UnifiedLimits.aggregateCustomResourceRequirements
                        |> Expect.equal
                            [ Ok { resource = customResourceA100, count = 4 }
                            , Ok { resource = customResourceNVMe, count = 2 }
                            , Err "unresolved"
                            ]
            , test "indexes resolved requirements by flavor ID" <|
                \_ ->
                    [ flavorWithAliasSpec "A100:1,NVMe:2" ]
                        |> UnifiedLimits.customResourceRequirementsByFlavor
                            [ customResourceA100
                            , customResourceNVMe
                            ]
                        |> Dict.get "flavor-id"
                        |> Expect.equal
                            (Just
                                [ { resource = customResourceA100, count = 1 }
                                , { resource = customResourceNVMe, count = 2 }
                                ]
                            )
            ]
        , describe "flavorWarningMessages"
            [ test "returns no warnings when the flavor fits its custom resource quotas" <|
                \_ ->
                    UnifiedLimits.flavorWarningMessages
                        Nothing
                        [ { resourceName = customResourceA100.resource
                          , quota =
                                { inUse = 1
                                , limit = OSTypes.Limit 2
                                }
                          }
                        ]
                        [ { resource = customResourceA100, count = 1 } ]
                        |> Expect.equal []
            , test "returns friendly warnings for custom resource quotas the flavor would exceed" <|
                \_ ->
                    UnifiedLimits.flavorWarningMessages
                        Nothing
                        [ { resourceName = customResourceA100.resource
                          , quota =
                                { inUse = 2
                                , limit = OSTypes.Limit 2
                                }
                          }
                        ]
                        [ { resource = customResourceA100, count = 1 } ]
                        |> Expect.equal [ "A100: 3 required, 2/2 in use." ]
            , test "preserves a compute quota warning before custom resource warnings" <|
                \_ ->
                    UnifiedLimits.flavorWarningMessages
                        (Just "Compute quota exceeded.")
                        [ { resourceName = customResourceNVMe.resource
                          , quota =
                                { inUse = 3
                                , limit = OSTypes.Limit 4
                                }
                          }
                        ]
                        [ { resource = customResourceNVMe, count = 2 } ]
                        |> Expect.equal
                            [ "Compute quota exceeded."
                            , "NVMe: 5 required, 3/4 in use."
                            ]
            ]
        ]


flavorWithAliasSpec : String -> OSTypes.Flavor
flavorWithAliasSpec value =
    { id = "flavor-id"
    , name = "flavor-name"
    , description = Nothing
    , vcpu = 1
    , ram_mb = 1024
    , disk_root = 0
    , disk_ephemeral = 0
    , extra_specs = [ OSTypes.MetadataItem "pci_passthrough:alias" value ]
    }


customResourceA100 : HelperTypes.CustomResource
customResourceA100 =
    { resource = "CUSTOM_A100", friendlyName = "A100", alias = Just "A100" }


customResourceNVMe : HelperTypes.CustomResource
customResourceNVMe =
    { resource = "CUSTOM_NVME", friendlyName = "NVMe", alias = Just "NVMe" }
