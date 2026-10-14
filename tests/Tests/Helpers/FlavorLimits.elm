module Tests.Helpers.FlavorLimits exposing (flavorLimitsSuite)

import Expect
import FormatNumber.Locales exposing (usLocale)
import Helpers.FlavorLimits as FlavorLimits
import OpenStack.Types as OSTypes
import Test exposing (Test, describe, test)
import Types.Defaults
import Types.HelperTypes as HelperTypes


flavorLimitsSuite : Test
flavorLimitsSuite =
    describe "Helpers.FlavorLimits"
        [ test "indexes compute and custom resource warnings by flavor ID" <|
            \_ ->
                let
                    evaluation =
                        FlavorLimits.evaluate completeParams
                in
                Expect.all
                    [ \_ ->
                        FlavorLimits.warningMessagesFor "safe" evaluation
                            |> Expect.equal []
                    , \_ ->
                        FlavorLimits.warningMessagesFor "resource-over" evaluation
                            |> Expect.equal [ "A100: 3 required, 1 / 2 in use." ]
                    , \_ ->
                        FlavorLimits.warningMessagesFor "compute-over" evaluation
                            |> Expect.equal [ "Cores: 4 required, 1 / 3 in use." ]
                    ]
                    ()
        , test "reports compute quota warnings before custom resource warnings" <|
            \_ ->
                let
                    params =
                        completeParams

                    bothOver =
                        flavor "both-over" "A100:2" 3 1024
                in
                FlavorLimits.evaluate { params | flavors = [ bothOver ] }
                    |> FlavorLimits.warningMessagesFor bothOver.id
                    |> Expect.equal
                        [ "Cores: 4 required, 1 / 3 in use."
                        , "A100: 3 required, 1 / 2 in use."
                        ]
        , test "create counts the target flavor as a new instance" <|
            \_ ->
                let
                    targetFlavor =
                        flavor "target" "" 1 1024

                    computeQuota =
                        { cores = { inUse = 10, limit = OSTypes.Unlimited }
                        , instances = { inUse = 1, limit = OSTypes.Limit 1 }
                        , ram = { inUse = 10240, limit = OSTypes.Unlimited }
                        , keypairsLimit = 10
                        }
                in
                computeOnlyEvaluation FlavorLimits.Create computeQuota [ targetFlavor ]
                    |> FlavorLimits.warningMessagesFor targetFlavor.id
                    |> Expect.equal [ "Instances: 2 required, 1 / 1 in use." ]
        , test "describes every compute resource that would exceed its quota" <|
            \_ ->
                let
                    targetFlavor =
                        flavor "target" "" 4 4096

                    computeQuota =
                        { cores = { inUse = 8, limit = OSTypes.Limit 10 }
                        , instances = { inUse = 1, limit = OSTypes.Limit 1 }
                        , ram = { inUse = 8000, limit = OSTypes.Limit 10000 }
                        , keypairsLimit = 10
                        }
                in
                computeOnlyEvaluation FlavorLimits.Create computeQuota [ targetFlavor ]
                    |> FlavorLimits.warningMessagesFor targetFlavor.id
                    |> Expect.equal
                        [ "Cores: 12 required, 8 / 10 in use."
                        , "RAM: 11.8 GB required, 7.8 GB / 9.8 GB in use."
                        , "Instances: 2 required, 1 / 1 in use."
                        ]
        , test "localizes the instance quota resource name" <|
            \_ ->
                let
                    targetFlavor =
                        flavor "target" "" 1 1024

                    computeQuota =
                        { cores = { inUse = 0, limit = OSTypes.Unlimited }
                        , instances = { inUse = 1, limit = OSTypes.Limit 1 }
                        , ram = { inUse = 0, limit = OSTypes.Unlimited }
                        , keypairsLimit = 10
                        }

                    defaultLocalization =
                        Types.Defaults.localization

                    localization =
                        { defaultLocalization | virtualComputer = "server" }
                in
                computeOnlyEvaluationWithLocalization localization FlavorLimits.Create computeQuota [ targetFlavor ]
                    |> FlavorLimits.warningMessagesFor targetFlavor.id
                    |> Expect.equal [ "Servers: 2 required, 1 / 1 in use." ]
        , test "resize computes the quota delta from the current flavor (legacy quota behavior)" <|
            \_ ->
                let
                    currentFlavor =
                        flavor "current" "" 4 4096

                    targetFlavor =
                        flavor "target" "" 6 5632

                    computeQuota =
                        { cores = { inUse = 8, limit = OSTypes.Limit 10 }
                        , instances = { inUse = 1, limit = OSTypes.Limit 1 }
                        , ram = { inUse = 8000, limit = OSTypes.Limit 10000 }
                        , keypairsLimit = 10
                        }

                    evaluation =
                        computeOnlyEvaluation
                            (FlavorLimits.ResizeFrom (Just currentFlavor))
                            computeQuota
                            [ targetFlavor ]
                in
                FlavorLimits.exceedsLimit targetFlavor.id evaluation
                    |> Expect.equal False
        , test "resize evaluates the target flavor's full custom resource requirement" <|
            \_ ->
                let
                    params =
                        completeParams

                    -- Both flavors need one A100, so a delta would consume none.
                    currentFlavor =
                        flavor "current" "A100:1" 1 1024

                    targetFlavor =
                        flavor "target" "A100:1" 1 1024
                in
                FlavorLimits.evaluate
                    { params
                        | computeQuotaOperation = FlavorLimits.ResizeFrom (Just currentFlavor)
                        , flavors = [ targetFlavor ]
                        , projectUsages =
                            Just
                                [ { resourceName = OSTypes.UsageResourceName "CUSTOM_A100"
                                  , resourceUsage = 2
                                  }
                                ]
                    }
                    |> FlavorLimits.warningMessagesFor targetFlavor.id
                    |> Expect.equal [ "A100: 3 required, 2 / 2 in use." ]
        , test "resize still evaluates custom resources when the current flavor is unknown" <|
            \_ ->
                let
                    params =
                        completeParams

                    targetFlavor =
                        flavor "target" "A100:2" 1 1024
                in
                FlavorLimits.evaluate
                    { params
                        | computeQuotaOperation = FlavorLimits.ResizeFrom Nothing
                        , flavors = [ targetFlavor ]
                    }
                    |> FlavorLimits.warningMessagesFor targetFlavor.id
                    |> Expect.equal [ "A100: 3 required, 1 / 2 in use." ]
        , test "resize fails open for compute quota when the current flavor is unknown" <|
            \_ ->
                let
                    targetFlavor =
                        flavor "target" "" 6 5632

                    -- Every compute resource is already at its limit, so
                    -- charging the target flavor as a new server would warn.
                    computeQuota =
                        { cores = { inUse = 10, limit = OSTypes.Limit 10 }
                        , instances = { inUse = 1, limit = OSTypes.Limit 1 }
                        , ram = { inUse = 10000, limit = OSTypes.Limit 10000 }
                        , keypairsLimit = 10
                        }
                in
                computeOnlyEvaluation (FlavorLimits.ResizeFrom Nothing) computeQuota [ targetFlavor ]
                    |> FlavorLimits.warningMessagesFor targetFlavor.id
                    |> Expect.equal []
        , test "fails open for custom resources until all unified limit data is available" <|
            \_ ->
                let
                    params =
                        completeParams
                in
                FlavorLimits.evaluate { params | projectUsages = Nothing }
                    |> FlavorLimits.warningMessagesFor "resource-over"
                    |> Expect.equal []
        , test "reports whether a flavor exceeds any evaluated limit" <|
            \_ ->
                let
                    evaluation =
                        FlavorLimits.evaluate completeParams
                in
                Expect.all
                    [ \_ ->
                        FlavorLimits.exceedsLimit "safe" evaluation
                            |> Expect.equal False
                    , \_ ->
                        FlavorLimits.exceedsLimit "resource-over" evaluation
                            |> Expect.equal True
                    ]
                    ()
        , test "builds selection messages from localized resource names" <|
            \_ ->
                let
                    defaultLocalization =
                        Types.Defaults.localization

                    localization =
                        { defaultLocalization
                            | unitOfTenancy = "workspace"
                            , maxResourcesPerProject = "capacity cap"
                            , virtualComputerHardwareConfig = "flavour"
                        }

                    evaluation =
                        FlavorLimits.evaluate { completeParams | localization = localization }
                in
                Expect.equal
                    { computeWarning = [ "Cores: 4 required, 1 / 3 in use." ]
                    , guidance = "Please select a flavour that does not exceed your workspace's capacity caps."
                    , invalid = "Please select a valid flavour."
                    }
                    { computeWarning = FlavorLimits.warningMessagesFor "compute-over" evaluation
                    , guidance = FlavorLimits.selectionGuidanceMessage localization
                    , invalid = FlavorLimits.invalidSelectionMessage localization
                    }
        , describe "capacities"
            [ test "bounds the count by the tightest limit across every kind of quota" <|
                \_ ->
                    completeCapacityParams
                        |> FlavorLimits.capacities
                        |> FlavorLimits.maxCount
                        |> Expect.equal (Just 3)
            , test "reports every limit that bounds the count, not only the binding one" <|
                \_ ->
                    completeCapacityParams
                        |> FlavorLimits.capacities
                        |> List.map (FlavorLimits.capacityMessage usLocale Types.Defaults.localization)
                        |> Expect.equal
                            [ "Cores: 6 supported, 4 / 10 in use."
                            , "RAM: 5 supported, 5 GB / 10 GB in use."
                            , "Instances: 8 supported, 2 / 10 in use."
                            , "A100: 3 supported, 1 / 4 in use."
                            ]
            , test "counts a custom resource a flavor consumes more than once" <|
                \_ ->
                    { completeCapacityParams | flavor = flavor "greedy" "A100:3" 1 1024 }
                        |> FlavorLimits.capacities
                        |> List.filter (\limit -> String.startsWith "A100" (FlavorLimits.capacityMessage usLocale Types.Defaults.localization limit))
                        |> List.map (FlavorLimits.capacityMessage usLocale Types.Defaults.localization)
                        |> Expect.equal [ "A100: 1 supported, 1 / 4 in use." ]
            , test "ignores volume quotas unless the server is volume backed" <|
                \_ ->
                    Expect.equal
                        { withoutVolume = Just 3
                        , withVolume = Just 2
                        }
                        { withoutVolume =
                            FlavorLimits.capacities completeCapacityParams
                                |> FlavorLimits.maxCount
                        , withVolume =
                            FlavorLimits.capacities { completeCapacityParams | volumeBackedGb = Just 50 }
                                |> FlavorLimits.maxCount
                        }
            , test "describes volume quotas with localized names and units" <|
                \_ ->
                    { completeCapacityParams | volumeBackedGb = Just 50 }
                        |> FlavorLimits.capacities
                        |> List.map (FlavorLimits.capacityMessage usLocale Types.Defaults.localization)
                        |> List.filter (String.contains "olume")
                        |> Expect.equal
                            [ "Volumes: 4 supported, 6 / 10 in use."
                            , "Volume storage: 2 supported, 400 GB / 500 GB in use."
                            ]
            , test "yields no limit when nothing constrains the count" <|
                \_ ->
                    { completeCapacityParams
                        | computeQuota = unlimitedComputeQuota
                        , customResources = []
                        , registeredLimits = Nothing
                        , projectLimits = Nothing
                        , projectUsages = Nothing
                    }
                        |> FlavorLimits.capacities
                        |> FlavorLimits.maxCount
                        |> Expect.equal Nothing
            , test "reports no capacity rather than a negative count when usage exceeds a limit" <|
                \_ ->
                    { completeCapacityParams
                        | computeQuota =
                            { cores = { inUse = 20, limit = OSTypes.Limit 10 }
                            , instances = { inUse = 2, limit = OSTypes.Unlimited }
                            , ram = { inUse = 1024, limit = OSTypes.Unlimited }
                            , keypairsLimit = 10
                            }
                        , customResources = []
                    }
                        |> FlavorLimits.capacities
                        |> FlavorLimits.maxCount
                        |> Expect.equal (Just 0)
            , test "fails open for custom resources until all unified limit data is available" <|
                \_ ->
                    { completeCapacityParams | projectUsages = Nothing }
                        |> FlavorLimits.capacities
                        |> List.map (FlavorLimits.capacityMessage usLocale Types.Defaults.localization)
                        |> Expect.equal
                            [ "Cores: 6 supported, 4 / 10 in use."
                            , "RAM: 5 supported, 5 GB / 10 GB in use."
                            , "Instances: 8 supported, 2 / 10 in use."
                            ]
            ]
        ]


{-| A selection bounded by each kind of limit at once, so that a change to one
kind of limit cannot silently stop being reported.

The flavor consumes 1 vCPU, 1024 MiB, and one A100, against quotas that leave
room for 6 by cores, 5 by RAM, 8 by instances, and 3 by A100 limit.

-}
completeCapacityParams : FlavorLimits.CapacityParams
completeCapacityParams =
    { computeQuota =
        { cores = { inUse = 4, limit = OSTypes.Limit 10 }
        , instances = { inUse = 2, limit = OSTypes.Limit 10 }
        , ram = { inUse = 5120, limit = OSTypes.Limit 10240 }
        , keypairsLimit = 10
        }
    , customResources = completeParams.customResources
    , flavor = flavor "target" "A100:1" 1 1024
    , registeredLimits =
        Just
            [ { id = "reg-a100"
              , regionId = Just "RegionOne"
              , resourceName = OSTypes.LimitResourceName "class:CUSTOM_A100"
              , defaultLimit = 4
              , description = Nothing
              }
            ]
    , projectLimits = Just []
    , projectUsages =
        Just
            [ { resourceName = OSTypes.UsageResourceName "CUSTOM_A100"
              , resourceUsage = 1
              }
            ]
    , volumeBackedGb = Nothing
    , volumeQuota =
        { volumes = { inUse = 6, limit = OSTypes.Limit 10 }
        , gigabytes = { inUse = 400, limit = OSTypes.Limit 500 }
        }
    }


unlimitedComputeQuota : OSTypes.ComputeQuota
unlimitedComputeQuota =
    { cores = { inUse = 4, limit = OSTypes.Unlimited }
    , instances = { inUse = 2, limit = OSTypes.Unlimited }
    , ram = { inUse = 5120, limit = OSTypes.Unlimited }
    , keypairsLimit = 10
    }


completeParams : FlavorLimits.Params
completeParams =
    { computeQuota =
        { cores =
            { inUse = 1
            , limit = OSTypes.Limit 3
            }
        , instances =
            { inUse = 1
            , limit = OSTypes.Unlimited
            }
        , ram =
            { inUse = 1024
            , limit = OSTypes.Unlimited
            }
        , keypairsLimit = 10
        }
    , computeQuotaOperation = FlavorLimits.Create
    , customResources =
        [ { resource = "CUSTOM_A100"
          , friendlyName = "A100"
          , alias = Just "A100"
          }
        ]
    , flavors =
        [ flavor "safe" "A100:1" 1 1024
        , flavor "resource-over" "A100:2" 1 1024
        , flavor "compute-over" "" 3 1024
        ]
    , locale = usLocale
    , localization = Types.Defaults.localization
    , registeredLimits =
        Just
            [ { id = "reg-a100"
              , regionId = Just "RegionOne"
              , resourceName = OSTypes.LimitResourceName "class:CUSTOM_A100"
              , defaultLimit = 2
              , description = Nothing
              }
            ]
    , projectLimits = Just []
    , projectUsages =
        Just
            [ { resourceName = OSTypes.UsageResourceName "CUSTOM_A100"
              , resourceUsage = 1
              }
            ]
    }


computeOnlyEvaluation : FlavorLimits.ComputeQuotaOperation -> OSTypes.ComputeQuota -> List OSTypes.Flavor -> FlavorLimits.Evaluation
computeOnlyEvaluation operation computeQuota flavors =
    computeOnlyEvaluationWithLocalization Types.Defaults.localization operation computeQuota flavors


computeOnlyEvaluationWithLocalization : HelperTypes.Localization -> FlavorLimits.ComputeQuotaOperation -> OSTypes.ComputeQuota -> List OSTypes.Flavor -> FlavorLimits.Evaluation
computeOnlyEvaluationWithLocalization localization operation computeQuota flavors =
    let
        params =
            completeParams
    in
    FlavorLimits.evaluate
        { params
            | computeQuota = computeQuota
            , computeQuotaOperation = operation
            , localization = localization
            , customResources = []
            , flavors = flavors
            , registeredLimits = Nothing
            , projectLimits = Nothing
            , projectUsages = Nothing
        }


flavor : OSTypes.FlavorId -> String -> Int -> Int -> OSTypes.Flavor
flavor id aliasSpec vcpu ramMb =
    { id = id
    , name = id
    , description = Nothing
    , vcpu = vcpu
    , ram_mb = ramMb
    , disk_root = 0
    , disk_ephemeral = 0
    , extra_specs =
        if String.isEmpty aliasSpec then
            []

        else
            [ OSTypes.MetadataItem "pci_passthrough:alias" aliasSpec ]
    }
