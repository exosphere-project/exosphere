module Tests.Helpers.FlavorLimits exposing (flavorLimitsSuite)

import Expect
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
                            |> Expect.equal [ "A100: 3 required, 1/2 in use." ]
                    , \_ ->
                        FlavorLimits.warningMessagesFor "compute-over" evaluation
                            |> Expect.equal [ "Cores: 4 required, 1/3 in use." ]
                    ]
                    ()
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
                    |> Expect.equal [ "Instances: 2 required, 1/1 in use." ]
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
                        [ "Cores: 12 required, 8/10 in use."
                        , "RAM: 12096 MiB required, 8000/10000 MiB in use."
                        , "Instances: 2 required, 1/1 in use."
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
                    |> Expect.equal [ "Servers: 2 required, 1/1 in use." ]
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
                    { computeWarning = [ "Cores: 4 required, 1/3 in use." ]
                    , guidance = "Please select a flavour that does not exceed your workspace's capacity caps."
                    , invalid = "Please select a valid flavour."
                    }
                    { computeWarning = FlavorLimits.warningMessagesFor "compute-over" evaluation
                    , guidance = FlavorLimits.selectionGuidanceMessage localization
                    , invalid = FlavorLimits.invalidSelectionMessage localization
                    }
        ]


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
