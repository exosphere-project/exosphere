module Tests.Helpers.FlavorLimits exposing (flavorLimitsSuite)

import Expect
import Helpers.FlavorLimits as FlavorLimits
import OpenStack.Types as OSTypes
import Test exposing (Test, describe, test)
import Types.Defaults


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
                            |> Expect.equal [ "This size would exceed your project's resource limit." ]
                    ]
                    ()
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
                    { computeWarning = [ "This flavour would exceed your workspace's capacity cap." ]
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
    , customResources =
        [ { resource = "CUSTOM_A100"
          , friendlyName = "A100"
          , alias = Just "A100"
          }
        ]
    , flavors =
        [ flavor "safe" "A100:1" 1
        , flavor "resource-over" "A100:2" 1
        , flavor "compute-over" "" 3
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


flavor : OSTypes.FlavorId -> String -> Int -> OSTypes.Flavor
flavor id aliasSpec vcpu =
    { id = id
    , name = id
    , description = Nothing
    , vcpu = vcpu
    , ram_mb = 1024
    , disk_root = 0
    , disk_ephemeral = 0
    , extra_specs =
        if String.isEmpty aliasSpec then
            []

        else
            [ OSTypes.MetadataItem "pci_passthrough:alias" aliasSpec ]
    }
