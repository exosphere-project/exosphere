module Tests.OpenStack.Quotas exposing
    ( computeQuotasAndLimitsSuite
    , manilaQuotasAndLimitsSuite
    , volumeQuotasAndLimitsSuite
    )

import Expect
import Fuzz
import Json.Decode as Decode
import OpenStack.Quotas
    exposing
        ( computeQuotaDecoder
        , computeQuotaFlavorCapacities
        , computeQuotaFlavorResizeCapacities
        , exceedsQuota
        , shareQuotaDecoder
        , volumeQuotaDecoder
        )
import OpenStack.Types as OSTypes
import Test exposing (Test, describe, test)


computeQuotasAndLimitsSuite : Test
computeQuotasAndLimitsSuite =
    let
        novaLimits : String
        novaLimits =
            """
            {
                "limits": {
                    "rate": [],
                    "absolute": {
                        "maxServerMeta": 128,
                        "maxPersonality": 5,
                        "totalServerGroupsUsed": 0,
                        "maxImageMeta": 128,
                        "maxPersonalitySize": 10240,
                        "maxTotalKeypairs": 100,
                        "maxSecurityGroupRules": 20,
                        "maxServerGroups": 10,
                        "totalCoresUsed": 1,
                        "totalRAMUsed": 1024,
                        "totalInstancesUsed": 1,
                        "maxSecurityGroups": 10,
                        "totalFloatingIpsUsed": 0,
                        "maxTotalCores": 48,
                        "maxServerGroupMembers": 10,
                        "maxTotalFloatingIps": 10,
                        "totalSecurityGroupsUsed": 1,
                        "maxTotalInstances": 10,
                        "maxTotalRAMSize": 999999
                    }
                }
            }
            """
    in
    describe "Decoding compute quotas and limits"
        [ test "compute limits" <|
            \_ ->
                Expect.equal
                    (Decode.decodeString
                        (Decode.field "limits" computeQuotaDecoder)
                        novaLimits
                    )
                    (Ok
                        { cores =
                            { inUse = 1
                            , limit = OSTypes.Limit 48
                            }
                        , instances =
                            { inUse = 1
                            , limit = OSTypes.Limit 10
                            }
                        , ram =
                            { inUse = 1024
                            , limit = OSTypes.Limit 999999
                            }
                        , keypairsLimit = 100
                        }
                    )
        , test "create reports every exceeded compute quota" <|
            \_ ->
                let
                    computeQuota =
                        { cores = { inUse = 8, limit = OSTypes.Limit 10 }
                        , instances = { inUse = 1, limit = OSTypes.Limit 1 }
                        , ram = { inUse = 8000, limit = OSTypes.Limit 10000 }
                        , keypairsLimit = 10
                        }
                in
                computeQuotaFlavorCapacities
                    computeQuota
                    (quotaTestFlavor "target" 4 4096)
                    |> List.filter exceedsQuota
                    |> List.map exceededSummary
                    |> Expect.equal
                        [ { resource = OpenStack.Quotas.Cores
                          , required = 12
                          , inUse = 8
                          , limit = 10
                          }
                        , { resource = OpenStack.Quotas.Ram
                          , required = 12096
                          , inUse = 8000
                          , limit = 10000
                          }
                        , { resource = OpenStack.Quotas.Instances
                          , required = 2
                          , inUse = 1
                          , limit = 1
                          }
                        ]
        , test "resize checks only positive cores and RAM deltas and ignores instance quota" <|
            \_ ->
                let
                    currentFlavor =
                        quotaTestFlavor "current" 4 4096

                    computeQuota =
                        { cores = { inUse = 8, limit = OSTypes.Limit 10 }
                        , instances = { inUse = 1, limit = OSTypes.Limit 1 }
                        , ram = { inUse = 8000, limit = OSTypes.Limit 10000 }
                        , keypairsLimit = 10
                        }
                in
                Expect.all
                    [ \_ ->
                        computeQuotaFlavorResizeCapacities
                            computeQuota
                            currentFlavor
                            (quotaTestFlavor "within" 6 5632)
                            |> List.filter exceedsQuota
                            |> List.map exceededSummary
                            |> Expect.equal []
                    , \_ ->
                        computeQuotaFlavorResizeCapacities
                            computeQuota
                            currentFlavor
                            (quotaTestFlavor "cores-over" 7 5632)
                            |> List.filter exceedsQuota
                            |> List.map exceededSummary
                            |> Expect.equal
                                [ { resource = OpenStack.Quotas.Cores
                                  , required = 11
                                  , inUse = 8
                                  , limit = 10
                                  }
                                ]
                    , \_ ->
                        computeQuotaFlavorResizeCapacities
                            computeQuota
                            currentFlavor
                            (quotaTestFlavor "ram-over" 6 6656)
                            |> List.filter exceedsQuota
                            |> List.map exceededSummary
                            |> Expect.equal
                                [ { resource = OpenStack.Quotas.Ram
                                  , required = 10560
                                  , inUse = 8000
                                  , limit = 10000
                                  }
                                ]
                    ]
                    ()
        , test "resize permits non-increasing resources when usage is already over quota" <|
            \_ ->
                let
                    currentFlavor =
                        quotaTestFlavor "current" 8 8192

                    computeQuota =
                        { cores = { inUse = 11, limit = OSTypes.Limit 10 }
                        , instances = { inUse = 2, limit = OSTypes.Limit 1 }
                        , ram = { inUse = 11000, limit = OSTypes.Limit 10000 }
                        , keypairsLimit = 10
                        }
                in
                Expect.all
                    [ \_ ->
                        computeQuotaFlavorResizeCapacities
                            computeQuota
                            currentFlavor
                            (quotaTestFlavor "same" 8 8192)
                            |> List.filter exceedsQuota
                            |> List.map exceededSummary
                            |> Expect.equal []
                    , \_ ->
                        computeQuotaFlavorResizeCapacities
                            computeQuota
                            currentFlavor
                            (quotaTestFlavor "smaller" 4 4096)
                            |> List.filter exceedsQuota
                            |> List.map exceededSummary
                            |> Expect.equal []
                    ]
                    ()
        , Test.fuzz3 (Fuzz.intRange 0 500) (Fuzz.intRange 0 500) (Fuzz.intRange 1 64) "exceeding a quota is exactly having no capacity" <|
            \inUse limit vcpu ->
                let
                    computeQuota =
                        { cores = { inUse = inUse, limit = OSTypes.Limit limit }
                        , instances = { inUse = 0, limit = OSTypes.Unlimited }
                        , ram = { inUse = 0, limit = OSTypes.Unlimited }
                        , keypairsLimit = 10
                        }
                in
                computeQuotaFlavorCapacities computeQuota (quotaTestFlavor "fuzzed" vcpu 1024)
                    |> List.filter (\capacity -> capacity.resource == OpenStack.Quotas.Cores)
                    |> List.map exceedsQuota
                    |> Expect.equal [ inUse + vcpu > limit ]
        ]


quotaTestFlavor : OSTypes.FlavorId -> Int -> Int -> OSTypes.Flavor
quotaTestFlavor id vcpu ramMb =
    { id = id
    , name = id
    , description = Nothing
    , vcpu = vcpu
    , ram_mb = ramMb
    , disk_root = 0
    , disk_ephemeral = 0
    , extra_specs = []
    }


volumeQuotasAndLimitsSuite : Test
volumeQuotasAndLimitsSuite =
    let
        cinderLimits : String
        cinderLimits =
            """
            {
                "limits": {
                    "rate": [],
                    "absolute": {
                        "totalSnapshotsUsed": 0,
                        "maxTotalBackups": -1,
                        "maxTotalVolumeGigabytes": 1000,
                        "maxTotalSnapshots": 10,
                        "maxTotalBackupGigabytes": 1000,
                        "totalBackupGigabytesUsed": 267,
                        "maxTotalVolumes": 10,
                        "totalVolumesUsed": 5,
                        "totalBackupsUsed": 13,
                        "totalGigabytesUsed": 82
                    }
                }
            }
            """
    in
    describe "Decoding volume quotas and limits"
        [ test "volume limits" <|
            \_ ->
                Expect.equal
                    (Decode.decodeString
                        (Decode.field "limits" volumeQuotaDecoder)
                        cinderLimits
                    )
                    (Ok
                        { volumes =
                            { inUse = 5
                            , limit = OSTypes.Limit 10
                            }
                        , gigabytes =
                            { inUse = 82
                            , limit = OSTypes.Limit 1000
                            }
                        }
                    )
        ]


manilaQuotasAndLimitsSuite : Test
manilaQuotasAndLimitsSuite =
    let
        manilaLimits : String
        manilaLimits =
            """
            {
                "limits": {
                    "rate": [],
                    "absolute": {
                        "maxTotalShares": 50,
                        "maxTotalShareSnapshots": 50,
                        "maxTotalShareGigabytes": 1000,
                        "maxTotalSnapshotGigabytes": 1000,
                        "maxTotalShareNetworks": 10,
                        "totalSharesUsed": 5,
                        "totalShareSnapshotsUsed": 0,
                        "totalShareGigabytesUsed": 122,
                        "totalSnapshotGigabytesUsed": 0,
                        "totalShareNetworksUsed": 0
                    }
                }
            }
            """
    in
    describe "Decoding share quotas and limits"
        [ test "quota limits" <|
            \_ ->
                Expect.equal
                    (Decode.decodeString shareQuotaDecoder manilaLimits)
                    (Ok
                        { gigabytes = { inUse = 122, limit = OSTypes.Limit 1000 }
                        , snapshots = { inUse = 0, limit = OSTypes.Limit 50 }
                        , shares = { inUse = 5, limit = OSTypes.Limit 50 }
                        , snapshotGigabytes = { inUse = 0, limit = OSTypes.Limit 1000 }
                        , shareNetworks = Just { inUse = 0, limit = OSTypes.Limit 10 }
                        , shareReplicas = Nothing
                        , shareReplicaGigabytes = Nothing
                        , shareGroups = Nothing
                        , shareGroupSnapshots = Nothing
                        , perShareGigabytes = Nothing
                        }
                    )
        ]


{-| The facts an exceeded quota reports, without the capacity that is zero by construction.
-}
exceededSummary : OSTypes.QuotaCapacity resource -> { resource : resource, required : Int, inUse : Int, limit : Int }
exceededSummary capacity =
    { resource = capacity.resource
    , required = capacity.required
    , inUse = capacity.inUse
    , limit = capacity.limit
    }
