module Tests.OpenStack.Quotas exposing
    ( computeQuotasAndLimitsSuite
    , manilaQuotasAndLimitsSuite
    , volumeQuotasAndLimitsSuite
    )

-- Test related Modules
-- Exosphere Modules Under Test

import Expect
import Json.Decode as Decode
import OpenStack.Quotas
    exposing
        ( computeQuotaDecoder
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
        ]


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
