module Tests.Helpers.Jetstream2 exposing (totalAllocationBurnRateSuite)

import Expect
import Helpers.Jetstream2
import Helpers.RemoteDataPlusPlus as RDPP
import OpenStack.Types
import Test exposing (Test, describe, test)
import Time
import Types.Defaults exposing (localization)


totalAllocationBurnRateSuite : Test
totalAllocationBurnRateSuite =
    describe "calculateTotalAllocationBurnRate"
        [ test "returns total burn rate and caveats for unsupported and missing flavors" <|
            \_ ->
                let
                    testLocalization =
                        { localization
                            | virtualComputer = "server"
                            , virtualComputerHardwareConfig = "flavor"
                        }

                    result =
                        Helpers.Jetstream2.calculateTotalAllocationBurnRate
                            testLocalization
                            [ flavor "known-1" "m3.small" 4
                            , flavor "unsupported-1" "x3.large" 8
                            ]
                            [ server "active-server" "known-1" OpenStack.Types.ServerActive
                            , server "paused-server" "known-1" OpenStack.Types.ServerPaused
                            , server "unsupported-server-a" "unsupported-1" OpenStack.Types.ServerActive
                            , server "unsupported-server-b" "unsupported-1" OpenStack.Types.ServerStopped
                            , server "missing-server" "missing-1" OpenStack.Types.ServerActive
                            ]
                in
                Expect.equal
                    { totalBurnRate = 7.0
                    , caveats =
                        [ "Missing burn rate for flavor x3.large (unsupported-1)"
                        , "Missing flavor missing-1 for server missing-server"
                        ]
                    }
                    result
        , test "reports missing flavors per server" <|
            \_ ->
                Helpers.Jetstream2.calculateTotalAllocationBurnRate
                    { localization
                        | virtualComputer = "server"
                        , virtualComputerHardwareConfig = "flavor"
                    }
                    []
                    [ server "server-a" "missing-1" OpenStack.Types.ServerActive
                    , server "server-b" "missing-1" OpenStack.Types.ServerActive
                    ]
                    |> .caveats
                    |> Expect.equal
                        [ "Missing flavor missing-1 for server server-a"
                        , "Missing flavor missing-1 for server server-b"
                        ]
        ]


flavor : String -> String -> Int -> OpenStack.Types.Flavor
flavor id name vcpu =
    { id = id
    , name = name
    , description = Nothing
    , vcpu = vcpu
    , ram_mb = 0
    , disk_root = 0
    , disk_ephemeral = 0
    , extra_specs = []
    }


server : String -> OpenStack.Types.FlavorId -> OpenStack.Types.ServerStatus -> OpenStack.Types.Server
server name flavorId status =
    { name = name
    , uuid = name ++ "-uuid"
    , details =
        { openstackStatus = status
        , created = Time.millisToPosix 0
        , powerState = OpenStack.Types.PowerRunning
        , imageUuid = "image-uuid"
        , flavorId = flavorId
        , keypairName = Nothing
        , metadata = []
        , userUuid = "user-uuid"
        , volumesAttached = []
        , tags = []
        , lockStatus = OpenStack.Types.ServerUnlocked
        , fault = Nothing
        }
    , consoleUrl = RDPP.empty
    }
