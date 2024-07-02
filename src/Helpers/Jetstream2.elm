module Helpers.Jetstream2 exposing (calculateAllocationBurnRate)

import List.Extra
import OpenStack.Types



{-
   https://docs.jetstream-cloud.org/general/access/
-}


calculateAllocationBurnRate : List OpenStack.Types.Flavor -> OpenStack.Types.Server -> Maybe Float
calculateAllocationBurnRate flavors { details } =
    let
        serverFlavor =
            flavors
                |> List.Extra.find (\{ id } -> id == details.flavorId)
    in
    serverFlavor
        |> Maybe.andThen (allocationBurnRate details.openstackStatus)


allocationBurnRate : OpenStack.Types.ServerStatus -> OpenStack.Types.Flavor -> Maybe Float
allocationBurnRate serverStatus flavor =
    flavor
        |> flavorToResource
        |> Maybe.andThen resourceMultiplier
        |> Maybe.map
            (\multiplier ->
                multiplier * stateChargeMultiplier serverStatus * Basics.toFloat flavor.vcpu
            )


flavorToResource : { a | name : String } -> Maybe String
flavorToResource flavor =
    flavor.name |> String.split "." |> List.head


resourceMultiplier : String -> Maybe Float
resourceMultiplier resource =
    case resource of
        "m3" ->
            Just 1.0

        "g3" ->
            Just 2.0

        "r3" ->
            Just 2.0

        _ ->
            Nothing


stateChargeMultiplier : OpenStack.Types.ServerStatus -> Float
stateChargeMultiplier openStackStatus =
    case openStackStatus of
        OpenStack.Types.ServerActive ->
            1

        OpenStack.Types.ServerDeleted ->
            0

        OpenStack.Types.ServerSoftDeleted ->
            0

        OpenStack.Types.ServerError ->
            0

        OpenStack.Types.ServerBuild ->
            0

        OpenStack.Types.ServerPaused ->
            0.75

        OpenStack.Types.ServerResize ->
            1

        OpenStack.Types.ServerShelved ->
            0

        OpenStack.Types.ServerShelvedOffloaded ->
            0

        OpenStack.Types.ServerStopped ->
            0.5

        OpenStack.Types.ServerShutoff ->
            0.5

        OpenStack.Types.ServerSuspended ->
            0.75

        -- Rest are all considered to have 1
        OpenStack.Types.ServerHardReboot ->
            1

        OpenStack.Types.ServerMigrating ->
            1

        OpenStack.Types.ServerPassword ->
            1

        OpenStack.Types.ServerReboot ->
            1

        OpenStack.Types.ServerRebuild ->
            1

        OpenStack.Types.ServerRescue ->
            1

        OpenStack.Types.ServerRevertResize ->
            1

        OpenStack.Types.ServerUnknown ->
            1

        OpenStack.Types.ServerVerifyResize ->
            1
