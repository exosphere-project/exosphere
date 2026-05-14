module Helpers.Jetstream2 exposing (allocationBurnRate, calculateAllocationBurnRate, calculateTotalAllocationBurnRate, isJetstream2Cloud)

import Dict
import Helpers.Url
import List.Extra
import OpenStack.Types
import Types.HelperTypes exposing (Localization, Url)



{-
   https://docs.jetstream-cloud.org/general/access/
-}


isJetstream2Cloud : { a | keystone : Url } -> Bool
isJetstream2Cloud { keystone } =
    Helpers.Url.hostnameFromUrl keystone == "js2.jetstream-cloud.org"


calculateTotalAllocationBurnRate : Localization -> List OpenStack.Types.Flavor -> List OpenStack.Types.Server -> { totalBurnRate : Float, caveats : List String }
calculateTotalAllocationBurnRate localization flavors servers =
    let
        flavorsById : Dict.Dict OpenStack.Types.FlavorId OpenStack.Types.Flavor
        flavorsById =
            flavors
                |> List.map (\flavor -> ( flavor.id, flavor ))
                |> Dict.fromList

        serverOutcomes : List { burnRate : Maybe Float, caveats : List String }
        serverOutcomes =
            servers
                |> List.map (serverBurnRateOutcome localization flavorsById)

        burnRates : List Float
        burnRates =
            serverOutcomes
                |> List.filterMap .burnRate

        caveats : List String
        caveats =
            serverOutcomes
                |> List.concatMap .caveats
                |> List.Extra.unique
    in
    { totalBurnRate = burnRates |> List.sum, caveats = caveats }


calculateAllocationBurnRate : List OpenStack.Types.Flavor -> OpenStack.Types.Server -> Maybe Float
calculateAllocationBurnRate flavors { details } =
    let
        serverFlavor =
            flavors
                |> List.Extra.find (\{ id } -> id == details.flavorId)
    in
    serverFlavor
        |> Maybe.andThen (allocationBurnRate details.openstackStatus)


serverBurnRateOutcome : Localization -> Dict.Dict OpenStack.Types.FlavorId OpenStack.Types.Flavor -> OpenStack.Types.Server -> { burnRate : Maybe Float, caveats : List String }
serverBurnRateOutcome localization flavorsById server =
    case Dict.get server.details.flavorId flavorsById of
        Just flavor ->
            case allocationBurnRate server.details.openstackStatus flavor of
                Just burnRate ->
                    { burnRate = Just burnRate, caveats = [] }

                Nothing ->
                    { burnRate = Nothing
                    , caveats = [ missingBurnRateCaveat localization flavor ]
                    }

        Nothing ->
            { burnRate = Nothing
            , caveats = [ missingFlavorCaveat localization server ]
            }


missingBurnRateCaveat : Localization -> OpenStack.Types.Flavor -> String
missingBurnRateCaveat localization flavor =
    "Missing burn rate for " ++ localization.virtualComputerHardwareConfig ++ " " ++ flavor.name ++ " (" ++ flavor.id ++ ")"


missingFlavorCaveat : Localization -> OpenStack.Types.Server -> String
missingFlavorCaveat localization server =
    "Missing " ++ localization.virtualComputerHardwareConfig ++ " " ++ server.details.flavorId ++ " for " ++ localization.virtualComputer ++ " " ++ server.name


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

        "g4" ->
            Just 7.0

        "g5" ->
            Just 6.4

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
