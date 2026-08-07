module Helpers.FlavorLimits exposing
    ( ComputeQuotaOperation(..)
    , Evaluation
    , Params
    , evaluate
    , exceedsLimit
    , invalidSelectionMessage
    , selectionGuidanceMessage
    , warningMessagesFor
    )

import Dict exposing (Dict)
import Helpers.String
import Helpers.UnifiedLimits as UnifiedLimits
import OpenStack.Quotas as OSQuotas
import OpenStack.Types as OSTypes
import Types.HelperTypes as HelperTypes


type Evaluation
    = Evaluation (Dict OSTypes.FlavorId (List String))


type ComputeQuotaOperation
    = Create
    | ResizeFrom OSTypes.Flavor


type alias Params =
    { computeQuota : OSTypes.ComputeQuota
    , computeQuotaOperation : ComputeQuotaOperation
    , customResources : List HelperTypes.CustomResource
    , flavors : List OSTypes.Flavor
    , localization : HelperTypes.Localization
    , registeredLimits : Maybe (List OSTypes.RegisteredLimit)
    , projectLimits : Maybe (List OSTypes.ProjectLimit)
    , projectUsages : Maybe (List OSTypes.ProjectUsage)
    }


evaluate : Params -> Evaluation
evaluate params =
    let
        unifiedLimitQuotas =
            Maybe.map3
                UnifiedLimits.quotasFromUnifiedLimits
                params.registeredLimits
                params.projectLimits
                params.projectUsages
                |> Maybe.withDefault []

        requirementsByFlavor =
            UnifiedLimits.customResourceRequirementsByFlavor params.customResources params.flavors
    in
    params.flavors
        |> List.map
            (\flavor ->
                ( flavor.id
                , requirementsByFlavor
                    |> Dict.get flavor.id
                    |> Maybe.withDefault []
                    |> UnifiedLimits.flavorWarningMessages
                        (computeQuotaWarning params.localization params.computeQuotaOperation params.computeQuota flavor)
                        unifiedLimitQuotas
                )
            )
        |> Dict.fromList
        |> Evaluation


computeQuotaWarning : HelperTypes.Localization -> ComputeQuotaOperation -> OSTypes.ComputeQuota -> OSTypes.Flavor -> Maybe String
computeQuotaWarning localization operation computeQuota targetFlavor =
    if computeQuotaExceeded operation computeQuota targetFlavor then
        -- TODO: Provide more granular detail on how the compute quota is exceeded.
        Just <|
            "This "
                ++ localization.virtualComputerHardwareConfig
                ++ " would exceed your "
                ++ localization.unitOfTenancy
                ++ "'s "
                ++ localization.maxResourcesPerProject
                ++ "."

    else
        Nothing


computeQuotaExceeded : ComputeQuotaOperation -> OSTypes.ComputeQuota -> OSTypes.Flavor -> Bool
computeQuotaExceeded operation computeQuota targetFlavor =
    case operation of
        Create ->
            OSQuotas.computeQuotaFlavorAvailServers computeQuota targetFlavor
                |> Maybe.map (\launchableServers -> launchableServers < 1)
                |> Maybe.withDefault False

        ResizeFrom currentFlavor ->
            OSQuotas.computeQuotaFlavorResizeExceedsLimit computeQuota currentFlavor targetFlavor


warningMessagesFor : OSTypes.FlavorId -> Evaluation -> List String
warningMessagesFor flavorId (Evaluation warningMessagesByFlavor) =
    warningMessagesByFlavor
        |> Dict.get flavorId
        |> Maybe.withDefault []


exceedsLimit : OSTypes.FlavorId -> Evaluation -> Bool
exceedsLimit flavorId evaluation =
    evaluation
        |> warningMessagesFor flavorId
        |> List.isEmpty
        |> not


selectionGuidanceMessage : HelperTypes.Localization -> String
selectionGuidanceMessage localization =
    String.join " "
        [ "Please select"
        , Helpers.String.indefiniteArticle localization.virtualComputerHardwareConfig
        , localization.virtualComputerHardwareConfig
        , "that does not exceed your"
        , localization.unitOfTenancy ++ "'s"
        , localization.maxResourcesPerProject
            |> Helpers.String.pluralize
            |> (\limits -> limits ++ ".")
        ]


invalidSelectionMessage : HelperTypes.Localization -> String
invalidSelectionMessage localization =
    "Please select a valid "
        ++ localization.virtualComputerHardwareConfig
        ++ "."
