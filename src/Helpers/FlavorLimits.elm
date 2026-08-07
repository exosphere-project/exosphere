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


{-| What the user is about to do with the flavor, which determines how much
compute quota it is expected to consume.

`ResizeFrom Nothing` means the server's current flavor could not be resolved,
for example because the operator has retired it. Without a baseline there is no
sound delta to evaluate, so compute quota is not evaluated at all rather than
being treated as if the target flavor were an additional server.

-}
type ComputeQuotaOperation
    = Create
    | ResizeFrom (Maybe OSTypes.Flavor)


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


{-| Collect, per flavor, the reasons that flavor would exceed a limit.
-}
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
                        (computeQuotaWarnings params.localization params.computeQuotaOperation params.computeQuota flavor)
                        unifiedLimitQuotas
                )
            )
        |> Dict.fromList
        |> Evaluation


computeQuotaWarnings : HelperTypes.Localization -> ComputeQuotaOperation -> OSTypes.ComputeQuota -> OSTypes.Flavor -> List String
computeQuotaWarnings localization operation computeQuota targetFlavor =
    computeQuotaOverages operation computeQuota targetFlavor
        |> List.map (computeQuotaOverageWarning localization)


computeQuotaOverages : ComputeQuotaOperation -> OSTypes.ComputeQuota -> OSTypes.Flavor -> List OSQuotas.ComputeQuotaOverage
computeQuotaOverages operation computeQuota targetFlavor =
    case operation of
        Create ->
            OSQuotas.computeQuotaFlavorOverages computeQuota targetFlavor

        ResizeFrom (Just currentFlavor) ->
            OSQuotas.computeQuotaFlavorResizeOverages computeQuota currentFlavor targetFlavor

        ResizeFrom Nothing ->
            []


computeQuotaOverageWarning : HelperTypes.Localization -> OSQuotas.ComputeQuotaOverage -> String
computeQuotaOverageWarning localization overage =
    let
        ( resourceName, unit ) =
            case overage.resource of
                OSQuotas.Cores ->
                    ( "Cores", "" )

                OSQuotas.Instances ->
                    ( localization.virtualComputer
                        |> Helpers.String.pluralize
                        |> Helpers.String.toTitleCase
                    , ""
                    )

                OSQuotas.Ram ->
                    ( "RAM", " MiB" )
    in
    String.concat
        [ resourceName
        , ": "
        , String.fromInt overage.required
        , unit
        , " required, "
        , String.fromInt overage.inUse
        , "/"
        , String.fromInt overage.limit
        , unit
        , " in use."
        ]


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
