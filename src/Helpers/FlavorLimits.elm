module Helpers.FlavorLimits exposing
    ( CapacityParams
    , ComputeQuotaOperation(..)
    , Evaluation
    , Params
    , Resource
    , ResourceCapacity
    , capacities
    , capacityMessage
    , evaluate
    , exceedsLimit
    , invalidSelectionMessage
    , maxCount
    , selectionGuidanceMessage
    , warningMessagesFor
    )

import Dict exposing (Dict)
import FormatNumber.Locales exposing (Locale)
import Helpers.Formatting as Formatting exposing (Unit(..))
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

This governs compute quota only. Custom resources backed by unified limits are
always evaluated at the target flavor's full requirement, on resize as on
create, because Placement holds the source allocation under the migration
consumer until the resize is confirmed. Both allocations therefore exist at
once, and a delta would understate what the resize needs.

-}
type ComputeQuotaOperation
    = Create
    | ResizeFrom (Maybe OSTypes.Flavor)


type alias Params =
    { computeQuota : OSTypes.ComputeQuota
    , computeQuotaOperation : ComputeQuotaOperation
    , customResources : List HelperTypes.CustomResource
    , flavors : List OSTypes.Flavor
    , locale : Locale
    , localization : HelperTypes.Localization
    , registeredLimits : Maybe (List OSTypes.RegisteredLimit)
    , projectLimits : Maybe (List OSTypes.ProjectLimit)
    , projectUsages : Maybe (List OSTypes.ProjectUsage)
    }


{-| Collect, per flavor, the reasons that flavor would exceed a limit.

Compute quota is evaluated according to the operation, whereas custom resources
are always evaluated at the flavor's full requirement. See `ComputeQuotaOperation`.

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
                , List.concat
                    [ computeQuotaCapacities params.computeQuotaOperation params.computeQuota flavor
                        |> List.map (toResourceCapacity ComputeResource)
                    , requirementsByFlavor
                        |> Dict.get flavor.id
                        |> Maybe.withDefault []
                        |> UnifiedLimits.customResourceCapacities unifiedLimitQuotas
                        |> List.map (toResourceCapacity CustomResource)
                    ]
                    |> List.filter OSQuotas.exceedsQuota
                    |> List.map (exceededMessage params.locale params.localization)
                )
            )
        |> Dict.fromList
        |> Evaluation


computeQuotaCapacities : ComputeQuotaOperation -> OSTypes.ComputeQuota -> OSTypes.Flavor -> List (OSTypes.QuotaCapacity OSQuotas.ComputeQuotaResource)
computeQuotaCapacities operation computeQuota targetFlavor =
    case operation of
        Create ->
            OSQuotas.computeQuotaFlavorCapacities computeQuota targetFlavor

        ResizeFrom (Just currentFlavor) ->
            OSQuotas.computeQuotaFlavorResizeCapacities computeQuota currentFlavor targetFlavor

        ResizeFrom Nothing ->
            []


{-| Describe how a resource's capacity is exceeded.
-}
exceededMessage : Locale -> HelperTypes.Localization -> ResourceCapacity -> String
exceededMessage locale localization capacity =
    let
        ( resourceName, unit ) =
            resourceLabel localization capacity.resource
    in
    String.concat
        [ resourceName
        , ": "
        , quantity locale unit capacity.required
        , " required, "
        , usage locale unit capacity.inUse capacity.limit
        ]


{-| A resource's display name & units.
-}
resourceLabel : HelperTypes.Localization -> Resource -> ( String, Unit )
resourceLabel localization resource =
    case resource of
        ComputeResource OSQuotas.Cores ->
            ( "Cores", Count )

        ComputeResource OSQuotas.Instances ->
            ( localization.virtualComputer
                |> Helpers.String.pluralize
                |> Helpers.String.toTitleCase
            , Count
            )

        ComputeResource OSQuotas.Ram ->
            ( "RAM", MebiBytes )

        VolumeResource OSQuotas.Volumes ->
            ( localization.blockDevice
                |> Helpers.String.pluralize
                |> Helpers.String.toTitleCase
            , Count
            )

        VolumeResource OSQuotas.VolumeStorage ->
            ( String.join " "
                [ Helpers.String.toTitleCase localization.blockDevice
                , "storage"
                ]
            , GibiBytes
            )

        CustomResource customResource ->
            ( customResource.friendlyName, Count )


{-| Readable resource quantity in its units.
-}
quantity : Locale -> Unit -> Int -> String
quantity locale unit value =
    case unit of
        Count ->
            Formatting.humanCount locale value

        _ ->
            let
                ( formatted, suffix ) =
                    Formatting.humanNumber locale unit value
            in
            formatted ++ " " ++ suffix


{-| Render a usage against its limit.
-}
usage : Locale -> Unit -> Int -> Int -> String
usage locale unit inUse limit =
    String.concat
        [ quantity locale unit inUse
        , " / "
        , quantity locale unit limit
        , " in use."
        ]


type Resource
    = ComputeResource OSQuotas.ComputeQuotaResource
    | VolumeResource OSQuotas.VolumeQuotaResource
    | CustomResource HelperTypes.CustomResource


type alias ResourceCapacity =
    OSTypes.QuotaCapacity Resource


type alias CapacityParams =
    { computeQuota : OSTypes.ComputeQuota
    , customResources : List HelperTypes.CustomResource
    , flavor : OSTypes.Flavor
    , registeredLimits : Maybe (List OSTypes.RegisteredLimit)
    , projectLimits : Maybe (List OSTypes.ProjectLimit)
    , projectUsages : Maybe (List OSTypes.ProjectUsage)
    , volumeBackedGb : Maybe OSTypes.VolumeSize
    , volumeQuota : OSTypes.VolumeQuota
    }


{-| Measure every limit that bounds how many servers of a flavor may be created.

Volume quotas apply only to a volume-backed server.

Limits that constrain nothing are absent.

-}
capacities : CapacityParams -> List ResourceCapacity
capacities params =
    let
        unifiedLimitQuotas =
            Maybe.map3
                UnifiedLimits.quotasFromUnifiedLimits
                params.registeredLimits
                params.projectLimits
                params.projectUsages
                |> Maybe.withDefault []

        customResourceRequirements =
            UnifiedLimits.customResourceRequirementsForFlavor params.customResources params.flavor
                |> List.filterMap Result.toMaybe
    in
    List.concat
        [ OSQuotas.computeQuotaFlavorCapacities params.computeQuota params.flavor
            |> List.map (toResourceCapacity ComputeResource)
        , params.volumeBackedGb
            |> Maybe.map (\gb -> OSQuotas.volumeQuotaFlavorCapacities gb params.volumeQuota)
            |> Maybe.withDefault []
            |> List.map (toResourceCapacity VolumeResource)
        , UnifiedLimits.customResourceCapacities unifiedLimitQuotas customResourceRequirements
            |> List.map (toResourceCapacity CustomResource)
        ]


toResourceCapacity : (resource -> Resource) -> OSTypes.QuotaCapacity resource -> ResourceCapacity
toResourceCapacity toResource capacity =
    { resource = toResource capacity.resource
    , capacity = capacity.capacity
    , required = capacity.required
    , inUse = capacity.inUse
    , limit = capacity.limit
    }


{-| The greatest number of servers the given capacities jointly permit.

`Nothing` means no limit constrains the count (not that none may be created).

-}
maxCount : List ResourceCapacity -> Maybe Int
maxCount resourceCapacities =
    resourceCapacities
        |> List.map .capacity
        |> List.minimum


{-| Describe how much of a resource could potentially be supported.
-}
capacityMessage : Locale -> HelperTypes.Localization -> ResourceCapacity -> String
capacityMessage locale localization capacity =
    let
        ( resourceName, unit ) =
            resourceLabel localization capacity.resource
    in
    String.concat
        [ resourceName
        , ": "
        , Formatting.humanCount locale capacity.capacity
        , " supported, "
        , usage locale unit capacity.inUse capacity.limit
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
