module Helpers.UnifiedLimits exposing
    ( CustomResourceRequirement
    , ResourceAliasRequirement
    , ResourceLimitQuota
    , aggregateCustomResourceRequirements
    , comparableStringForLimitResourceName
    , customResourceCapacities
    , customResourceConfigProblems
    , customResourceRequirementsByFlavor
    , customResourceRequirementsForFlavor
    , parseFlavorCustomResourceRequirements
    , quotasFromUnifiedLimits
    )

import Dict exposing (Dict)
import Helpers.String
import List.Extra
import OpenStack.Types as OSTypes exposing (QuotaItemLimit(..))
import String.Extra
import Types.HelperTypes as HelperTypes


type alias ResourceAliasRequirement =
    { alias : String
    , count : Int
    }


type alias CustomResourceRequirement =
    { resource : HelperTypes.CustomResource
    , count : Int
    }


type alias ResourceLimitQuota =
    { resourceName : String
    , quota : OSTypes.QuotaItem
    }


parseFlavorCustomResourceRequirements : OSTypes.Flavor -> List (Result String ResourceAliasRequirement)
parseFlavorCustomResourceRequirements flavor =
    flavor.extra_specs
        |> List.Extra.find (\item -> item.key == "pci_passthrough:alias")
        |> Maybe.map (.value >> parseCustomResourceRequirements)
        |> Maybe.withDefault []


parseCustomResourceRequirements : String -> List (Result String ResourceAliasRequirement)
parseCustomResourceRequirements value =
    value
        |> String.split ","
        -- Filter out trailing commas.
        |> List.filter (String.trim >> String.isEmpty >> not)
        |> List.map parseCustomResourceRequirement


parseCustomResourceRequirement : String -> Result String ResourceAliasRequirement
parseCustomResourceRequirement rawRequirement =
    let
        requirement =
            String.trim rawRequirement
    in
    case String.split ":" requirement of
        [ alias, countString ] ->
            let
                trimmedAlias =
                    String.trim alias
            in
            if String.isEmpty trimmedAlias then
                Err ("Custom resource alias is empty in requirement: " ++ rawRequirement)

            else
                let
                    trimmedCount =
                        String.trim countString
                in
                case String.toInt trimmedCount of
                    Just count ->
                        if count > 0 then
                            Ok
                                { alias = trimmedAlias
                                , count = count
                                }

                        else
                            Err ("Custom resource count must be positive in requirement: " ++ rawRequirement)

                    Nothing ->
                        Err ("Custom resource count is not an integer in requirement: " ++ rawRequirement)

        _ ->
            Err ("Custom resource requirement must have the form alias:count: " ++ rawRequirement)


customResourceRequirementsForFlavor : List HelperTypes.CustomResource -> OSTypes.Flavor -> List (Result String CustomResourceRequirement)
customResourceRequirementsForFlavor customResources flavor =
    flavor
        |> parseFlavorCustomResourceRequirements
        |> List.map (mapCustomResourceRequirement customResources)
        |> aggregateCustomResourceRequirements


mapCustomResourceRequirement : List HelperTypes.CustomResource -> Result String ResourceAliasRequirement -> Result String CustomResourceRequirement
mapCustomResourceRequirement customResources parsedRequirement =
    parsedRequirement
        |> Result.andThen
            (\requirement ->
                customResources
                    |> List.filterMap (\customResource -> customResource.alias |> Maybe.map (\alias -> ( alias, customResource )))
                    |> List.Extra.find (\( alias, _ ) -> Helpers.String.equalsCaseInsensitive alias requirement.alias)
                    |> Maybe.map
                        (\( _, customResource ) ->
                            Ok
                                { resource = customResource
                                , count = requirement.count
                                }
                        )
                    |> Maybe.withDefault
                        (Err ("No custom resource is configured for alias: " ++ requirement.alias))
            )


aggregateCustomResourceRequirements : List (Result String CustomResourceRequirement) -> List (Result String CustomResourceRequirement)
aggregateCustomResourceRequirements requirements =
    let
        ( resolved, unresolved ) =
            List.foldl
                (\requirement ( resolvedRequirements, unresolvedRequirements ) ->
                    case requirement of
                        Ok mappedRequirement ->
                            ( addOrAccumulate mappedRequirement resolvedRequirements, unresolvedRequirements )

                        Err reason ->
                            ( resolvedRequirements, unresolvedRequirements ++ [ Err reason ] )
                )
                ( [], [] )
                requirements
    in
    List.map Ok resolved ++ unresolved


addOrAccumulate : CustomResourceRequirement -> List CustomResourceRequirement -> List CustomResourceRequirement
addOrAccumulate requirement requirements =
    case requirements of
        [] ->
            [ requirement ]

        first :: rest ->
            if first.resource == requirement.resource then
                { first | count = first.count + requirement.count } :: rest

            else
                first :: addOrAccumulate requirement rest


customResourceRequirementsByFlavor : List HelperTypes.CustomResource -> List OSTypes.Flavor -> Dict OSTypes.FlavorId (List CustomResourceRequirement)
customResourceRequirementsByFlavor customResources flavors =
    flavors
        |> List.map (\f -> ( f.id, customResourceRequirementsForFlavor customResources f |> List.filterMap Result.toMaybe ))
        |> Dict.fromList


{-| This helps trace config problems:

Requirements that cannot be resolved are dropped rather than enforced, so a
mistyped alias exempts a flavor from its limits.

-}
customResourceConfigProblems : HelperTypes.Localization -> List HelperTypes.CustomResource -> List OSTypes.Flavor -> List String
customResourceConfigProblems localization customResources flavors =
    duplicateAliasProblems customResources
        ++ unresolvedRequirementProblems localization customResources flavors


duplicateAliasProblems : List HelperTypes.CustomResource -> List String
duplicateAliasProblems customResources =
    customResources
        |> List.filterMap (\r -> r.alias |> Maybe.map (\a -> ( a, r.resource )))
        |> List.Extra.gatherEqualsBy (Tuple.first >> String.toLower)
        |> List.filterMap
            (\( ( alias, preferredResource ), duplicates ) ->
                if List.isEmpty duplicates then
                    Nothing

                else
                    Just <|
                        String.concat
                            [ "Multiple custom resources are configured for alias "
                            , alias
                            , ", of which only "
                            , preferredResource
                            , " is used. Also configured: "
                            , duplicates |> List.map Tuple.second |> String.join ", "
                            , "."
                            ]
            )


unresolvedRequirementProblems : HelperTypes.Localization -> List HelperTypes.CustomResource -> List OSTypes.Flavor -> List String
unresolvedRequirementProblems localization customResources flavors =
    flavors
        |> List.concatMap
            (\flavor ->
                customResourceRequirementsForFlavor customResources flavor
                    |> List.filterMap
                        (\requirement ->
                            case requirement of
                                Err reason ->
                                    Just ( reason, flavor.name )

                                Ok _ ->
                                    Nothing
                        )
            )
        |> List.Extra.gatherEqualsBy Tuple.first
        |> List.map
            (\( ( reason, flavorName ), sameReason ) ->
                String.concat
                    [ reason
                    , ". Affected "
                    , Helpers.String.pluralize localization.virtualComputerHardwareConfig
                    , ": "
                    , (flavorName :: List.map Tuple.second sameReason) |> String.join ", "
                    , "."
                    ]
            )


{-| Describe what each custom resource limit would permit to provision, given a flavor's requirements.

A requirement with no matching quota, or one whose quota is unlimited, yields no
capacity, since a resource that is not limited cannot constrain anything.

(Usage in excess of the limit yields a capacity of zero rather than a negative count.)

-}
customResourceCapacities : List ResourceLimitQuota -> List CustomResourceRequirement -> List (OSTypes.QuotaCapacity HelperTypes.CustomResource)
customResourceCapacities quotas requirements =
    requirements
        |> List.filterMap
            (\requirement ->
                if requirement.count <= 0 then
                    Nothing

                else
                    quotas
                        |> List.Extra.find (\q -> q.resourceName == requirement.resource.resource)
                        |> Maybe.andThen
                            (\{ quota } ->
                                case quota.limit of
                                    Unlimited ->
                                        Nothing

                                    Limit limit ->
                                        Just
                                            { resource = requirement.resource
                                            , capacity = max 0 ((limit - quota.inUse) // requirement.count)
                                            , required = quota.inUse + requirement.count
                                            , inUse = quota.inUse
                                            , limit = limit
                                            }
                            )
            )


quotasFromUnifiedLimits : List OSTypes.RegisteredLimit -> List OSTypes.ProjectLimit -> List OSTypes.ProjectUsage -> List ResourceLimitQuota
quotasFromUnifiedLimits registeredLimits projectLimits projectUsages =
    let
        -- Edge case: If there are multiple registered limits for the same resource, prefer the regional one.
        registeredLimitsDict =
            List.foldl
                (\registeredLimit acc ->
                    let
                        resourceName =
                            comparableStringForLimitResourceName registeredLimit.resourceName
                    in
                    case Dict.get resourceName acc of
                        Just existing ->
                            if existing.regionId == Nothing && registeredLimit.regionId /= Nothing then
                                Dict.insert resourceName registeredLimit acc

                            else
                                acc

                        Nothing ->
                            Dict.insert resourceName registeredLimit acc
                )
                Dict.empty
                registeredLimits

        projectLimitsDict =
            Dict.fromList <|
                List.map
                    (\projectLimit ->
                        let
                            limitResourceName =
                                comparableStringForLimitResourceName projectLimit.resourceName
                        in
                        ( limitResourceName, projectLimit )
                    )
                    projectLimits

        usagesDict =
            Dict.fromList <|
                List.map
                    (\projectUsage ->
                        let
                            (OSTypes.UsageResourceName usageResourceName) =
                                projectUsage.resourceName
                        in
                        ( usageResourceName, projectUsage )
                    )
                    projectUsages
    in
    registeredLimitsDict
        |> Dict.toList
        |> List.map
            (\( resourceName, registeredLimit ) ->
                let
                    matchingProjectLimit =
                        Dict.get resourceName projectLimitsDict

                    matchingUsage =
                        Dict.get resourceName usagesDict

                    inUse =
                        matchingUsage |> Maybe.map .resourceUsage |> Maybe.withDefault 0
                in
                -- All unified limit resources have a registered limit.
                -- A project limit overrides a registered limit.
                case matchingProjectLimit of
                    Just projectLimit ->
                        { resourceName = resourceName
                        , quota =
                            { limit = toQuotaItemLimit projectLimit.resourceLimit
                            , inUse = inUse
                            }
                        }

                    Nothing ->
                        { resourceName = resourceName
                        , quota =
                            { limit = toQuotaItemLimit registeredLimit.defaultLimit
                            , inUse = inUse
                            }
                        }
            )


comparableStringForLimitResourceName : OSTypes.LimitResourceName -> String
comparableStringForLimitResourceName (OSTypes.LimitResourceName resourceName) =
    -- Unified limit resources have a "class:" prefix for Placement-tracked resources.
    -- e.g. "class:VCPU", "class:CUSTOM_A100X_10C" vs "servers"
    if String.startsWith "class:" resourceName then
        String.Extra.rightOf "class:" resourceName

    else
        resourceName


toQuotaItemLimit : Int -> OSTypes.QuotaItemLimit
toQuotaItemLimit limit =
    if limit == -1 then
        OSTypes.Unlimited

    else
        OSTypes.Limit limit
