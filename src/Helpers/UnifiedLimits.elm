module Helpers.UnifiedLimits exposing
    ( CustomResourceRequirement
    , ResourceAliasRequirement
    , ResourceLimitQuota
    , aggregateCustomResourceRequirements
    , comparableStringForLimitResourceName
    , customResourceRequirementsByFlavor
    , customResourceRequirementsForFlavor
    , evaluateResourceLimit
    , flavorWarningMessages
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


evaluateResourceLimit : List ResourceLimitQuota -> List CustomResourceRequirement -> List (Result String ())
evaluateResourceLimit quotas requirements =
    requirements
        |> List.map
            (\r ->
                quotas
                    |> List.Extra.find (\q -> q.resourceName == r.resource.resource)
                    |> Maybe.map
                        (\{ quota } ->
                            case quota.limit of
                                Unlimited ->
                                    Ok ()

                                Limit limit ->
                                    if quota.inUse + r.count > limit then
                                        Err
                                            (String.concat
                                                [ r.resource.friendlyName
                                                , ": "
                                                , String.fromInt (quota.inUse + r.count)
                                                , " required, "
                                                , String.fromInt quota.inUse
                                                , "/"
                                                , String.fromInt limit
                                                , " in use."
                                                ]
                                            )

                                    else
                                        Ok ()
                        )
                    |> Maybe.withDefault (Ok ())
            )


flavorWarningMessages : Maybe String -> List ResourceLimitQuota -> List CustomResourceRequirement -> List String
flavorWarningMessages maybeComputeQuotaWarning quotas requirements =
    maybeComputeQuotaWarning
        :: (evaluateResourceLimit quotas requirements
                |> List.map
                    (\result ->
                        case result of
                            Err message ->
                                Just message

                            Ok _ ->
                                Nothing
                    )
           )
        |> List.filterMap identity


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
