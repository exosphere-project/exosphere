module Helpers.UnifiedLimits exposing (comparableStringForLimitResourceName, quotasFromUnifiedLimits)

import Dict
import OpenStack.Types as OSTypes
import String.Extra


quotasFromUnifiedLimits : List OSTypes.RegisteredLimit -> List OSTypes.ProjectLimit -> List OSTypes.ProjectUsage -> List { resourceName : String, quota : OSTypes.QuotaItem }
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
