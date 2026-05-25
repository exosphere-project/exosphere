module Helpers.UnifiedLimits exposing (comparableStringForLimitResourceName, quotasFromUnifiedLimits)

import Dict
import OpenStack.Types as OSTypes
import String.Extra


quotasFromUnifiedLimits : List OSTypes.RegisteredLimit -> List OSTypes.ProjectLimit -> List OSTypes.ProjectUsage -> List { resourceName : String, quota : OSTypes.QuotaItem }
quotasFromUnifiedLimits registeredLimits projectLimits projectUsages =
    let
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
    registeredLimits
        |> List.map
            (\registeredLimit ->
                let
                    resourceName =
                        comparableStringForLimitResourceName registeredLimit.resourceName

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
