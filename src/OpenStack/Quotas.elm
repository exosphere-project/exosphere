module OpenStack.Quotas exposing
    ( computeQuotaDecoder
    , computeQuotaFlavorAvailServers
    , overallQuotaAvailServers
    , requestComputeQuota
    , requestNetworkQuota
    , requestShareQuota
    , requestVolumeQuota
    , volumeQuotaAvail
    , volumeQuotaDecoder
    )

import Helpers.GetterSetters as GetterSetters
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import OpenStack.Types as OSTypes
import Rest.Helpers exposing (expectJsonWithErrorBody, openstackCredentialedRequest)
import Types.Error exposing (ErrorContext, ErrorLevel(..))
import Types.HelperTypes exposing (HttpRequestMethod(..), Url, UserOrProject(..))
import Types.Project exposing (Project)
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))



-- Compute Quota


requestComputeQuota : Project -> Cmd SharedMsg
requestComputeQuota project =
    let
        errorContext =
            ErrorContext
                "get details of compute quota"
                ErrorCrit
                Nothing

        resultToMsg result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveComputeQuota errorContext result)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        []
        (project.endpoints.nova ++ "/limits")
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg
            (Decode.field "limits" computeQuotaDecoder)
        )


computeQuotaDecoder : Decode.Decoder OSTypes.ComputeQuota
computeQuotaDecoder =
    Decode.map4 OSTypes.ComputeQuota
        (Decode.map2 OSTypes.QuotaItemDetail
            (Decode.at [ "absolute", "totalCoresUsed" ] Decode.int)
            (Decode.at [ "absolute", "maxTotalCores" ] specialIntToMaybe)
        )
        (Decode.map2 OSTypes.QuotaItemDetail
            (Decode.at [ "absolute", "totalInstancesUsed" ] Decode.int)
            (Decode.at [ "absolute", "maxTotalInstances" ] specialIntToMaybe)
        )
        (Decode.map2 OSTypes.QuotaItemDetail
            (Decode.at [ "absolute", "totalRAMUsed" ] Decode.int)
            (Decode.at [ "absolute", "maxTotalRAMSize" ] specialIntToMaybe)
        )
        (Decode.at [ "absolute", "maxTotalKeypairs" ] Decode.int)



-- Volume Quota


requestVolumeQuota : Project -> Cmd SharedMsg
requestVolumeQuota project =
    let
        errorContext =
            ErrorContext
                "get details of volume quota"
                ErrorCrit
                Nothing

        resultToMsg result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveVolumeQuota errorContext result)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        []
        (project.endpoints.cinder ++ "/limits")
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg
            (Decode.field "limits" volumeQuotaDecoder)
        )


volumeQuotaDecoder : Decode.Decoder OSTypes.VolumeQuota
volumeQuotaDecoder =
    Decode.map2 OSTypes.VolumeQuota
        (Decode.map2 OSTypes.QuotaItemDetail
            (Decode.at [ "absolute", "totalVolumesUsed" ] Decode.int)
            (Decode.at [ "absolute", "maxTotalVolumes" ] specialIntToMaybe)
        )
        (Decode.map2 OSTypes.QuotaItemDetail
            (Decode.at [ "absolute", "totalGigabytesUsed" ] Decode.int)
            (Decode.at [ "absolute", "maxTotalVolumeGigabytes" ] specialIntToMaybe)
        )



-- Network quota


requestNetworkQuota : Project -> Cmd SharedMsg
requestNetworkQuota project =
    let
        errorContext =
            ErrorContext
                "get details of network quota"
                ErrorCrit
                Nothing

        resultToMsg result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveNetworkQuota errorContext result)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        []
        (project.endpoints.neutron ++ "/v2.0/quotas/" ++ project.auth.project.uuid ++ "/details.json")
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg
            (Decode.field "quota" networkQuotaDecoder)
        )


networkQuotaDecoder : Decode.Decoder OSTypes.NetworkQuota
networkQuotaDecoder =
    Decode.map OSTypes.NetworkQuota
        (Decode.map2 OSTypes.QuotaItemDetail
            (Decode.at [ "floatingip", "used" ] Decode.int)
            (Decode.at [ "floatingip", "limit" ] specialIntToMaybe)
        )



-- Share Quota


requestShareQuota : UserOrProject -> Project -> Url -> Cmd SharedMsg
requestShareQuota userOrProject project url =
    let
        errorContext =
            ErrorContext
                "get details of share quota"
                ErrorCrit
                Nothing

        resultToMsg result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveShareQuota userOrProject errorContext result)

        fullUrl =
            (url ++ "/quota-sets/" ++ project.auth.project.uuid ++ "/detail")
                ++ (case userOrProject of
                        IsUser ->
                            "?user_id=" ++ project.auth.user.uuid

                        IsProject ->
                            ""
                   )
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        [ ( "X-OpenStack-Manila-API-Version", "2.62" ) ]
        fullUrl
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg
            (Decode.field "quota_set" shareLimitsDecoder)
        )


quotaItemDecoder : Decode.Decoder OSTypes.QuotaItemDetail
quotaItemDecoder =
    Decode.succeed OSTypes.QuotaItemDetail
        |> required "in_use" Decode.int
        |> required "limit" specialIntToMaybe


shareLimitsDecoder : Decode.Decoder OSTypes.ShareQuota
shareLimitsDecoder =
    Decode.succeed OSTypes.ShareQuota
        |> required "gigabytes" quotaItemDecoder
        |> required "snapshots" quotaItemDecoder
        |> required "shares" quotaItemDecoder
        |> required "snapshot_gigabytes" quotaItemDecoder
        |> required "share_networks" (Decode.maybe quotaItemDecoder)
        |> required "share_groups" quotaItemDecoder
        |> required "share_group_snapshots" quotaItemDecoder
        |> required "share_replicas" quotaItemDecoder
        |> required "replica_gigabytes" quotaItemDecoder
        |> required "per_share_gigabytes" quotaItemDecoder



-- Helpers


specialIntToMaybe : Decode.Decoder (Maybe Int)
specialIntToMaybe =
    Decode.int
        |> Decode.map
            (\i ->
                if i == -1 then
                    Nothing

                else
                    Just i
            )


{-| Given a compute quota and a flavor, determine how many servers of that flavor can be launched
-}
computeQuotaFlavorAvailServers : OSTypes.ComputeQuota -> OSTypes.Flavor -> Maybe Int
computeQuotaFlavorAvailServers computeQuota flavor =
    [ computeQuota.cores.limit
        |> Maybe.map
            (\coreLimit ->
                (coreLimit - computeQuota.cores.inUse) // flavor.vcpu
            )
    , computeQuota.ram.limit
        |> Maybe.map
            (\ramLimit ->
                (ramLimit - computeQuota.ram.inUse) // flavor.ram_mb
            )
    , computeQuota.instances.limit
        |> Maybe.map
            (\countLimit ->
                countLimit - computeQuota.instances.inUse
            )
    ]
        |> List.filterMap identity
        |> List.minimum


{-| Returns tuple showing # volumes and # total gigabytes that are available given quota and usage.

Nothing implies no limit.

-}
volumeQuotaAvail : OSTypes.VolumeQuota -> ( Maybe Int, Maybe Int )
volumeQuotaAvail volumeQuota =
    -- Returns tuple showing # volumes and # total gigabytes that are available given quota and usage.
    -- Nothing implies no limit.
    ( volumeQuota.volumes.limit
        |> Maybe.map
            (\volLimit ->
                volLimit - volumeQuota.volumes.inUse
            )
    , volumeQuota.gigabytes.limit
        |> Maybe.map
            (\gbLimit ->
                gbLimit - volumeQuota.gigabytes.inUse
            )
    )


overallQuotaAvailServers : Maybe OSTypes.VolumeSize -> OSTypes.Flavor -> OSTypes.ComputeQuota -> OSTypes.VolumeQuota -> Maybe Int
overallQuotaAvailServers maybeVolBackedGb flavor computeQuota volumeQuota =
    let
        computeQuotaAvailServers =
            computeQuotaFlavorAvailServers computeQuota flavor
    in
    case maybeVolBackedGb of
        Nothing ->
            computeQuotaAvailServers

        Just volBackedGb ->
            let
                ( volumeQuotaAvailVolumes, volumeQuotaAvailGb ) =
                    volumeQuotaAvail volumeQuota

                volumeQuotaAvailGbCount =
                    volumeQuotaAvailGb
                        |> Maybe.map
                            (\availGb ->
                                availGb // volBackedGb
                            )
            in
            [ computeQuotaAvailServers
            , volumeQuotaAvailVolumes
            , volumeQuotaAvailGbCount
            ]
                |> List.filterMap identity
                |> List.minimum
