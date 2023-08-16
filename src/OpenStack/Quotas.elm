module OpenStack.Quotas exposing
    ( computeQuotaDecoder
    , computeQuotaFlavorAvailServers
    , overallQuotaAvailServers
    , requestComputeQuota
    , requestNetworkQuota
    , requestShareQuota
    , requestVolumeQuota
    , shareQuotaDecoder
    , volumeQuotaAvail
    , volumeQuotaDecoder
    )

import Helpers.GetterSetters as GetterSetters
import Http
import Json.Decode as Decode exposing (maybe)
import Json.Decode.Pipeline exposing (custom, hardcoded)
import OpenStack.Types as OSTypes
import Rest.Helpers exposing (expectJsonWithErrorBody, openstackCredentialedRequest)
import Types.Error exposing (ErrorContext, ErrorLevel(..))
import Types.HelperTypes exposing (HttpRequestMethod(..), Url)
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
    Decode.field "absolute" <|
        Decode.map4 OSTypes.ComputeQuota
            (quotaItemPairDecoder "totalCoresUsed" "maxTotalCores")
            (quotaItemPairDecoder "totalInstancesUsed" "maxTotalInstances")
            (quotaItemPairDecoder "totalRAMUsed" "maxTotalRAMSize")
            (Decode.field "maxTotalKeypairs" Decode.int)



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
    Decode.field "absolute" <|
        Decode.map2 OSTypes.VolumeQuota
            (quotaItemPairDecoder "totalVolumesUsed" "maxTotalVolumes")
            (quotaItemPairDecoder "totalGigabytesUsed" "maxTotalVolumeGigabytes")



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
    Decode.map OSTypes.NetworkQuota <|
        Decode.field "floatingip" (quotaItemPairDecoder "used" "limit")



-- Share Quota


requestShareQuota : Project -> Url -> Cmd SharedMsg
requestShareQuota project url =
    let
        errorContext =
            ErrorContext
                "get details of share quota"
                ErrorCrit
                Nothing

        resultToMsg result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveShareQuota errorContext result)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        [ ( "X-OpenStack-Manila-API-Version", "2.42" ) ]
        (url
            ++ "/"
            ++ project.auth.project.uuid
            ++ "/limits"
        )
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg
            shareQuotaDecoder
        )



{- Hardcoded Nothing fields are configurable manila limits that are not exposed in the older microversion limits api -}


shareQuotaDecoder : Decode.Decoder OSTypes.ShareQuota
shareQuotaDecoder =
    Decode.at [ "limits", "absolute" ]
        (Decode.succeed OSTypes.ShareQuota
            |> custom (quotaItemPairDecoder "totalShareGigabytesUsed" "maxTotalShareGigabytes")
            |> custom (quotaItemPairDecoder "totalShareSnapshotsUsed" "maxTotalShareSnapshots")
            |> custom (quotaItemPairDecoder "totalSharesUsed" "maxTotalShares")
            |> custom (quotaItemPairDecoder "totalSnapshotGigabytesUsed" "maxTotalSnapshotGigabytes")
            |> custom (maybe (quotaItemPairDecoder "totalShareNetworksUsed" "maxTotalShareNetworks"))
            |> custom (maybe (quotaItemPairDecoder "totalShareReplicasUsed" "maxTotalShareReplicas"))
            |> custom (maybe (quotaItemPairDecoder "totalReplicaGigabytesUsed" "maxTotalReplicaGigabytes"))
            |> hardcoded Nothing
            |> hardcoded Nothing
            |> hardcoded Nothing
        )



-- Helpers


quotaItemLimitDecoder : Decode.Decoder OSTypes.QuotaItemLimit
quotaItemLimitDecoder =
    Decode.int
        |> Decode.map
            (\i ->
                if i == -1 then
                    OSTypes.Unlimited

                else
                    OSTypes.Limit i
            )


quotaItemLimitMap : (Int -> Int) -> OSTypes.QuotaItemLimit -> OSTypes.QuotaItemLimit
quotaItemLimitMap func limit =
    case limit of
        OSTypes.Limit l ->
            OSTypes.Limit <| func l

        OSTypes.Unlimited ->
            OSTypes.Unlimited


{-| Given a compute quota and a flavor, determine how many servers of that flavor can be launched

In the future this could use a refactor to return an OSTypes.QuotaItemLimit

-}
computeQuotaFlavorAvailServers : OSTypes.ComputeQuota -> OSTypes.Flavor -> Maybe Int
computeQuotaFlavorAvailServers computeQuota flavor =
    [ case computeQuota.cores.limit of
        OSTypes.Limit l ->
            Just <| (l - computeQuota.cores.inUse) // flavor.vcpu

        OSTypes.Unlimited ->
            Nothing
    , case computeQuota.ram.limit of
        OSTypes.Limit l ->
            Just <| (l - computeQuota.ram.inUse) // flavor.ram_mb

        OSTypes.Unlimited ->
            Nothing
    , case computeQuota.instances.limit of
        OSTypes.Limit l ->
            Just <| l - computeQuota.instances.inUse

        OSTypes.Unlimited ->
            Nothing
    ]
        |> List.filterMap identity
        |> List.minimum



{- Decode an OSTypes.QuotaItem from a pair of keys -}


quotaItemPairDecoder : String -> String -> Decode.Decoder OSTypes.QuotaItem
quotaItemPairDecoder usedKey totalKey =
    Decode.map2 OSTypes.QuotaItem
        (Decode.field usedKey Decode.int)
        (Decode.field totalKey quotaItemLimitDecoder)


{-| Returns tuple showing # volumes and # total gigabytes that are available given quota and usage.

Nothing implies no limit.

-}
volumeQuotaAvail : OSTypes.VolumeQuota -> ( OSTypes.QuotaItemLimit, OSTypes.QuotaItemLimit )
volumeQuotaAvail volumeQuota =
    -- Returns tuple showing # volumes and # total gigabytes that are available given quota and usage.
    ( volumeQuota.volumes.limit
        |> quotaItemLimitMap
            (\l -> l - volumeQuota.volumes.inUse)
    , volumeQuota.gigabytes.limit
        |> quotaItemLimitMap
            (\l -> l - volumeQuota.gigabytes.inUse)
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

                volumeQuotaAvailVolumesCount =
                    case volumeQuotaAvailVolumes of
                        OSTypes.Limit l ->
                            Just l

                        OSTypes.Unlimited ->
                            Nothing

                volumeQuotaAvailGbCount =
                    case volumeQuotaAvailGb of
                        OSTypes.Limit l ->
                            Just <| l // volBackedGb

                        OSTypes.Unlimited ->
                            Nothing
            in
            [ computeQuotaAvailServers
            , volumeQuotaAvailVolumesCount
            , volumeQuotaAvailGbCount
            ]
                |> List.filterMap identity
                |> List.minimum
