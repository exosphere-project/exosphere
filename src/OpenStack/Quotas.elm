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
            (quotaItemDetailPairDecoder "totalCoresUsed" "maxTotalCores")
            (quotaItemDetailPairDecoder "totalInstancesUsed" "maxTotalInstances")
            (quotaItemDetailPairDecoder "totalRAMUsed" "maxTotalRAMSize")
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
            (quotaItemDetailPairDecoder "totalVolumesUsed" "maxTotalVolumes")
            (quotaItemDetailPairDecoder "totalGigabytesUsed" "maxTotalVolumeGigabytes")



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
        Decode.field "floatingip" (quotaItemDetailPairDecoder "used" "limit")



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
            |> custom (quotaItemDetailPairDecoder "totalShareGigabytesUsed" "maxTotalShareGigabytes")
            |> custom (quotaItemDetailPairDecoder "totalShareSnapshotsUsed" "maxTotalShareSnapshots")
            |> custom (quotaItemDetailPairDecoder "totalSharesUsed" "maxTotalShares")
            |> custom (quotaItemDetailPairDecoder "totalSnapshotGigabytesUsed" "maxTotalSnapshotGigabytes")
            |> custom (maybe (quotaItemDetailPairDecoder "totalShareNetworksUsed" "maxTotalShareNetworks"))
            |> custom (maybe (quotaItemDetailPairDecoder "totalShareReplicasUsed" "maxTotalShareReplicas"))
            |> custom (maybe (quotaItemDetailPairDecoder "totalReplicaGigabytesUsed" "maxTotalReplicaGigabytes"))
            |> hardcoded Nothing
            |> hardcoded Nothing
            |> hardcoded Nothing
        )



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



{- Decode an OSTypes.QuotaItemDetail from a pair of keys -}


quotaItemDetailPairDecoder : String -> String -> Decode.Decoder OSTypes.QuotaItemDetail
quotaItemDetailPairDecoder usedKey totalKey =
    Decode.map2 OSTypes.QuotaItemDetail
        (Decode.field usedKey Decode.int)
        (Decode.field totalKey specialIntToMaybe)


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
