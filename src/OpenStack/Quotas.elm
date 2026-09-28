module OpenStack.Quotas exposing
    ( ComputeQuotaResource(..)
    , VolumeQuotaResource(..)
    , computeQuotaDecoder
    , computeQuotaFlavorAvailServers
    , computeQuotaFlavorCapacities
    , computeQuotaFlavorResizeCapacities
    , exceedsQuota
    , requestComputeQuota
    , requestNetworkQuota
    , requestShareQuota
    , requestVolumeQuota
    , shareQuotaAvail
    , shareQuotaDecoder
    , volumeQuotaAvail
    , volumeQuotaDecoder
    , volumeQuotaFlavorCapacities
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


type ComputeQuotaResource
    = Cores
    | Instances
    | Ram


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
        ( project.endpoints.nova, [ "limits" ], [] )
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg
            (Decode.field "limits" computeQuotaDecoder)
        )


computeQuotaDecoder : Decode.Decoder OSTypes.ComputeQuota
computeQuotaDecoder =
    Decode.field "absolute" <|
        Decode.map4 OSTypes.ComputeQuota
            (makeQuotaItemPairDecoder "totalCoresUsed" "maxTotalCores")
            (makeQuotaItemPairDecoder "totalInstancesUsed" "maxTotalInstances")
            (makeQuotaItemPairDecoder "totalRAMUsed" "maxTotalRAMSize")
            (Decode.field "maxTotalKeypairs" Decode.int)



-- Volume Quota


type VolumeQuotaResource
    = Volumes
    | VolumeStorage


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
        ( project.endpoints.cinder, [ "limits" ], [] )
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg
            (Decode.field "limits" volumeQuotaDecoder)
        )


volumeQuotaDecoder : Decode.Decoder OSTypes.VolumeQuota
volumeQuotaDecoder =
    Decode.field "absolute" <|
        Decode.map2 OSTypes.VolumeQuota
            (makeQuotaItemPairDecoder "totalVolumesUsed" "maxTotalVolumes")
            (makeQuotaItemPairDecoder "totalGigabytesUsed" "maxTotalVolumeGigabytes")



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
        ( project.endpoints.neutron, [ "v2.0", "quotas", project.auth.project.uuid, "details.json" ], [] )
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg
            (Decode.field "quota" networkQuotaDecoder)
        )


networkQuotaDecoder : Decode.Decoder OSTypes.NetworkQuota
networkQuotaDecoder =
    Decode.map OSTypes.NetworkQuota <|
        Decode.field "floatingip" (makeQuotaItemPairDecoder "used" "limit")



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
        ( url, [ project.auth.project.uuid, "limits" ], [] )
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
            |> custom (makeQuotaItemPairDecoder "totalShareGigabytesUsed" "maxTotalShareGigabytes")
            |> custom (makeQuotaItemPairDecoder "totalShareSnapshotsUsed" "maxTotalShareSnapshots")
            |> custom (makeQuotaItemPairDecoder "totalSharesUsed" "maxTotalShares")
            |> custom (makeQuotaItemPairDecoder "totalSnapshotGigabytesUsed" "maxTotalSnapshotGigabytes")
            |> custom (maybe (makeQuotaItemPairDecoder "totalShareNetworksUsed" "maxTotalShareNetworks"))
            |> custom (maybe (makeQuotaItemPairDecoder "totalShareReplicasUsed" "maxTotalShareReplicas"))
            |> custom (maybe (makeQuotaItemPairDecoder "totalReplicaGigabytesUsed" "maxTotalReplicaGigabytes"))
            |> hardcoded Nothing
            |> hardcoded Nothing
            |> hardcoded Nothing
        )


{-| Returns tuple showing # shares, # total gigabytes & # gigabytes per share that are available given quota and usage.

Nothing implies no limit.

-}
shareQuotaAvail : OSTypes.ShareQuota -> ( OSTypes.QuotaItemLimit, OSTypes.QuotaItemLimit, OSTypes.QuotaItemLimit )
shareQuotaAvail shareQuota =
    ( shareQuota.shares.limit
        |> quotaItemLimitMap
            (\l -> l - shareQuota.shares.inUse)
    , shareQuota.gigabytes.limit
        |> quotaItemLimitMap
            (\l -> l - shareQuota.gigabytes.inUse)
    , case shareQuota.perShareGigabytes of
        Just perShareGigabytes ->
            perShareGigabytes.limit
                |> quotaItemLimitMap
                    (\l -> l)

        Nothing ->
            OSTypes.Unlimited
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


{-| Given a compute quota and a flavor, determine how many servers of that flavor can be launched.

`Nothing` means no compute quota constrains the number of servers (not that none can be launched).

-}
computeQuotaFlavorAvailServers : OSTypes.ComputeQuota -> OSTypes.Flavor -> Maybe Int
computeQuotaFlavorAvailServers computeQuota flavor =
    computeQuotaFlavorCapacities computeQuota flavor
        |> List.map .capacity
        |> List.minimum


{-| Describe how many servers of the given flavor each compute quota permits.

Every limited resource is reported, whether or not it is the one presently
binding the count, so that a user can see the constraints they are approaching
and not only the one they have reached.

-}
computeQuotaFlavorCapacities : OSTypes.ComputeQuota -> OSTypes.Flavor -> List (OSTypes.QuotaCapacity ComputeQuotaResource)
computeQuotaFlavorCapacities computeQuota flavor =
    [ quotaCapacity Cores flavor.vcpu computeQuota.cores
    , quotaCapacity Ram flavor.ram_mb computeQuota.ram
    , quotaCapacity Instances 1 computeQuota.instances
    ]
        |> List.filterMap identity


{-| Describe how many volume-backed servers of the given root disk size each volume quota permits.
-}
volumeQuotaFlavorCapacities : OSTypes.VolumeSize -> OSTypes.VolumeQuota -> List (OSTypes.QuotaCapacity VolumeQuotaResource)
volumeQuotaFlavorCapacities volumeBackedGb volumeQuota =
    [ quotaCapacity Volumes 1 volumeQuota.volumes
    , quotaCapacity VolumeStorage volumeBackedGb volumeQuota.gigabytes
    ]
        |> List.filterMap identity


{-| What a single quota permits given the consumption per operation.

An operation that does not draw on a resource cannot be constrained by it,
so it yields no capacity.

(Usage already in excess of the limit yields a capacity of zero rather than a negative count.)

-}
quotaCapacity : resource -> Int -> OSTypes.QuotaItem -> Maybe (OSTypes.QuotaCapacity resource)
quotaCapacity resource consumedPerOperation quota =
    if consumedPerOperation <= 0 then
        Nothing

    else
        case quota.limit of
            OSTypes.Limit limit ->
                Just
                    { resource = resource
                    , capacity = max 0 ((limit - quota.inUse) // consumedPerOperation)
                    , required = quota.inUse + consumedPerOperation
                    , inUse = quota.inUse
                    , limit = limit
                    }

            OSTypes.Unlimited ->
                Nothing


{-| Whether a quota leaves no room for the operation it was measured against.

Having no capacity and exceeding the quota are the same condition: an operation
fits only while the limit's remaining headroom covers what it consumes.

-}
exceedsQuota : OSTypes.QuotaCapacity resource -> Bool
exceedsQuota capacity =
    capacity.capacity == 0


{-| Describe what each compute quota permits when resizing from one flavor to another.

This implements Nova's legacy quota behavior.

Only positive changes in cores and RAM consume additional quota, so a resize
that shrinks or preserves a resource is unconstrained by it. Resizing an
existing server does not consume another instance from the instance quota.

-}
computeQuotaFlavorResizeCapacities : OSTypes.ComputeQuota -> OSTypes.Flavor -> OSTypes.Flavor -> List (OSTypes.QuotaCapacity ComputeQuotaResource)
computeQuotaFlavorResizeCapacities computeQuota currentFlavor targetFlavor =
    [ quotaCapacity Cores (targetFlavor.vcpu - currentFlavor.vcpu) computeQuota.cores
    , quotaCapacity Ram (targetFlavor.ram_mb - currentFlavor.ram_mb) computeQuota.ram
    ]
        |> List.filterMap identity


{-| Decode an OSTypes.QuotaItem from a pair of keys.
-}
makeQuotaItemPairDecoder : String -> String -> Decode.Decoder OSTypes.QuotaItem
makeQuotaItemPairDecoder usedKey totalKey =
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
