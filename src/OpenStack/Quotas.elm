module OpenStack.Quotas exposing
    ( computeQuotaDecoder
    , computeQuotaFlavorAvailServers
    , overallQuotaAvailServers
    , requestComputeQuota
    , requestVolumeQuota
    , volumeQuotaAvail
    , volumeQuotaDecoder
    )

import Http
import Json.Decode as Decode
import OpenStack.Types as OSTypes
import Rest.Helpers exposing (expectJsonWithErrorBody, openstackCredentialedRequest, resultToMsgErrorBody)
import Types.Error exposing (ErrorContext, ErrorLevel(..))
import Types.Types
    exposing
        ( HttpRequestMethod(..)
        , Msg(..)
        , Project
        , ProjectSpecificMsgConstructor(..)
        )



-- Compute Quota


requestComputeQuota : Project -> Cmd Msg
requestComputeQuota project =
    let
        errorContext =
            ErrorContext
                "get details of compute quota"
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\quota ->
                    ProjectMsg
                        project.auth.project.uuid
                        (ReceiveComputeQuota quota)
                )
    in
    openstackCredentialedRequest
        project.auth.project.uuid
        Get
        Nothing
        (project.endpoints.nova ++ "/limits")
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg_
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
        (Decode.map2 OSTypes.QuotaItemDetail
            (Decode.at [ "absolute", "totalFloatingIpsUsed" ] Decode.int)
            (Decode.at [ "absolute", "maxTotalFloatingIps" ] specialIntToMaybe)
        )



-- Volume Quota


requestVolumeQuota : Project -> Cmd Msg
requestVolumeQuota project =
    let
        errorContext =
            ErrorContext
                "get details of volume quota"
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\quota ->
                    ProjectMsg
                        project.auth.project.uuid
                        (ReceiveVolumeQuota quota)
                )
    in
    openstackCredentialedRequest
        project.auth.project.uuid
        Get
        Nothing
        (project.endpoints.cinder ++ "/limits")
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg_
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


computeQuotaFlavorAvailServers : OSTypes.ComputeQuota -> OSTypes.Flavor -> Maybe Int
computeQuotaFlavorAvailServers computeQuota flavor =
    -- Given a compute quota and a flavor, determine how many servers of that flavor can be launched
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
