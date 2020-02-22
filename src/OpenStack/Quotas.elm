module OpenStack.Quotas exposing
    ( requestComputeQuota
    , requestVolumeQuota
    )

import Error exposing (ErrorContext, ErrorLevel(..))
import Helpers.Helpers as Helpers
import Http
import Json.Decode as Decode
import OpenStack.Types as OSTypes
import Rest.Helpers exposing (openstackCredentialedRequest, resultToMsg)
import Types.Types
    exposing
        ( HttpRequestMethod(..)
        , Msg(..)
        , Project
        , ProjectSpecificMsgConstructor(..)
        )


requestComputeQuota : Project -> Cmd Msg
requestComputeQuota project =
    let
        errorContext =
            ErrorContext
                "get details of compute quota"
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsg
                errorContext
                (\quota ->
                    ProjectMsg
                        (Helpers.getProjectId project)
                        (ReceiveComputeQuota quota)
                )
    in
    openstackCredentialedRequest
        project
        Get
        Nothing
        (project.endpoints.nova ++ "/os-quota-sets/" ++ project.auth.project.uuid ++ "/detail")
        Http.emptyBody
        (Http.expectJson
            resultToMsg_
            (Decode.field "quota_set" computeQuotaDecoder)
        )


computeQuotaDecoder : Decode.Decoder OSTypes.ComputeQuota
computeQuotaDecoder =
    Decode.map3 OSTypes.ComputeQuota
        (Decode.field "cores" quotaItemDetailDecoder)
        (Decode.field "instances" quotaItemDetailDecoder)
        (Decode.field "ram" quotaItemDetailDecoder)


requestVolumeQuota : Project -> Cmd Msg
requestVolumeQuota project =
    let
        errorContext =
            ErrorContext
                "get details of volume quota"
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsg
                errorContext
                (\quota ->
                    ProjectMsg
                        (Helpers.getProjectId project)
                        (ReceiveVolumeQuota quota)
                )
    in
    openstackCredentialedRequest
        project
        Get
        Nothing
        (project.endpoints.cinder ++ "/os-quota-sets/" ++ project.auth.project.uuid ++ "?usage=True")
        Http.emptyBody
        (Http.expectJson
            resultToMsg_
            (Decode.field "quota_set" volumeQuotaDecoder)
        )


volumeQuotaDecoder : Decode.Decoder OSTypes.VolumeQuota
volumeQuotaDecoder =
    Decode.map2 OSTypes.VolumeQuota
        (Decode.field "volumes" quotaItemDetailDecoder)
        (Decode.field "gigabytes" quotaItemDetailDecoder)


quotaItemDetailDecoder : Decode.Decoder OSTypes.QuotaItemDetail
quotaItemDetailDecoder =
    let
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
    in
    Decode.map3 OSTypes.QuotaItemDetail
        (Decode.field "in_use" Decode.int)
        (Decode.field "limit" specialIntToMaybe)
        (Decode.field "reserved" Decode.int)
