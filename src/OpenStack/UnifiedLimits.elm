module OpenStack.UnifiedLimits exposing (requestLimits, requestRegisteredLimits, requestUsages)

import Helpers.GetterSetters as GetterSetters
import Http
import Json.Decode as Decode
import OpenStack.Types as OSTypes
import Rest.Helpers exposing (expectJsonWithErrorBody, openstackCredentialedRequest)
import Types.Error exposing (ErrorContext, ErrorLevel(..))
import Types.HelperTypes exposing (HttpRequestMethod(..))
import Types.Project exposing (Project)
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))
import Url.Builder



-- Registered Limits


requestRegisteredLimits : Project -> Cmd SharedMsg
requestRegisteredLimits project =
    let
        errorContext =
            ErrorContext
                "get default resource limits"
                ErrorWarn
                Nothing

        resultToMsg result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveRegisteredLimits errorContext result)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        []
        ( project.endpoints.keystone
        , [ "registered_limits" ]
        , case project.region of
            Just region ->
                [ Url.Builder.string "region_id" region.id ]

            Nothing ->
                []
        )
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg
            (Decode.field "registered_limits" registeredLimitsDecoder)
        )


registeredLimitsDecoder : Decode.Decoder (List OSTypes.RegisteredLimit)
registeredLimitsDecoder =
    Decode.list registeredLimitDecoder


registeredLimitDecoder : Decode.Decoder OSTypes.RegisteredLimit
registeredLimitDecoder =
    Decode.map5 OSTypes.RegisteredLimit
        (Decode.field "id" Decode.string)
        (Decode.field "region_id" (Decode.nullable Decode.string))
        (Decode.field "resource_name" Decode.string)
        (Decode.field "default_limit" Decode.int)
        (Decode.field "description" (Decode.nullable Decode.string))



-- Applied Project Limits


requestLimits : Project -> Cmd SharedMsg
requestLimits project =
    let
        errorContext =
            ErrorContext
                "get applied resource limits"
                ErrorWarn
                Nothing

        resultToMsg result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveProjectLimits errorContext result)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        []
        ( project.endpoints.keystone
        , [ "limits" ]
        , case project.region of
            Just region ->
                [ Url.Builder.string "region_id" region.id ]

            Nothing ->
                []
        )
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg
            (Decode.field "limits" limitsDecoder)
        )


limitsDecoder : Decode.Decoder (List OSTypes.ProjectLimit)
limitsDecoder =
    Decode.list limitDecoder


limitDecoder : Decode.Decoder OSTypes.ProjectLimit
limitDecoder =
    Decode.map6 OSTypes.ProjectLimit
        (Decode.field "id" Decode.string)
        (Decode.field "project_id" Decode.string)
        (Decode.field "region_id" (Decode.nullable Decode.string))
        (Decode.field "resource_name" Decode.string)
        (Decode.field "resource_limit" Decode.int)
        (Decode.field "description" (Decode.nullable Decode.string))



-- Usages


requestUsages : Project -> Cmd SharedMsg
requestUsages project =
    case project.endpoints.placement of
        Just placementUrl ->
            let
                errorContext =
                    ErrorContext
                        "get resource usages"
                        ErrorWarn
                        Nothing

                resultToMsg result =
                    ProjectMsg
                        (GetterSetters.projectIdentifier project)
                        (ReceiveProjectUsages errorContext result)
            in
            openstackCredentialedRequest
                (GetterSetters.projectIdentifier project)
                Get
                -- Microversion 1.9 is required for `/usages` (with a newer map using `consumer_type` from 1.38).
                (Just "placement 1.9")
                []
                ( placementUrl
                , [ "usages" ]
                , [ Url.Builder.string "project_id" project.auth.project.uuid ]
                )
                Http.emptyBody
                (expectJsonWithErrorBody
                    resultToMsg
                    (Decode.field "usages" usagesDecoder)
                )

        Nothing ->
            Cmd.none


usagesDecoder : Decode.Decoder (List OSTypes.ProjectUsage)
usagesDecoder =
    Decode.keyValuePairs Decode.int
        |> Decode.map (List.map usageTupleToProjectUsage)


usageTupleToProjectUsage : ( String, Int ) -> OSTypes.ProjectUsage
usageTupleToProjectUsage ( resourceName, resourceUsage ) =
    OSTypes.ProjectUsage resourceName resourceUsage
