module OpenStack.Shares exposing (requestCreateAccessRule, requestCreateShare, requestDeleteShare, requestShareAccessRules, requestShareExportLocations, requestShares)

import Helpers.GetterSetters as GetterSetters
import Helpers.Time
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode
import OpenStack.Types as OSTypes
import Rest.Helpers
    exposing
        ( expectJsonWithErrorBody
        , expectStringWithErrorBody
        , openstackCredentialedRequest
        , resultToMsgErrorBody
        )
import Types.Error exposing (ErrorContext, ErrorLevel(..))
import Types.HelperTypes exposing (HttpRequestMethod(..), Url)
import Types.Project exposing (Project)
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))
import Url.Builder


requestCreateShare : Project -> Url -> OSTypes.CreateShareRequest -> Cmd SharedMsg
requestCreateShare project url createShareRequest =
    let
        body =
            Json.Encode.object
                [ ( "share"
                  , Json.Encode.object
                        [ ( "name", Json.Encode.string createShareRequest.name )
                        , ( "description", Json.Encode.string createShareRequest.description )
                        , ( "metadata", Json.Encode.dict identity Json.Encode.string createShareRequest.metadata )
                        , ( "size", Json.Encode.int createShareRequest.size )
                        , ( "share_proto", Json.Encode.string <| OSTypes.shareProtocolToString createShareRequest.protocol )
                        , ( "share_type", Json.Encode.string <| createShareRequest.shareType )
                        ]
                  )
                ]

        errorContext =
            ErrorContext
                ("create a share of size " ++ String.fromInt createShareRequest.size ++ " GB")
                ErrorCrit
                (Just "Confirm that your quota has sufficient capacity to create a share of this size, perhaps check with your cloud administrator.")

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\share ->
                    ProjectMsg
                        (GetterSetters.projectIdentifier project)
                        (ReceiveCreateShare share)
                )
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Post
        Nothing
        -- `user_id` is only returned from v2.16 onwards
        [ ( "X-OpenStack-Manila-API-Version", "2.16" ) ]
        ( url, [ "shares" ], [] )
        (Http.jsonBody body)
        (expectJsonWithErrorBody
            resultToMsg_
            (Decode.field "share" <| shareDecoder)
        )


requestCreateAccessRule : Project -> Url -> OSTypes.CreateAccessRuleRequest -> Cmd SharedMsg
requestCreateAccessRule project url createAccessRuleRequest =
    let
        body =
            Json.Encode.object
                [ ( "allow_access"
                  , Json.Encode.object
                        [ ( "access_level", Json.Encode.string <| OSTypes.accessRuleAccessLevelToApiString <| createAccessRuleRequest.accessLevel )
                        , ( "access_type", Json.Encode.string <| String.toLower <| OSTypes.accessRuleAccessTypeToString <| createAccessRuleRequest.accessType )
                        , ( "access_to", Json.Encode.string createAccessRuleRequest.accessTo )

                        -- TODO: Add metadata key-value pairs to share access rules.
                        ]
                  )
                ]

        errorContext =
            ErrorContext
                ("create access rule for share " ++ createAccessRuleRequest.shareUuid)
                ErrorCrit
                (Just "Perhaps you are trying to create a share access rule with a name that already exists?")

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\accessRule ->
                    ProjectMsg
                        (GetterSetters.projectIdentifier project)
                        (ReceiveCreateAccessRule ( createAccessRuleRequest.shareUuid, accessRule ))
                )
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Post
        Nothing
        -- Access rule metadata is supported from 2.45.
        [ ( "X-OpenStack-Manila-API-Version", "2.45" ) ]
        ( url, [ "shares", createAccessRuleRequest.shareUuid, "action" ], [] )
        (Http.jsonBody body)
        (expectJsonWithErrorBody
            resultToMsg_
            (Decode.field "access" <| accessRulesDecoder)
        )


requestShares : Project -> Url -> Cmd SharedMsg
requestShares project url =
    let
        errorContext =
            ErrorContext
                "get a list of shares"
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\shares ->
                    ProjectMsg
                        (GetterSetters.projectIdentifier project)
                        (ReceiveShares shares)
                )
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        -- `user_id` is only returned from v2.16 onwards
        [ ( "X-OpenStack-Manila-API-Version", "2.16" ) ]
        ( url, [ "shares", "detail" ], [] )
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg_
            (Decode.field "shares" <| Decode.list shareDecoder)
        )


shareDecoder : Decode.Decoder OSTypes.Share
shareDecoder =
    Decode.succeed
        OSTypes.Share
        |> Pipeline.required "name" (Decode.maybe Decode.string)
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "status" (Decode.string |> Decode.map OSTypes.stringToShareStatus)
        |> Pipeline.required "size" Decode.int
        |> Pipeline.required "description" (Decode.maybe Decode.string)
        |> Pipeline.required "metadata" (Decode.dict Decode.string)
        |> Pipeline.required "created_at" (Decode.string |> Decode.andThen Helpers.Time.makeIso8601StringToPosixDecoder)
        |> Pipeline.required "user_id" Decode.string
        |> Pipeline.required "is_public" (Decode.bool |> Decode.map OSTypes.boolToShareVisibility)
        |> Pipeline.required "share_proto" (Decode.string |> Decode.map OSTypes.stringToShareProtocol)
        |> Pipeline.required "share_type_name" Decode.string


requestShareAccessRules : Project -> Url -> OSTypes.ShareUuid -> Cmd SharedMsg
requestShareAccessRules project url shareUuid =
    let
        errorContext =
            ErrorContext
                ("get access rules of share with UUID \"" ++ shareUuid ++ "\"")
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\accessRules ->
                    ProjectMsg
                        (GetterSetters.projectIdentifier project)
                        (ReceiveShareAccessRules ( shareUuid, accessRules ))
                )
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        -- Replaces the older list share access rules API from before 2.45.
        [ ( "X-OpenStack-Manila-API-Version", "2.45" ) ]
        ( url, [ "share-access-rules" ], [ Url.Builder.string "share_id" shareUuid ] )
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg_
            (Decode.field "access_list" <| Decode.list accessRulesDecoder)
        )


accessRulesDecoder : Decode.Decoder OSTypes.AccessRule
accessRulesDecoder =
    Decode.succeed
        OSTypes.AccessRule
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "access_level" (Decode.string |> Decode.map OSTypes.stringToAccessRuleAccessLevel)
        |> Pipeline.required "access_type" (Decode.string |> Decode.map OSTypes.stringToAccessRuleAccessType)
        |> Pipeline.required "access_to" Decode.string
        |> Pipeline.required "access_key" (Decode.maybe Decode.string)
        |> Pipeline.required "state" (Decode.string |> Decode.map OSTypes.stringToAccessRuleState)
        |> Pipeline.required "created_at" (Decode.string |> Decode.andThen Helpers.Time.makeIso8601StringToPosixDecoder)


requestShareExportLocations : Project -> Url -> OSTypes.ShareUuid -> Cmd SharedMsg
requestShareExportLocations project url shareUuid =
    let
        errorContext =
            ErrorContext
                ("get export locations of share with UUID \"" ++ shareUuid ++ "\"")
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\exportLocations ->
                    ProjectMsg
                        (GetterSetters.projectIdentifier project)
                        (ReceiveShareExportLocations ( shareUuid, exportLocations ))
                )
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        -- `preferred` is returned from v2.14 onwards to identify which export locations are most efficient
        [ ( "X-OpenStack-Manila-API-Version", "2.14" ) ]
        ( url, [ "shares", shareUuid, "export_locations" ], [] )
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg_
            (Decode.field "export_locations" <| Decode.list exportLocationDecoder)
        )


exportLocationDecoder : Decode.Decoder OSTypes.ExportLocation
exportLocationDecoder =
    Decode.succeed
        OSTypes.ExportLocation
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "path" Decode.string
        |> Pipeline.required "preferred" Decode.bool


requestDeleteShare : Project -> Url -> OSTypes.ShareUuid -> Cmd SharedMsg
requestDeleteShare project url shareUuid =
    let
        errorContext =
            ErrorContext
                ("delete share " ++ shareUuid)
                ErrorCrit
                (Just "Perhaps you are trying to delete a share that has an action in progress? If so, please try again later.")

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\_ -> ProjectMsg (GetterSetters.projectIdentifier project) (ReceiveDeleteShare shareUuid))
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Delete
        Nothing
        []
        ( url, [ "shares", shareUuid ], [] )
        Http.emptyBody
        (expectStringWithErrorBody resultToMsg_)
