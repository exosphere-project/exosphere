module OpenStack.Volumes exposing
    ( requestCreateVolume
    , requestDeleteVolume
    , requestUpdateVolumeName
    , requestVolumeSnapshots
    , requestVolumes
    , volumeLookup
    )

import Helpers.GetterSetters as GetterSetters
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode
import List.Extra
import OpenStack.Types as OSTypes
import RemoteData
import Rest.Helpers
    exposing
        ( expectJsonWithErrorBody
        , expectStringWithErrorBody
        , iso8601StringToPosixDecodeError
        , openstackCredentialedRequest
        , resultToMsgErrorBody
        )
import Types.Error exposing (ErrorContext, ErrorLevel(..))
import Types.HelperTypes exposing (HttpRequestMethod(..))
import Types.Project exposing (Project)
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))


requestCreateVolume : Project -> OSTypes.CreateVolumeRequest -> Cmd SharedMsg
requestCreateVolume project createVolumeRequest =
    let
        body =
            Json.Encode.object
                [ ( "volume"
                  , Json.Encode.object
                        [ ( "name", Json.Encode.string createVolumeRequest.name )
                        , ( "size", Json.Encode.int createVolumeRequest.size )
                        ]
                  )
                ]

        errorContext =
            ErrorContext
                ("create a volume of size " ++ String.fromInt createVolumeRequest.size ++ " GB")
                ErrorCrit
                (Just "Confirm that your quota has sufficient capacity to create a volume of this size, perhaps check with your cloud administrator.")

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\_ ->
                    ProjectMsg
                        (GetterSetters.projectIdentifier project)
                        ReceiveCreateVolume
                )
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Post
        Nothing
        []
        (project.endpoints.cinder ++ "/volumes")
        (Http.jsonBody body)
        (expectJsonWithErrorBody
            resultToMsg_
            (Decode.field "volume" <| volumeDecoder)
        )


requestVolumes : Project -> Cmd SharedMsg
requestVolumes project =
    let
        errorContext =
            ErrorContext
                "get a list of volumes"
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\vols ->
                    ProjectMsg
                        (GetterSetters.projectIdentifier project)
                        (ReceiveVolumes vols)
                )
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        []
        (project.endpoints.cinder ++ "/volumes/detail")
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg_
            (Decode.field "volumes" <| Decode.list volumeDecoder)
        )


requestVolumeSnapshots : Project -> Cmd SharedMsg
requestVolumeSnapshots project =
    let
        errorContext =
            ErrorContext
                "get a list of volume snapshots"
                ErrorWarn
                Nothing

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\snapshots ->
                    ProjectMsg
                        (GetterSetters.projectIdentifier project)
                        (ReceiveVolumeSnapshots snapshots)
                )
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        []
        (project.endpoints.cinder ++ "/snapshots")
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg_
            (Decode.field "snapshots" <| Decode.list volumeSnapshotDecoder)
        )


requestDeleteVolume : Project -> OSTypes.VolumeUuid -> Cmd SharedMsg
requestDeleteVolume project volumeUuid =
    let
        errorContext =
            ErrorContext
                ("delete volume " ++ volumeUuid)
                ErrorCrit
                (Just "Perhaps you are trying to delete a volume that is attached to a server? If so, please detach it first.")

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\_ -> ProjectMsg (GetterSetters.projectIdentifier project) ReceiveDeleteVolume)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Delete
        Nothing
        []
        (project.endpoints.cinder ++ "/volumes/" ++ volumeUuid)
        Http.emptyBody
        (expectStringWithErrorBody resultToMsg_)


requestUpdateVolumeName : Project -> OSTypes.VolumeUuid -> String -> Cmd SharedMsg
requestUpdateVolumeName project volumeUuid name =
    let
        body =
            Json.Encode.object
                [ ( "volume"
                  , Json.Encode.object
                        [ ( "name", Json.Encode.string name )
                        ]
                  )
                ]

        errorContext =
            ErrorContext
                ("Set name " ++ name ++ " on volume " ++ volumeUuid)
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\_ -> ProjectMsg (GetterSetters.projectIdentifier project) ReceiveUpdateVolumeName)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Put
        Nothing
        []
        (project.endpoints.cinder ++ "/volumes/" ++ volumeUuid)
        (Http.jsonBody body)
        (expectStringWithErrorBody resultToMsg_)


volumeDecoder : Decode.Decoder OSTypes.Volume
volumeDecoder =
    Decode.succeed OSTypes.Volume
        |> Pipeline.required "name" (Decode.nullable Decode.string)
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "status" (Decode.string |> Decode.andThen volumeStatusDecoder)
        |> Pipeline.required "size" Decode.int
        |> Pipeline.required "description" (Decode.nullable Decode.string)
        |> Pipeline.required "attachments" (Decode.list cinderVolumeAttachmentDecoder)
        |> Pipeline.optional "volume_image_metadata" (imageMetadataDecoder |> Decode.map Maybe.Just) Nothing
        |> Pipeline.required "created_at" (Decode.string |> Decode.andThen iso8601StringToPosixDecodeError)
        |> Pipeline.required "user_id" Decode.string


volumeSnapshotDecoder : Decode.Decoder OSTypes.VolumeSnapshot
volumeSnapshotDecoder =
    Decode.succeed OSTypes.VolumeSnapshot
        |> Pipeline.required "id" Decode.string
        |> Pipeline.optional "name" (Decode.string |> Decode.map Maybe.Just) Maybe.Nothing
        |> Pipeline.required "description" Decode.string
        |> Pipeline.required "volume_id" Decode.string
        |> Pipeline.required "size" Decode.int
        |> Pipeline.required "created_at" (Decode.string |> Decode.andThen iso8601StringToPosixDecodeError)


volumeStatusDecoder : String -> Decode.Decoder OSTypes.VolumeStatus
volumeStatusDecoder status =
    case status of
        "creating" ->
            Decode.succeed OSTypes.Creating

        "available" ->
            Decode.succeed OSTypes.Available

        "reserved" ->
            Decode.succeed OSTypes.Reserved

        "attaching" ->
            Decode.succeed OSTypes.Attaching

        "detaching" ->
            Decode.succeed OSTypes.Detaching

        "in-use" ->
            Decode.succeed OSTypes.InUse

        "maintenance" ->
            Decode.succeed OSTypes.Maintenance

        "deleting" ->
            Decode.succeed OSTypes.Deleting

        "awaiting-transfer" ->
            Decode.succeed OSTypes.AwaitingTransfer

        "error" ->
            Decode.succeed OSTypes.Error

        "error_deleting" ->
            Decode.succeed OSTypes.ErrorDeleting

        "backing-up" ->
            Decode.succeed OSTypes.BackingUp

        "restoring-backup" ->
            Decode.succeed OSTypes.RestoringBackup

        "error_backing-up" ->
            Decode.succeed OSTypes.ErrorBackingUp

        "error_restoring" ->
            Decode.succeed OSTypes.ErrorRestoring

        "error_extending" ->
            Decode.succeed OSTypes.ErrorExtending

        "downloading" ->
            Decode.succeed OSTypes.Downloading

        "uploading" ->
            Decode.succeed OSTypes.Uploading

        "retyping" ->
            Decode.succeed OSTypes.Retyping

        "extending" ->
            Decode.succeed OSTypes.Extending

        _ ->
            Decode.fail "Unrecognized volume status"


cinderVolumeAttachmentDecoder : Decode.Decoder OSTypes.VolumeAttachment
cinderVolumeAttachmentDecoder =
    Decode.map3 OSTypes.VolumeAttachment
        (Decode.field "server_id" Decode.string)
        (Decode.field "attachment_id" Decode.string)
        (Decode.field "device" Decode.string)


imageMetadataDecoder : Decode.Decoder OSTypes.NameAndUuid
imageMetadataDecoder =
    Decode.map2 OSTypes.NameAndUuid
        (Decode.field "image_name" Decode.string)
        (Decode.field "image_id" Decode.string)


volumeLookup : Project -> OSTypes.VolumeUuid -> Maybe OSTypes.Volume
volumeLookup project volumeUuid =
    {- TODO fix or justify other lookup functions being in Helpers.Helpers -}
    project.volumes
        |> RemoteData.withDefault []
        |> List.Extra.find (\v -> v.uuid == volumeUuid)
