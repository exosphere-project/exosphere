module OpenStack.Volumes exposing
    ( requestCreateVolume
    , requestDeleteVolume
    , requestUpdateVolumeName
    , requestVolumes
    , volumeLookup
    )

import Error exposing (ErrorContext, ErrorLevel(..))
import Helpers.Helpers as Helpers
import Http
import Json.Decode as Decode
import Json.Encode
import OpenStack.Types as OSTypes
import RemoteData
import Rest.Helpers exposing (openstackCredentialedRequest, resultToMsg)
import Types.Types
    exposing
        ( HttpRequestMethod(..)
        , Msg(..)
        , Project
        , ProjectSpecificMsgConstructor(..)
        )


requestCreateVolume : Project -> OSTypes.CreateVolumeRequest -> Cmd Msg
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
            resultToMsg
                errorContext
                (\_ ->
                    ProjectMsg
                        (Helpers.getProjectId project)
                        ReceiveCreateVolume
                )
    in
    openstackCredentialedRequest
        project
        Post
        (project.endpoints.cinder ++ "/volumes")
        (Http.jsonBody body)
        (Http.expectJson
            resultToMsg_
            (Decode.field "volume" <| volumeDecoder)
        )


requestVolumes : Project -> Cmd Msg
requestVolumes project =
    let
        errorContext =
            ErrorContext
                "get a list of volumes"
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsg
                errorContext
                (\vols ->
                    ProjectMsg
                        (Helpers.getProjectId project)
                        (ReceiveVolumes vols)
                )
    in
    openstackCredentialedRequest
        project
        Get
        (project.endpoints.cinder ++ "/volumes/detail")
        Http.emptyBody
        (Http.expectJson
            resultToMsg_
            (Decode.field "volumes" <| Decode.list volumeDecoder)
        )


requestDeleteVolume : Project -> OSTypes.VolumeUuid -> Cmd Msg
requestDeleteVolume project volumeUuid =
    let
        errorContext =
            ErrorContext
                ("delete volume " ++ volumeUuid)
                ErrorCrit
                (Just "Perhaps you are trying to delete a volume that is attached to a server? If so, please detach it first.")

        resultToMsg_ =
            resultToMsg
                errorContext
                (\_ -> ProjectMsg (Helpers.getProjectId project) ReceiveDeleteVolume)
    in
    openstackCredentialedRequest
        project
        Delete
        (project.endpoints.cinder ++ "/volumes/" ++ volumeUuid)
        Http.emptyBody
        (Http.expectString resultToMsg_)


requestUpdateVolumeName : Project -> OSTypes.VolumeUuid -> String -> Cmd Msg
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
            resultToMsg
                errorContext
                (\_ -> ProjectMsg (Helpers.getProjectId project) ReceiveUpdateVolumeName)
    in
    openstackCredentialedRequest
        project
        Put
        (project.endpoints.cinder ++ "/volumes/" ++ volumeUuid)
        (Http.jsonBody body)
        (Http.expectString resultToMsg_)


volumeDecoder : Decode.Decoder OSTypes.Volume
volumeDecoder =
    Decode.map7 OSTypes.Volume
        (Decode.field "name" Decode.string)
        (Decode.field "id" Decode.string)
        (Decode.field "status" (Decode.string |> Decode.andThen volumeStatusDecoder))
        (Decode.field "size" Decode.int)
        (Decode.field "description" <| Decode.nullable Decode.string)
        (Decode.field "attachments" (Decode.list cinderVolumeAttachmentDecoder))
        (Decode.maybe (Decode.field "volume_image_metadata" imageMetadataDecoder))


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
    List.filter (\v -> v.uuid == volumeUuid) (RemoteData.withDefault [] project.volumes) |> List.head
