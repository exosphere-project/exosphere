module OpenStack.ServerVolumes exposing (requestAttachVolume, requestDetachVolume)

import Http
import Json.Decode as Decode
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
import Types.Msg exposing (Msg(..), ProjectSpecificMsgConstructor(..), ServerSpecificMsgConstructor(..))
import Types.Project exposing (Project)
import Types.Types exposing (HttpRequestMethod(..))


requestAttachVolume : Project -> OSTypes.ServerUuid -> OSTypes.VolumeUuid -> Cmd Msg
requestAttachVolume project serverUuid volumeUuid =
    let
        body =
            Json.Encode.object
                [ ( "volumeAttachment"
                  , Json.Encode.object
                        [ ( "volumeId", Json.Encode.string volumeUuid )
                        ]
                  )
                ]

        errorContext =
            ErrorContext
                ("attach volume " ++ volumeUuid ++ " to server " ++ serverUuid)
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\attachment ->
                    ProjectMsg
                        project.auth.project.uuid
                        (ReceiveAttachVolume attachment)
                )
    in
    openstackCredentialedRequest
        project.auth.project.uuid
        Post
        Nothing
        (project.endpoints.nova ++ "/servers/" ++ serverUuid ++ "/os-volume_attachments")
        (Http.jsonBody body)
        (expectJsonWithErrorBody
            resultToMsg_
            (Decode.field "volumeAttachment" <| novaVolumeAttachmentDecoder)
        )


requestDetachVolume : Project -> OSTypes.ServerUuid -> OSTypes.VolumeUuid -> Cmd Msg
requestDetachVolume project serverUuid volumeUuid =
    let
        errorContext =
            ErrorContext
                ("detach volume " ++ volumeUuid ++ " from server " ++ serverUuid)
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\_ ->
                    ProjectMsg
                        project.auth.project.uuid
                        ReceiveDetachVolume
                )
    in
    openstackCredentialedRequest
        project.auth.project.uuid
        Delete
        Nothing
        (project.endpoints.nova ++ "/servers/" ++ serverUuid ++ "/os-volume_attachments/" ++ volumeUuid)
        Http.emptyBody
        (expectStringWithErrorBody
            resultToMsg_
        )


novaVolumeAttachmentDecoder : Decode.Decoder OSTypes.VolumeAttachment
novaVolumeAttachmentDecoder =
    Decode.map3 OSTypes.VolumeAttachment
        (Decode.field "serverId" Decode.string)
        (Decode.field "id" Decode.string)
        (Decode.field "device" Decode.string)
