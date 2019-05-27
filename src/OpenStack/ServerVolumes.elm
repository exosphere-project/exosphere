module OpenStack.ServerVolumes exposing (requestAttachVolume, requestDetachVolume)

import Helpers.Helpers as Helpers
import Http
import Json.Decode as Decode
import Json.Encode
import OpenStack.Types as OSTypes
import Rest.Helpers exposing (openstackCredentialedRequest)
import Types.Types exposing (..)


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
    in
    openstackCredentialedRequest
        project
        Post
        (project.endpoints.nova ++ "/servers/" ++ serverUuid ++ "/os-volume_attachments")
        (Http.jsonBody body)
        (Http.expectJson (\result -> ProjectMsg (Helpers.getProjectId project) (ReceiveAttachVolume result)) (Decode.field "volumeAttachment" <| novaVolumeAttachmentDecoder))


requestDetachVolume : Project -> OSTypes.ServerUuid -> OSTypes.VolumeUuid -> Cmd Msg
requestDetachVolume project serverUuid volumeUuid =
    openstackCredentialedRequest
        project
        Delete
        (project.endpoints.nova ++ "/servers/" ++ serverUuid ++ "/os-volume_attachments/" ++ volumeUuid)
        Http.emptyBody
        (Http.expectString (\result -> ProjectMsg (Helpers.getProjectId project) (ReceiveDetachVolume result)))


novaVolumeAttachmentDecoder : Decode.Decoder OSTypes.VolumeAttachment
novaVolumeAttachmentDecoder =
    Decode.map3 OSTypes.VolumeAttachment
        (Decode.field "serverId" Decode.string)
        (Decode.field "id" Decode.string)
        (Decode.field "device" Decode.string)
