module OpenStack.ServerVolumes exposing (requestAttachVolume, requestDetachVolume)

import Helpers.Helpers as Helpers
import Http
import Json.Decode as Decode
import Json.Encode
import OpenStack.Types as OSTypes
import Rest.Helpers exposing (openstackCredentialedRequest)
import Types.HelperTypes as HelperTypes
import Types.Types exposing (..)


requestAttachVolume : Project -> Maybe HelperTypes.Url -> OSTypes.ServerUuid -> OSTypes.VolumeUuid -> Cmd Msg
requestAttachVolume project maybeProxyUrl serverUuid volumeUuid =
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
        maybeProxyUrl
        Post
        (project.endpoints.nova ++ "/servers/" ++ serverUuid ++ "/os-volume_attachments")
        (Http.jsonBody body)
        (Http.expectJson (\result -> ProjectMsg (Helpers.getProjectId project) (ReceiveAttachVolume result)) (Decode.field "volumeAttachment" <| novaVolumeAttachmentDecoder))


requestDetachVolume : Project -> Maybe HelperTypes.Url -> OSTypes.ServerUuid -> OSTypes.VolumeUuid -> Cmd Msg
requestDetachVolume project maybeProxyUrl serverUuid volumeUuid =
    openstackCredentialedRequest
        project
        maybeProxyUrl
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
