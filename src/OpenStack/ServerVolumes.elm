module OpenStack.ServerVolumes exposing (requestAttachVolume, requestDetachVolume, requestVolumeAttachments, serverCanHaveVolumeAttached, serversCanHaveVolumeAttached)

import Helpers.GetterSetters as GetterSetters
import Http
import Json.Decode as Decode
import Json.Encode
import OpenStack.Types as OSTypes
import Rest.Helpers
    exposing
        ( expectJsonWithErrorBody
        , expectVoidWithErrorBody
        , openstackCredentialedRequest
        )
import Types.Error exposing (ErrorContext, ErrorLevel(..))
import Types.HelperTypes exposing (HttpRequestMethod(..))
import Types.Project exposing (Project)
import Types.Server exposing (Server)
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))


requestVolumeAttachments : Project -> OSTypes.ServerUuid -> Cmd SharedMsg
requestVolumeAttachments project serverUuid =
    let
        errorContext =
            ErrorContext
                ("get volume attachments for server " ++ serverUuid)
                ErrorDebug
                Nothing

        resultToMsg result =
            ProjectMsg (GetterSetters.projectIdentifier project) <|
                ReceiveServerVolumeAttachments serverUuid errorContext result
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        []
        ( project.endpoints.nova, [ "servers", serverUuid, "os-volume_attachments" ], [] )
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg
            (Decode.field "volumeAttachments" <| Decode.list novaVolumeAttachmentDecoder)
        )


requestAttachVolume : Project -> OSTypes.ServerUuid -> OSTypes.VolumeUuid -> OSTypes.MetadataValue -> Cmd SharedMsg
requestAttachVolume project serverUuid volumeUuid metadata =
    let
        body =
            Json.Encode.object
                [ ( "volumeAttachment"
                  , Json.Encode.object
                        [ ( "volumeId", Json.Encode.string volumeUuid )
                        , ( "tag", Json.Encode.string <| OSTypes.volumeExoTags.deviceMetadata ++ metadata )
                        ]
                  )
                ]

        errorContext =
            ErrorContext
                ("attach volume " ++ volumeUuid ++ " to server " ++ serverUuid)
                ErrorCrit
                Nothing

        resultToMsg result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveAttachVolume errorContext ( serverUuid, volumeUuid ) result)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Post
        Nothing
        -- From v2.20, attaching a volume from an instance in SHELVED or SHELVED_OFFLOADED state is allowed.
        -- From v2.49, device tagging is allowed. The guest OS can access hardware metadata about the tagged devices.
        [ ( "X-OpenStack-Nova-API-Version", "2.49" ) ]
        ( project.endpoints.nova, [ "servers", serverUuid, "os-volume_attachments" ], [] )
        (Http.jsonBody body)
        (expectJsonWithErrorBody
            resultToMsg
            (Decode.field "volumeAttachment" <| novaVolumeAttachmentDecoder)
        )


requestDetachVolume : Project -> OSTypes.ServerUuid -> OSTypes.VolumeUuid -> Cmd SharedMsg
requestDetachVolume project serverUuid volumeUuid =
    let
        errorContext =
            ErrorContext
                ("detach volume " ++ volumeUuid ++ " from server " ++ serverUuid)
                ErrorCrit
                Nothing

        resultToMsg =
            \result ->
                ProjectMsg
                    (GetterSetters.projectIdentifier project)
                    (ReceiveDetachVolume errorContext ( serverUuid, volumeUuid ) result)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Delete
        Nothing
        -- From v2.20, detaching a volume from an instance in SHELVED or SHELVED_OFFLOADED state is allowed.
        [ ( "X-OpenStack-Nova-API-Version", "2.20" ) ]
        ( project.endpoints.nova, [ "servers", serverUuid, "os-volume_attachments", volumeUuid ], [] )
        Http.emptyBody
        (expectVoidWithErrorBody
            resultToMsg
        )


novaVolumeAttachmentDecoder : Decode.Decoder OSTypes.VolumeAttachment
novaVolumeAttachmentDecoder =
    Decode.map4 OSTypes.VolumeAttachment
        (Decode.field "volumeId" Decode.string)
        (Decode.field "serverId" Decode.string)
        (Decode.field "id" Decode.string)
        -- Device can be null when attachment is made to a shelved instance.
        (Decode.field "device" <| Decode.nullable Decode.string)


serversCanHaveVolumeAttached : List Server -> List Server
serversCanHaveVolumeAttached serverList =
    serverList
        |> List.filter
            (\s ->
                serverCanHaveVolumeAttached s
            )


serverCanHaveVolumeAttached : Server -> Bool
serverCanHaveVolumeAttached server =
    (not <|
        List.member
            server.osProps.details.openstackStatus
            [ OSTypes.ServerShelved
            , OSTypes.ServerShelvedOffloaded
            , OSTypes.ServerError
            , OSTypes.ServerSoftDeleted
            , OSTypes.ServerBuild
            ]
    )
        && server.osProps.details.lockStatus
        == OSTypes.ServerUnlocked
