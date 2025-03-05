module OpenStack.ServerVolumes exposing (requestAttachVolume, requestDetachVolume, serverCanHaveVolumeAttached, serversCanHaveVolumeAttached)

import Helpers.GetterSetters as GetterSetters
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
import Types.HelperTypes exposing (HttpRequestMethod(..))
import Types.Project exposing (Project)
import Types.Server exposing (Server)
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))


requestAttachVolume : Project -> OSTypes.ServerUuid -> OSTypes.VolumeUuid -> Cmd SharedMsg
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
                        (GetterSetters.projectIdentifier project)
                        (ReceiveAttachVolume attachment)
                )
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Post
        Nothing
        []
        ( project.endpoints.nova, [ "servers", serverUuid, "os-volume_attachments" ], [] )
        (Http.jsonBody body)
        (expectJsonWithErrorBody
            resultToMsg_
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

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\_ ->
                    ProjectMsg
                        (GetterSetters.projectIdentifier project)
                        ReceiveDetachVolume
                )
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Delete
        Nothing
        []
        ( project.endpoints.nova, [ "servers", serverUuid, "os-volume_attachments", volumeUuid ], [] )
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
