module Rest.Swift exposing
    ( requestDownloadObject
    , requestGetObjectCapped
    )

{-| HTTP wiring for OpenStack Object Storage (Swift).

Every request goes through `Rest.Helpers.openstackCredentialedRequest` (Keystone token + CORS
proxy). The base URL is the project's catalog endpoint (`project.endpoints.swift`), passed in by
the caller. Swift-specific wiring stays behind the backend-agnostic `OpenStack.ObjectStorage`
types, so an S3 implementation can live alongside it later.

PROVISIONAL: verified against devstack Swift 2.37 and live against Jetstream2 Ceph RGW (2026-07)
through an Exosphere-style proxy. A proxy must expose the Swift response headers (deployed proxies
often expose only `X-Subject-Token`) and lift its 1 MiB body cap for uploads/HEAD reads.

Scope: this module currently carries only the CloudShield card's **read** path (`GET` an object,
capped, parsed into the app as a `String` for JSON decoding — `phase-0-spec.md` §3.3/§5.5). The
browser-facing upload/download-to-disk feature is a separate track (`feature/object-storage`,
see the bnr repo `CLAUDE.md`) and does not belong on this branch.

-}

import Helpers.GetterSetters as GetterSetters
import Http
import OpenStack.ObjectStorage as ObjectStorage
import Rest.Helpers
    exposing
        ( expectBytesWithErrorBody
        , expectCappedStringWithErrorBody
        , openstackCredentialedRequest
        )
import Types.Error exposing (ErrorContext, HttpErrorWithBody)
import Types.HelperTypes exposing (HttpRequestMethod(..), Url)
import Types.Project exposing (Project)
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))
import Url


{-| `GET` an object's body, fail-closed above `capBytes` (checked on the wire before the body is
ever decoded into a `String` — see `expectCappedStringWithErrorBody`). This is the one read
primitive the CloudShield card needs: it is a **programmatic** consumer of object storage (no
upload queue, no save-to-disk), fetching `manifest.json` / result objects and handing the raw
JSON string back to `toMsg` for the card to decode.
-}
requestGetObjectCapped : Project -> Url -> ObjectStorage.ContainerName -> ObjectStorage.ObjectName -> Int -> (Result HttpErrorWithBody String -> SharedMsg) -> Cmd SharedMsg
requestGetObjectCapped project url containerName objectName capBytes toMsg =
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        []
        ( url
        , ObjectStorage.objectPath containerName objectName |> List.map Url.percentEncode
        , []
        )
        Http.emptyBody
        (expectCappedStringWithErrorBody capBytes toMsg)


requestDownloadObject : Project -> Url -> ObjectStorage.ContainerName -> ObjectStorage.ObjectName -> ErrorContext -> Cmd SharedMsg
requestDownloadObject project url containerName objectName errorContext =
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        []
        ( url
        , ObjectStorage.objectPath containerName objectName |> List.map Url.percentEncode
        , []
        )
        Http.emptyBody
        (expectBytesWithErrorBody
            (\result ->
                ProjectMsg (GetterSetters.projectIdentifier project) <|
                    ReceiveDownloadObject errorContext objectName result
            )
        )
