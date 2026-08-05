module Rest.Swift exposing
    ( requestDownloadObject
    , requestUploadObject
    )

{-| HTTP wiring for OpenStack Object Storage (Swift).

Every request goes through `Rest.Helpers.openstackCredentialedRequest` (Keystone token + CORS
proxy). The base URL is the project's catalog endpoint (`project.endpoints.swift`), passed in by
the caller. Swift-specific wiring stays behind the `OpenStack.ObjectStorage` types and `SharedMsg`
constructors.

PROVISIONAL: verified against devstack Swift 2.37 and live against Jetstream2 Ceph RGW (2026-07)
through an Exosphere-style proxy. A proxy must expose the Swift response headers (deployed proxies
often expose only `X-Subject-Token`) and lift its 1 MiB body cap for uploads/HEAD reads.

-}

import Bytes exposing (Bytes)
import Helpers.GetterSetters as GetterSetters
import Http
import OpenStack.ObjectStorage as ObjectStorage
import Rest.Helpers
    exposing
        ( expectBytesWithErrorBody
        , expectVoidWithErrorBody
        , openstackCredentialedRequest
        )
import Time
import Types.Error exposing (ErrorContext, ErrorLevel(..))
import Types.HelperTypes exposing (HttpRequestMethod(..), Url)
import Types.Project exposing (Project)
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))
import Url
import Url.Builder


{-| Upload an object with a `PUT` to `<container>/<object path>`. `objectName` is the FULL name (the
current pseudo-folder prefix already prepended by the caller), split on `/` into separate
percent-encoded URL segments exactly like the delete path.

The body is `Http.bytesBody contentType bytes`: that Content-Type header is carried by the body, and
Content-Length is set by the browser/proxy — so we deliberately set **neither** header manually
(fetch forbids setting Content-Length, and a second Content-Type would conflict). Swift replies `201
Created`; `expectVoidWithErrorBody` treats any 2xx as success. The unique upload `id` +
container/prefix/objectName ride back through `ReceiveUploadObject` so `State.State` can flip the
matching queue entry (id-guarded against a superseded re-enqueue) + refresh the listing.

-}
requestUploadObject : Project -> Url -> ObjectStorage.ContainerName -> Maybe ObjectStorage.Prefix -> ObjectStorage.ObjectName -> Int -> String -> Bytes -> Cmd SharedMsg
requestUploadObject project url containerName maybePrefix objectName uploadId contentType bytes =
    let
        errorContext =
            ErrorContext
                ("upload object " ++ objectName ++ " to container " ++ containerName)
                ErrorCrit
                Nothing

        resultToMsg_ result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveUploadObject errorContext uploadId containerName maybePrefix result)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Put
        Nothing
        []
        ( url, ObjectStorage.objectPath containerName objectName |> List.map Url.percentEncode, [] )
        (Http.bytesBody contentType bytes)
        (expectVoidWithErrorBody resultToMsg_)


{-| Fetched through the proxy because an anchor can't carry the token + proxy headers;
`State.State` hands the bytes to `File.Download.bytes`.
-}
requestDownloadObject : Project -> Url -> Time.Posix -> ObjectStorage.ContainerName -> ObjectStorage.ObjectName -> Cmd SharedMsg
requestDownloadObject project url currentTime containerName objectName =
    let
        errorContext =
            ErrorContext
                ("download object " ++ objectName ++ " in container " ++ containerName)
                ErrorCrit
                Nothing

        resultToMsg_ result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveDownloadObject errorContext objectName result)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        []
        ( url
        , ObjectStorage.objectPath containerName objectName |> List.map Url.percentEncode
        , [ Url.Builder.int "t" (Time.posixToMillis currentTime) ]
        )
        Http.emptyBody
        (expectBytesWithErrorBody resultToMsg_)
