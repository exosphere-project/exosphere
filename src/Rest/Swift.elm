module Rest.Swift exposing
    ( containerPageLimit
    , postContainerMetadata
    , recursiveDeleteMaxCycles
    , requestBulkDelete
    , requestContainerMetadata
    , requestContainerObjectNames
    , requestContainers
    , requestContainersPage
    , requestCopyObject
    , requestCreateContainer
    , requestCreateFolder
    , requestDeleteContainer
    , requestDeleteContainerObject
    , requestDeleteObject
    , requestDownloadObject
    , requestObjects
    , requestUploadObject
    )

{-| HTTP wiring for OpenStack Object Storage (Swift).

Every request goes through `Rest.Helpers.openstackCredentialedRequest` (Keystone token + CORS
proxy). The base URL is the project's catalog endpoint (`project.endpoints.swift`), passed in by
the caller. Swift-specific wiring stays behind the backend-agnostic `OpenStack.ObjectStorage`
types and `SharedMsg` constructors, so an S3 implementation can live alongside it later.

PROVISIONAL: verified against devstack Swift 2.37 and live against Jetstream2 Ceph RGW (2026-07)
through an Exosphere-style proxy. A proxy must expose the Swift response headers (deployed proxies
often expose only `X-Subject-Token`) and lift its 1 MiB body cap for uploads/HEAD reads.

-}

import Bytes exposing (Bytes)
import Helpers.GetterSetters as GetterSetters
import Http
import Json.Decode
import OpenStack.ObjectStorage as ObjectStorage
import Rest.Helpers
    exposing
        ( expectBytesWithErrorBody
        , expectMetadataWithErrorBody
        , expectVoidWithErrorBody
        , httpResponseStringToResult
        , openstackCredentialedRequest
        )
import Time
import Types.Error exposing (ErrorContext, ErrorLevel(..), HttpErrorWithBody)
import Types.HelperTypes exposing (Headers, HttpRequestMethod(..), Url)
import Types.Project exposing (Project)
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))
import Url
import Url.Builder


containerPageLimit : Int
containerPageLimit =
    10000


requestContainers : Project -> Url -> Time.Posix -> Cmd SharedMsg
requestContainers project url currentTime =
    requestContainersPage project url currentTime Nothing


requestContainersPage : Project -> Url -> Time.Posix -> Maybe String -> Cmd SharedMsg
requestContainersPage project url currentTime maybeMarker =
    let
        errorContext =
            ErrorContext
                "get a list of object storage containers"
                ErrorCrit
                Nothing

        -- Swift sends Last-Modified without Cache-Control, so browser heuristic caching can stale
        -- listing reads; match Rest.AppVersion's timestamp query-param precedent.
        queryParams =
            [ Url.Builder.string "format" "json"
            , Url.Builder.int "limit" containerPageLimit
            , Url.Builder.int "t" (Time.posixToMillis currentTime)
            ]
                ++ (case maybeMarker of
                        Just marker ->
                            [ Url.Builder.string "marker" marker ]

                        Nothing ->
                            []
                   )

        resultToMsg_ result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveContainers errorContext maybeMarker result)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        []
        ( url, [], queryParams )
        Http.emptyBody
        (expectSwiftJsonOrEmpty resultToMsg_ ObjectStorage.parseContainersResponse)


expectSwiftJsonOrEmpty :
    (Result HttpErrorWithBody a -> SharedMsg)
    -> (String -> Result Json.Decode.Error a)
    -> Http.Expect SharedMsg
expectSwiftJsonOrEmpty toMsg parse =
    Http.expectStringResponse toMsg <|
        httpResponseStringToResult <|
            \body ->
                case parse body of
                    Ok value ->
                        Ok value

                    Err err ->
                        Err <| HttpErrorWithBody (Http.BadBody (Json.Decode.errorToString err)) body


{-| The recursive "delete a non-empty container" flow deletes objects a page at a time and re-lists
between pages. This bounds the number of re-list cycles so a persistent server race / error can
never spin forever; each cycle can clear up to a full listing page (`containerPageLimit`) of
objects. See the `ReceiveContainerObjectNamesForDeletion` / `ReceiveDeleteContainerObject` handlers
in `State.State`.
-}
recursiveDeleteMaxCycles : Int
recursiveDeleteMaxCycles =
    100


containerPath : ObjectStorage.ContainerName -> List String
containerPath containerName =
    [ Url.percentEncode containerName ]


requestCreateContainer : Project -> Url -> ObjectStorage.ContainerName -> Cmd SharedMsg
requestCreateContainer project url containerName =
    let
        errorContext =
            ErrorContext
                ("create object storage container " ++ containerName)
                ErrorCrit
                Nothing

        resultToMsg_ result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveCreateContainer errorContext result)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Put
        Nothing
        []
        ( url, containerPath containerName, [] )
        Http.emptyBody
        (expectVoidWithErrorBody resultToMsg_)


requestDeleteContainer : Project -> Url -> ObjectStorage.ContainerName -> Cmd SharedMsg
requestDeleteContainer project url containerName =
    let
        errorContext =
            ErrorContext
                ("delete object storage container " ++ containerName)
                ErrorCrit
                (Just "If the container is not empty, delete its objects first, then try again.")

        resultToMsg_ result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveDeleteContainer errorContext result)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Delete
        Nothing
        []
        ( url, containerPath containerName, [] )
        Http.emptyBody
        (expectSwiftDeleteOrGone resultToMsg_)


{-| List the **object names** in a container for the recursive-delete flow: a plain
`GET <container>?format=json` with **no** `delimiter`, so every ordinary object (at any depth) is
returned as a flat name. `budget` is the remaining re-list cycle allowance, threaded back through
the `ReceiveContainerObjectNamesForDeletion` message so `State.State` can stop a runaway loop.

NOTE (Out Of Scope, per plan): this cannot tell SLO/DLO manifests apart from ordinary objects, so
the recursive delete makes **no large-object guarantee** — segments of a large object may be
orphaned. That is warned about in the UI; large-object cleanup is CLI/rclone territory.

-}
requestContainerObjectNames : Project -> Url -> Time.Posix -> ObjectStorage.ContainerName -> Int -> Cmd SharedMsg
requestContainerObjectNames project url currentTime containerName budget =
    let
        errorContext =
            ErrorContext
                ("list objects in container " ++ containerName ++ " for deletion")
                ErrorCrit
                Nothing

        queryParams =
            [ Url.Builder.string "format" "json"
            , Url.Builder.int "limit" containerPageLimit
            , Url.Builder.int "t" (Time.posixToMillis currentTime)
            ]

        resultToMsg_ result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveContainerObjectNamesForDeletion errorContext containerName budget result)

        parseNames body =
            ObjectStorage.parseObjectListingResponse body
                |> Result.map (\listing -> List.map .name listing.objects)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        []
        ( url, containerPath containerName, queryParams )
        Http.emptyBody
        (expectSwiftJsonOrEmpty resultToMsg_ parseNames)


{-| Delete a single ordinary object with a `DELETE` to `<container>/<object path>`.

`204`/`404` are both success (idempotent). `budget` and `remaining` (the object names still queued
for this cycle) are threaded back through `ReceiveDeleteContainerObject` so the handler can delete
the next one without holding loop state in the model.

-}
requestDeleteContainerObject : Project -> Url -> ObjectStorage.ContainerName -> Int -> List ObjectStorage.ObjectName -> ObjectStorage.ObjectName -> Cmd SharedMsg
requestDeleteContainerObject project url containerName budget remaining objectName =
    let
        errorContext =
            ErrorContext
                ("delete object " ++ objectName ++ " in container " ++ containerName)
                ErrorCrit
                Nothing

        resultToMsg_ result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveDeleteContainerObject errorContext containerName budget remaining result)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Delete
        Nothing
        []
        ( url, ObjectStorage.objectPath containerName objectName |> List.map Url.percentEncode, [] )
        Http.emptyBody
        (expectSwiftDeleteOrGone resultToMsg_)


requestObjects : Project -> Url -> Time.Posix -> ObjectStorage.ContainerName -> Maybe ObjectStorage.Prefix -> Maybe String -> Cmd SharedMsg
requestObjects project url currentTime containerName maybePrefix maybeMarker =
    let
        errorContext =
            ErrorContext
                ("get a list of objects in container " ++ containerName)
                ErrorCrit
                Nothing

        queryParams =
            [ Url.Builder.string "format" "json"
            , Url.Builder.string "delimiter" "/"
            , Url.Builder.int "limit" ObjectStorage.listingPageLimit
            , Url.Builder.int "t" (Time.posixToMillis currentTime)
            ]
                ++ (case maybePrefix of
                        Just prefix ->
                            [ Url.Builder.string "prefix" prefix ]

                        Nothing ->
                            []
                   )
                ++ (case maybeMarker of
                        Just marker ->
                            [ Url.Builder.string "marker" marker ]

                        Nothing ->
                            []
                   )

        resultToMsg_ result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveObjectListing errorContext containerName maybePrefix maybeMarker result)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        []
        ( url, containerPath containerName, queryParams )
        Http.emptyBody
        (expectSwiftJsonOrEmpty resultToMsg_ ObjectStorage.parseObjectListingResponse)


requestDeleteObject : Project -> Url -> ObjectStorage.ContainerName -> Maybe ObjectStorage.Prefix -> ObjectStorage.ObjectName -> Cmd SharedMsg
requestDeleteObject project url containerName maybePrefix objectName =
    let
        errorContext =
            ErrorContext
                ("delete object " ++ objectName ++ " in container " ++ containerName)
                ErrorCrit
                Nothing

        resultToMsg_ result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveDeleteObject errorContext containerName maybePrefix result)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Delete
        Nothing
        []
        ( url, ObjectStorage.objectPath containerName objectName |> List.map Url.percentEncode, [] )
        Http.emptyBody
        (expectSwiftDeleteOrGone resultToMsg_)


{-| Bulk-delete a set of objects in one request: `POST <storage-url>?bulk-delete`, `Content-Type:
text/plain`, `Accept: application/json`, body = newline-separated leading-slash `/container/object`
paths with each segment percent-encoded (see `OpenStack.ObjectStorage.bulkDeleteBody`).

Swift returns **200 even on partial failure** — the `ReceiveBulkDeleteObjects` handler parses the
body (`OpenStack.ObjectStorage.parseBulkDeleteResponse`) for per-object errors rather than trusting
the status. `Accept: application/json` makes that body deterministic.

PROVISIONAL: bulk-delete is validated on native devstack Swift 2.37 but unverified on RGW; if
the endpoint is unsupported the POST surfaces a plain error (the sequential
`requestDeleteContainerObject` machinery already exists as a fallback if the gate later demands it).
The valueless `?bulk-delete` flag is sent as `bulk-delete=` (empty value) because `Url.Builder` emits
`key=value`; Swift's bulk middleware treats the parameter as present.

-}
requestBulkDelete : Project -> Url -> ObjectStorage.ContainerName -> Maybe ObjectStorage.Prefix -> List ObjectStorage.ObjectName -> Cmd SharedMsg
requestBulkDelete project url containerName maybePrefix objectNames =
    let
        errorContext =
            ErrorContext
                ("bulk-delete objects in container " ++ containerName)
                ErrorCrit
                Nothing

        resultToMsg_ result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveBulkDeleteObjects errorContext containerName maybePrefix result)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Post
        Nothing
        [ ( "Accept", "application/json" ) ]
        ( url, [], [ Url.Builder.string "bulk-delete" "" ] )
        (Http.stringBody "text/plain" (ObjectStorage.bulkDeleteBody containerName objectNames))
        (expectSwiftJsonOrEmpty resultToMsg_ ObjectStorage.parseBulkDeleteResponse)


{-| `objectName` is the full name (prefix already prepended), split on `/` into percent-encoded
URL segments. Content-Type rides on the body and Content-Length is set by the browser -- setting
either header manually breaks fetch/the proxy. The upload `id` rides back through
`ReceiveUploadObject` so `State.State` can flip the matching queue entry, id-guarded against a
superseded re-enqueue.
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


{-| Server-side **copy** an object: `PUT` to the DESTINATION object URL with an `X-Copy-From:
/source-container/source-object` header (URL-encoded, leading slash — see
`OpenStack.ObjectStorage.copyFromHeaderValue`) and an EMPTY body. Swift copies the object server-side
(no bytes through the browser). Swift replies `201 Created`; `expectVoidWithErrorBody` treats any 2xx
as success.

This is the standard Swift copy form (PUT + `X-Copy-From`), deliberately NOT a bespoke `COPY` HTTP
method, so no `HelperTypes.HttpRequestMethod` variant is added and the proxy needs no new verb — only
the `X-Copy-From` request header must be allow-listed on a legacy CORS proxy.

`isMove` rides back through `ReceiveCopyObject` so `State.State` can, on a 2xx copy, DELETE the source
(a move = copy-then-delete, sequenced — never fire-and-forget both) and refresh the affected listings.

NOTE (documented in the copy/move form footer): copying an SLO/DLO manifest copies ONLY the manifest,
not its segments. We do not HEAD each object to detect that, so the UI warns statically.

-}
requestCopyObject : Project -> Url -> ObjectStorage.ContainerName -> Maybe ObjectStorage.Prefix -> ObjectStorage.ObjectName -> ObjectStorage.ContainerName -> ObjectStorage.ObjectName -> Bool -> Cmd SharedMsg
requestCopyObject project url sourceContainer sourcePrefix sourceObject destContainer destObject isMove =
    let
        errorContext =
            ErrorContext
                ((if isMove then
                    "move object "

                  else
                    "copy object "
                 )
                    ++ sourceObject
                    ++ " to "
                    ++ destContainer
                    ++ "/"
                    ++ destObject
                )
                ErrorCrit
                Nothing

        resultToMsg_ result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveCopyObject errorContext sourceContainer sourcePrefix sourceObject destContainer destObject isMove result)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Put
        Nothing
        [ ( "X-Copy-From", ObjectStorage.copyFromHeaderValue sourceContainer sourceObject ) ]
        ( url, ObjectStorage.objectPath destContainer destObject |> List.map Url.percentEncode, [] )
        Http.emptyBody
        (expectVoidWithErrorBody resultToMsg_)


{-| Create a pseudo-folder: `PUT` a **zero-byte** object named `<prefix><name>/` (trailing slash) with
`Content-Type: application/directory` (see `OpenStack.ObjectStorage.folderPlaceholderObjectName` /
`directoryContentType`). With a `delimiter=/` listing that object comes back as a `subdir` row, so it
renders as a folder even while empty — no special client handling needed. Swift replies `201 Created`;
`expectVoidWithErrorBody` treats any 2xx as success. The container + prefix ride back through
`ReceiveCreateFolder` so `State.State` re-lists that level.

`Http.stringBody directoryContentType ""` sends the empty body AND the `application/directory`
Content-Type in one shot (Content-Length is set by the browser/proxy).

-}
requestCreateFolder : Project -> Url -> ObjectStorage.ContainerName -> Maybe ObjectStorage.Prefix -> ObjectStorage.ObjectName -> Cmd SharedMsg
requestCreateFolder project url containerName maybePrefix placeholderName =
    let
        errorContext =
            ErrorContext
                ("create folder " ++ placeholderName ++ " in container " ++ containerName)
                ErrorCrit
                Nothing

        resultToMsg_ result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveCreateFolder errorContext containerName maybePrefix result)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Put
        Nothing
        []
        ( url, ObjectStorage.objectPath containerName placeholderName |> List.map Url.percentEncode, [] )
        (Http.stringBody ObjectStorage.directoryContentType "")
        (expectVoidWithErrorBody resultToMsg_)


{-| Read a container's ACL + usage + creation-time + storage-policy with a `HEAD <container>` through the
proxy. The payload is entirely in the response HEADERS (`X-Container-Read/Write`, `X-Container-Bytes-Used`,
`X-Container-Object-Count`, `X-Timestamp`, `X-Storage-Policy`), so this uses the metadata-preserving
`expectMetadataWithErrorBody` (the JSON/void helpers discard headers) and decodes them via
`OpenStack.ObjectStorage.containerMetadataFromHeaders`. Result rides back through
`ReceiveContainerMetadata` to populate `project.objectStorageContainerMetadata`.

PROVISIONAL: a CORS proxy must EXPOSE these `X-Container-*` / `X-Timestamp` / `X-Storage-Policy`
response headers for the browser to read them. Live-confirmed on Jetstream2 (2026-07): RGW returns
the headers, but a proxy exposing only `X-Subject-Token` strips them and this metadata renders as
absent. The fix is proxy config, not code.

-}
requestContainerMetadata : Project -> Url -> Time.Posix -> ObjectStorage.ContainerName -> Cmd SharedMsg
requestContainerMetadata project url currentTime containerName =
    let
        errorContext =
            ErrorContext
                ("get access settings for object storage container " ++ containerName)
                ErrorCrit
                Nothing

        resultToMsg_ result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveContainerMetadata errorContext
                    containerName
                    (Result.map (.headers >> ObjectStorage.containerMetadataFromHeaders) result)
                )
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Head
        Nothing
        []
        ( url, containerPath containerName, [ Url.Builder.int "t" (Time.posixToMillis currentTime) ] )
        Http.emptyBody
        (expectMetadataWithErrorBody resultToMsg_)


{-| Set a container's read/write ACL with a `POST <container>` (empty body) carrying the ACL headers.
Swift replies `204 No Content` with an empty body, so `expectVoidWithErrorBody` treats any 2xx as
success. The header set is derived from the `ContainerAclUpdate`:

  - `SetAcl v` → `X-Container-Read`/`-Write: v`.
  - `RemoveAcl` → `X-Remove-Container-Read`/`-Write: true` (NEVER an empty-valued `X-Container-*`,
    which an Exosphere-style CORS proxy strips).
  - `LeaveAcl` → no header for that field (Swift only changes headers you send), so an unrelated ACL
    is never clobbered.

The container/ACL headers go through `openstackCredentialedRequest`'s `Headers` param, so the token +
proxy are never hand-rolled. Result rides back through `ReceiveSetContainerMetadata`.

-}
postContainerMetadata : Project -> Url -> ObjectStorage.ContainerName -> ObjectStorage.ContainerAclUpdate -> Cmd SharedMsg
postContainerMetadata project url containerName update =
    let
        errorContext =
            ErrorContext
                ("update access settings for object storage container " ++ containerName)
                ErrorCrit
                Nothing

        resultToMsg_ result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveSetContainerMetadata errorContext containerName result)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Post
        Nothing
        (aclUpdateHeaders update)
        ( url, containerPath containerName, [] )
        Http.emptyBody
        (expectVoidWithErrorBody resultToMsg_)


aclUpdateHeaders : ObjectStorage.ContainerAclUpdate -> Headers
aclUpdateHeaders update =
    aclChangeHeaders ( "X-Container-Read", "X-Remove-Container-Read" ) update.read
        ++ aclChangeHeaders ( "X-Container-Write", "X-Remove-Container-Write" ) update.write


aclChangeHeaders : ( String, String ) -> ObjectStorage.AclChange -> Headers
aclChangeHeaders ( setName, removeName ) change =
    case change of
        ObjectStorage.SetAcl value ->
            [ ( setName, value ) ]

        ObjectStorage.RemoveAcl ->
            [ ( removeName, "true" ) ]

        ObjectStorage.LeaveAcl ->
            []


expectSwiftDeleteOrGone : (Result HttpErrorWithBody () -> SharedMsg) -> Http.Expect SharedMsg
expectSwiftDeleteOrGone toMsg =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.GoodStatus_ _ _ ->
                    Ok ()

                Http.BadStatus_ metadata body ->
                    if metadata.statusCode == 404 then
                        Ok ()

                    else
                        Err <| HttpErrorWithBody (Http.BadStatus metadata.statusCode) body

                Http.BadUrl_ badUrl ->
                    Err <| HttpErrorWithBody (Http.BadUrl badUrl) ""

                Http.Timeout_ ->
                    Err <| HttpErrorWithBody Http.Timeout ""

                Http.NetworkError_ ->
                    Err <| HttpErrorWithBody Http.NetworkError ""
