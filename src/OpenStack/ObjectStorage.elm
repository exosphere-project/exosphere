module OpenStack.ObjectStorage exposing
    ( ContainerName
    , ObjectName
    , Prefix
    , Upload
    , UploadStatus(..)
    , containerNameError
    , contentTypeForFilename
    , nextUploadId
    , objectNameError
    , objectPath
    , setUploadStatusById
    , uploadSizeError
    , uploadSizeLimitBytes
    )

{-| Types + helpers for OpenStack Object Storage (Swift).

This module is **backend-agnostic at the type level** for the transport subset: object names,
container names, and queued uploads can serve a Swift backend now and an S3 backend later. Swift
HTTP wiring lives in `Rest.Swift`; this module is pure data + pure helpers so it is fully
unit-testable with no live dependency.

PROVISIONAL: verified on devstack Swift 2.37 and live on Jetstream2 Ceph RGW (2026-07).
Keep Swift-specific assumptions isolated to the helpers below + `Rest.Swift`.

-}

import Helpers.String


{-| The maximum length of a Swift container name, in **encoded UTF-8 bytes**.
-}
containerNameMaxBytes : Int
containerNameMaxBytes =
    256


{-| Validate a proposed container name against Swift's rules, returning a human-readable error
message when it is invalid, or `Nothing` when it is acceptable.

Swift's constraints: the name must be non-empty, must not contain `/` (that separator delimits
the container from the object path in the URL), and must be at most 256 **bytes** when UTF-8
encoded — measured with `Helpers.String.utf8ByteLength`, NOT `String.length`, since the limit is
on bytes and a single character can be up to 4 bytes. (This is _not_ the stricter DNS-safe rule an
S3 backend would want.)

-}
containerNameError : ContainerName -> Maybe String
containerNameError name =
    if String.isEmpty name then
        Just "Name cannot be empty."

    else if String.contains "/" name then
        Just "Name cannot contain a slash (/)."

    else if Helpers.String.utf8ByteLength name > containerNameMaxBytes then
        Just ("Name is too long (must be at most " ++ String.fromInt containerNameMaxBytes ++ " bytes when UTF-8 encoded).")

    else
        Nothing


{-| A Swift container name. UTF-8, ≤256 **bytes**, may not contain `/`.
-}
type alias ContainerName =
    String


{-| A Swift object name. May contain `/` (pseudo-folders), spaces, unicode, etc.
-}
type alias ObjectName =
    String


{-| A pseudo-folder prefix used with `delimiter=/`, e.g. `"a/b/"`.
-}
type alias Prefix =
    String


{-| One entry in the browser-side upload queue. The queue is transient (never persisted) and lives on
the `Project` (not page-local) because `openstackCredentialedRequest` returns `Cmd SharedMsg`, so
upload results can only arrive in `State.State`; a page-local status would go permanently stale.

`objectName` is the FULL object name (the current pseudo-folder `prefix` prepended to the picked
file's name); `prefix` is retained so the detail page can render only the entries at the level it is
showing. Backend-agnostic: an S3 backend would reuse this shape unchanged.

`id` is a per-entry unique marker assigned at enqueue (see `nextUploadId`). It is the stale-result
guard: re-picking a file already in flight REPLACES the entry (same container/prefix/objectName) but
mints a NEW `id`, so when the superseded upload's `File.toBytes`/PUT finally completes it targets the
OLD `id`, finds no match (see `setUploadStatusById`), and is correctly ignored instead of clobbering
the replacement's status.

-}
type alias Upload =
    { id : Int
    , containerName : ContainerName
    , prefix : Maybe Prefix
    , objectName : ObjectName
    , sizeBytes : Int
    , status : UploadStatus
    }


nextUploadId : List Upload -> Int
nextUploadId uploads =
    1 + List.foldl (\upload acc -> max upload.id acc) 0 uploads


{-| Upload completions are id-guarded so superseded re-enqueues cannot update the replacement entry.
-}
setUploadStatusById : Int -> UploadStatus -> List Upload -> List Upload
setUploadStatusById id status uploads =
    List.map
        (\upload ->
            if upload.id == id then
                { upload | status = status }

            else
                upload
        )
        uploads


{-| The honest lifecycle of a queued upload. Note there is NO percentage/progress variant: `elm/http`
exposes no upload progress without a JS port (out of scope), so `Uploading` is an indeterminate
"in flight" state only. `Rejected` carries the too-large/CLI message for a file that failed the size
guard BEFORE it was ever read into memory; `Failed` carries a server/transport error reason.
-}
type UploadStatus
    = Queued
    | Uploading
    | Succeeded
    | Failed String
    | Rejected String



-- PURE HELPERS


{-| Build the URL path segments for an object, splitting the object name on `/` so each
pseudo-folder segment is encoded **separately**. Passing `"a/b.txt"` as one segment would
percent-encode the slash and address the wrong object.

`Url.Builder.crossOrigin` percent-encodes each segment, so spaces / unicode / `#` / `?` in a
name are handled correctly. Consecutive and trailing slashes are preserved as empty segments,
matching how Swift addresses such (unusual but legal) object names.

-}
objectPath : ContainerName -> ObjectName -> List String
objectPath containerName objectName =
    containerName :: String.split "/" objectName


{-| The maximum length of a Swift object name, in **encoded UTF-8 bytes**.
-}
objectNameMaxBytes : Int
objectNameMaxBytes =
    1024


{-| Validate a proposed object name (a copy/move destination), returning a human-readable error when
it is invalid, or `Nothing` when acceptable. Swift object names must be non-empty and at most 1024
**bytes** UTF-8 encoded (measured via `Helpers.String.utf8ByteLength`, NOT `String.length`). Unlike a
container name, a `/` IS allowed — it delimits pseudo-folders.
-}
objectNameError : ObjectName -> Maybe String
objectNameError name =
    if String.isEmpty name then
        Just "Name cannot be empty."

    else if Helpers.String.utf8ByteLength name > objectNameMaxBytes then
        Just ("Name is too long (must be at most " ++ String.fromInt objectNameMaxBytes ++ " bytes when UTF-8 encoded).")

    else
        Nothing


{-| Browser uploads are capped at 100 MiB; larger objects belong in CLI/rclone large-object flows.
-}
uploadSizeLimitBytes : Int
uploadSizeLimitBytes =
    100 * 1024 * 1024


{-| Check `File.size` before `File.toBytes` so oversized files are never read into memory.
-}
uploadSizeError : Int -> Maybe String
uploadSizeError sizeBytes =
    if sizeBytes <= uploadSizeLimitBytes then
        Nothing

    else
        Just "This file is larger than the 100 MiB upload limit. Upload large files with the CLI (e.g. the swift/openstack client) or rclone instead."


contentTypeForFilename : String -> String
contentTypeForFilename filename =
    let
        extension =
            case List.reverse (String.split "." filename) of
                last :: _ :: _ ->
                    String.toLower last

                _ ->
                    ""
    in
    case extension of
        "txt" ->
            "text/plain"

        "csv" ->
            "text/csv"

        "css" ->
            "text/css"

        "html" ->
            "text/html"

        "htm" ->
            "text/html"

        "json" ->
            "application/json"

        "xml" ->
            "application/xml"

        "pdf" ->
            "application/pdf"

        "zip" ->
            "application/zip"

        "gz" ->
            "application/gzip"

        "png" ->
            "image/png"

        "jpg" ->
            "image/jpeg"

        "jpeg" ->
            "image/jpeg"

        "gif" ->
            "image/gif"

        "svg" ->
            "image/svg+xml"

        "mp4" ->
            "video/mp4"

        _ ->
            "application/octet-stream"
