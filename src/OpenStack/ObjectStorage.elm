module OpenStack.ObjectStorage exposing
    ( ContainerName
    , ObjectName
    , Prefix
    , objectPath
    )

{-| Types + helpers for OpenStack Object Storage (Swift).

This module is **backend-agnostic at the type level**: object names, container names, and
paths can serve a Swift backend now and an S3 backend later. Swift HTTP wiring lives in
`Rest.Swift`; this module is pure data + pure helpers so it is fully unit-testable with no live
dependency.

Scope: this module only carries what the CloudShield card's **read** path needs (resolve +
fetch `manifest.json` / result objects from object storage, §3.2 / §3.3 of `phase-0-spec.md`).
The browser-facing object-storage **browser** feature (user upload/download queue) is a
separate track (`feature/object-storage`, see the bnr repo `CLAUDE.md`) and does not belong on
this branch.

PROVISIONAL: verified on devstack Swift 2.37 and live on Jetstream2 Ceph RGW (2026-07).
Keep Swift-specific assumptions isolated to the helpers below + `Rest.Swift`.

-}


{-| A Swift container name. UTF-8, ≤256 bytes, may not contain `/`.
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
