module OpenStack.ObjectStorage exposing
    ( Acl
    , AclChange(..)
    , BulkDeleteResult
    , Container
    , ContainerAclUpdate
    , ContainerMetadata
    , ContainerName
    , Grantee(..)
    , ObjectListing
    , ObjectName
    , Prefix
    , SwiftObject
    , Upload
    , UploadStatus(..)
    , aclHasListings
    , aclIsPublic
    , aclToChange
    , addGrant
    , breadcrumbSegments
    , bulkDeleteBody
    , bulkDeleteMaxPerRequest
    , bulkDeleteSucceeded
    , chunkForBulkDelete
    , clearFinishedUploads
    , containerMetadataFromHeaders
    , containerNameError
    , containersDecoder
    , contentTypeForFilename
    , copyFromHeaderValue
    , directoryContentType
    , folderNameError
    , folderPlaceholderObjectName
    , hidePrefixPlaceholder
    , listingPageLimit
    , markerForNextPage
    , newFolderError
    , nextListingMarker
    , nextUploadId
    , objectContainingPrefix
    , objectListingDecoder
    , objectNameError
    , objectPath
    , parentPrefix
    , parseAcl
    , parseBulkDeleteResponse
    , parseContainersResponse
    , parseObjectListingResponse
    , parseXTimestamp
    , publicContainerUrl
    , publicObjectUrl
    , rawAclChange
    , rcloneConfigSnippet
    , readAclIsPublic
    , removeGrant
    , serializeAcl
    , setAclListings
    , setAclPublicRead
    , setUploadStatusById
    , stitchPage
    , stripPrefix
    , uploadIsFinished
    , uploadSizeError
    , uploadSizeLimitBytes
    )

{-| Pure Object Storage types, decoders, and helpers.

Swift JSON/header shapes are decoded here; HTTP wiring stays in `Rest.Swift`.

PROVISIONAL: verified on devstack Swift 2.37 and live on Jetstream2 Ceph RGW (2026-07); remaining
RGW gaps are flagged on the specific helpers below.

-}

import Dict exposing (Dict)
import Helpers.String
import Helpers.Time
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Time
import Url
import Url.Builder


containerNameMaxBytes : Int
containerNameMaxBytes =
    256


{-| Swift container names are non-empty, contain no `/`, and are capped at 256 UTF-8 bytes.
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


directoryContentType : String
directoryContentType =
    "application/directory"


{-| Pseudo-folder names follow the same single-segment, 256 UTF-8-byte constraint.
-}
folderNameError : String -> Maybe String
folderNameError name =
    if String.isEmpty name then
        Just "Folder name cannot be empty."

    else if String.contains "/" name then
        Just "Folder name cannot contain a slash (/)."

    else if Helpers.String.utf8ByteLength name > containerNameMaxBytes then
        Just ("Folder name is too long (must be at most " ++ String.fromInt containerNameMaxBytes ++ " bytes when UTF-8 encoded).")

    else
        Nothing


{-| Empty pseudo-folders are zero-byte `<prefix>/<name>/` objects with `delimiter=/` listings.
-}
folderPlaceholderObjectName : Maybe Prefix -> String -> ObjectName
folderPlaceholderObjectName maybePrefix folderName =
    Maybe.withDefault "" maybePrefix ++ folderName ++ "/"


{-| A new folder must pass the folder name rule and its placeholder object `<prefix><name>/` must fit
the object name limit, so a deep prefix leaves less room for the name.
-}
newFolderError : Maybe Prefix -> String -> Maybe String
newFolderError maybePrefix folderName =
    case folderNameError folderName of
        Just error ->
            Just error

        Nothing ->
            case objectNameError (folderPlaceholderObjectName maybePrefix folderName) of
                Just _ ->
                    Just
                        ("Folder name is too long for this location. The full path, including parent folders, can be at most "
                            ++ String.fromInt objectNameMaxBytes
                            ++ " bytes when UTF-8 encoded."
                        )

                Nothing ->
                    Nothing


type alias ContainerName =
    String


type alias ObjectName =
    String


type alias Prefix =
    String


type alias Container =
    { name : ContainerName
    , count : Int
    , bytes : Int
    }


type alias SwiftObject =
    { name : ObjectName
    , bytes : Int
    , lastModified : Time.Posix
    , contentType : String
    , hash : String
    }


type alias ObjectListing =
    { objects : List SwiftObject
    , subdirs : List Prefix
    , nextMarker : Maybe String
    }


{-| Hide the zero-byte object that represents the folder being listed.

Only a zero-byte object qualifies. An object named exactly like the prefix that carries data is a
real object a user can download, so it stays in the listing. Content type is deliberately not part
of the test: third-party tools write these placeholders with whatever content type they please, and
requiring the directory type would leave their placeholders showing as empty rows.

The root listing has no prefix, so an object whose name is genuinely empty remains visible there.

-}
hidePrefixPlaceholder : Maybe Prefix -> ObjectListing -> ObjectListing
hidePrefixPlaceholder maybePrefix listing =
    case maybePrefix of
        Just prefix ->
            { listing
                | objects =
                    List.filter
                        (\object -> not (object.name == prefix && object.bytes == 0))
                        listing.objects
            }

        Nothing ->
            listing


type alias ContainerMetadata =
    { readAcl : Maybe String
    , writeAcl : Maybe String
    , bytesUsed : Maybe Int
    , objectCount : Maybe Int
    , createdAt : Maybe Time.Posix
    , storagePolicy : Maybe String
    }


type Grantee
    = PublicReadGrantee
    | ListingsGrantee
    | OtherGrantee String


type alias Acl =
    List Grantee


{-| Unknown ACL tokens are preserved as `OtherGrantee` so managed toggles never clobber them.
-}
parseAcl : String -> Acl
parseAcl raw =
    raw
        |> String.split ","
        |> List.map String.trim
        |> List.filter (not << String.isEmpty)
        |> List.map toGrantee


toGrantee : String -> Grantee
toGrantee token =
    case token of
        ".r:*" ->
            PublicReadGrantee

        ".rlistings" ->
            ListingsGrantee

        other ->
            OtherGrantee other


granteeToString : Grantee -> String
granteeToString grantee =
    case grantee of
        PublicReadGrantee ->
            ".r:*"

        ListingsGrantee ->
            ".rlistings"

        OtherGrantee token ->
            token


{-| An empty ACL serializes to `Nothing`, the revoke signal the request layer turns into the paired
`X-Container-*` (empty) and `X-Remove-Container-*` headers.
-}
serializeAcl : Acl -> Maybe String
serializeAcl acl =
    case acl of
        [] ->
            Nothing

        _ ->
            Just (acl |> List.map granteeToString |> String.join ",")


aclIsPublic : Acl -> Bool
aclIsPublic acl =
    List.member PublicReadGrantee acl


aclHasListings : Acl -> Bool
aclHasListings acl =
    List.member ListingsGrantee acl


setAclPublicRead : Bool -> Acl -> Acl
setAclPublicRead enabled acl =
    if enabled then
        if List.member PublicReadGrantee acl then
            acl

        else
            acl ++ [ PublicReadGrantee ]

    else
        List.filter ((/=) PublicReadGrantee) acl


{-| `.rlistings` is independent from `.r:*`; toggling one never changes the other.
-}
setAclListings : Bool -> Acl -> Acl
setAclListings enabled acl =
    if enabled then
        if List.member ListingsGrantee acl then
            acl

        else
            acl ++ [ ListingsGrantee ]

    else
        List.filter ((/=) ListingsGrantee) acl


{-| Adds one principal idempotently and preserves every other grantee in order (no-clobber).
-}
addGrant : String -> Acl -> Acl
addGrant principal acl =
    if List.member (OtherGrantee principal) acl then
        acl

    else
        acl ++ [ OtherGrantee principal ]


{-| Removes only the matching principal and preserves every other grantee in order (no-clobber).
-}
removeGrant : String -> Acl -> Acl
removeGrant principal acl =
    List.filter ((/=) (OtherGrantee principal)) acl


type AclChange
    = SetAcl String
    | RemoveAcl
    | LeaveAcl


type alias ContainerAclUpdate =
    { read : AclChange
    , write : AclChange
    }


aclToChange : Acl -> AclChange
aclToChange acl =
    case serializeAcl acl of
        Just value ->
            SetAcl value

        Nothing ->
            RemoveAcl


{-| Empty raw ACL text revokes via `RemoveAcl`; non-empty text is passed through trimmed.
-}
rawAclChange : String -> AclChange
rawAclChange raw =
    let
        trimmed =
            String.trim raw
    in
    if String.isEmpty trimmed then
        RemoveAcl

    else
        SetAcl trimmed


{-| Public read requires the parsed `.r:*` grantee; `.rlistings` alone is not public read.
-}
readAclIsPublic : Maybe String -> Bool
readAclIsPublic maybeReadAcl =
    maybeReadAcl
        |> Maybe.map (parseAcl >> aclIsPublic)
        |> Maybe.withDefault False


{-| PROVISIONAL: verified on devstack Swift 2.37 and on Jetstream2 Ceph RGW (2026-07, RGW returns
the count/bytes/timestamp headers). The browser only sees them when the proxy exposes them.
-}
containerMetadataFromHeaders : Dict String String -> ContainerMetadata
containerMetadataFromHeaders headers =
    let
        lowerHeaders =
            headers
                |> Dict.toList
                |> List.map (\( k, v ) -> ( String.toLower k, v ))
                |> Dict.fromList

        get name =
            Dict.get (String.toLower name) lowerHeaders
    in
    { readAcl = get "X-Container-Read"
    , writeAcl = get "X-Container-Write"
    , bytesUsed = get "X-Container-Bytes-Used" |> Maybe.andThen String.toInt
    , objectCount = get "X-Container-Object-Count" |> Maybe.andThen String.toInt
    , createdAt = get "X-Timestamp" |> Maybe.andThen parseXTimestamp
    , storagePolicy = get "X-Storage-Policy"
    }


parseXTimestamp : String -> Maybe Time.Posix
parseXTimestamp raw =
    String.toFloat (String.trim raw)
        |> Maybe.map (\seconds -> Time.millisToPosix (round (seconds * 1000)))


type alias BulkDeleteResult =
    { numberDeleted : Int
    , numberNotFound : Int
    , responseStatus : String
    , errors : List ( String, String )
    }


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


uploadIsFinished : Upload -> Bool
uploadIsFinished upload =
    case upload.status of
        Queued ->
            False

        Uploading ->
            False

        Succeeded ->
            True

        Failed _ ->
            True

        Rejected _ ->
            True


clearFinishedUploads : List Upload -> List Upload
clearFinishedUploads uploads =
    List.filter (not << uploadIsFinished) uploads


type UploadStatus
    = Queued
    | Uploading
    | Succeeded
    | Failed String
    | Rejected String


containerDecoder : Decode.Decoder Container
containerDecoder =
    Decode.succeed Container
        |> Pipeline.required "name" Decode.string
        |> Pipeline.optional "count" Decode.int 0
        |> Pipeline.optional "bytes" Decode.int 0


containersDecoder : Decode.Decoder (List Container)
containersDecoder =
    Decode.list containerDecoder


objectDecoder : Decode.Decoder SwiftObject
objectDecoder =
    Decode.succeed SwiftObject
        |> Pipeline.required "name" Decode.string
        |> Pipeline.optional "bytes" Decode.int 0
        |> Pipeline.required "last_modified" (Decode.string |> Decode.andThen makeIso8601Decoder)
        |> Pipeline.optional "content_type" Decode.string "application/octet-stream"
        |> Pipeline.optional "hash" Decode.string ""


type ListingRow
    = RowObject SwiftObject
    | RowSubdir Prefix


listingRowDecoder : Decode.Decoder ListingRow
listingRowDecoder =
    Decode.oneOf
        [ Decode.map RowSubdir (Decode.field "subdir" Decode.string)
        , Decode.map RowObject objectDecoder
        ]


objectListingDecoder : Decode.Decoder ObjectListing
objectListingDecoder =
    Decode.list listingRowDecoder
        |> Decode.map partitionRows


partitionRows : List ListingRow -> ObjectListing
partitionRows rows =
    let
        step row acc =
            case row of
                RowObject obj ->
                    { acc | objects = obj :: acc.objects }

                RowSubdir prefix ->
                    { acc | subdirs = prefix :: acc.subdirs }
    in
    List.foldr step { objects = [], subdirs = [], nextMarker = Nothing } rows


makeIso8601Decoder : String -> Decode.Decoder Time.Posix
makeIso8601Decoder =
    Helpers.Time.makeIso8601StringToPosixDecoder



-- Swift account/container listings return 200 with a JSON array (possibly `[]`) **or** 204 No
-- Content with an empty body. `Decode.decodeString` fails on an empty string, so treat a blank
-- body as an empty listing rather than a decode error.


parseContainersResponse : String -> Result Decode.Error (List Container)
parseContainersResponse body =
    if String.trim body == "" then
        Ok []

    else
        Decode.decodeString containersDecoder body


parseObjectListingResponse : String -> Result Decode.Error ObjectListing
parseObjectListingResponse body =
    if String.trim body == "" then
        Ok { objects = [], subdirs = [], nextMarker = Nothing }

    else
        Decode.decodeString objectListingDecoder body


{-| Split object names on `/` before URL encoding so pseudo-folder separators remain path separators.
-}
objectPath : ContainerName -> ObjectName -> List String
objectPath containerName objectName =
    containerName :: String.split "/" objectName


{-| `X-Copy-From` uses the same split-then-encode path shape as object requests.
-}
copyFromHeaderValue : ContainerName -> ObjectName -> String
copyFromHeaderValue sourceContainer sourceObject =
    "/" ++ (objectPath sourceContainer sourceObject |> List.map Url.percentEncode |> String.join "/")


objectNameMaxBytes : Int
objectNameMaxBytes =
    1024


objectNameError : ObjectName -> Maybe String
objectNameError name =
    if String.isEmpty name then
        Just "Name cannot be empty."

    else if Helpers.String.utf8ByteLength name > objectNameMaxBytes then
        Just ("Name is too long (must be at most " ++ String.fromInt objectNameMaxBytes ++ " bytes when UTF-8 encoded).")

    else
        Nothing


objectContainingPrefix : ObjectName -> Maybe Prefix
objectContainingPrefix objectName =
    case List.reverse (String.split "/" objectName) of
        _ :: [] ->
            Nothing

        _ :: revInit ->
            Just (String.join "/" (List.reverse revInit) ++ "/")

        [] ->
            Nothing


stitchPage : Maybe String -> List a -> List a -> List a
stitchPage requestedMarker existing page =
    case requestedMarker of
        Nothing ->
            page

        Just _ ->
            existing ++ page


markerForNextPage : Int -> List { a | name : String } -> Maybe String
markerForNextPage limit page =
    if List.length page >= limit && limit > 0 then
        page |> List.reverse |> List.head |> Maybe.map .name

    else
        Nothing


stripPrefix : Maybe Prefix -> String -> String
stripPrefix maybePrefix name =
    case maybePrefix of
        Nothing ->
            name

        Just prefix ->
            if String.startsWith prefix name then
                String.dropLeft (String.length prefix) name

            else
                name


breadcrumbSegments : Maybe Prefix -> List ( String, Prefix )
breadcrumbSegments maybePrefix =
    case maybePrefix of
        Nothing ->
            []

        Just prefix ->
            let
                significantParts =
                    case List.reverse (String.split "/" prefix) of
                        "" :: rest ->
                            List.reverse rest

                        _ ->
                            String.split "/" prefix
            in
            significantParts
                |> List.foldl
                    (\part ( acc, cumulative ) ->
                        let
                            nextCumulative =
                                cumulative ++ part ++ "/"
                        in
                        ( acc ++ [ ( part, nextCumulative ) ], nextCumulative )
                    )
                    ( [], "" )
                |> Tuple.first


parentPrefix : Prefix -> Maybe Prefix
parentPrefix prefix =
    breadcrumbSegments (Just prefix)
        |> List.reverse
        |> List.drop 1
        |> List.head
        |> Maybe.map Tuple.second


{-| Ceph RGW silently caps Swift bulk-delete at 1024 paths per request. Use 1000 to
leave margin and match S3 multi-delete norms.
-}
bulkDeleteMaxPerRequest : Int
bulkDeleteMaxPerRequest =
    1000


listingPageLimit : Int
listingPageLimit =
    10000


bulkDeletePath : ContainerName -> ObjectName -> String
bulkDeletePath containerName objectName =
    "/" ++ (objectPath containerName objectName |> List.map Url.percentEncode |> String.join "/")


{-| Bulk delete bodies use leading-slash `/container/object` paths; Swift requires the leading slash.
-}
bulkDeleteBody : ContainerName -> List ObjectName -> String
bulkDeleteBody containerName objectNames =
    objectNames
        |> List.map (bulkDeletePath containerName)
        |> String.join "\n"


chunkForBulkDelete : List a -> List (List a)
chunkForBulkDelete items =
    case items of
        [] ->
            []

        _ ->
            List.take bulkDeleteMaxPerRequest items
                :: chunkForBulkDelete (List.drop bulkDeleteMaxPerRequest items)


{-| Swift bulk delete can return HTTP 200 with per-object failures in `Errors`; parse the body.
-}
parseBulkDeleteResponse : String -> Result Decode.Error BulkDeleteResult
parseBulkDeleteResponse body =
    if String.trim body == "" then
        Ok { numberDeleted = 0, numberNotFound = 0, responseStatus = "", errors = [] }

    else
        Decode.decodeString bulkDeleteResultDecoder body


bulkDeleteResultDecoder : Decode.Decoder BulkDeleteResult
bulkDeleteResultDecoder =
    Decode.succeed BulkDeleteResult
        |> Pipeline.optional "Number Deleted" Decode.int 0
        |> Pipeline.optional "Number Not Found" Decode.int 0
        |> Pipeline.optional "Response Status" Decode.string ""
        |> Pipeline.optional "Errors" (Decode.list bulkDeleteErrorDecoder) []


bulkDeleteErrorDecoder : Decode.Decoder ( String, String )
bulkDeleteErrorDecoder =
    Decode.map2 Tuple.pair
        (Decode.index 0 Decode.string)
        (Decode.index 1 Decode.string)


{-| Whether a bulk delete did everything it was asked to do, which takes both a 2xx status and no
per-object `Errors`. Swift answers a partly failed bulk delete with 200 and lists the casualties in
`Errors`. A body carrying no status at all decodes to a blank one, which counts as a 2xx.
-}
bulkDeleteSucceeded : BulkDeleteResult -> Bool
bulkDeleteSucceeded result =
    (result.responseStatus == "" || String.startsWith "2" result.responseStatus)
        && List.isEmpty result.errors


{-| With `delimiter=/`, the next marker is the later of the last object and last subdir prefix.
Only expose a marker when the just-received page was full.
-}
nextListingMarker : Int -> ObjectListing -> ObjectListing -> Maybe String
nextListingMarker limit receivedPage accumulatedListing =
    let
        receivedTotal =
            List.length receivedPage.objects + List.length receivedPage.subdirs
    in
    if limit > 0 && receivedTotal >= limit then
        let
            lastObject =
                accumulatedListing.objects |> List.reverse |> List.head |> Maybe.map .name

            lastSubdir =
                accumulatedListing.subdirs |> List.reverse |> List.head
        in
        case ( lastObject, lastSubdir ) of
            ( Just o, Just s ) ->
                Just (max o s)

            ( Just o, Nothing ) ->
                Just o

            ( Nothing, Just s ) ->
                Just s

            ( Nothing, Nothing ) ->
                Nothing

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


{-| PROVISIONAL: devstack Swift path-style public URLs verified; Ceph RGW shape is unverified.
-}
publicObjectUrl : String -> ContainerName -> ObjectName -> String
publicObjectUrl swiftBaseUrl containerName objectName =
    -- Url.Builder.crossOrigin does NOT percent-encode path segments (it just joins with "/"), so we
    -- encode each segment before building the public URL.
    Url.Builder.crossOrigin
        swiftBaseUrl
        (objectPath containerName objectName |> List.map Url.percentEncode)
        []


{-| PROVISIONAL: devstack Swift path-style container URLs verified; Ceph RGW shape is unverified.
-}
publicContainerUrl : String -> ContainerName -> String
publicContainerUrl swiftBaseUrl containerName =
    Url.Builder.crossOrigin
        swiftBaseUrl
        [ Url.percentEncode containerName ]
        []


{-| Path-style S3 config matches Ceph RGW/s3api; the placeholder region satisfies S3 clients.
-}
rcloneConfigSnippet : { endpoint : String, access : String, secret : String } -> String
rcloneConfigSnippet { endpoint, access, secret } =
    String.join "\n"
        [ "[exosphere]"
        , "type = s3"
        , "provider = Other"
        , "endpoint = " ++ endpoint
        , "access_key_id = " ++ access
        , "secret_access_key = " ++ secret
        , "region = us-east-1"
        , "force_path_style = true"
        ]
