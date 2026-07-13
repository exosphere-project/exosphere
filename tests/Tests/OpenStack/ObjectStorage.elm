module Tests.OpenStack.ObjectStorage exposing
    ( aclNoClobberSuite
    , aclRevokeSuite
    , aclRoundTripSuite
    , breadcrumbSegmentsSuite
    , bulkDeleteBodySuite
    , bulkDeleteResponseSuite
    , clearFinishedUploadsSuite
    , containerListingDecoderSuite
    , containerMetadataFromHeadersSuite
    , containerNameValidationSuite
    , contentTypeForFilenameSuite
    , copyFromHeaderValueSuite
    , emptyBodyToleranceSuite
    , folderNameErrorSuite
    , folderPlaceholderObjectNameSuite
    , grantEditorSuite
    , markerPaginationSuite
    , nextListingMarkerSuite
    , objectContainingPrefixSuite
    , objectListingDecoderSuite
    , objectNameErrorSuite
    , objectPathSuite
    , parentPrefixSuite
    , popconfirmIdCollisionSuite
    , publicContainerUrlSuite
    , rawAclChangeSuite
    , rcloneConfigSnippetSuite
    , readAclIsPublicSuite
    , stitchPageSuite
    , stripPrefixSuite
    , uploadIdGuardSuite
    , uploadSizeGuardSuite
    , xTimestampParseSuite
    )

import Dict
import Expect
import Json.Decode
import OpenStack.ObjectStorage as ObjectStorage exposing (Grantee(..), UploadStatus(..))
import Page.ObjectStorageList as ObjectStorageList
import Test exposing (Test, describe, test)
import Time


containerNameValidationSuite : Test
containerNameValidationSuite =
    describe "containerNameError enforces the Swift name rule (UTF-8 <=256 bytes, no /, non-empty)"
        [ test "a normal name is accepted" <|
            \_ ->
                Expect.equal Nothing (ObjectStorage.containerNameError "documents")
        , test "256 ASCII characters (256 bytes) is accepted" <|
            \_ ->
                Expect.equal Nothing (ObjectStorage.containerNameError (String.repeat 256 "a"))
        , test "257 ASCII characters (257 bytes) is rejected" <|
            \_ ->
                Expect.notEqual Nothing (ObjectStorage.containerNameError (String.repeat 257 "a"))
        , test "85 three-byte CJK chars (255 bytes) is accepted" <|
            \_ ->
                Expect.equal Nothing (ObjectStorage.containerNameError (String.repeat 85 "中"))
        , test "86 three-byte CJK chars (258 bytes) is rejected — proves BYTES not String.length" <|
            \_ ->
                Expect.notEqual Nothing (ObjectStorage.containerNameError (String.repeat 86 "中"))
        , test "a name containing '/' is rejected" <|
            \_ ->
                Expect.notEqual Nothing (ObjectStorage.containerNameError "a/b")
        , test "a name with a leading '/' is rejected" <|
            \_ ->
                Expect.notEqual Nothing (ObjectStorage.containerNameError "/leading")
        , test "an empty name is rejected" <|
            \_ ->
                Expect.notEqual Nothing (ObjectStorage.containerNameError "")
        ]


accountListingFixture : String
accountListingFixture =
    """
    [
      { "name": "documents", "count": 3, "bytes": 12345, "last_modified": "2026-01-02T03:04:05.000000" },
      { "name": "backups", "count": 0, "bytes": 0, "last_modified": "2026-01-01T00:00:00.000000" },
      { "name": "with spaces", "count": 1, "bytes": 7 }
    ]
    """


containerListingFixture : String
containerListingFixture =
    """
    [
      { "subdir": "photos/" },
      { "name": "readme.txt", "bytes": 42, "last_modified": "2026-02-03T04:05:06.000000", "content_type": "text/plain", "hash": "abc123" },
      { "subdir": "logs/" },
      { "name": "data.bin", "bytes": 1024, "last_modified": "2026-02-03T04:05:07.000000", "content_type": "application/octet-stream", "hash": "def456" }
    ]
    """


containerListingDecoderSuite : Test
containerListingDecoderSuite =
    describe "containersDecoder decodes an account (container) listing"
        [ test "decodes all containers with name/count/bytes" <|
            \_ ->
                case ObjectStorage.parseContainersResponse accountListingFixture of
                    Ok containers ->
                        Expect.equal
                            [ ( "documents", 3, 12345 )
                            , ( "backups", 0, 0 )
                            , ( "with spaces", 1, 7 )
                            ]
                            (List.map (\c -> ( c.name, c.count, c.bytes )) containers)

                    Err e ->
                        Expect.fail ("decode failed: " ++ Json.Decode.errorToString e)
        , test "tolerates a container row missing count/bytes (defaults to 0)" <|
            \_ ->
                case ObjectStorage.parseContainersResponse """[ { "name": "minimal" } ]""" of
                    Ok [ c ] ->
                        Expect.equal ( "minimal", 0, 0 ) ( c.name, c.count, c.bytes )

                    _ ->
                        Expect.fail "expected exactly one container"
        ]


objectListingDecoderSuite : Test
objectListingDecoderSuite =
    describe "objectListingDecoder splits object rows from subdir pseudo-folders"
        [ test "collects objects, preserving order" <|
            \_ ->
                case ObjectStorage.parseObjectListingResponse containerListingFixture of
                    Ok listing ->
                        Expect.equal [ "readme.txt", "data.bin" ] (List.map .name listing.objects)

                    Err e ->
                        Expect.fail ("decode failed: " ++ Json.Decode.errorToString e)
        , test "collects subdir prefixes, preserving order" <|
            \_ ->
                case ObjectStorage.parseObjectListingResponse containerListingFixture of
                    Ok listing ->
                        Expect.equal [ "photos/", "logs/" ] listing.subdirs

                    Err e ->
                        Expect.fail ("decode failed: " ++ Json.Decode.errorToString e)
        , test "decodes object content_type + bytes" <|
            \_ ->
                case ObjectStorage.parseObjectListingResponse containerListingFixture of
                    Ok listing ->
                        Expect.equal
                            [ ( "readme.txt", 42, "text/plain" )
                            , ( "data.bin", 1024, "application/octet-stream" )
                            ]
                            (List.map (\o -> ( o.name, o.bytes, o.contentType )) listing.objects)

                    Err e ->
                        Expect.fail ("decode failed: " ++ Json.Decode.errorToString e)
        ]


emptyBodyToleranceSuite : Test
emptyBodyToleranceSuite =
    describe "listing parsers treat 200-empty-array and 204-empty-body alike"
        [ test "empty body (204 No Content) -> empty container list, not an error" <|
            \_ ->
                Expect.equal (Ok []) (ObjectStorage.parseContainersResponse "")
        , test "whitespace-only body -> empty container list" <|
            \_ ->
                Expect.equal (Ok []) (ObjectStorage.parseContainersResponse "   \n  ")
        , test "empty JSON array (200) -> empty container list" <|
            \_ ->
                Expect.equal (Ok []) (ObjectStorage.parseContainersResponse "[]")
        , test "empty body -> empty object listing" <|
            \_ ->
                Expect.equal
                    (Ok { objects = [], subdirs = [], nextMarker = Nothing })
                    (ObjectStorage.parseObjectListingResponse "")
        ]


stitchPageSuite : Test
stitchPageSuite =
    describe "stitchPage resets on the first page and appends thereafter"
        [ test "first page (marker Nothing) REPLACES stale data — a refresh must not duplicate" <|
            \_ ->
                Expect.equal [ "fresh1", "fresh2" ]
                    (ObjectStorage.stitchPage Nothing [ "stale1", "stale2" ] [ "fresh1", "fresh2" ])
        , test "a continuation page (marker Just) appends onto the accumulated data" <|
            \_ ->
                Expect.equal [ "page1a", "page1b", "page2a" ]
                    (ObjectStorage.stitchPage (Just "page1b") [ "page1a", "page1b" ] [ "page2a" ])
        , test "first page onto empty cache is just the page" <|
            \_ ->
                Expect.equal [ "a", "b" ] (ObjectStorage.stitchPage Nothing [] [ "a", "b" ])
        ]


markerPaginationSuite : Test
markerPaginationSuite =
    let
        mk names =
            List.map (\n -> { name = n, count = 0, bytes = 0 }) names
    in
    describe "markerForNextPage drives the Swift marker loop"
        [ test "a full page (length == limit) yields the last name as the next marker" <|
            \_ ->
                Expect.equal (Just "c") (ObjectStorage.markerForNextPage 3 (mk [ "a", "b", "c" ]))
        , test "a short page (length < limit) yields Nothing (done)" <|
            \_ ->
                Expect.equal Nothing (ObjectStorage.markerForNextPage 3 (mk [ "a", "b" ]))
        , test "an exactly-full page across a stitch: page two continues from page one's marker" <|
            \_ ->
                let
                    pageOneMarker =
                        ObjectStorage.markerForNextPage 2 (mk [ "a", "b" ])

                    pageTwoMarker =
                        ObjectStorage.markerForNextPage 2 (mk [ "c" ])
                in
                Expect.equal ( Just "b", Nothing ) ( pageOneMarker, pageTwoMarker )
        , test "limit 0 yields Nothing (guards against an infinite loop)" <|
            \_ ->
                Expect.equal Nothing (ObjectStorage.markerForNextPage 0 (mk [ "a" ]))
        , test "an empty page yields Nothing" <|
            \_ ->
                Expect.equal Nothing (ObjectStorage.markerForNextPage 3 (mk []))
        ]


objectPathSuite : Test
objectPathSuite =
    describe "objectPath splits object names on / into separate URL segments"
        [ test "a simple object becomes [container, name]" <|
            \_ ->
                Expect.equal [ "mycontainer", "file.txt" ] (ObjectStorage.objectPath "mycontainer" "file.txt")
        , test "a pseudo-folder path splits each segment" <|
            \_ ->
                Expect.equal [ "c", "a", "b", "file.txt" ] (ObjectStorage.objectPath "c" "a/b/file.txt")
        , test "spaces, #, ? are kept in the segment (Url.Builder encodes them later)" <|
            \_ ->
                Expect.equal [ "c", "a", "file name #1?.txt" ] (ObjectStorage.objectPath "c" "a/file name #1?.txt")
        , test "unicode segments are preserved" <|
            \_ ->
                Expect.equal [ "c", "café", "£.txt" ] (ObjectStorage.objectPath "c" "café/£.txt")
        , test "a trailing slash yields a trailing empty segment" <|
            \_ ->
                Expect.equal [ "c", "a", "b", "" ] (ObjectStorage.objectPath "c" "a/b/")
        , test "consecutive slashes yield empty segments" <|
            \_ ->
                Expect.equal [ "c", "a", "", "b" ] (ObjectStorage.objectPath "c" "a//b")
        ]


stripPrefixSuite : Test
stripPrefixSuite =
    describe "stripPrefix derives folder/leaf display names under the current prefix"
        [ test "a subdir under a prefix keeps its trailing slash, prefix stripped" <|
            \_ ->
                Expect.equal "c/" (ObjectStorage.stripPrefix (Just "a/b/") "a/b/c/")
        , test "an object under a prefix becomes its leaf name" <|
            \_ ->
                Expect.equal "x.txt" (ObjectStorage.stripPrefix (Just "a/b/") "a/b/x.txt")
        , test "at the top level (Nothing prefix) the name is unchanged" <|
            \_ ->
                Expect.equal "photos/" (ObjectStorage.stripPrefix Nothing "photos/")
        , test "an object at the top level is unchanged" <|
            \_ ->
                Expect.equal "readme.txt" (ObjectStorage.stripPrefix Nothing "readme.txt")
        , test "unicode + space names survive stripping" <|
            \_ ->
                Expect.equal "日本.txt" (ObjectStorage.stripPrefix (Just "a/b c/") "a/b c/日本.txt")
        , test "a name not under the prefix is returned unchanged (defensive)" <|
            \_ ->
                Expect.equal "other/thing" (ObjectStorage.stripPrefix (Just "a/b/") "other/thing")
        ]


breadcrumbSegmentsSuite : Test
breadcrumbSegmentsSuite =
    describe "breadcrumbSegments turns a prefix into cumulative (label, prefix) crumbs"
        [ test "a deep prefix with spaces + unicode yields cumulative crumbs" <|
            \_ ->
                Expect.equal
                    [ ( "a", "a/" )
                    , ( "b c", "a/b c/" )
                    , ( "日本", "a/b c/日本/" )
                    ]
                    (ObjectStorage.breadcrumbSegments (Just "a/b c/日本/"))
        , test "Nothing prefix yields no crumbs" <|
            \_ ->
                Expect.equal [] (ObjectStorage.breadcrumbSegments Nothing)
        , test "an empty prefix yields no crumbs" <|
            \_ ->
                Expect.equal [] (ObjectStorage.breadcrumbSegments (Just ""))
        , test "a single-level prefix yields one crumb" <|
            \_ ->
                Expect.equal [ ( "photos", "photos/" ) ] (ObjectStorage.breadcrumbSegments (Just "photos/"))
        , test "consecutive slashes keep an interior empty crumb so cumulative prefixes stay exact" <|
            \_ ->
                Expect.equal
                    [ ( "a", "a/" ), ( "", "a//" ), ( "b", "a//b/" ) ]
                    (ObjectStorage.breadcrumbSegments (Just "a//b/"))
        ]


parentPrefixSuite : Test
parentPrefixSuite =
    describe "parentPrefix walks one pseudo-folder level up (container root = Nothing)"
        [ test "a two-segment prefix's parent is the one-segment prefix" <|
            \_ ->
                Expect.equal (Just "a/") (ObjectStorage.parentPrefix "a/b/")
        , test "a single-segment prefix's parent is the container root (Nothing)" <|
            \_ ->
                Expect.equal Nothing (ObjectStorage.parentPrefix "a/")
        , test "a deeper (three-segment) prefix drops only the last segment" <|
            \_ ->
                Expect.equal (Just "a/b/") (ObjectStorage.parentPrefix "a/b/c/")
        , test "an interior-empty prefix (a//b/) keeps the empty segment in its parent (a//)" <|
            \_ ->
                Expect.equal (Just "a//") (ObjectStorage.parentPrefix "a//b/")
        , test "spaces + unicode segments round-trip through the parent computation" <|
            \_ ->
                Expect.equal (Just "a/b c/") (ObjectStorage.parentPrefix "a/b c/日本/")
        ]


bulkDeleteBodySuite : Test
bulkDeleteBodySuite =
    describe "bulkDeleteBody builds leading-slash, per-segment-URL-encoded, newline-joined paths"
        [ test "each path is /container/object with every segment percent-encoded" <|
            \_ ->
                Expect.equal
                    "/c/readme.txt\n/c/logs/app.log"
                    (ObjectStorage.bulkDeleteBody "c" [ "readme.txt", "logs/app.log" ])
        , test "spaces, unicode, #, ? are percent-encoded per segment (slashes kept as separators)" <|
            \_ ->
                Expect.equal
                    "/c/a/file%20name%20%231%3F.txt"
                    (ObjectStorage.bulkDeleteBody "c" [ "a/file name #1?.txt" ])
        , test "a container name with a space is encoded too" <|
            \_ ->
                Expect.equal
                    "/my%20container/x.txt"
                    (ObjectStorage.bulkDeleteBody "my container" [ "x.txt" ])
        , test "an empty selection yields an empty body" <|
            \_ ->
                Expect.equal "" (ObjectStorage.bulkDeleteBody "c" [])
        , test "chunkForBulkDelete caps requests below RGW's silent 1024-path limit" <|
            \_ ->
                Expect.equal 1000 ObjectStorage.bulkDeleteMaxPerRequest
        , test "chunkForBulkDelete at the cap boundary: exactly the cap is one chunk" <|
            \_ ->
                Expect.equal 1 (List.length (ObjectStorage.chunkForBulkDelete (List.repeat ObjectStorage.bulkDeleteMaxPerRequest "x")))
        , test "chunkForBulkDelete just over the cap splits into two chunks" <|
            \_ ->
                let
                    chunks =
                        ObjectStorage.chunkForBulkDelete (List.repeat (ObjectStorage.bulkDeleteMaxPerRequest + 1) "x")
                in
                Expect.equal [ ObjectStorage.bulkDeleteMaxPerRequest, 1 ] (List.map List.length chunks)
        , test "chunkForBulkDelete splits a full recursive listing page into RGW-safe requests" <|
            \_ ->
                let
                    objectNames =
                        List.range 1 ObjectStorage.listingPageLimit
                            |> List.map (\n -> "object-" ++ String.fromInt n)

                    chunks =
                        ObjectStorage.chunkForBulkDelete objectNames
                in
                Expect.all
                    [ \chunks_ ->
                        Expect.equal ObjectStorage.listingPageLimit
                            (chunks_
                                |> List.map List.length
                                |> List.sum
                            )
                    , \chunks_ ->
                        Expect.equal True
                            (chunks_
                                |> List.all (\chunk -> List.length chunk <= ObjectStorage.bulkDeleteMaxPerRequest)
                            )
                    ]
                    chunks
        ]


bulkDeleteResponseSuite : Test
bulkDeleteResponseSuite =
    describe "parseBulkDeleteResponse surfaces per-object errors from the 200 body (Swift 200s even on partial failure)"
        [ test "a partial-failure body surfaces the per-object error (path + status) with correct counts" <|
            \_ ->
                let
                    body =
                        """{"Number Deleted":2,"Number Not Found":1,"Response Status":"400 Bad Request","Response Body":"","Errors":[["/c/locked.txt","403 Forbidden"]]}"""
                in
                case ObjectStorage.parseBulkDeleteResponse body of
                    Ok result ->
                        Expect.equal
                            ( 2, 1, [ ( "/c/locked.txt", "403 Forbidden" ) ] )
                            ( result.numberDeleted, result.numberNotFound, result.errors )

                    Err e ->
                        Expect.fail ("decode failed: " ++ Json.Decode.errorToString e)
        , test "an all-success body yields no errors" <|
            \_ ->
                let
                    body =
                        """{"Number Deleted":3,"Number Not Found":0,"Response Status":"200 OK","Errors":[]}"""
                in
                case ObjectStorage.parseBulkDeleteResponse body of
                    Ok result ->
                        Expect.equal ( 3, [] ) ( result.numberDeleted, result.errors )

                    Err e ->
                        Expect.fail ("decode failed: " ++ Json.Decode.errorToString e)
        , test "an empty body is tolerated as a zeroed result (no error)" <|
            \_ ->
                case ObjectStorage.parseBulkDeleteResponse "" of
                    Ok result ->
                        Expect.equal ( 0, 0, [] ) ( result.numberDeleted, result.numberNotFound, result.errors )

                    Err e ->
                        Expect.fail ("decode failed: " ++ Json.Decode.errorToString e)
        , test "bulkDeleteStatusOk: a 2xx body status is OK" <|
            \_ ->
                Expect.equal True
                    (ObjectStorage.bulkDeleteStatusOk
                        { numberDeleted = 3, numberNotFound = 0, responseStatus = "200 OK", errors = [] }
                    )
        , test "bulkDeleteStatusOk: a blank body status is tolerated as OK" <|
            \_ ->
                Expect.equal True
                    (ObjectStorage.bulkDeleteStatusOk
                        { numberDeleted = 0, numberNotFound = 0, responseStatus = "", errors = [] }
                    )
        , test "bulkDeleteStatusOk: a whole-request failure (400 in the body, empty Errors) is NOT OK" <|
            \_ ->
                Expect.equal False
                    (ObjectStorage.bulkDeleteStatusOk
                        { numberDeleted = 0, numberNotFound = 0, responseStatus = "400 Bad Request", errors = [] }
                    )
        ]


contentTypeForFilenameSuite : Test
contentTypeForFilenameSuite =
    describe "contentTypeForFilename maps an extension (case-insensitively) to a MIME type"
        [ test "readme.txt -> text/plain" <|
            \_ ->
                Expect.equal "text/plain" (ObjectStorage.contentTypeForFilename "readme.txt")
        , test "photo.JPG (uppercase) -> image/jpeg" <|
            \_ ->
                Expect.equal "image/jpeg" (ObjectStorage.contentTypeForFilename "photo.JPG")
        , test "data.json -> application/json" <|
            \_ ->
                Expect.equal "application/json" (ObjectStorage.contentTypeForFilename "data.json")
        , test "page.html -> text/html" <|
            \_ ->
                Expect.equal "text/html" (ObjectStorage.contentTypeForFilename "page.html")
        , test "img.png -> image/png" <|
            \_ ->
                Expect.equal "image/png" (ObjectStorage.contentTypeForFilename "img.png")
        , test "report.pdf -> application/pdf" <|
            \_ ->
                Expect.equal "application/pdf" (ObjectStorage.contentTypeForFilename "report.pdf")
        , test "archive.zip -> application/zip" <|
            \_ ->
                Expect.equal "application/zip" (ObjectStorage.contentTypeForFilename "archive.zip")
        , test "clip.mp4 -> video/mp4" <|
            \_ ->
                Expect.equal "video/mp4" (ObjectStorage.contentTypeForFilename "clip.mp4")
        , test "style.css -> text/css" <|
            \_ ->
                Expect.equal "text/css" (ObjectStorage.contentTypeForFilename "style.css")
        , test "notes.csv -> text/csv" <|
            \_ ->
                Expect.equal "text/csv" (ObjectStorage.contentTypeForFilename "notes.csv")
        , test "pic.svg -> image/svg+xml" <|
            \_ ->
                Expect.equal "image/svg+xml" (ObjectStorage.contentTypeForFilename "pic.svg")
        , test "a multi-dot name uses the LAST extension (backup.tar.gz -> gz mapping)" <|
            \_ ->
                Expect.equal
                    (ObjectStorage.contentTypeForFilename "x.gz")
                    (ObjectStorage.contentTypeForFilename "backup.tar.gz")
        , test "backup.tar.gz is NOT application/octet-stream (gz is a known extension)" <|
            \_ ->
                Expect.notEqual "application/octet-stream" (ObjectStorage.contentTypeForFilename "backup.tar.gz")
        , test "an unknown extension -> application/octet-stream" <|
            \_ ->
                Expect.equal "application/octet-stream" (ObjectStorage.contentTypeForFilename "file.xyz")
        , test "a name with no extension -> application/octet-stream" <|
            \_ ->
                Expect.equal "application/octet-stream" (ObjectStorage.contentTypeForFilename "Makefile")
        , test "a trailing dot -> application/octet-stream" <|
            \_ ->
                Expect.equal "application/octet-stream" (ObjectStorage.contentTypeForFilename "weird.")
        ]


uploadSizeGuardSuite : Test
uploadSizeGuardSuite =
    describe "uploadSizeError guards against oversized uploads (whole-file-in-memory, no multipart)"
        [ test "the threshold constant is 100 MiB" <|
            \_ ->
                Expect.equal (100 * 1024 * 1024) ObjectStorage.uploadSizeLimitBytes
        , test "a zero-byte file is accepted (Nothing)" <|
            \_ ->
                Expect.equal Nothing (ObjectStorage.uploadSizeError 0)
        , test "a one-byte file is accepted (Nothing)" <|
            \_ ->
                Expect.equal Nothing (ObjectStorage.uploadSizeError 1)
        , test "a file at exactly the limit is accepted (Nothing)" <|
            \_ ->
                Expect.equal Nothing (ObjectStorage.uploadSizeError ObjectStorage.uploadSizeLimitBytes)
        , test "one byte over the limit is rejected (Just message)" <|
            \_ ->
                Expect.notEqual Nothing (ObjectStorage.uploadSizeError (ObjectStorage.uploadSizeLimitBytes + 1))
        , test "a multi-GiB file is rejected (Just message)" <|
            \_ ->
                Expect.notEqual Nothing (ObjectStorage.uploadSizeError (4 * 1024 * 1024 * 1024))
        , test "an oversized file yields a non-empty rejection message (its exact wording is UI copy, not asserted)" <|
            \_ ->
                case ObjectStorage.uploadSizeError (ObjectStorage.uploadSizeLimitBytes + 1) of
                    Just message ->
                        Expect.equal False (String.isEmpty message)

                    Nothing ->
                        Expect.fail "expected a rejection message"
        ]


nextListingMarkerSuite : Test
nextListingMarkerSuite =
    let
        mkObj n =
            { name = n, bytes = 0, lastModified = Time.millisToPosix 0, contentType = "", hash = "" }

        mkListing objectNames subdirs =
            { objects = List.map mkObj objectNames
            , subdirs = subdirs
            , nextMarker = Nothing
            }
    in
    describe "nextListingMarker drives user-driven load-more, counting objects AND subdirs together"
        [ test "a full page (objects + subdirs == limit) returns the lexicographically-last row key" <|
            \_ ->
                Expect.equal (Just "zebra.txt")
                    (ObjectStorage.nextListingMarker 3
                        (mkListing [ "zebra.txt" ] [ "logs/", "photos/" ])
                        (mkListing [ "zebra.txt" ] [ "logs/", "photos/" ])
                    )
        , test "when the last subdir sorts after the last object, the subdir is the marker" <|
            \_ ->
                Expect.equal (Just "zzz/")
                    (ObjectStorage.nextListingMarker 3
                        (mkListing [ "apple.txt" ] [ "logs/", "zzz/" ])
                        (mkListing [ "apple.txt" ] [ "logs/", "zzz/" ])
                    )
        , test "a short received page returns Nothing even when accumulated rows are an exact multiple of the limit" <|
            \_ ->
                Expect.equal Nothing
                    (ObjectStorage.nextListingMarker 3
                        (mkListing [] [])
                        (mkListing [ "a.txt", "b.txt", "c.txt" ] [])
                    )
        , test "an empty received page returns Nothing" <|
            \_ ->
                Expect.equal Nothing
                    (ObjectStorage.nextListingMarker 3
                        (mkListing [] [])
                        (mkListing [] [])
                    )
        ]


{-| The typed ACL parse/serialize layer round-trips a `X-Container-Read` / `X-Container-Write`
grantee string: `.r:*` (public read), `.rlistings` (name-listing) and any other grantee
(`project:user`, referrer hosts, …) survive parse→serialize unchanged, in order. Tolerance IS the
no-clobber guarantee: the RGW ACL grammar is PROVISIONAL, so any token we do not recognize is kept
verbatim as an `OtherGrantee`, never dropped. Whitespace around commas is trimmed.
-}
aclRoundTripSuite : Test
aclRoundTripSuite =
    describe "parseAcl / serializeAcl round-trip the Swift container ACL grammar"
        [ test "`.r:*` parses to the PublicRead grantee" <|
            \_ ->
                Expect.equal [ PublicReadGrantee ] (ObjectStorage.parseAcl ".r:*")
        , test "`.rlistings` parses to the Listings grantee" <|
            \_ ->
                Expect.equal [ ListingsGrantee ] (ObjectStorage.parseAcl ".rlistings")
        , test "`project:user` parses to an OtherGrantee, verbatim" <|
            \_ ->
                Expect.equal [ OtherGrantee "project:user" ] (ObjectStorage.parseAcl "project:user")
        , test "a combination `.r:*,.rlistings,projA:userB` parses in order" <|
            \_ ->
                Expect.equal
                    [ PublicReadGrantee, ListingsGrantee, OtherGrantee "projA:userB" ]
                    (ObjectStorage.parseAcl ".r:*,.rlistings,projA:userB")
        , test "the combination serializes back to the exact same string" <|
            \_ ->
                Expect.equal
                    (Just ".r:*,.rlistings,projA:userB")
                    (ObjectStorage.serializeAcl (ObjectStorage.parseAcl ".r:*,.rlistings,projA:userB"))
        , test "whitespace around commas is tolerated on parse" <|
            \_ ->
                Expect.equal
                    [ PublicReadGrantee, OtherGrantee "projA:userB" ]
                    (ObjectStorage.parseAcl "  .r:* ,  projA:userB  ")
        , test "whitespace-tolerant parse serializes to the trimmed canonical form" <|
            \_ ->
                Expect.equal
                    (Just ".r:*,projA:userB")
                    (ObjectStorage.serializeAcl (ObjectStorage.parseAcl "  .r:* ,  projA:userB  "))
        , test "an UNKNOWN token (a referrer host `.r:example.com`) is preserved verbatim" <|
            \_ ->
                Expect.equal
                    [ OtherGrantee ".r:example.com", ListingsGrantee ]
                    (ObjectStorage.parseAcl ".r:example.com,.rlistings")
        , test "the unknown-token ACL serializes back unchanged" <|
            \_ ->
                Expect.equal
                    (Just ".r:example.com,.rlistings")
                    (ObjectStorage.serializeAcl (ObjectStorage.parseAcl ".r:example.com,.rlistings"))
        , test "empty segments (trailing comma) are dropped, not kept as empty grantees" <|
            \_ ->
                Expect.equal [ PublicReadGrantee ] (ObjectStorage.parseAcl ".r:*,")
        ]


{-| No-clobber semantics: toggling public read (`.r:*`) or name-listing (`.rlistings`) adds/removes
ONLY that one fragment and leaves every other grantee (e.g. `projA:userB`) untouched. The two are
NEVER bundled — enabling `.rlistings` never implicitly adds `.r:*`, and disabling public never
touches `.rlistings`.
-}
aclNoClobberSuite : Test
aclNoClobberSuite =
    describe "setAclPublicRead / setAclListings preserve all other grantees (no-clobber)"
        [ test "enabling public on an ACL with projA:userB yields the old grantee plus `.r:*` only" <|
            \_ ->
                let
                    result =
                        ObjectStorage.setAclPublicRead True (ObjectStorage.parseAcl "projA:userB")
                in
                Expect.equal
                    ( [ OtherGrantee "projA:userB", PublicReadGrantee ], False )
                    ( result, ObjectStorage.aclHasListings result )
        , test "enabling public serializes to the existing grantees plus `.r:*` (no `.rlistings`)" <|
            \_ ->
                Expect.equal
                    (Just "projA:userB,.r:*")
                    (ObjectStorage.serializeAcl (ObjectStorage.setAclPublicRead True (ObjectStorage.parseAcl "projA:userB")))
        , test "enabling public is idempotent (no duplicate `.r:*`)" <|
            \_ ->
                Expect.equal
                    (Just ".r:*,projA:userB")
                    (ObjectStorage.serializeAcl (ObjectStorage.setAclPublicRead True (ObjectStorage.parseAcl ".r:*,projA:userB")))
        , test "disabling public removes ONLY `.r:*`, leaving projA:userB untouched" <|
            \_ ->
                Expect.equal
                    [ OtherGrantee "projA:userB" ]
                    (ObjectStorage.setAclPublicRead False (ObjectStorage.parseAcl ".r:*,projA:userB"))
        , test "disabling public keeps `.rlistings` (each fragment toggles independently)" <|
            \_ ->
                Expect.equal
                    (Just ".rlistings,projA:userB")
                    (ObjectStorage.serializeAcl (ObjectStorage.setAclPublicRead False (ObjectStorage.parseAcl ".r:*,.rlistings,projA:userB")))
        , test "enabling `.rlistings` never implicitly adds `.r:*`" <|
            \_ ->
                let
                    result =
                        ObjectStorage.setAclListings True (ObjectStorage.parseAcl "projA:userB")
                in
                Expect.equal
                    ( False, True )
                    ( ObjectStorage.aclIsPublic result, ObjectStorage.aclHasListings result )
        , test "disabling `.rlistings` removes ONLY that fragment, keeping `.r:*` and projA:userB" <|
            \_ ->
                Expect.equal
                    (Just ".r:*,projA:userB")
                    (ObjectStorage.serializeAcl (ObjectStorage.setAclListings False (ObjectStorage.parseAcl ".r:*,.rlistings,projA:userB")))
        ]


{-| Revoke-to-empty: an ACL with zero grantees serializes to `Nothing`, which the request layer
turns into `X-Remove-Container-Read` — never an empty-valued `X-Container-Read` header (an
Exosphere-style CORS proxy strips empty-valued headers).
-}
aclRevokeSuite : Test
aclRevokeSuite =
    describe "serializeAcl signals revoke-to-empty as Nothing (never an empty header value)"
        [ test "an empty ACL serializes to Nothing" <|
            \_ ->
                Expect.equal Nothing (ObjectStorage.serializeAcl [])
        , test "removing the only grantee (`.r:*`) leaves an empty ACL → Nothing" <|
            \_ ->
                Expect.equal
                    Nothing
                    (ObjectStorage.serializeAcl (ObjectStorage.setAclPublicRead False (ObjectStorage.parseAcl ".r:*")))
        , test "an ACL that still has a grantee serializes to Just (not the revoke signal)" <|
            \_ ->
                Expect.equal
                    (Just "projA:userB")
                    (ObjectStorage.serializeAcl (ObjectStorage.setAclPublicRead False (ObjectStorage.parseAcl ".r:*,projA:userB")))
        ]


{-| The structured "Who has access" editor sits on top of the typed ACL layer: `addGrant` merges a
`project:user` (or `project:*`, `*:*`) principal into an ACL idempotently, and `removeGrant` takes it
back out. Both preserve every OTHER grantee verbatim and in order — public read (`.r:*`), name-listing
(`.rlistings`), other principals, AND any unknown fragment the PROVISIONAL RGW grammar might carry.
The read/read+write distinction is expressed at the call site by applying these to the read ACL,
the write ACL, or both (see the last two cases).
-}
grantEditorSuite : Test
grantEditorSuite =
    describe "addGrant / removeGrant merge a user/project grant while preserving everything else (no-clobber)"
        [ test "addGrant appends the principal, keeping public read + an unrelated grant, in order" <|
            \_ ->
                Expect.equal
                    (Just ".r:*,projX:userY,projA:userB")
                    (ObjectStorage.parseAcl ".r:*,projX:userY"
                        |> ObjectStorage.addGrant "projA:userB"
                        |> ObjectStorage.serializeAcl
                    )
        , test "addGrant onto an empty ACL yields just that grant" <|
            \_ ->
                Expect.equal
                    (Just "projA:*")
                    (ObjectStorage.addGrant "projA:*" [] |> ObjectStorage.serializeAcl)
        , test "addGrant is idempotent (no duplicate principal)" <|
            \_ ->
                Expect.equal
                    (Just "projA:userB")
                    (ObjectStorage.parseAcl "projA:userB"
                        |> ObjectStorage.addGrant "projA:userB"
                        |> ObjectStorage.serializeAcl
                    )
        , test "addGrant carries the principal as an OtherGrantee (matches parseAcl's classification)" <|
            \_ ->
                Expect.equal
                    [ OtherGrantee "projA:userB" ]
                    (ObjectStorage.addGrant "projA:userB" [])
        , test "removeGrant removes ONLY that principal, keeping .r:*, .rlistings and another grant" <|
            \_ ->
                Expect.equal
                    (Just ".r:*,.rlistings,projX:userY")
                    (ObjectStorage.parseAcl ".r:*,.rlistings,projA:userB,projX:userY"
                        |> ObjectStorage.removeGrant "projA:userB"
                        |> ObjectStorage.serializeAcl
                    )
        , test "removeGrant preserves an UNKNOWN fragment (a referrer host) untouched" <|
            \_ ->
                Expect.equal
                    (Just ".r:example.com")
                    (ObjectStorage.parseAcl ".r:example.com,projA:userB"
                        |> ObjectStorage.removeGrant "projA:userB"
                        |> ObjectStorage.serializeAcl
                    )
        , test "removeGrant of an absent principal is a no-op" <|
            \_ ->
                Expect.equal
                    (Just ".r:*,projX:userY")
                    (ObjectStorage.parseAcl ".r:*,projX:userY"
                        |> ObjectStorage.removeGrant "projA:userB"
                        |> ObjectStorage.serializeAcl
                    )
        , test "removing the sole grant leaves an empty ACL → the revoke-to-empty signal (Nothing)" <|
            \_ ->
                Expect.equal
                    Nothing
                    (ObjectStorage.parseAcl "projA:userB"
                        |> ObjectStorage.removeGrant "projA:userB"
                        |> ObjectStorage.serializeAcl
                    )
        , test "read-only grant: added to read, removed from write (the call-site merge)" <|
            \_ ->
                let
                    read =
                        ObjectStorage.parseAcl "projX:userY" |> ObjectStorage.addGrant "projA:userB"

                    write =
                        ObjectStorage.parseAcl "projA:userB" |> ObjectStorage.removeGrant "projA:userB"
                in
                Expect.equal
                    ( Just "projX:userY,projA:userB", Nothing )
                    ( ObjectStorage.serializeAcl read, ObjectStorage.serializeAcl write )
        , test "read+write grant: added to BOTH read and write" <|
            \_ ->
                let
                    read =
                        ObjectStorage.parseAcl ".r:*" |> ObjectStorage.addGrant "projA:userB"

                    write =
                        ObjectStorage.parseAcl "" |> ObjectStorage.addGrant "projA:userB"
                in
                Expect.equal
                    ( Just ".r:*,projA:userB", Just "projA:userB" )
                    ( ObjectStorage.serializeAcl read, ObjectStorage.serializeAcl write )
        ]


{-| `rawAclChange` maps a raw advanced-ACL text field to an `AclChange`: a blank (after trimming)
field is the revoke signal (`RemoveAcl` → `X-Remove-Container-*`); any other content is sent verbatim
after trimming (`SetAcl`). This is the semantics the advanced-ACL escape hatch relies on to send a
`RemoveAcl` only when the user explicitly clears a field.
-}
rawAclChangeSuite : Test
rawAclChangeSuite =
    describe "rawAclChange maps raw advanced-ACL text to SetAcl/RemoveAcl (blank clears, else verbatim-trimmed)"
        [ test "an empty string is the revoke signal (RemoveAcl)" <|
            \_ ->
                Expect.equal ObjectStorage.RemoveAcl (ObjectStorage.rawAclChange "")
        , test "a whitespace-only string trims to blank → RemoveAcl" <|
            \_ ->
                Expect.equal ObjectStorage.RemoveAcl (ObjectStorage.rawAclChange "   ")
        , test "a non-blank value is sent verbatim after trimming (SetAcl)" <|
            \_ ->
                Expect.equal (ObjectStorage.SetAcl ".r:*,projA:userB") (ObjectStorage.rawAclChange "  .r:*,projA:userB  ")
        , test "the untouched-field mapping used by the page (Maybe.map rawAclChange, default LeaveAcl) sends nothing for Nothing but RemoveAcl for an explicit blank" <|
            \_ ->
                let
                    mapInput input =
                        input |> Maybe.map ObjectStorage.rawAclChange |> Maybe.withDefault ObjectStorage.LeaveAcl
                in
                Expect.equal
                    ( ObjectStorage.LeaveAcl, ObjectStorage.RemoveAcl )
                    ( mapInput Nothing, mapInput (Just "") )
        ]


containerMetadataFromHeadersSuite : Test
containerMetadataFromHeadersSuite =
    describe "containerMetadataFromHeaders decodes HEAD-container response headers"
        [ test "a full lowercase header set decodes ACLs + usage + created-time + policy" <|
            \_ ->
                Expect.equal
                    { readAcl = Just ".r:*,projA:userB"
                    , writeAcl = Just "projA:userB"
                    , bytesUsed = Just 1234
                    , objectCount = Just 5
                    , createdAt = Just (Time.millisToPosix 1719750766262)
                    , storagePolicy = Just "Policy-0"
                    }
                    (ObjectStorage.containerMetadataFromHeaders
                        (Dict.fromList
                            [ ( "x-container-read", ".r:*,projA:userB" )
                            , ( "x-container-write", "projA:userB" )
                            , ( "x-container-bytes-used", "1234" )
                            , ( "x-container-object-count", "5" )
                            , ( "x-timestamp", "1719750766.26202" )
                            , ( "x-storage-policy", "Policy-0" )
                            ]
                        )
                    )
        , test "header lookup is case-insensitive (mixed/upper-case names still resolve)" <|
            \_ ->
                Expect.equal
                    { readAcl = Just ".r:*"
                    , writeAcl = Nothing
                    , bytesUsed = Just 42
                    , objectCount = Just 7
                    , createdAt = Nothing
                    , storagePolicy = Nothing
                    }
                    (ObjectStorage.containerMetadataFromHeaders
                        (Dict.fromList
                            [ ( "X-Container-Read", ".r:*" )
                            , ( "X-Container-Bytes-Used", "42" )
                            , ( "X-Container-Object-Count", "7" )
                            ]
                        )
                    )
        , test "missing headers decode to Nothing" <|
            \_ ->
                Expect.equal
                    { readAcl = Nothing
                    , writeAcl = Nothing
                    , bytesUsed = Nothing
                    , objectCount = Nothing
                    , createdAt = Nothing
                    , storagePolicy = Nothing
                    }
                    (ObjectStorage.containerMetadataFromHeaders Dict.empty)
        , test "a non-integer usage header decodes to Nothing (not a crash)" <|
            \_ ->
                Expect.equal
                    Nothing
                    (ObjectStorage.containerMetadataFromHeaders
                        (Dict.fromList [ ( "x-container-bytes-used", "not-a-number" ) ])
                    ).bytesUsed
        , test "a garbage X-Timestamp decodes to Nothing (not a crash)" <|
            \_ ->
                Expect.equal
                    Nothing
                    (ObjectStorage.containerMetadataFromHeaders
                        (Dict.fromList [ ( "x-timestamp", "not-a-timestamp" ) ])
                    ).createdAt
        ]


xTimestampParseSuite : Test
xTimestampParseSuite =
    describe "parseXTimestamp decodes Swift's epoch-seconds X-Timestamp (fraction → millis; garbage → Nothing)"
        [ test "a fractional epoch-seconds value folds the fraction into milliseconds" <|
            \_ ->
                Expect.equal
                    (Just (Time.millisToPosix 1719750766262))
                    (ObjectStorage.parseXTimestamp "1719750766.26202")
        , test "a whole-seconds value (no fraction) decodes to on-the-second millis" <|
            \_ ->
                Expect.equal
                    (Just (Time.millisToPosix 1719750766000))
                    (ObjectStorage.parseXTimestamp "1719750766")
        , test "surrounding whitespace is tolerated" <|
            \_ ->
                Expect.equal
                    (Just (Time.millisToPosix 1719750766000))
                    (ObjectStorage.parseXTimestamp "  1719750766  ")
        , test "non-numeric garbage decodes to Nothing" <|
            \_ ->
                Expect.equal Nothing (ObjectStorage.parseXTimestamp "not-a-timestamp")
        , test "a value with trailing junk is rejected (strict, not partial)" <|
            \_ ->
                Expect.equal Nothing (ObjectStorage.parseXTimestamp "1719750766x")
        , test "an empty string decodes to Nothing" <|
            \_ ->
                Expect.equal Nothing (ObjectStorage.parseXTimestamp "")
        ]


publicContainerUrlSuite : Test
publicContainerUrlSuite =
    describe "publicContainerUrl joins the swift base with a percent-encoded container segment"
        [ test "an ordinary container name is appended as one path segment" <|
            \_ ->
                Expect.equal
                    "https://swift.example.com/v1/AUTH_abc/my-container"
                    (ObjectStorage.publicContainerUrl "https://swift.example.com/v1/AUTH_abc" "my-container")
        , test "a name with spaces / unicode is percent-encoded" <|
            \_ ->
                Expect.equal
                    "https://swift.example.com/v1/AUTH_abc/my%20b%C3%B6x"
                    (ObjectStorage.publicContainerUrl "https://swift.example.com/v1/AUTH_abc" "my böx")
        ]


{-| The `containerIsPublic` upgrade: public iff the TYPED parse of the read ACL contains the `.r:*`
grantee. A read ACL of only `projA:userB` is NOT public; `.rlistings` alone (name-listing) is NOT
public read; a missing ACL is NOT public.
-}
readAclIsPublicSuite : Test
readAclIsPublicSuite =
    describe "readAclIsPublic is True iff the typed read-ACL parse contains `.r:*`"
        [ test "`.r:*,projA:userB` is public" <|
            \_ ->
                Expect.equal True (ObjectStorage.readAclIsPublic (Just ".r:*,projA:userB"))
        , test "`projA:userB` alone is NOT public" <|
            \_ ->
                Expect.equal False (ObjectStorage.readAclIsPublic (Just "projA:userB"))
        , test "`.rlistings` alone is NOT public read" <|
            \_ ->
                Expect.equal False (ObjectStorage.readAclIsPublic (Just ".rlistings"))
        , test "a missing (Nothing) read ACL is NOT public" <|
            \_ ->
                Expect.equal False (ObjectStorage.readAclIsPublic Nothing)
        , test "a referrer host `.r:example.com` (not the wildcard) is NOT world-public" <|
            \_ ->
                Expect.equal False (ObjectStorage.readAclIsPublic (Just ".r:example.com"))
        ]


sampleUpload : Int -> ObjectStorage.ObjectName -> UploadStatus -> ObjectStorage.Upload
sampleUpload id objectName status =
    { id = id
    , containerName = "docs"
    , prefix = Nothing
    , objectName = objectName
    , sizeBytes = 10
    , status = status
    }


{-| Task 1b — the stale-result guard. Re-enqueueing an in-flight target mints a NEW id; a late
completion for the OLD id must find no match and leave the replacement untouched. `nextUploadId`
gives distinct, monotonically increasing ids so this guard holds.
-}
uploadIdGuardSuite : Test
uploadIdGuardSuite =
    describe "upload-queue op-id guard (nextUploadId + setUploadStatusById)"
        [ test "nextUploadId of an empty queue is 1" <|
            \_ ->
                Expect.equal 1 (ObjectStorage.nextUploadId [])
        , test "nextUploadId is one past the largest existing id (order-independent)" <|
            \_ ->
                Expect.equal 8
                    (ObjectStorage.nextUploadId
                        [ sampleUpload 3 "a" Queued, sampleUpload 7 "b" Uploading, sampleUpload 1 "c" Succeeded ]
                    )
        , test "setUploadStatusById updates only the matching id" <|
            \_ ->
                Expect.equal
                    [ sampleUpload 1 "a" Queued, sampleUpload 2 "b" Succeeded ]
                    (ObjectStorage.setUploadStatusById 2
                        Succeeded
                        [ sampleUpload 1 "a" Queued, sampleUpload 2 "b" Uploading ]
                    )
        , test "a completion for a superseded (absent) id is ignored — the replacement is untouched" <|
            \_ ->
                -- Entry id=1 (same target "report.csv") was replaced by id=2 on re-enqueue; the stale
                let
                    afterReenqueue =
                        [ sampleUpload 2 "report.csv" Uploading ]
                in
                Expect.equal
                    afterReenqueue
                    (ObjectStorage.setUploadStatusById 1 Succeeded afterReenqueue)
        ]


clearFinishedUploadsSuite : Test
clearFinishedUploadsSuite =
    describe "clearFinishedUploads drops only terminal entries"
        [ test "uploadIsFinished classifies each status" <|
            \_ ->
                Expect.equal
                    [ False, False, True, True, True ]
                    (List.map (ObjectStorage.uploadIsFinished << sampleUpload 1 "x")
                        [ Queued, Uploading, Succeeded, Failed "boom", Rejected "too big" ]
                    )
        , test "clearFinishedUploads keeps in-flight, drops finished" <|
            \_ ->
                Expect.equal
                    [ sampleUpload 1 "a" Queued, sampleUpload 2 "b" Uploading ]
                    (ObjectStorage.clearFinishedUploads
                        [ sampleUpload 1 "a" Queued
                        , sampleUpload 2 "b" Uploading
                        , sampleUpload 3 "c" Succeeded
                        , sampleUpload 4 "d" (Failed "boom")
                        , sampleUpload 5 "e" (Rejected "too big")
                        ]
                    )
        , test "clearing an all-finished queue yields empty" <|
            \_ ->
                Expect.equal []
                    (ObjectStorage.clearFinishedUploads
                        [ sampleUpload 1 "a" Succeeded, sampleUpload 2 "b" (Rejected "too big") ]
                    )
        ]


popconfirmIdCollisionSuite : Test
popconfirmIdCollisionSuite =
    describe "list-page delete popconfirm ids are collision-proof"
        [ test "a container named `bulk` does not collide with the bulk-delete popover id" <|
            \_ ->
                Expect.notEqual
                    (ObjectStorageList.deletePopconfirmId "proj-uuid" "bulk")
                    (ObjectStorageList.bulkDeletePopconfirmId "proj-uuid")
        , test "no ordinary container name collides with the bulk-delete popover id" <|
            \_ ->
                Expect.equalLists
                    []
                    (List.filter
                        (\name -> ObjectStorageList.deletePopconfirmId "proj-uuid" name == ObjectStorageList.bulkDeletePopconfirmId "proj-uuid")
                        [ "bulk", "documents", "", "a-b-c", "bulk-delete" ]
                    )
        ]


{-| Task 2 — the `X-Copy-From` header value: a leading-slash `/container/object` path with each
segment percent-encoded (spaces/unicode/`#`/`?`), the pseudo-folder `/` kept as real separators.
Mirrors objectPathSuite (same split-then-encode discipline as the API path).
-}
copyFromHeaderValueSuite : Test
copyFromHeaderValueSuite =
    describe "copyFromHeaderValue builds a leading-slash, per-segment-encoded /container/object path"
        [ test "a simple object" <|
            \_ ->
                Expect.equal "/docs/report.csv" (ObjectStorage.copyFromHeaderValue "docs" "report.csv")
        , test "pseudo-folder slashes are preserved as separators (not encoded)" <|
            \_ ->
                Expect.equal "/c/a/b/file.txt" (ObjectStorage.copyFromHeaderValue "c" "a/b/file.txt")
        , test "spaces, #, ? are percent-encoded within a segment" <|
            \_ ->
                Expect.equal "/c/a/x%20%231%3F.y" (ObjectStorage.copyFromHeaderValue "c" "a/x #1?.y")
        , test "unicode segments are percent-encoded (UTF-8)" <|
            \_ ->
                Expect.equal "/c/caf%C3%A9/%C2%A3.txt" (ObjectStorage.copyFromHeaderValue "c" "café/£.txt")
        , test "the leading slash is always present" <|
            \_ ->
                Expect.equal True (String.startsWith "/" (ObjectStorage.copyFromHeaderValue "c" "o"))
        ]


objectNameErrorSuite : Test
objectNameErrorSuite =
    describe "objectNameError enforces the Swift object-name rule (non-empty, <=1024 bytes, / allowed)"
        [ test "a normal name is accepted" <|
            \_ ->
                Expect.equal Nothing (ObjectStorage.objectNameError "a/b/file.txt")
        , test "an empty name is rejected" <|
            \_ ->
                Expect.notEqual Nothing (ObjectStorage.objectNameError "")
        , test "1024 ASCII bytes is accepted" <|
            \_ ->
                Expect.equal Nothing (ObjectStorage.objectNameError (String.repeat 1024 "a"))
        , test "1025 ASCII bytes is rejected" <|
            \_ ->
                Expect.notEqual Nothing (ObjectStorage.objectNameError (String.repeat 1025 "a"))
        , test "343 three-byte CJK chars (1029 bytes) is rejected — proves BYTES not String.length" <|
            \_ ->
                Expect.notEqual Nothing (ObjectStorage.objectNameError (String.repeat 343 "中"))
        ]


objectContainingPrefixSuite : Test
objectContainingPrefixSuite =
    describe "objectContainingPrefix returns the folder up to and including the last slash"
        [ test "a nested object" <|
            \_ ->
                Expect.equal (Just "a/b/") (ObjectStorage.objectContainingPrefix "a/b/c.txt")
        , test "a root object has no containing prefix" <|
            \_ ->
                Expect.equal Nothing (ObjectStorage.objectContainingPrefix "top.txt")
        , test "interior empty segments (a//b.txt) are preserved" <|
            \_ ->
                Expect.equal (Just "a//") (ObjectStorage.objectContainingPrefix "a//b.txt")
        ]


folderNameErrorSuite : Test
folderNameErrorSuite =
    describe "folderNameError enforces the pseudo-folder name rule (non-empty, no /, <=256 bytes)"
        [ test "a normal folder name is accepted" <|
            \_ ->
                Expect.equal Nothing (ObjectStorage.folderNameError "reports")
        , test "an empty name is rejected" <|
            \_ ->
                Expect.notEqual Nothing (ObjectStorage.folderNameError "")
        , test "a name containing '/' is rejected (that is the delimiter)" <|
            \_ ->
                Expect.notEqual Nothing (ObjectStorage.folderNameError "a/b")
        , test "256 ASCII bytes is accepted, 257 rejected" <|
            \_ ->
                Expect.equal ( Nothing, True )
                    ( ObjectStorage.folderNameError (String.repeat 256 "a")
                    , ObjectStorage.folderNameError (String.repeat 257 "a") /= Nothing
                    )
        , test "86 three-byte CJK chars (258 bytes) is rejected — proves BYTES not String.length" <|
            \_ ->
                Expect.notEqual Nothing (ObjectStorage.folderNameError (String.repeat 86 "中"))
        ]


folderPlaceholderObjectNameSuite : Test
folderPlaceholderObjectNameSuite =
    describe "folderPlaceholderObjectName builds <prefix><name>/"
        [ test "at the container root the placeholder is just <name>/" <|
            \_ ->
                Expect.equal "reports/" (ObjectStorage.folderPlaceholderObjectName Nothing "reports")
        , test "inside a folder the current prefix is prepended" <|
            \_ ->
                Expect.equal "a/b/reports/" (ObjectStorage.folderPlaceholderObjectName (Just "a/b/") "reports")
        , test "unicode + spaces in the name are preserved (encoding happens in the Rest path)" <|
            \_ ->
                Expect.equal "a/報告 書/" (ObjectStorage.folderPlaceholderObjectName (Just "a/") "報告 書")
        ]


rcloneConfigSnippetSuite : Test
rcloneConfigSnippetSuite =
    describe "rcloneConfigSnippet renders an exact rclone remote block"
        [ test "pins the full snippet for a given endpoint/access/secret" <|
            \_ ->
                Expect.equal
                    (String.join "\n"
                        [ "[exosphere]"
                        , "type = s3"
                        , "provider = Other"
                        , "endpoint = http://100.103.247.45:8085"
                        , "access_key_id = AKIA1"
                        , "secret_access_key = s3cr3t"
                        , "region = us-east-1"
                        , "force_path_style = true"
                        ]
                    )
                    (ObjectStorage.rcloneConfigSnippet
                        { endpoint = "http://100.103.247.45:8085"
                        , access = "AKIA1"
                        , secret = "s3cr3t"
                        }
                    )
        ]
