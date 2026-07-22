module Tests.OpenStack.ObjectStorage exposing
    ( containerNameValidationSuite
    , contentTypeForFilenameSuite
    , objectNameErrorSuite
    , objectPathSuite
    , uploadIdGuardSuite
    , uploadSizeGuardSuite
    )

import Expect
import OpenStack.ObjectStorage as ObjectStorage exposing (UploadStatus(..))
import Test exposing (Test, describe, test)


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


sampleUpload : Int -> ObjectStorage.ObjectName -> UploadStatus -> ObjectStorage.Upload
sampleUpload id objectName status =
    { id = id
    , containerName = "docs"
    , prefix = Nothing
    , objectName = objectName
    , sizeBytes = 10
    , status = status
    }


{-| The stale-result guard: re-enqueueing an in-flight target mints a NEW id; a late
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
