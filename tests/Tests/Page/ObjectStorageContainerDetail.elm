module Tests.Page.ObjectStorageContainerDetail exposing (crumbsSuite, uploadStatusLabelSuite, uploadsForLevelSuite)

import Expect
import OpenStack.ObjectStorage as ObjectStorage
import Page.ObjectStorageContainerDetail as ObjectStorageContainerDetail
import Test exposing (Test, describe, test)


crumbsSuite : Test
crumbsSuite =
    describe "Page.ObjectStorageContainerDetail.crumbs derives the breadcrumb trail"
        [ test "(1) prefix Nothing yields exactly the container-root crumb (current level)" <|
            \_ ->
                Expect.equal
                    [ ( "my-container", Nothing ) ]
                    (ObjectStorageContainerDetail.crumbs "my-container" Nothing)
        , test "(2) a nested prefix yields root-first cumulative crumbs, last = current level" <|
            \_ ->
                Expect.equal
                    [ ( "my-container", Nothing )
                    , ( "logs", Just "logs/" )
                    , ( "2026", Just "logs/2026/" )
                    ]
                    (ObjectStorageContainerDetail.crumbs "my-container" (Just "logs/2026/"))
        , test "(3) an interior empty segment (a//b/) is kept, not silently dropped" <|
            \_ ->
                Expect.equal
                    [ ( "my-container", Nothing )
                    , ( "a", Just "a/" )
                    , ( "", Just "a//" )
                    , ( "b", Just "a//b/" )
                    ]
                    (ObjectStorageContainerDetail.crumbs "my-container" (Just "a//b/"))
        , test "(4) a unicode/space prefix round-trips as the crumb label unchanged" <|
            \_ ->
                Expect.equal
                    [ ( "my-container", Nothing )
                    , ( "fólder ñ", Just "fólder ñ/" )
                    ]
                    (ObjectStorageContainerDetail.crumbs "my-container" (Just "fólder ñ/"))
        ]


uploadStatusLabelSuite : Test
uploadStatusLabelSuite =
    let
        label =
            ObjectStorageContainerDetail.uploadStatusLabel
    in
    describe "Page.ObjectStorageContainerDetail.uploadStatusLabel gives each queue state its own honest label"
        [ test "(1) Queued yields a non-empty label distinct from the Uploading and Succeeded labels" <|
            \_ ->
                Expect.equal ( True, True, True )
                    ( not (String.isEmpty (label ObjectStorage.Queued))
                    , label ObjectStorage.Queued /= label ObjectStorage.Uploading
                    , label ObjectStorage.Queued /= label ObjectStorage.Succeeded
                    )
        , test "(2) Uploading is a non-empty label carrying no fabricated percentage" <|
            \_ ->
                Expect.equal ( True, False )
                    ( not (String.isEmpty (label ObjectStorage.Uploading))
                    , String.contains "%" (label ObjectStorage.Uploading)
                    )
        , test "(3) Succeeded yields a non-empty label distinct from the in-flight Uploading label" <|
            \_ ->
                Expect.equal ( True, True )
                    ( not (String.isEmpty (label ObjectStorage.Succeeded))
                    , label ObjectStorage.Succeeded /= label ObjectStorage.Uploading
                    )
        , test "(4) Failed surfaces the underlying reason (not swallowed)" <|
            \_ ->
                Expect.equal True
                    (String.contains "503 Service Unavailable" (label (ObjectStorage.Failed "503 Service Unavailable")))
        , test "(5) Rejected passes its reason through verbatim (carries the caller's guard message unchanged)" <|
            \_ ->
                let
                    reason =
                        "SENTINEL rejection reason"
                in
                Expect.equal reason (label (ObjectStorage.Rejected reason))
        ]


uploadsForLevelSuite : Test
uploadsForLevelSuite =
    let
        mkUpload container prefix name =
            { id = 1
            , containerName = container
            , prefix = prefix
            , objectName = name
            , sizeBytes = 0
            , status = ObjectStorage.Queued
            }

        rootA =
            mkUpload "a" Nothing "x.txt"

        logsA =
            mkUpload "a" (Just "logs/") "logs/y.txt"

        logsB =
            mkUpload "b" (Just "logs/") "logs/z.txt"
    in
    describe "Page.ObjectStorageContainerDetail.uploadsForLevel keeps only entries matching container AND prefix exactly"
        [ test "(1) a Just-prefix level keeps only that container+prefix, excluding root and other containers" <|
            \_ ->
                Expect.equal [ logsA ]
                    (ObjectStorageContainerDetail.uploadsForLevel "a" (Just "logs/") [ rootA, logsA, logsB ])
        , test "(2) a root (Nothing prefix) level excludes Just-prefix entries" <|
            \_ ->
                Expect.equal [ rootA ]
                    (ObjectStorageContainerDetail.uploadsForLevel "a" Nothing [ rootA, logsA, logsB ])
        , test "(3) a different container with the same prefix is excluded" <|
            \_ ->
                Expect.equal [ logsB ]
                    (ObjectStorageContainerDetail.uploadsForLevel "b" (Just "logs/") [ rootA, logsA, logsB ])
        , test "(4) an empty upload list yields an empty level" <|
            \_ ->
                Expect.equal []
                    (ObjectStorageContainerDetail.uploadsForLevel "a" Nothing [])
        ]
