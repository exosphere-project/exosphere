module Tests.Page.ObjectStorageContainerDetail exposing (aclFieldValueSuite, containerUsageLabelSuite, crumbsSuite, uploadStatusLabelSuite, uploadsForLevelSuite)

import Expect
import FormatNumber.Locales
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


containerUsageLabelSuite : Test
containerUsageLabelSuite =
    let
        locale =
            FormatNumber.Locales.base

        mk bytesUsed objectCount =
            { readAcl = Nothing
            , writeAcl = Nothing
            , bytesUsed = bytesUsed
            , objectCount = objectCount
            , createdAt = Nothing
            , storagePolicy = Nothing
            }

        label md =
            ObjectStorageContainerDetail.containerUsageLabel "object" locale md
    in
    describe "Page.ObjectStorageContainerDetail.containerUsageLabel formats the usage summary shared by both variants"
        [ test "(1) no bytes and no count yields Nothing (no usage row rendered)" <|
            \_ ->
                Expect.equal Nothing (label (mk Nothing Nothing))
        , test "(2) both present -> a Just surfacing the formatted bytes AND the count value" <|
            \_ ->
                Expect.equal (Just ( True, True ))
                    (label (mk (Just 1536) (Just 3))
                        |> Maybe.map (\s -> ( String.contains "1.5 KB" s, String.contains "3" s ))
                    )
        , test "(3) bytes only -> a Just surfacing the formatted bytes" <|
            \_ ->
                Expect.equal (Just True)
                    (label (mk (Just 1536) Nothing)
                        |> Maybe.map (String.contains "1.5 KB")
                    )
        , test "(4) count only -> a Just surfacing the count value" <|
            \_ ->
                Expect.equal (Just True)
                    (label (mk Nothing (Just 1))
                        |> Maybe.map (String.contains "1")
                    )
        , test "(5) Just 0 / Just 0 is real data (empty container) -> a Just showing 0 B, NOT Nothing" <|
            \_ ->
                Expect.equal (Just True)
                    (label (mk (Just 0) (Just 0))
                        |> Maybe.map (String.contains "0 B")
                    )
        ]


{-| The advanced-ACL field's displayed value, extracted from `advancedAclControl`'s
read/write duplication. `Nothing` (untouched)
falls back to the container's current metadata ACL; `Just s` is the user's edit and always wins, even
`Just ""` (edited-to-empty), which must show blank, NOT the metadata fallback, because an empty field
is what drives the `X-Remove-Container-*` revoke path.
-}
aclFieldValueSuite : Test
aclFieldValueSuite =
    describe "Page.ObjectStorageContainerDetail.aclFieldValue picks the displayed raw-ACL string"
        [ test "(1) untouched field falls back to the current metadata ACL" <|
            \_ ->
                Expect.equal ".r:*,project:user"
                    (ObjectStorageContainerDetail.aclFieldValue Nothing (Just ".r:*,project:user"))
        , test "(2) untouched field with no metadata ACL shows empty" <|
            \_ ->
                Expect.equal ""
                    (ObjectStorageContainerDetail.aclFieldValue Nothing Nothing)
        , test "(3) an edit wins over the metadata value" <|
            \_ ->
                Expect.equal "project:other"
                    (ObjectStorageContainerDetail.aclFieldValue (Just "project:other") (Just ".r:*,project:user"))
        , test "(4) edited-to-empty stays empty (drives the revoke path), NOT the metadata fallback" <|
            \_ ->
                Expect.equal ""
                    (ObjectStorageContainerDetail.aclFieldValue (Just "") (Just ".r:*"))
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
