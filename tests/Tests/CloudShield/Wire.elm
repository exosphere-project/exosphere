module Tests.CloudShield.Wire exposing
    ( countsLabelSuite
    , embedRequestSuite
    , embedResultSuite
    , getEmbedBlockedSuite
    , indexSuite
    , requestBytesSuite
    , requestKindSuite
    )

{-| The CloudShield extension's own wire payloads: the bodies it puts inside the exoext envelope,
and the reads that make sense of what comes back. The envelope itself (§7.1 framing, run status,
cancel channel, caps) is covered by `Tests.Exoext.Transport`.
-}

import CloudShield.Card as Card
import CloudShield.Wire as Wire
import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (Test, describe, test)


meta : List ( String, String ) -> List { key : String, value : String }
meta pairs =
    List.map (\( k, v ) -> { key = k, value = v }) pairs


{-| The two request bodies, pinned as EXACT bytes rather than by decoded fields.

The §4.1 contract is frozen and the publisher is a deployed bridge, so what matters is not that
these objects contain the right values but that they serialize to the same string they always
have: same keys, same order, same compact separators. These two assertions are what a refactor of
who-encodes-what has to keep true — a field test would pass on a body whose key order changed.

-}
requestBytesSuite : Test
requestBytesSuite =
    describe "the §4.1 request bodies serialize to their frozen bytes"
        [ test "a scan request" <|
            \_ ->
                Expect.equal
                    """{"schemaVersion":"1.0","requestId":"exo-cs-req-1","batchId":null,"createdAt":"2026-06-22T00:00:00Z","requestedBy":{"source":"exosphere","projectId":"proj-1"},"target":{"instanceId":"i-1","instanceName":"alpha"},"scan":{"profile":"quick","method":"snapshot-clone"}}"""
                    (Wire.scanRequestJson
                        { requestId = "exo-cs-req-1"
                        , batchId = Nothing
                        , createdAt = "2026-06-22T00:00:00Z"
                        , projectId = "proj-1"
                        , target = { instanceId = "i-1", instanceName = "alpha" }
                        , profile = "quick"
                        }
                    )
        , test "a scan request that belongs to a batch carries the shared id" <|
            \_ ->
                Expect.equal
                    """{"schemaVersion":"1.0","requestId":"exo-cs-req-2","batchId":"exo-cs-batch-1","createdAt":"2026-06-22T00:00:00Z","requestedBy":{"source":"exosphere","projectId":"proj-1"},"target":{"instanceId":"i-2","instanceName":"beta"},"scan":{"profile":"quick","method":"snapshot-clone"}}"""
                    (Wire.scanRequestJson
                        { requestId = "exo-cs-req-2"
                        , batchId = Just "exo-cs-batch-1"
                        , createdAt = "2026-06-22T00:00:00Z"
                        , projectId = "proj-1"
                        , target = { instanceId = "i-2", instanceName = "beta" }
                        , profile = "quick"
                        }
                    )
        , test "a getEmbed request" <|
            \_ ->
                Expect.equal
                    """{"schemaVersion":"1.0","requestId":"exo-cs-req-1700000000000","action":"getEmbed","batchId":"b-1","resultId":"exo-cs-req-1699999999999","createdAt":"2026-07-17T00:00:00Z"}"""
                    (Wire.embedRequestJson
                        { requestId = "exo-cs-req-1700000000000"
                        , batchId = "b-1"
                        , resultId = "exo-cs-req-1699999999999"
                        , createdAt = "2026-07-17T00:00:00Z"
                        }
                    )
        ]


{-| The host↔adapter request boundary, driven from both ends.

The property under test is that the request VERB travels as data all the way from the press to the
body, and that the host contributes only stamps. So: the dispatcher puts a kind on the
out-message and on the tracker (which is where a batch continuation reads it back from), and the
encoder turns a kind plus the host's stamps into a body, declining a kind it does not speak.

-}
requestKindSuite : Test
requestKindSuite =
    let
        context =
            { requestId = "exo-cs-req-1"
            , batchId = Nothing
            , createdAt = "2026-06-22T00:00:00Z"
            , projectId = "proj-1"
            , subject = { id = "i-1", name = "alpha" }
            }

        writeRequestParams kind =
            Encode.object
                [ ( "kind", Encode.string kind )
                , ( "targetInstanceIds", Encode.list Encode.string [ "i-1" ] )
                ]

        dispatch kind =
            Card.dispatchVerb (Card.resolveAction "exoext.writeRequest" (writeRequestParams kind))
                (writeRequestParams kind)
                Card.init
    in
    describe "a request's kind travels from the press to the body"
        [ test "a write-request press emits its kind, and records it on the tracker" <|
            \_ ->
                -- The tracker is the ONLY carrier for a batch continuation: the host writes each
                -- sibling with no second press, so a kind the tracker did not keep is a kind the
                -- tail could not be written with.
                let
                    ( model, outMsg ) =
                        dispatch Wire.kindScan
                in
                Expect.equal
                    ( Just (Card.WriteRequested { kind = "scan", seq = 1, targetIds = [ "i-1" ] })
                    , Just "scan"
                    )
                    ( outMsg, model.pending |> Maybe.map .kind )
        , test "a kind outside writeRequestKinds changes nothing at all" <|
            \_ ->
                -- Fail-closed on BOTH halves: no out-message, and no optimistic row state either.
                -- Rows flipped to `queued` for a body the encoder would decline would advertise a
                -- request that never reaches the wire.
                Expect.equal ( Nothing, Nothing )
                    ( Tuple.second (dispatch "backup")
                    , Tuple.first (dispatch "backup") |> .pending
                    )
        , test "the scan kind encodes the §4.1 scan body from the host's stamps" <|
            \_ ->
                Expect.equal
                    (Just
                        (Wire.scanRequestJson
                            { requestId = "exo-cs-req-1"
                            , batchId = Nothing
                            , createdAt = "2026-06-22T00:00:00Z"
                            , projectId = "proj-1"
                            , target = { instanceId = "i-1", instanceName = "alpha" }
                            , profile = "quick"
                            }
                        )
                    )
                    (Wire.encodeRequestBody Wire.kindScan context)
        , test "the session kind encodes the getEmbed body, taking the subject as the result id" <|
            \_ ->
                Expect.equal
                    (Just
                        (Wire.embedRequestJson
                            { requestId = "exo-cs-req-1"
                            , batchId = "b-1"
                            , resultId = "exo-cs-res-9"
                            , createdAt = "2026-06-22T00:00:00Z"
                            }
                        )
                    )
                    (Wire.encodeRequestBody Wire.kindOpenSession
                        { context | batchId = Just "b-1", subject = { id = "exo-cs-res-9", name = "exo-cs-res-9" } }
                    )
        , test "a kind this adapter does not speak encodes nothing, so nothing is written" <|
            \_ ->
                Expect.equal Nothing (Wire.encodeRequestBody "backup" context)
        , test "an unnamed kind resolves to the scan, which is what resumes a tail across a reload" <|
            \_ ->
                -- §4.3 carries no verb, so a run adopted from the wire after a reload has none to
                -- recover; the host passes `""` through rather than inventing one, and THIS
                -- adapter answers it, because a batch of §4.1 siblings can only be scans here.
                Expect.equal (Wire.encodeRequestBody Wire.kindScan context)
                    (Wire.encodeRequestBody "" context)
        ]


goodIndex : String
goodIndex =
    """
    [ { "batchId": "b-1", "targetId": "i-1", "targetName": "alpha", "completedAt": "2026-07-01T00:00:00Z", "status": "done", "counts": { "critical": 0, "high": 7, "medium": 14, "low": 3, "info": 0 } }
    , { "batchId": "b-2", "targetId": "i-2", "targetName": "beta", "completedAt": "2026-07-02T00:00:00Z", "status": "done", "counts": { "critical": 0, "high": 0, "medium": 0, "low": 0, "info": 0 } }
    ]
    """


indexSuite : Test
indexSuite =
    describe "decodeIndex reads the append-only history index, fail-closed"
        [ test "a good index yields its rows in file order (oldest first)" <|
            \_ ->
                Expect.equal [ "b-1", "b-2" ]
                    (Wire.decodeIndex goodIndex |> List.map .batchId)
        , test "a row's counts are decoded" <|
            \_ ->
                case Wire.decodeIndex goodIndex of
                    first :: _ ->
                        Expect.equal ( 7, 14, 3 )
                            ( first.counts.high, first.counts.medium, first.counts.low )

                    [] ->
                        Expect.fail "expected at least one row"
        , test "a row's requestId is decoded as its per-run result identity" <|
            \_ ->
                Expect.equal [ Just "exo-cs-req-42" ]
                    (Wire.decodeIndex """[ { "batchId": "b-1", "requestId": "exo-cs-req-42" } ]"""
                        |> List.map .requestId
                    )
        , test "a legacy row with no requestId decodes as Nothing (the batchId fallback)" <|
            \_ ->
                Expect.equal [ Nothing ] (Wire.decodeIndex goodIndex |> List.map .requestId |> List.take 1)
        , test "an empty requestId is Nothing, never an id that identifies nothing" <|
            \_ ->
                Expect.equal [ Nothing ]
                    (Wire.decodeIndex """[ { "batchId": "b-1", "requestId": "" } ]"""
                        |> List.map .requestId
                    )
        , test "unknown fields are tolerated and missing counts default to zero" <|
            \_ ->
                let
                    rows =
                        Wire.decodeIndex """[ { "batchId": "b-9", "targetName": "gamma", "extra": "ignored" } ]"""
                in
                case rows of
                    row :: _ ->
                        Expect.equal ( "b-9", "gamma", 0 )
                            ( row.batchId, row.targetName, row.counts.high )

                    [] ->
                        Expect.fail "expected a tolerant row"
        , test "malformed JSON reads as no history" <|
            \_ ->
                Expect.equal [] (Wire.decodeIndex "not json at all")
        , test "a non-array top level reads as no history" <|
            \_ ->
                Expect.equal [] (Wire.decodeIndex """{"batchId":"b-1"}""")
        , test "an over-cap body reads as no history (fail-closed at indexCapBytes)" <|
            \_ ->
                let
                    -- A valid JSON array padded past the 64 KiB cap.
                    oversize =
                        "[" ++ String.repeat (Wire.indexCapBytes + 16) " " ++ "]"
                in
                Expect.equal [] (Wire.decodeIndex oversize)
        , test "indexCapBytes is 64 KiB" <|
            \_ ->
                Expect.equal (64 * 1024) Wire.indexCapBytes
        , test "indexObjectName appends results/index.json to the prefix without a separator" <|
            \_ ->
                Expect.equal "instance-abc/results/index.json"
                    (Wire.indexObjectName "instance-abc/")
        ]


countsLabelSuite : Test
countsLabelSuite =
    describe "countsLabel renders a human severity summary, omitting zeros"
        [ test "non-zero severities are joined in descending order" <|
            \_ ->
                Expect.equal "7 high · 14 medium · 3 low"
                    (Wire.countsLabel { critical = 0, high = 7, medium = 14, low = 3, info = 0 })
        , test "all-zero counts read as no findings" <|
            \_ ->
                Expect.equal "no findings"
                    (Wire.countsLabel { critical = 0, high = 0, medium = 0, low = 0, info = 0 })
        , test "critical and info are included when present" <|
            \_ ->
                Expect.equal "2 critical · 5 info"
                    (Wire.countsLabel { critical = 2, high = 0, medium = 0, low = 0, info = 5 })
        ]


embedRequestSuite : Test
embedRequestSuite =
    describe "embedRequestJson encodes the getEmbed request shape"
        [ test "it carries schemaVersion, action=getEmbed, batchId, resultId and createdAt" <|
            \_ ->
                let
                    json =
                        Wire.embedRequestJson
                            { requestId = "exo-cs-req-1700000000000"
                            , batchId = "b-1"
                            , resultId = "exo-cs-req-1699999999999"
                            , createdAt = "2026-07-17T00:00:00Z"
                            }

                    field key =
                        Decode.decodeString (Decode.field key Decode.string) json
                in
                Expect.equal
                    ( Ok "1.0", Ok "getEmbed", ( Ok "b-1", Ok "exo-cs-req-1699999999999" ) )
                    ( field "schemaVersion", field "action", ( field "batchId", field "resultId" ) )
        ]


embedResultSuite : Test
embedResultSuite =
    describe "embedResultFromBody recognizes only kind:embed bodies"
        [ test "a kind:embed body is recognized with its fields" <|
            \_ ->
                let
                    body =
                        """{"schemaVersion":"1.0","requestId":"r-1","batchId":"b-1","completedAt":"2026-07-17T00:00:00Z","kind":"embed","status":"ok","embedUrl":"https://1-2-3-4.sslip.io/embed","embedExpiresAt":"2026-07-17T00:15:00Z","error":null}"""
                in
                case Wire.embedResultFromBody body of
                    Just embed ->
                        Expect.equal ( "b-1", "ok", "https://1-2-3-4.sslip.io/embed" )
                            ( embed.batchId, embed.status, embed.embedUrl )

                    Nothing ->
                        Expect.fail "expected an embed result"
        , test "an echoed resultId is decoded: the session's own §4.2 identity" <|
            \_ ->
                Wire.embedResultFromBody """{"kind":"embed","requestId":"r-1","batchId":"b-1","resultId":"exo-cs-req-7","status":"ok"}"""
                    |> Maybe.andThen .resultId
                    |> Expect.equal (Just "exo-cs-req-7")
        , test "a publisher echoing no resultId decodes as Nothing (the reader falls back)" <|
            \_ ->
                Wire.embedResultFromBody """{"kind":"embed","requestId":"r-1","batchId":"b-1","status":"ok"}"""
                    |> Maybe.andThen .resultId
                    |> Expect.equal Nothing
        , test "an empty resultId is Nothing, never an id that identifies nothing" <|
            \_ ->
                Wire.embedResultFromBody """{"kind":"embed","requestId":"r-1","batchId":"b-1","resultId":"","status":"ok"}"""
                    |> Maybe.andThen .resultId
                    |> Expect.equal Nothing
        , test "a scan-result body (no kind field) is not an embed result" <|
            \_ ->
                Expect.equal Nothing
                    (Wire.embedResultFromBody """{"schemaVersion":"1.0","findings":[],"embedUrl":"https://x"}""")
        , test "an error embed result carries its status" <|
            \_ ->
                Wire.embedResultFromBody """{"kind":"embed","batchId":"b-1","status":"error","error":{"code":"expired"}}"""
                    |> Maybe.map .status
                    |> Expect.equal (Just "error")
        ]


getEmbedBlockedSuite : Test
getEmbedBlockedSuite =
    describe "getEmbedBlocked: the scan-protection guard (only blocks a genuinely in-flight scan)"
        [ test "blocks while the run is queued (no pending scan needed)" <|
            \_ ->
                Expect.equal True
                    (Wire.getEmbedBlocked Nothing (meta [ ( "exoext.v1.run.state", "queued" ) ]))
        , test "blocks while the run is running (no pending scan needed)" <|
            \_ ->
                Expect.equal True
                    (Wire.getEmbedBlocked Nothing (meta [ ( "exoext.v1.run.state", "running" ) ]))
        , test "blocks while THIS reader's just-written scan is unclaimed (claimed missing)" <|
            \_ ->
                Expect.equal True
                    (Wire.getEmbedBlocked (Just 1700000000000)
                        (meta [ ( "exoext.v1.req.seq", "1700000000000" ) ])
                    )
        , test "blocks while THIS reader's scan is written but claimed lags its seq" <|
            \_ ->
                Expect.equal True
                    (Wire.getEmbedBlocked (Just 1700000000001)
                        (meta
                            [ ( "exoext.v1.req.seq", "1700000000001" )
                            , ( "exoext.v1.req.claimed", "1700000000000" )
                            , ( "exoext.v1.run.state", "done" )
                            ]
                        )
                    )
        , test "allows a claimed request whose run is done (terminal)" <|
            \_ ->
                Expect.equal False
                    (Wire.getEmbedBlocked (Just 1700000000000)
                        (meta
                            [ ( "exoext.v1.req.seq", "1700000000000" )
                            , ( "exoext.v1.req.claimed", "1700000000000" )
                            , ( "exoext.v1.run.state", "done" )
                            ]
                        )
                    )
        , test "allows a getEmbed to supersede a prior unclaimed getEmbed (the wire req is not the scan's seq)" <|
            \_ ->
                -- The req slot is unclaimed, but it belongs to an earlier getEmbed (seq 1700000000200),
                -- NOT to the reader's pending scan (seq 100). A timed-out getEmbed must not wedge the
                -- next View, so this is allowed.
                Expect.equal False
                    (Wire.getEmbedBlocked (Just 100)
                        (meta
                            [ ( "exoext.v1.req.seq", "1700000000200" )
                            , ( "exoext.v1.run.state", "done" )
                            ]
                        )
                    )
        , test "allows when an unclaimed req exists but the reader has no pending scan" <|
            \_ ->
                Expect.equal False
                    (Wire.getEmbedBlocked Nothing (meta [ ( "exoext.v1.req.seq", "1700000000000" ) ]))
        , test "allows on error and expired terminal states" <|
            \_ ->
                Expect.equal ( False, False )
                    ( Wire.getEmbedBlocked Nothing (meta [ ( "exoext.v1.run.state", "error" ) ])
                    , Wire.getEmbedBlocked Nothing (meta [ ( "exoext.v1.run.state", "expired" ) ])
                    )
        , test "allows when there is no run and no request slot" <|
            \_ ->
                Expect.equal False (Wire.getEmbedBlocked Nothing (meta [ ( "foo", "bar" ) ]))
        ]
