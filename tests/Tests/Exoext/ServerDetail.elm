module Tests.Exoext.ServerDetail exposing (extensionBusyProjectionSuite, extensionEmbedProjectionSuite, extensionPendingEmbedSuite, extensionScanBlockedSuite, extensionScanTimerSuite)

{-| The ADAPTER's own host wiring: the history/embed projection, the scan-completion timer, the
busy projections the reference manifest binds against, and the pending-embed tracker.

The generic host behavior these suites used to sit next to now lives in `Tests.Exoext.Host`, and the
fixtures both share live in `Tests.Exoext.Fixtures`.

-}

import Exoext.Card as Card
import Exoext.Reader as Reader
import Expect
import ISO8601
import Json.Decode as Decode
import Json.Encode as Encode
import Page.ServerDetail as ServerDetail
import Test exposing (Test, describe, test)
import Tests.Exoext.CardFixtures exposing (cardViewConfig)
import Tests.Exoext.Fixtures exposing (advance, afterExpiry, archivedFindingsJson, beforeExpiry, embedResultMetadata, errorEmbedBody, expiresAtIso, modelAfterStartScan, modelPendingEmbed, modelRecording, modelWithArchivedFindings, modelWithResultRef, objectFor, okEmbedBody, okEmbedBodyWithResultId, projectPublishing, runSlot, serverPublishing, writeRequestAt)
import Time


{-| The §4.2 body of a just-completed live scan — the one the pane auto-opens when a run settles.
Its findings and embed URL are deliberately unlike any history pick's, so a test can tell WHICH of
the two the pane bound.
-}
liveScanBody : String
liveScanBody =
    """{"schemaVersion":"1.0","requestId":"exoext-req-42","batchId":null,"completedAt":"2026-07-20T21:00:00.000Z","status":"ok","findings":[{"severity":"high"}],"embedUrl":"https://vm.example/live-scan"}"""


liveScanFindingsJson : String
liveScanFindingsJson =
    """[{"severity":"high"}]"""


extensionEmbedProjectionSuite : Test
extensionEmbedProjectionSuite =
    describe "Exoext.Reader.projection (history-View reader)"
        [ test "an ok, unexpired embed result renders findings + the iframe url and reports EmbedReady" <|
            \_ ->
                let
                    projection =
                        ServerDetail.exoextReaderProjection
                            (embedResultMetadata (okEmbedBody expiresAtIso))
                            (objectFor "b1")
                            beforeExpiry
                            Nothing
                            Nothing
                            modelWithArchivedFindings
                in
                Expect.equal
                    ( "https://vm.example/embed", Just archivedFindingsJson, Card.EmbedReady )
                    ( projection.embedUrl
                    , projection.results |> Maybe.map (Encode.encode 0)
                    , projection.embedState
                    )
        , test "switching siblings does NOT bind the previous scan's cached findings (the stale-body fix)" <|
            \_ ->
                -- The live shape: results/b1.json is cached from viewing one sibling, the user
                -- presses View on the next, and its body has not landed yet. The row and the iframe
                -- are already the NEW scan's, so binding the cached findings would put two
                -- different scans on screen at once. It must read as not-yet-loaded instead.
                let
                    projection =
                        ServerDetail.exoextReaderProjection
                            (embedResultMetadata (okEmbedBodyWithResultId "exoext-req-7"))
                            (objectFor "exoext-req-7")
                            beforeExpiry
                            Nothing
                            Nothing
                            modelWithArchivedFindings
                in
                Expect.equal ( Nothing, Just "exoext-req-7" )
                    ( projection.results |> Maybe.map (Encode.encode 0)
                    , projection.activeResultId
                    )
        , test "a cached body IS bound once its object is the one the current result names" <|
            \_ ->
                -- The other half of the gate, so the test above cannot pass by binding nothing ever.
                let
                    projection =
                        ServerDetail.exoextReaderProjection
                            (embedResultMetadata (okEmbedBodyWithResultId "b1"))
                            (objectFor "b1")
                            beforeExpiry
                            Nothing
                            Nothing
                            modelWithArchivedFindings
                in
                Expect.equal ( Just archivedFindingsJson, Just "b1" )
                    ( projection.results |> Maybe.map (Encode.encode 0)
                    , projection.activeResultId
                    )
        , test "a cached body from the right object but a stale manifest etag stays unbound" <|
            \_ ->
                -- The etag gate is an AND with the object name, not a replacement for it.
                let
                    projection =
                        ServerDetail.exoextReaderProjection
                            (embedResultMetadata (okEmbedBody expiresAtIso))
                            (objectFor "b1")
                            beforeExpiry
                            Nothing
                            Nothing
                            (modelWithResultRef "etag-old" "results/b1.json" """{"findings":[{"severity":"low"}]}""")
                in
                Expect.equal Nothing projection.results
        , test "the same result past its expiry unmounts the iframe, reports EmbedExpired, and marks the row expired (not active) — the expiry fix" <|
            \_ ->
                let
                    projection =
                        ServerDetail.exoextReaderProjection
                            (embedResultMetadata (okEmbedBody expiresAtIso))
                            (objectFor "b1")
                            afterExpiry
                            Nothing
                            Nothing
                            modelWithArchivedFindings
                in
                -- The expiry fix: an expired session no longer reads as active ("Now viewing"). The
                -- iframe unmounts (embedUrl ""), the state is EmbedExpired, and the previously-viewed
                -- row becomes `expiredResultId` (a muted "Expired"/View row) instead of `activeResultId`.
                Expect.equal ( "", Card.EmbedExpired, ( Nothing, Just "b1" ) )
                    ( projection.embedUrl, projection.embedState, ( projection.activeResultId, projection.expiredResultId ) )
        , test "an error embed result reports EmbedError with the unwrapped message and no iframe" <|
            \_ ->
                let
                    projection =
                        ServerDetail.exoextReaderProjection
                            (embedResultMetadata errorEmbedBody)
                            Nothing
                            beforeExpiry
                            Nothing
                            Nothing
                            (ServerDetail.init "self")
                in
                Expect.equal ( "", Card.EmbedError "remint failed" )
                    ( projection.embedUrl, projection.embedState )
        , test "a pending getEmbed with no matching result yet reports EmbedLoading and its resultId as pendingResultId" <|
            \_ ->
                let
                    now =
                        Time.millisToPosix 1000000

                    projection =
                        ServerDetail.exoextReaderProjection
                            [ { key = "exoext.v1.etag", value = "etag-1" } ]
                            Nothing
                            now
                            Nothing
                            Nothing
                            (modelPendingEmbed now)
                in
                Expect.equal ( Card.EmbedLoading, Just "b1" )
                    ( projection.embedState, projection.pendingResultId )
        , test "a pending getEmbed older than the timeout reports EmbedError and clears pendingResultId (no stuck loading)" <|
            \_ ->
                let
                    now =
                        Time.millisToPosix 1000000

                    projection =
                        ServerDetail.exoextReaderProjection
                            [ { key = "exoext.v1.etag", value = "etag-1" } ]
                            Nothing
                            now
                            Nothing
                            Nothing
                            (modelPendingEmbed (Time.millisToPosix (1000000 - 31000)))
                in
                Expect.equal ( Card.EmbedError "the request timed out", Nothing )
                    ( projection.embedState, projection.pendingResultId )
        , test "a matching error result reports EmbedError and clears pendingResultId (no stuck loading)" <|
            \_ ->
                let
                    -- pending marker whose requestId matches the error body's (exoext-req-100).
                    model =
                        let
                            base =
                                ServerDetail.init "self"
                        in
                        { base
                            | exoextPendingEmbed =
                                Just { seq = 0, requestId = "exoext-req-100", kind = "getEmbed", subject = "b1", since = beforeExpiry }
                        }

                    projection =
                        ServerDetail.exoextReaderProjection
                            (embedResultMetadata errorEmbedBody)
                            Nothing
                            beforeExpiry
                            Nothing
                            Nothing
                            model
                in
                Expect.equal ( Card.EmbedError "remint failed", Nothing )
                    ( projection.embedState, projection.pendingResultId )
        , test "a just-completed scan whose result batchId is null keys activeResultId on its requestId (mirrors the index row so the row reads Now viewing)" <|
            \_ ->
                let
                    doneScanBody =
                        """{"schemaVersion":"1.0","requestId":"exoext-req-42","batchId":null,"completedAt":"2026-07-20T21:00:00.000Z","status":"ok","findings":[]}"""

                    projection =
                        ServerDetail.exoextReaderProjection
                            [ { key = "exoext.v1.etag", value = "etag-1" } ]
                            Nothing
                            beforeExpiry
                            (Just { targetId = "i-1", state = "done" })
                            (Just doneScanBody)
                            (ServerDetail.init "self")
                in
                Expect.equal (Just "exoext-req-42") projection.activeResultId
        , test "a just-completed scan in a BATCH keys activeResultId on its own requestId, not the shared batchId" <|
            \_ ->
                -- The history row it has to match is keyed by requestId too, and every sibling of
                -- the batch shares `batch-9` — keying on that flags them all "Now viewing" at once.
                let
                    doneScanBody =
                        """{"schemaVersion":"1.0","requestId":"exoext-req-42","batchId":"batch-9","completedAt":"2026-07-20T21:00:00.000Z","status":"ok","findings":[]}"""

                    projection =
                        ServerDetail.exoextReaderProjection
                            [ { key = "exoext.v1.etag", value = "etag-1" } ]
                            Nothing
                            beforeExpiry
                            (Just { targetId = "i-1", state = "done" })
                            (Just doneScanBody)
                            (ServerDetail.init "self")
                in
                Expect.equal (Just "exoext-req-42") projection.activeResultId
        , test "a result body carrying no requestId at all falls back to its batchId" <|
            \_ ->
                let
                    doneScanBody =
                        """{"schemaVersion":"1.0","batchId":"batch-9","completedAt":"2026-07-20T21:00:00.000Z","status":"ok","findings":[]}"""

                    projection =
                        ServerDetail.exoextReaderProjection
                            [ { key = "exoext.v1.etag", value = "etag-1" } ]
                            Nothing
                            beforeExpiry
                            (Just { targetId = "i-1", state = "done" })
                            (Just doneScanBody)
                            (ServerDetail.init "self")
                in
                Expect.equal (Just "batch-9") projection.activeResultId
        , test "a settled live scan auto-opens its own findings + iframe when nothing else is being opened" <|
            \_ ->
                -- (a) The baseline this pane is for: a run finishes and its results appear by
                -- themselves. Pinned so the supersession below cannot be mistaken for it breaking.
                let
                    projection =
                        ServerDetail.exoextReaderProjection
                            [ { key = "exoext.v1.etag", value = "etag-1" } ]
                            Nothing
                            beforeExpiry
                            (Just { targetId = "i-1", state = "done" })
                            (Just liveScanBody)
                            (ServerDetail.init "self")
                in
                Expect.equal
                    ( Just liveScanFindingsJson, "https://vm.example/live-scan", Just "exoext-req-42" )
                    ( projection.results |> Maybe.map (Encode.encode 0)
                    , projection.embedUrl
                    , projection.activeResultId
                    )
        , test "pressing View on another row closes the auto-opened live scan AT THE PRESS, not when the bridge answers" <|
            \_ ->
                -- The live bug: the getEmbed is in flight (~10s), and until it landed the finished
                -- scan's findings and iframe stayed mounted under an "Opening…" row, so View read as
                -- "won't close" — and a live embedUrl was on screen outside EmbedReady.
                let
                    projection =
                        ServerDetail.exoextReaderProjection
                            [ { key = "exoext.v1.etag", value = "etag-1" } ]
                            Nothing
                            beforeExpiry
                            (Just { targetId = "i-1", state = "done" })
                            (Just liveScanBody)
                            (modelPendingEmbed beforeExpiry)
                in
                Expect.equal
                    ( ( Nothing, "", Nothing )
                    , ( Card.EmbedLoading, Just "b1" )
                    )
                    ( ( projection.results, projection.embedUrl, projection.activeResultId )
                    , ( projection.embedState, projection.pendingResultId )
                    )
        , test "when the embed result lands, the pane shows the picked scan — not the still-done live one" <|
            \_ ->
                -- (c) The far end of the same press. `clearResolvedPendingEmbed` has dropped the
                -- pending marker, the run slot still reads `done`, and the history pick must win
                -- both bindings. Not covered elsewhere: every other ok-embed test passes no
                -- statusOverride, so none of them pit the two branches against each other.
                let
                    projection =
                        ServerDetail.exoextReaderProjection
                            (embedResultMetadata (okEmbedBodyWithResultId "exoext-req-7"))
                            (objectFor "exoext-req-7")
                            beforeExpiry
                            (Just { targetId = "i-1", state = "done" })
                            (Just liveScanBody)
                            (modelWithResultRef "etag-1" "results/exoext-req-7.json" """{"findings":[{"severity":"low"}]}""")
                in
                Expect.equal
                    ( ( Just archivedFindingsJson, "https://vm.example/embed" )
                    , ( Just "exoext-req-7", Card.EmbedReady )
                    )
                    ( ( projection.results |> Maybe.map (Encode.encode 0), projection.embedUrl )
                    , ( projection.activeResultId, projection.embedState )
                    )
        , test "the response's echoed resultId names the open session, with no host record at all (the reload case)" <|
            \_ ->
                -- After a page reload the host's request record is gone. Only the echoed resultId
                -- can still say which archived run is on screen; without it the reader would fall
                -- back to the SHARED batchId and flag every sibling of the batch again.
                let
                    projection =
                        ServerDetail.exoextReaderProjection
                            (embedResultMetadata (okEmbedBodyWithResultId "exoext-req-7"))
                            (objectFor "exoext-req-7")
                            beforeExpiry
                            Nothing
                            Nothing
                            modelWithArchivedFindings
                in
                Expect.equal ( Card.EmbedReady, Just "exoext-req-7", Nothing )
                    ( projection.embedState
                    , projection.activeResultId
                    , modelWithArchivedFindings.exoextEmbedResultId
                    )
        , test "with no echoed resultId, the host's own request record names the session" <|
            \_ ->
                -- A publisher predating the echoed field: the record is the second source.
                let
                    projection =
                        ServerDetail.exoextReaderProjection
                            (embedResultMetadata (okEmbedBody expiresAtIso))
                            (objectFor "exoext-req-7")
                            beforeExpiry
                            Nothing
                            Nothing
                            (modelRecording "exoext-req-100" "exoext-req-7")
                in
                Expect.equal ( Card.EmbedReady, Just "exoext-req-7" )
                    ( projection.embedState, projection.activeResultId )
        , test "an echoed resultId wins over a record for the same request" <|
            \_ ->
                -- Precedence, not merely coverage: the wire is self-describing and authoritative.
                let
                    projection =
                        ServerDetail.exoextReaderProjection
                            (embedResultMetadata (okEmbedBodyWithResultId "exoext-req-7"))
                            (objectFor "exoext-req-7")
                            beforeExpiry
                            Nothing
                            Nothing
                            (modelRecording "exoext-req-100" "exoext-req-STALE")
                in
                Expect.equal (Just "exoext-req-7") projection.activeResultId
        , test "neither source present falls back to the response's batchId (legacy archive)" <|
            \_ ->
                let
                    projection =
                        ServerDetail.exoextReaderProjection
                            (embedResultMetadata (okEmbedBody expiresAtIso))
                            (objectFor "b1")
                            beforeExpiry
                            Nothing
                            Nothing
                            modelWithArchivedFindings
                in
                Expect.equal (Just "b1") projection.activeResultId
        , test "after a reload, exactly ONE of two siblings sharing a batchId reads as viewing" <|
            \_ ->
                -- The end of the chain the coordinator cares about: wire response -> host
                -- projection -> rendered row state, with no host record in play.
                let
                    projection =
                        ServerDetail.exoextReaderProjection
                            (embedResultMetadata (okEmbedBodyWithResultId "exoext-req-7"))
                            (objectFor "exoext-req-7")
                            beforeExpiry
                            Nothing
                            Nothing
                            modelWithArchivedFindings

                    sibling requestId =
                        { batchId = "b1"
                        , requestId = Just requestId
                        , targetId = "i-1"
                        , targetName = "alpha"
                        , completedAt = "2026-07-01T00:00:00Z"
                        , status = "done"
                        , counts = { critical = 0, high = 1, medium = 0, low = 0, info = 0 }
                        , error = Nothing
                        }

                    rowStates_ =
                        Card.projection Time.utc
                            { cardViewConfig
                                | history =
                                    { rows = [ sibling "exoext-req-6", sibling "exoext-req-7" ]
                                    , loading = False
                                    , loaded = True
                                    }
                                , activeResultId = projection.activeResultId
                                , pendingResultId = projection.pendingResultId
                                , erroredResultId = projection.erroredResultId
                                , expiredResultId = projection.expiredResultId
                            }
                            []
                            Card.init
                            |> Decode.decodeValue
                                (Decode.field "history" (Decode.list (Decode.field "state" Decode.string)))
                            |> Result.mapError Decode.errorToString
                in
                -- Newest first, so the viewed run (exoext-req-7) leads and its sibling stays idle.
                Expect.equal (Ok [ "viewing", "idle" ]) rowStates_
        ]


{-| The completion-timer descriptor: freeze to the full wall-clock flow (`completedAt - start`), and
never to the scanner-only `summary.durationSec`. Covers spec item #2.
-}
extensionScanTimerSuite : Test
extensionScanTimerSuite =
    let
        completedAtIso =
            "2026-07-20T21:02:10.000Z"

        completedMillis =
            ISO8601.fromString completedAtIso
                |> Result.map (ISO8601.toPosix >> Time.posixToMillis)
                |> Result.withDefault 0

        -- start 130s before completion (the full snapshot -> clone -> scan wall-clock flow).
        startMillis =
            completedMillis - 130000

        bodyWith completedField =
            "{\"requestId\":\"exoext-req-42\"" ++ completedField ++ ",\"summary\":{\"durationSec\":22}}"
    in
    describe "Exoext.Reader.completionTimer (frozen completion time)"
        [ test "no tracked run yields no descriptor" <|
            \_ ->
                Expect.equal Nothing (Reader.completionTimer Nothing "done" Nothing)
        , test "a running scan yields a live descriptor with no frozen duration (the row carries progress)" <|
            \_ ->
                Expect.equal (Just { startMillis = startMillis, doneDurationSec = Nothing })
                    (Reader.completionTimer (Just startMillis) "running" Nothing)
        , test "a done scan freezes to the full wall-clock flow (completedAt - start), not the 22s scanner time" <|
            \_ ->
                Expect.equal (Just { startMillis = startMillis, doneDurationSec = Just 130 })
                    (Reader.completionTimer (Just startMillis) "done" (Just (bodyWith (",\"completedAt\":\"" ++ completedAtIso ++ "\""))))
        , test "a done scan with no completedAt shows no frozen duration (no durationSec fallback)" <|
            \_ ->
                Expect.equal (Just { startMillis = startMillis, doneDurationSec = Nothing })
                    (Reader.completionTimer (Just startMillis) "done" (Just (bodyWith "")))
        , test "a terminal error state drops the descriptor" <|
            \_ ->
                Expect.equal Nothing (Reader.completionTimer (Just startMillis) "error" Nothing)
        ]



-- BATCH CONTINUATION (§7.1 sequential pacing)


{-| The two §7.1 guards as the manifest sees them, through the whole host path: `exoextViewConfig`
projected by `Exoext.Card.projection`. This is the wiring test — that `/scanBusy` is genuinely
`exoextRequestBlocked` (the predicate that gates a SCAN press) and not the View guard beside it.
-}
extensionBusyProjectionSuite : Test
extensionBusyProjectionSuite =
    let
        busyFlags metadata model =
            Card.projection Time.utc
                (ServerDetail.exoextViewConfig True (projectPublishing metadata) model (Time.millisToPosix 0) (serverPublishing metadata))
                []
                model.exoextCard
                |> Decode.decodeValue
                    (Decode.map2 Tuple.pair
                        (Decode.field "scanBusy" Decode.bool)
                        (Decode.field "requestBusy" Decode.bool)
                    )
                |> Result.mapError Decode.errorToString

        draining =
            writeRequestAt 1700 { subject = "i-1", batchId = Nothing } modelAfterStartScan
    in
    describe "ServerDetail /scanBusy and /requestBusy"
        [ test "a live run makes both guards busy" <|
            \_ ->
                Expect.equal (Ok ( True, True )) (busyFlags (runSlot 1700 "running") draining)
        , test "between two siblings BOTH guards stay busy — the slot is still spoken for" <|
            \_ ->
                -- The tracked run reads terminal, so the run clause has lifted, but the batch still
                -- has targets parked. Both flags must stay set: a Scan press here strands the
                -- batch's remaining targets, and a View press bumps the slot out from under the
                -- sibling that is about to be written. One slot, one answer.
                Expect.equal (Ok ( True, True )) (busyFlags (runSlot 1700 "done") draining)
        , test "an idle card is busy on neither" <|
            \_ ->
                Expect.equal (Ok ( False, False ))
                    (busyFlags (runSlot 1700 "done") (ServerDetail.init "self"))
        ]


extensionScanBlockedSuite : Test
extensionScanBlockedSuite =
    let
        afterFirstWrite =
            writeRequestAt 1000 { subject = "i-1", batchId = Nothing } modelAfterStartScan

        -- The same batch on its last leg: nothing left to write, target 3's run finished.
        drained =
            afterFirstWrite
                |> advance 1000 "done"
                |> writeRequestAt 2000 { subject = "i-2", batchId = Just "exoext-batch-1000" }
                |> advance 2000 "done"
                |> writeRequestAt 3000 { subject = "i-3", batchId = Just "exoext-batch-1000" }
                |> advance 3000 "done"
    in
    describe "ServerDetail exoextRequestBlocked (§7.1 guard on a user-initiated scan)"
        [ test "blocked while the tracked run is still going" <|
            \_ ->
                Expect.equal True
                    (ServerDetail.exoextRequestBlocked (projectPublishing (runSlot 1000 "running")) afterFirstWrite)
        , test "blocked between two siblings, when the tracked run already reads terminal" <|
            \_ ->
                -- The clause the run check alone would miss: target 1 is done, but targets 2 and 3
                -- are still parked, so a new scan here would replace the batch and strand them.
                Expect.equal True
                    (ServerDetail.exoextRequestBlocked (projectPublishing (runSlot 1000 "done")) afterFirstWrite)
        , test "blocked while a decided continuation's write is still on its way" <|
            \_ ->
                -- Last subject popped (`remaining` empty) but not yet written.
                Expect.equal True
                    (ServerDetail.exoextRequestBlocked (projectPublishing (runSlot 2000 "done"))
                        { afterFirstWrite | exoextBatch = Just { batchId = Just "exoext-batch-1000", remaining = [], awaitingWrite = True } }
                    )
        , test "not blocked once the batch has drained — a completed batch must allow a new scan" <|
            \_ ->
                Expect.equal False
                    (ServerDetail.exoextRequestBlocked (projectPublishing (runSlot 3000 "done")) drained)
        ]


extensionPendingEmbedSuite : Test
extensionPendingEmbedSuite =
    describe "ServerDetail clearResolvedPendingEmbed"
        [ test "a matching-requestId result clears the pending marker" <|
            \_ ->
                Expect.equal Nothing
                    (ServerDetail.clearResolvedPendingEmbed
                        (embedResultMetadata (okEmbedBody expiresAtIso))
                        (Just { seq = 0, requestId = "exoext-req-100", kind = "getEmbed", subject = "b1", since = Time.millisToPosix 0 })
                    )
        , test "a result for a different requestId leaves the pending marker in place" <|
            \_ ->
                let
                    pending =
                        Just { seq = 0, requestId = "exoext-req-999", kind = "getEmbed", subject = "b1", since = Time.millisToPosix 0 }
                in
                Expect.equal pending
                    (ServerDetail.clearResolvedPendingEmbed
                        (embedResultMetadata (okEmbedBody expiresAtIso))
                        pending
                    )
        ]
