module Tests.CloudShield.ServerDetail exposing (cloudShieldBatchSuite, cloudShieldEmbedProjectionSuite, cloudShieldPendingEmbedSuite, cloudShieldReadDecisionSuite, cloudShieldRecoverySuite, cloudShieldScanBlockedSuite, cloudShieldScanTimerSuite)

import CloudShield.Card as Card
import Dict
import Expect
import Helpers.RemoteDataPlusPlus as RDPP
import ISO8601
import Json.Decode as Decode
import Json.Encode as Encode
import OpenStack.Types as OSTypes
import Page.ServerDetail as ServerDetail
import Test exposing (Test, describe, test)
import Time
import Types.HelperTypes as HelperTypes
import Types.Interactivity as Interactivity
import Types.Project exposing (Project, ProjectSecret(..))
import Types.Server exposing (Server, ServerOrigin(..))


receivedAt : Time.Posix
receivedAt =
    Time.millisToPosix 0


modelWithManifest : String -> String -> ServerDetail.Model
modelWithManifest etag body =
    let
        model =
            ServerDetail.init "self"
    in
    { model
        | exoextManifest =
            RDPP.RemoteDataPlusPlus
                (RDPP.DoHave { etag = etag, body = body } receivedAt)
                (RDPP.NotLoading Nothing)
    }


modelWithResultRef : String -> String -> String -> ServerDetail.Model
modelWithResultRef etag objectName body =
    let
        model =
            ServerDetail.init "self"
    in
    { model
        | exoextResultRef =
            RDPP.RemoteDataPlusPlus
                (RDPP.DoHave { etag = etag, objectName = objectName, body = body } receivedAt)
                (RDPP.NotLoading Nothing)
    }


cloudShieldReadDecisionSuite : Test
cloudShieldReadDecisionSuite =
    describe "ServerDetail CloudShield Swift read decisions"
        [ test "a missing manifest body needs a fetch for the current etag" <|
            \_ ->
                Expect.equal True
                    (ServerDetail.exoextManifestNeedsFetch "etag-1" (ServerDetail.init "self"))
        , test "a matching manifest body is usable and does not refetch" <|
            \_ ->
                let
                    model =
                        modelWithManifest "etag-1" """{"ui":{}}"""
                in
                Expect.equal ( Just """{"ui":{}}""", False )
                    ( ServerDetail.exoextManifestBodyForEtag "etag-1" model
                    , ServerDetail.exoextManifestNeedsFetch "etag-1" model
                    )
        , test "a stale manifest body is ignored and needs a fetch for the new etag" <|
            \_ ->
                let
                    model =
                        modelWithManifest "etag-old" """{"ui":{"stale":true}}"""
                in
                Expect.equal ( Nothing, True )
                    ( ServerDetail.exoextManifestBodyForEtag "etag-new" model
                    , ServerDetail.exoextManifestNeedsFetch "etag-new" model
                    )
        , test "an in-flight manifest request suppresses a duplicate fetch for the same etag" <|
            \_ ->
                let
                    base =
                        ServerDetail.init "self"

                    model =
                        { base | exoextManifestRequestEtag = Just "etag-1" }
                in
                Expect.equal False (ServerDetail.exoextManifestNeedsFetch "etag-1" model)
        , test "inline result bodies are selected directly" <|
            \_ ->
                let
                    body =
                        """{"findings":[],"embedUrl":"https://example.test"}"""
                in
                Expect.equal (Just body)
                    (ServerDetail.effectiveExoextResultBody "etag-1" body (ServerDetail.init "self"))
        , test "a ref result waits until the pointed object has been fetched" <|
            \_ ->
                Expect.equal Nothing
                    (ServerDetail.effectiveExoextResultBody "etag-1" """{"ref":"results/run-1.json"}""" (ServerDetail.init "self"))
        , test "a ref result uses the matching fetched object body" <|
            \_ ->
                let
                    fetched =
                        """{"findings":[{"severity":"low"}]}"""
                in
                Expect.equal (Just fetched)
                    (ServerDetail.effectiveExoextResultBody
                        "etag-1"
                        """{"ref":"results/run-1.json"}"""
                        (modelWithResultRef "etag-1" "results/run-1.json" fetched)
                    )
        , test "a ref result ignores a fetched object from a stale etag" <|
            \_ ->
                Expect.equal Nothing
                    (ServerDetail.effectiveExoextResultBody
                        "etag-new"
                        """{"ref":"results/run-1.json"}"""
                        (modelWithResultRef "etag-old" "results/run-1.json" """{"findings":[]}""")
                    )
        ]



-- HISTORY-VIEW EMBED PROJECTION (spinner / error surface / expiry guard)


{-| The archived scan body's `findings`, as `/results` should carry them.
-}
archivedFindingsJson : String
archivedFindingsJson =
    """[{"severity":"low"}]"""


{-| A model that has fetched the archived scan body for `etag-1` (a history pick's findings). The
cached object is the one `objectFor "b1"` names.
-}
modelWithArchivedFindings : ServerDetail.Model
modelWithArchivedFindings =
    modelWithResultRef "etag-1" "results/b1.json" """{"findings":[{"severity":"low"}]}"""


{-| The archived-result object a given result id lives at (§4.2), the way
`exoextResultObjectName` builds it. A cached body is bound only when its object name is THIS one.
-}
objectFor : String -> Maybe String
objectFor resultId =
    Just ("results/" ++ resultId ++ ".json")


{-| The §4.2 body of a just-completed live scan — the one the pane auto-opens when a run settles.
Its findings and embed URL are deliberately unlike any history pick's, so a test can tell WHICH of
the two the pane bound.
-}
liveScanBody : String
liveScanBody =
    """{"schemaVersion":"1.0","requestId":"exo-cs-req-42","batchId":null,"completedAt":"2026-07-20T21:00:00.000Z","status":"ok","findings":[{"severity":"high"}],"embedUrl":"https://vm.example/live-scan"}"""


liveScanFindingsJson : String
liveScanFindingsJson =
    """[{"severity":"high"}]"""


{-| Res-slot metadata carrying a chunked `kind:"embed"` result body, plus the manifest etag the
archived-findings fetch is keyed on.
-}
embedResultMetadata : String -> List { key : String, value : String }
embedResultMetadata bodyJson =
    [ { key = "exoext.v1.etag", value = "etag-1" }
    , { key = "exoext.v1.res.body.n", value = "1" }
    , { key = "exoext.v1.res.body.0", value = bodyJson }
    ]


{-| An `status:"ok"` embed result body for requestId `exo-cs-req-100`, expiring at `expiresAt`.
-}
okEmbedBody : String -> String
okEmbedBody expiresAt =
    "{\"kind\":\"embed\",\"requestId\":\"exo-cs-req-100\",\"batchId\":\"b1\",\"status\":\"ok\",\"embedUrl\":\"https://vm.example/embed\",\"embedExpiresAt\":\"" ++ expiresAt ++ "\"}"


{-| The same ok embed result, but from a publisher that echoes the §4.2 `resultId` the session was
minted for. `batchId` stays `b1` — the id its siblings share.
-}
okEmbedBodyWithResultId : String -> String
okEmbedBodyWithResultId resultId =
    "{\"kind\":\"embed\",\"requestId\":\"exo-cs-req-100\",\"batchId\":\"b1\",\"resultId\":\""
        ++ resultId
        ++ "\",\"status\":\"ok\",\"embedUrl\":\"https://vm.example/embed\",\"embedExpiresAt\":\""
        ++ expiresAtIso
        ++ "\"}"


{-| An `status:"error"` embed result body with a plain-string error.
-}
errorEmbedBody : String
errorEmbedBody =
    "{\"kind\":\"embed\",\"requestId\":\"exo-cs-req-100\",\"batchId\":\"b1\",\"status\":\"error\",\"embedUrl\":\"\",\"embedExpiresAt\":\"\",\"error\":\"remint failed\"}"


{-| A fixed embed-token expiry, and the client clock set just before / just after it.
-}
expiresAtIso : String
expiresAtIso =
    "2026-07-20T21:00:00.000Z"


expiresMillis : Int
expiresMillis =
    ISO8601.fromString expiresAtIso
        |> Result.map (ISO8601.toPosix >> Time.posixToMillis)
        |> Result.withDefault 0


beforeExpiry : Time.Posix
beforeExpiry =
    Time.millisToPosix (expiresMillis - 60000)


afterExpiry : Time.Posix
afterExpiry =
    Time.millisToPosix (expiresMillis + 60000)


{-| A model that has fetched the archived findings AND still remembers which result its getEmbed
`requestId` was for — the pre-reload state, and the only source for a publisher that echoes no
`resultId` of its own.
-}
modelRecording : String -> String -> ServerDetail.Model
modelRecording requestId resultId =
    let
        base =
            modelWithArchivedFindings
    in
    { base | exoextEmbedResultId = Just { requestId = requestId, resultId = resultId } }


modelPendingEmbed : Time.Posix -> ServerDetail.Model
modelPendingEmbed since =
    let
        model =
            ServerDetail.init "self"
    in
    { model
        | exoextPendingEmbed =
            Just { seq = 0, requestId = "exo-cs-req-200", kind = "getEmbed", subject = "b1", since = since }
    }


cloudShieldEmbedProjectionSuite : Test
cloudShieldEmbedProjectionSuite =
    describe "ServerDetail exoextEmbedProjection (history-View reader)"
        [ test "an ok, unexpired embed result renders findings + the iframe url and reports EmbedReady" <|
            \_ ->
                let
                    projection =
                        ServerDetail.exoextEmbedProjection
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
                        ServerDetail.exoextEmbedProjection
                            (embedResultMetadata (okEmbedBodyWithResultId "exo-cs-req-7"))
                            (objectFor "exo-cs-req-7")
                            beforeExpiry
                            Nothing
                            Nothing
                            modelWithArchivedFindings
                in
                Expect.equal ( Nothing, Just "exo-cs-req-7" )
                    ( projection.results |> Maybe.map (Encode.encode 0)
                    , projection.activeResultId
                    )
        , test "a cached body IS bound once its object is the one the current result names" <|
            \_ ->
                -- The other half of the gate, so the test above cannot pass by binding nothing ever.
                let
                    projection =
                        ServerDetail.exoextEmbedProjection
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
                        ServerDetail.exoextEmbedProjection
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
                        ServerDetail.exoextEmbedProjection
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
                        ServerDetail.exoextEmbedProjection
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
                        ServerDetail.exoextEmbedProjection
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
                        ServerDetail.exoextEmbedProjection
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
                    -- pending marker whose requestId matches the error body's (exo-cs-req-100).
                    model =
                        let
                            base =
                                ServerDetail.init "self"
                        in
                        { base
                            | exoextPendingEmbed =
                                Just { seq = 0, requestId = "exo-cs-req-100", kind = "getEmbed", subject = "b1", since = beforeExpiry }
                        }

                    projection =
                        ServerDetail.exoextEmbedProjection
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
                        """{"schemaVersion":"1.0","requestId":"exo-cs-req-42","batchId":null,"completedAt":"2026-07-20T21:00:00.000Z","status":"ok","findings":[]}"""

                    projection =
                        ServerDetail.exoextEmbedProjection
                            [ { key = "exoext.v1.etag", value = "etag-1" } ]
                            Nothing
                            beforeExpiry
                            (Just { targetId = "i-1", state = "done" })
                            (Just doneScanBody)
                            (ServerDetail.init "self")
                in
                Expect.equal (Just "exo-cs-req-42") projection.activeResultId
        , test "a just-completed scan in a BATCH keys activeResultId on its own requestId, not the shared batchId" <|
            \_ ->
                -- The history row it has to match is keyed by requestId too, and every sibling of
                -- the batch shares `batch-9` — keying on that flags them all "Now viewing" at once.
                let
                    doneScanBody =
                        """{"schemaVersion":"1.0","requestId":"exo-cs-req-42","batchId":"batch-9","completedAt":"2026-07-20T21:00:00.000Z","status":"ok","findings":[]}"""

                    projection =
                        ServerDetail.exoextEmbedProjection
                            [ { key = "exoext.v1.etag", value = "etag-1" } ]
                            Nothing
                            beforeExpiry
                            (Just { targetId = "i-1", state = "done" })
                            (Just doneScanBody)
                            (ServerDetail.init "self")
                in
                Expect.equal (Just "exo-cs-req-42") projection.activeResultId
        , test "a result body carrying no requestId at all falls back to its batchId" <|
            \_ ->
                let
                    doneScanBody =
                        """{"schemaVersion":"1.0","batchId":"batch-9","completedAt":"2026-07-20T21:00:00.000Z","status":"ok","findings":[]}"""

                    projection =
                        ServerDetail.exoextEmbedProjection
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
                        ServerDetail.exoextEmbedProjection
                            [ { key = "exoext.v1.etag", value = "etag-1" } ]
                            Nothing
                            beforeExpiry
                            (Just { targetId = "i-1", state = "done" })
                            (Just liveScanBody)
                            (ServerDetail.init "self")
                in
                Expect.equal
                    ( Just liveScanFindingsJson, "https://vm.example/live-scan", Just "exo-cs-req-42" )
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
                        ServerDetail.exoextEmbedProjection
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
                        ServerDetail.exoextEmbedProjection
                            (embedResultMetadata (okEmbedBodyWithResultId "exo-cs-req-7"))
                            (objectFor "exo-cs-req-7")
                            beforeExpiry
                            (Just { targetId = "i-1", state = "done" })
                            (Just liveScanBody)
                            (modelWithResultRef "etag-1" "results/exo-cs-req-7.json" """{"findings":[{"severity":"low"}]}""")
                in
                Expect.equal
                    ( ( Just archivedFindingsJson, "https://vm.example/embed" )
                    , ( Just "exo-cs-req-7", Card.EmbedReady )
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
                        ServerDetail.exoextEmbedProjection
                            (embedResultMetadata (okEmbedBodyWithResultId "exo-cs-req-7"))
                            (objectFor "exo-cs-req-7")
                            beforeExpiry
                            Nothing
                            Nothing
                            modelWithArchivedFindings
                in
                Expect.equal ( Card.EmbedReady, Just "exo-cs-req-7", Nothing )
                    ( projection.embedState
                    , projection.activeResultId
                    , modelWithArchivedFindings.exoextEmbedResultId
                    )
        , test "with no echoed resultId, the host's own request record names the session" <|
            \_ ->
                -- A publisher predating the echoed field: the record is the second source.
                let
                    projection =
                        ServerDetail.exoextEmbedProjection
                            (embedResultMetadata (okEmbedBody expiresAtIso))
                            (objectFor "exo-cs-req-7")
                            beforeExpiry
                            Nothing
                            Nothing
                            (modelRecording "exo-cs-req-100" "exo-cs-req-7")
                in
                Expect.equal ( Card.EmbedReady, Just "exo-cs-req-7" )
                    ( projection.embedState, projection.activeResultId )
        , test "an echoed resultId wins over a record for the same request" <|
            \_ ->
                -- Precedence, not merely coverage: the wire is self-describing and authoritative.
                let
                    projection =
                        ServerDetail.exoextEmbedProjection
                            (embedResultMetadata (okEmbedBodyWithResultId "exo-cs-req-7"))
                            (objectFor "exo-cs-req-7")
                            beforeExpiry
                            Nothing
                            Nothing
                            (modelRecording "exo-cs-req-100" "exo-cs-req-STALE")
                in
                Expect.equal (Just "exo-cs-req-7") projection.activeResultId
        , test "neither source present falls back to the response's batchId (legacy archive)" <|
            \_ ->
                let
                    projection =
                        ServerDetail.exoextEmbedProjection
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
                        ServerDetail.exoextEmbedProjection
                            (embedResultMetadata (okEmbedBodyWithResultId "exo-cs-req-7"))
                            (objectFor "exo-cs-req-7")
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
                        }

                    rowStates_ =
                        Card.projection Time.utc
                            projection.activeResultId
                            projection.pendingResultId
                            projection.erroredResultId
                            projection.expiredResultId
                            False
                            { rows = [ sibling "exo-cs-req-6", sibling "exo-cs-req-7" ]
                            , loading = False
                            , loaded = True
                            }
                            Nothing
                            ""
                            Nothing
                            []
                            Card.init
                            |> Decode.decodeValue
                                (Decode.field "history" (Decode.list (Decode.field "state" Decode.string)))
                            |> Result.mapError Decode.errorToString
                in
                -- Newest first, so the viewed run (exo-cs-req-7) leads and its sibling stays idle.
                Expect.equal (Ok [ "viewing", "idle" ]) rowStates_
        ]


{-| The completion-timer descriptor: freeze to the full wall-clock flow (`completedAt - start`), and
never to the scanner-only `summary.durationSec`. Covers spec item #2.
-}
cloudShieldScanTimerSuite : Test
cloudShieldScanTimerSuite =
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
            "{\"requestId\":\"exo-cs-req-42\"" ++ completedField ++ ",\"summary\":{\"durationSec\":22}}"
    in
    describe "ServerDetail exoextScanTimer (frozen completion time)"
        [ test "no tracked run yields no descriptor" <|
            \_ ->
                Expect.equal Nothing (ServerDetail.exoextScanTimer Nothing "done" Nothing)
        , test "a running scan yields a live descriptor with no frozen duration (the row carries progress)" <|
            \_ ->
                Expect.equal (Just { startMillis = startMillis, doneDurationSec = Nothing })
                    (ServerDetail.exoextScanTimer (Just startMillis) "running" Nothing)
        , test "a done scan freezes to the full wall-clock flow (completedAt - start), not the 22s scanner time" <|
            \_ ->
                Expect.equal (Just { startMillis = startMillis, doneDurationSec = Just 130 })
                    (ServerDetail.exoextScanTimer (Just startMillis) "done" (Just (bodyWith (",\"completedAt\":\"" ++ completedAtIso ++ "\""))))
        , test "a done scan with no completedAt shows no frozen duration (no durationSec fallback)" <|
            \_ ->
                Expect.equal (Just { startMillis = startMillis, doneDurationSec = Nothing })
                    (ServerDetail.exoextScanTimer (Just startMillis) "done" (Just (bodyWith "")))
        , test "a terminal error state drops the descriptor" <|
            \_ ->
                Expect.equal Nothing (ServerDetail.exoextScanTimer (Just startMillis) "error" Nothing)
        ]



-- BATCH CONTINUATION (§7.1 sequential pacing)


{-| A minimal project: `update` needs one to build the Nova metadata write, but nothing here reads
its contents (an empty server list makes §5.4 target re-resolution fall back to the raw id).
-}
project : Project
project =
    { secret = NoProjectSecret
    , auth =
        { catalog = []
        , project = { name = "Project One", uuid = "project-uuid" }
        , projectDomain = { name = "Default", uuid = "project-domain-uuid" }
        , user = { name = "user-one", uuid = "user-uuid" }
        , userDomain = { name = "Default", uuid = "user-domain-uuid" }
        , expiresAt = Time.millisToPosix 0
        , tokenValue = "token"
        }
    , region = Just { id = "RegionOne", description = "Region One" }
    , endpoints =
        { cinder = "https://openstack.example/cinder"
        , glance = "https://openstack.example/glance"
        , keystone = "https://openstack.example/keystone/v3"
        , manila = Nothing
        , nova = "https://openstack.example/nova"
        , neutron = "https://openstack.example/neutron"
        , jetstream2Accounting = Nothing
        , designate = Nothing
        , swift = Nothing
        }
    , description = Nothing
    , images = RDPP.empty
    , servers = RDPP.empty
    , serverEvents = Dict.empty
    , serverExoActions = Dict.empty
    , serverSecurityGroups = Dict.empty
    , serverVolumeAttachments = Dict.empty
    , serverVolumeActions = Dict.empty
    , serverActionRequestQueue = Dict.empty
    , shares = RDPP.empty
    , shareAccessRules = Dict.empty
    , shareExportLocations = Dict.empty
    , shareTypes = RDPP.empty
    , objectStorageUploads = []
    , flavors = RDPP.empty
    , keypairs = RDPP.empty
    , volumes = RDPP.empty
    , volumeSnapshots = RDPP.empty
    , networks = RDPP.empty
    , autoAllocatedNetworkUuid = RDPP.empty
    , floatingIps = RDPP.empty
    , dnsRecordSets = RDPP.empty
    , ports = RDPP.empty
    , securityGroups = RDPP.empty
    , securityGroupActions = Dict.empty
    , computeQuota = RDPP.empty
    , volumeQuota = RDPP.empty
    , networkQuota = RDPP.empty
    , shareQuota = RDPP.empty
    , serverImages = []
    , jetstream2Allocations = RDPP.empty
    , knownUsernames = Dict.empty
    }


{-| The viewed CloudShield VM, publishing `metadata`. Only the uuid and the metadata are read by
anything under test; the rest is the inert filler a `Server` demands.
-}
serverPublishing : List OSTypes.MetadataItem -> Server
serverPublishing metadata =
    { osProps =
        { name = "cloudshield"
        , uuid = "self"
        , details =
            { openstackStatus = OSTypes.ServerActive
            , created = Time.millisToPosix 0
            , powerState = OSTypes.PowerRunning
            , imageUuid = "image-uuid"
            , flavorId = "flavor-id"
            , keypairName = Nothing
            , metadata = metadata
            , userUuid = "user-uuid"
            , volumesAttached = []
            , tags = []
            , lockStatus = OSTypes.ServerUnlocked
            , fault = Nothing
            }
        , consoleUrl = RDPP.empty
        }
    , exoProps =
        { floatingIpCreationOption = HelperTypes.DoNotUseFloatingIp
        , deletionAttempted = False
        , serverOrigin = ServerNotFromExo
        , receivedTime = Nothing
        , loadingSeparately = False
        }
    , interaction = Interactivity.NoInteraction
    }


{-| The project as the guard reads it: the viewed instance is present and its metadata carries the
given §7.1 status slot, so `exoextScanRequestPending` resolves against real wire state.
-}
projectPublishing : List OSTypes.MetadataItem -> Project
projectPublishing metadata =
    { project
        | servers =
            RDPP.RemoteDataPlusPlus
                (RDPP.DoHave [ serverPublishing metadata ] receivedAt)
                (RDPP.NotLoading Nothing)
    }


{-| The §7.1 status slot as the VM would publish it for run `seq`.
-}
runSlot : Int -> String -> List { key : String, value : String }
runSlot seq state =
    [ { key = "exoext.v1.run.seq", value = String.fromInt seq }
    , { key = "exoext.v1.run.state", value = state }
    ]


{-| The model exactly as the `CloudShieldMsg`/`ScanRequested` branch leaves it for a confirmed
3-target scan: all three rows optimistically `queued`, the card tracking the first target, and the
undrained tail parked in `exoextBatch` (no `batchId` yet — it is minted on the first write).
-}
modelAfterStartScan : ServerDetail.Model
modelAfterStartScan =
    let
        base =
            ServerDetail.init "self"

        card =
            base.exoextCard
    in
    { base
        | exoextCard =
            { card
                | scanState = Dict.fromList [ ( "i-1", "queued" ), ( "i-2", "queued" ), ( "i-3", "queued" ) ]
                , seq = 1
                , pending = Just { seq = 1, requestId = "", kind = "scan", subject = "i-1", since = Time.millisToPosix 0 }
            }
        , exoextBatch = Just { batchId = Nothing, remaining = [ "i-2", "i-3" ], awaitingWrite = True }
    }


writeRequestAt : Int -> { subject : String, batchId : Maybe String } -> ServerDetail.Model -> ServerDetail.Model
writeRequestAt millis req model =
    let
        ( updated, _, _ ) =
            ServerDetail.update (ServerDetail.ExoextWriteRequest req (Time.millisToPosix millis)) project model
    in
    updated


advance : Int -> String -> ServerDetail.Model -> ServerDetail.Model
advance seq state model =
    ServerDetail.advanceExoextBatch (runSlot seq state) model |> Tuple.first


{-| The tracked subject/seq and the durable per-row badges — what a reviewer would read off the
card while a batch drains.
-}
rowStates : ServerDetail.Model -> ( Maybe ( String, Int ), List (Maybe String) )
rowStates model =
    ( model.exoextCard.pending |> Maybe.map (\p -> ( p.subject, p.seq ))
    , [ "i-1", "i-2", "i-3" ] |> List.map (\id -> Dict.get id model.exoextCard.scanState)
    )


cloudShieldBatchSuite : Test
cloudShieldBatchSuite =
    let
        -- Target 1's request is written at t=1000; only its subject reaches the wire (the request
        -- carries one target by construction — see `writeScanRequestCmd`).
        afterFirstWrite =
            writeRequestAt 1000 { subject = "i-1", batchId = Nothing } modelAfterStartScan

        -- Run 1000 reports `done`: row 1 settles and target 2 becomes the next subject.
        afterFirstSettle =
            advance 1000 "done" afterFirstWrite

        -- The continuation is written at t=2000, carrying the batch's minted id.
        afterSecondWrite =
            writeRequestAt 2000 { subject = "i-2", batchId = Just "exo-cs-batch-1000" } afterFirstSettle
    in
    describe "ServerDetail batch continuation (§7.1 sequential pacing)"
        [ test "the first write mints the shared batchId and keeps the undrained tail" <|
            \_ ->
                Expect.equal
                    ( Just { batchId = Just "exo-cs-batch-1000", remaining = [ "i-2", "i-3" ], awaitingWrite = False }
                    , ( Just ( "i-1", 1000 ), [ Just "queued", Just "queued", Just "queued" ] )
                    )
                    ( afterFirstWrite.exoextBatch, rowStates afterFirstWrite )
        , test "a non-terminal run does not advance the batch" <|
            \_ ->
                Expect.equal
                    ( afterFirstWrite.exoextBatch, rowStates afterFirstWrite )
                    (let
                        stillRunning =
                            advance 1000 "running" afterFirstWrite
                     in
                     ( stillRunning.exoextBatch, rowStates stillRunning )
                    )
        , test "a settled run commits its row's terminal state and pops the next subject" <|
            \_ ->
                Expect.equal
                    ( Just { batchId = Just "exo-cs-batch-1000", remaining = [ "i-3" ], awaitingWrite = True }
                    , ( Just ( "i-1", 1000 ), [ Just "done", Just "queued", Just "queued" ] )
                    )
                    ( afterFirstSettle.exoextBatch, rowStates afterFirstSettle )
        , test "the continuation retargets the tracker, carries the SAME batchId, and row 1 holds done" <|
            \_ ->
                Expect.equal
                    ( Just { batchId = Just "exo-cs-batch-1000", remaining = [ "i-3" ], awaitingWrite = False }
                    , ( Just ( "i-2", 2000 ), [ Just "done", Just "queued", Just "queued" ] )
                    )
                    ( afterSecondWrite.exoextBatch, rowStates afterSecondWrite )
        , test "a second poll before the continuation's write lands pops nothing further" <|
            \_ ->
                -- The pre-write guard. Both polls read the identical terminal metadata and no write
                -- was issued in between, so the second must be inert: without it i-2 is popped, then
                -- i-3, racing two requests into the one §7.1 slot and skipping a target.
                let
                    twice =
                        afterFirstWrite |> advance 1000 "done" |> advance 1000 "done"
                in
                Expect.equal
                    ( afterFirstSettle.exoextBatch, rowStates afterFirstSettle )
                    ( twice.exoextBatch, rowStates twice )
        , test "a run slot still echoing the previous seq does not fire a second continuation" <|
            \_ ->
                Expect.equal
                    ( afterSecondWrite.exoextBatch, rowStates afterSecondWrite )
                    (let
                        stale =
                            advance 1000 "done" afterSecondWrite
                     in
                     ( stale.exoextBatch, rowStates stale )
                    )
        , test "the last target drains the batch, keeps every terminal badge, and leaves the tracker set" <|
            \_ ->
                let
                    drained =
                        afterSecondWrite
                            |> advance 2000 "done"
                            |> writeRequestAt 3000 { subject = "i-3", batchId = Just "exo-cs-batch-1000" }
                            |> advance 3000 "error"
                in
                Expect.equal
                    ( Nothing, ( Just ( "i-3", 3000 ), [ Just "done", Just "done", Just "error" ] ) )
                    ( drained.exoextBatch, rowStates drained )
        , test "a single-target scan leaves batchId null on the wire" <|
            \_ ->
                let
                    lone =
                        { modelAfterStartScan | exoextBatch = Just { batchId = Nothing, remaining = [], awaitingWrite = True } }
                            |> writeRequestAt 1000 { subject = "i-1", batchId = Nothing }
                in
                Expect.equal (Just { batchId = Nothing, remaining = [], awaitingWrite = False }) lone.exoextBatch
        ]


{-| The §7.1 status slot as a WP8-or-later publisher writes it: the required seq/state plus the
§4.3 descriptors that name the run's target and request.
-}
runSlotFor : Int -> String -> String -> List { key : String, value : String }
runSlotFor seq state target =
    runSlot seq state
        ++ [ { key = "exoext.v1.run.target", value = target }
           , { key = "exoext.v1.run.requestId", value = "exo-cs-req-" ++ String.fromInt seq }
           ]


cloudShieldRecoverySuite : Test
cloudShieldRecoverySuite =
    let
        -- A page just reloaded: nothing tracked, no batch, no durable row states.
        afterReload =
            ServerDetail.init "self"

        -- What the reader ends up drawing: the tracked subject/seq and the row the live run
        -- projects onto.
        projected metadata model =
            ( model.exoextCard.pending |> Maybe.map (\p -> ( p.subject, p.seq ))
            , ServerDetail.exoextStatusOverride metadata model
            )
    in
    describe "ServerDetail run recovery (reload mid-run)"
        [ test "a live run on the wire is adopted and projects onto ITS row, not the tracked-nothing idle state" <|
            \_ ->
                let
                    metadata =
                        runSlotFor 1700 "running" "i-9"
                in
                Expect.equal
                    ( Just ( "i-9", 1700 ), Just { targetId = "i-9", state = "running" } )
                    (projected metadata (ServerDetail.recoverExoextRun metadata afterReload))
        , test "without recovery the same wire state projects nothing (the bug being fixed)" <|
            \_ ->
                Expect.equal ( Nothing, Nothing )
                    (projected (runSlotFor 1700 "running" "i-9") afterReload)
        , test "a live session-local tracker survives recovery untouched, and keeps its own row" <|
            \_ ->
                -- The precedence guard at the host level: this session wrote i-1 at seq 5000 and the
                -- run slot still echoes the PREVIOUS run on another row. Recovery must not rewind
                -- the tracker to i-9, or the live scan's correlation is lost.
                let
                    metadata =
                        runSlotFor 1700 "running" "i-9"

                    live =
                        writeRequestAt 5000 { subject = "i-1", batchId = Nothing } modelAfterStartScan
                in
                Expect.equal
                    ( Just ( "i-1", 5000 ), Nothing )
                    (projected metadata (ServerDetail.recoverExoextRun metadata live))
        , test "a finished run restores the row's badge without inventing a tracker" <|
            \_ ->
                -- No tracker means no completion timer restart and no re-opened results pane; the
                -- durable per-row state is all a finished run leaves behind.
                let
                    recovered =
                        ServerDetail.recoverExoextRun (runSlotFor 1700 "done" "i-9") afterReload
                in
                Expect.equal ( Nothing, Just "done" )
                    ( recovered.exoextCard.pending, Dict.get "i-9" recovered.exoextCard.scanState )
        , test "a publisher that names no target changes nothing at all" <|
            \_ ->
                let
                    recovered =
                        ServerDetail.recoverExoextRun (runSlot 1700 "running") afterReload
                in
                Expect.equal ( Nothing, Dict.empty )
                    ( recovered.exoextCard.pending, recovered.exoextCard.scanState )
        , test "recovery is idempotent across polls" <|
            \_ ->
                let
                    metadata =
                        runSlotFor 1700 "running" "i-9"

                    once =
                        ServerDetail.recoverExoextRun metadata afterReload

                    twice =
                        ServerDetail.recoverExoextRun metadata once
                in
                Expect.equal (projected metadata once) (projected metadata twice)
        , test "a recovered run keeps the fast poll alive until the wire says it is over" <|
            \_ ->
                let
                    recovered state =
                        ServerDetail.recoverExoextRun (runSlotFor 1700 state "i-9") afterReload
                in
                Expect.equal ( True, False )
                    ( ServerDetail.exoextScanRequestPending (projectPublishing (runSlotFor 1700 "running" "i-9")) (recovered "running")
                    , ServerDetail.exoextScanRequestPending (projectPublishing (runSlotFor 1700 "done" "i-9")) (recovered "done")
                    )
        ]


cloudShieldScanBlockedSuite : Test
cloudShieldScanBlockedSuite =
    let
        afterFirstWrite =
            writeRequestAt 1000 { subject = "i-1", batchId = Nothing } modelAfterStartScan

        -- The same batch on its last leg: nothing left to write, target 3's run finished.
        drained =
            afterFirstWrite
                |> advance 1000 "done"
                |> writeRequestAt 2000 { subject = "i-2", batchId = Just "exo-cs-batch-1000" }
                |> advance 2000 "done"
                |> writeRequestAt 3000 { subject = "i-3", batchId = Just "exo-cs-batch-1000" }
                |> advance 3000 "done"
    in
    describe "ServerDetail exoextScanBlocked (§7.1 guard on a user-initiated scan)"
        [ test "blocked while the tracked run is still going" <|
            \_ ->
                Expect.equal True
                    (ServerDetail.exoextScanBlocked (projectPublishing (runSlot 1000 "running")) afterFirstWrite)
        , test "blocked between two siblings, when the tracked run already reads terminal" <|
            \_ ->
                -- The clause the run check alone would miss: target 1 is done, but targets 2 and 3
                -- are still parked, so a new scan here would replace the batch and strand them.
                Expect.equal True
                    (ServerDetail.exoextScanBlocked (projectPublishing (runSlot 1000 "done")) afterFirstWrite)
        , test "blocked while a decided continuation's write is still on its way" <|
            \_ ->
                -- Last subject popped (`remaining` empty) but not yet written.
                Expect.equal True
                    (ServerDetail.exoextScanBlocked (projectPublishing (runSlot 2000 "done"))
                        { afterFirstWrite | exoextBatch = Just { batchId = Just "exo-cs-batch-1000", remaining = [], awaitingWrite = True } }
                    )
        , test "not blocked once the batch has drained — a completed batch must allow a new scan" <|
            \_ ->
                Expect.equal False
                    (ServerDetail.exoextScanBlocked (projectPublishing (runSlot 3000 "done")) drained)
        ]


cloudShieldPendingEmbedSuite : Test
cloudShieldPendingEmbedSuite =
    describe "ServerDetail clearResolvedPendingEmbed"
        [ test "a matching-requestId result clears the pending marker" <|
            \_ ->
                Expect.equal Nothing
                    (ServerDetail.clearResolvedPendingEmbed
                        (embedResultMetadata (okEmbedBody expiresAtIso))
                        (Just { seq = 0, requestId = "exo-cs-req-100", kind = "getEmbed", subject = "b1", since = Time.millisToPosix 0 })
                    )
        , test "a result for a different requestId leaves the pending marker in place" <|
            \_ ->
                let
                    pending =
                        Just { seq = 0, requestId = "exo-cs-req-999", kind = "getEmbed", subject = "b1", since = Time.millisToPosix 0 }
                in
                Expect.equal pending
                    (ServerDetail.clearResolvedPendingEmbed
                        (embedResultMetadata (okEmbedBody expiresAtIso))
                        pending
                    )
        ]
