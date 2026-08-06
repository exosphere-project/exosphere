module Tests.CloudShield.ServerDetail exposing (cloudShieldBatchPersistenceSuite, cloudShieldBatchSuite, cloudShieldBusyProjectionSuite, cloudShieldCancelSuite, cloudShieldDismissSuite, cloudShieldEmbedProjectionSuite, cloudShieldPendingEmbedSuite, cloudShieldPreEchoStopSuite, cloudShieldReadDecisionSuite, cloudShieldRecoverySuite, cloudShieldScanBlockedSuite, cloudShieldScanTimerSuite, cloudShieldStaleRunSuite, cloudShieldStoppingSuite, cloudShieldTailStopSuite)

import CloudShield.Card as Card
import Dict
import Exoext.Lifecycle as Lifecycle
import Expect
import Helpers.RemoteDataPlusPlus as RDPP
import ISO8601
import Json.Decode as Decode
import Json.Encode as Encode
import OpenStack.Types as OSTypes
import Page.ServerDetail as ServerDetail
import Test exposing (Test, describe, test)
import Tests.CloudShield.Fixtures exposing (cardViewConfig)
import Time
import Types.ExtensionBatch exposing (ExtensionBatch)
import Types.HelperTypes as HelperTypes
import Types.Interactivity as Interactivity
import Types.Project exposing (Project, ProjectSecret(..))
import Types.Server exposing (Server, ServerOrigin(..))
import Types.SharedMsg as SharedMsg


receivedAt : Time.Posix
receivedAt =
    Time.millisToPosix 0


{-| The client clock a poll reads. Every fixture run `seq` in this module is a small number below
it, so no fixture run is anywhere near `Exoext.Lifecycle.staleRunAfterMillis` old — the safety valve
is exercised deliberately in `cloudShieldStaleRunSuite` and never fires by accident elsewhere.
-}
pollTime : Time.Posix
pollTime =
    Time.millisToPosix 10000


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
                            { cardViewConfig
                                | history =
                                    { rows = [ sibling "exo-cs-req-6", sibling "exo-cs-req-7" ]
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
                    (projected metadata (ServerDetail.recoverExoextRun pollTime metadata afterReload))
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
                    (projected metadata (ServerDetail.recoverExoextRun pollTime metadata live))
        , test "a finished run leaves the row alone: no tracker, and no durable badge either" <|
            \_ ->
                -- The stale-`done` fix. The run slot is never cleared, so a finished run sits there
                -- indefinitely; committing it would re-mark the row `done` on every page load from
                -- now to forever. The finished scan shows in the history panel, dated.
                let
                    recovered =
                        ServerDetail.recoverExoextRun pollTime (runSlotFor 1700 "done" "i-9") afterReload
                in
                Expect.equal ( Nothing, Nothing )
                    ( recovered.exoextCard.pending, Dict.get "i-9" recovered.exoextCard.scanState )
        , test "reloading again does not resurrect it either — a finished run is never adopted" <|
            \_ ->
                -- Each reload is a fresh `init`, so the only thing that could bring `done` back is
                -- recovery itself; running it over every terminal state proves none of them do.
                Expect.equal []
                    ([ "done", "error", "cancelled", "expired" ]
                        |> List.filterMap
                            (\state ->
                                ServerDetail.recoverExoextRun pollTime (runSlotFor 1700 state "i-9") afterReload
                                    |> .exoextCard
                                    |> .scanState
                                    |> Dict.get "i-9"
                            )
                    )
        , test "a publisher that names no target changes nothing at all" <|
            \_ ->
                let
                    recovered =
                        ServerDetail.recoverExoextRun pollTime (runSlot 1700 "running") afterReload
                in
                Expect.equal ( Nothing, Dict.empty )
                    ( recovered.exoextCard.pending, recovered.exoextCard.scanState )
        , test "recovery is idempotent across polls" <|
            \_ ->
                let
                    metadata =
                        runSlotFor 1700 "running" "i-9"

                    once =
                        ServerDetail.recoverExoextRun pollTime metadata afterReload

                    twice =
                        ServerDetail.recoverExoextRun pollTime metadata once
                in
                Expect.equal (projected metadata once) (projected metadata twice)
        , test "a recovered run keeps the fast poll alive until the wire says it is over" <|
            \_ ->
                let
                    recovered state =
                        ServerDetail.recoverExoextRun pollTime (runSlotFor 1700 state "i-9") afterReload
                in
                Expect.equal ( True, False )
                    ( ServerDetail.exoextScanRequestPending (projectPublishing (runSlotFor 1700 "running" "i-9")) (recovered "running")
                    , ServerDetail.exoextScanRequestPending (projectPublishing (runSlotFor 1700 "done" "i-9")) (recovered "done")
                    )
        ]


{-| The whole host path a poll takes: `GotExoextSync` against a project publishing `metadata`,
carrying the client clock. This is how the safety valve actually fires in the app, so it is what the
stale-run suite drives rather than calling the pieces directly.
-}
polledAt : Time.Posix -> List OSTypes.MetadataItem -> ServerDetail.Model -> ServerDetail.Model
polledAt now metadata model =
    let
        ( updated, _, _ ) =
            ServerDetail.update (ServerDetail.GotExoextSync now) (projectPublishing metadata) model
    in
    updated


{-| The clock as it reads when a run written at wall-clock `seq` is `ageMillis` old.
-}
clockAfter : Int -> Int -> Time.Posix
clockAfter seq ageMillis =
    Time.millisToPosix (seq + ageMillis)


{-| The defense-in-depth valve for a publisher that dies mid-run and never settles §7.1's
`run.state`. The host then reads a scan that is live forever, and — correctly — greys every Scan
affordance while a run is in flight, leaving the researcher with no way to start a new scan and no
way to clear the old one. Two correct behaviors composing into a trap.

The suite is written around the boundary rather than around a magic number: every case is stated in
terms of `Exoext.Lifecycle.staleRunAfterMillis`, so retuning the threshold cannot silently turn a
test vacuous.

-}
cloudShieldStaleRunSuite : Test
cloudShieldStaleRunSuite =
    let
        -- A publisher stuck mid-run on target 1, with targets 2 and 3 still parked in the tail.
        wedgedRun =
            runSlotFor 1700 "running" "i-1"

        wedged =
            writeRequestAt 1700 { subject = "i-1", batchId = Nothing } modelAfterStartScan

        -- A reader that came to the page fresh, with nothing of its own to go on.
        afterReload =
            ServerDetail.init "self"

        fresh =
            clockAfter 1700 (90 * 60 * 1000)

        stale =
            clockAfter 1700 (Lifecycle.staleRunAfterMillis + 1)

        -- What the researcher can actually do and see: may they start a scan, is a run still
        -- tracked, is a batch still draining, and what badge does target 1 carry.
        outcome metadata model =
            { blocked = ServerDetail.exoextScanBlocked (projectPublishing metadata) model
            , tracked = model.exoextCard.pending |> Maybe.map (\p -> ( p.subject, p.seq ))
            , batch = model.exoextBatch |> Maybe.map .remaining
            , rowState = Dict.get "i-1" model.exoextCard.scanState
            , liveRow = ServerDetail.exoextStatusOverride metadata model
            }

        scanBusy metadata model =
            Card.projection Time.utc
                (ServerDetail.exoextViewConfig True (projectPublishing metadata) model (Time.millisToPosix 0) (serverPublishing metadata))
                []
                model.exoextCard
                |> Decode.decodeValue (Decode.field "scanBusy" Decode.bool)
                |> Result.mapError Decode.errorToString
    in
    describe "ServerDetail stale-run safety valve"
        [ test "REGRESSION: a run that is merely slow still blocks, still tracks, and still shows its live row" <|
            \_ ->
                -- 90 minutes in — twice the reference §4.4 deadline — and the host must still be
                -- treating this as a scan in progress. Calling it dead here would invite a
                -- replacement scan that supersedes real work.
                Expect.equal
                    { blocked = True
                    , tracked = Just ( "i-1", 1700 )
                    , batch = Just [ "i-2", "i-3" ]
                    , rowState = Just "queued"
                    , liveRow = Just { targetId = "i-1", state = "running" }
                    }
                    (outcome wedgedRun (polledAt fresh wedgedRun wedged))
        , test "REGRESSION: a fresh run is still adopted after a reload, and blocks from there" <|
            \_ ->
                let
                    recovered =
                        polledAt fresh wedgedRun afterReload
                in
                Expect.equal ( Just ( "i-1", 1700 ), True )
                    ( recovered.exoextCard.pending |> Maybe.map (\p -> ( p.subject, p.seq ))
                    , ServerDetail.exoextScanBlocked (projectPublishing wedgedRun) recovered
                    )
        , test "a stale run releases the tracker, the tail and the row badges, and unblocks a new scan" <|
            \_ ->
                -- All of it at once, because releasing any subset leaves the trap half-shut: the
                -- tracker alone still blocks through the batch clause, the batch alone still shows
                -- a scanning row.
                Expect.equal
                    { blocked = False
                    , tracked = Nothing
                    , batch = Nothing
                    , rowState = Nothing
                    , liveRow = Nothing
                    }
                    (outcome wedgedRun (polledAt stale wedgedRun wedged))
        , test "a stale run is NOT adopted by recovery — reloading the page must not re-arm the block" <|
            \_ ->
                Expect.equal
                    { blocked = False
                    , tracked = Nothing
                    , batch = Nothing
                    , rowState = Nothing
                    , liveRow = Nothing
                    }
                    (outcome wedgedRun (polledAt stale wedgedRun afterReload))
        , test "the boundary in both directions: at the threshold the run still blocks, one ms past it does not" <|
            \_ ->
                Expect.equal ( True, False )
                    ( ServerDetail.exoextScanBlocked (projectPublishing wedgedRun)
                        (polledAt (clockAfter 1700 Lifecycle.staleRunAfterMillis) wedgedRun wedged)
                    , ServerDetail.exoextScanBlocked (projectPublishing wedgedRun)
                        (polledAt (clockAfter 1700 (Lifecycle.staleRunAfterMillis + 1)) wedgedRun wedged)
                    )
        , test "the manifest sees it: /scanBusy lifts once the run goes stale" <|
            \_ ->
                Expect.equal ( Ok True, Ok False )
                    ( scanBusy wedgedRun (polledAt fresh wedgedRun wedged)
                    , scanBusy wedgedRun (polledAt stale wedgedRun wedged)
                    )
        , test "a stale run offers no Stop control — a control nobody is listening to is not an escape hatch" <|
            \_ ->
                -- The §4.3 descriptors resolve a target and a request id with no tracker at all, so
                -- withdrawing the tracker is not by itself enough to withdraw the control. Pressing
                -- it would arm `stopping` against a publisher that answers nothing.
                Expect.equal ( Just { targetId = "i-1", requestId = "exo-cs-req-1700" }, Nothing )
                    ( ServerDetail.exoextCancellableRun fresh wedgedRun Nothing afterReload
                    , ServerDetail.exoextCancellableRun stale wedgedRun Nothing afterReload
                    )
        , test "an ancient TERMINAL run is untouched: its batch settles and continues as always" <|
            \_ ->
                -- Staleness applies only to a run claiming to be live. A terminal run is a settled
                -- record, and an undrained tail still adopts it to take its next step.
                let
                    continued =
                        polledAt (clockAfter 1700 (100 * Lifecycle.staleRunAfterMillis))
                            (runSlotFor 1700 "done" "i-1")
                            wedged
                in
                Expect.equal ( Just "done", Just [ "i-3" ], True )
                    ( Dict.get "i-1" continued.exoextCard.scanState
                    , continued.exoextBatch |> Maybe.map .remaining
                    , continued.exoextBatch |> Maybe.map .awaitingWrite |> Maybe.withDefault False
                    )
        , test "a NEWER tracked run is never released by an older stale slot" <|
            \_ ->
                -- The wire lags the tracker by a poll routinely. A request written after the stale
                -- one owns the batch, and the guard is the seq: only the run the slot names is let
                -- go of.
                let
                    polled =
                        writeRequestAt 9000 { subject = "i-1", batchId = Nothing } wedged
                            |> polledAt (clockAfter 1700 (Lifecycle.staleRunAfterMillis + 1)) wedgedRun
                in
                Expect.equal ( Just ( "i-1", 9000 ), True )
                    ( polled.exoextCard.pending |> Maybe.map (\p -> ( p.subject, p.seq ))
                    , ServerDetail.exoextScanBlocked (projectPublishing wedgedRun) polled
                    )
        , test "releasing a stale run also forgets the stored batch record, so a reload does not restore it" <|
            \_ ->
                let
                    ( _, _, sharedMsg ) =
                        ServerDetail.update (ServerDetail.GotExoextSync stale) (projectPublishing wedgedRun) wedged
                in
                Expect.equal (SharedMsg.ForgetExtensionBatch "self") sharedMsg
        ]


{-| A project publishing `metadata` on the viewed VM, plus ACTIVE scan targets with the given ids —
what `exoextInstances` sees, and therefore what the eligibility half of the batch stale check reads.
-}
projectWithTargets : List String -> List OSTypes.MetadataItem -> Project
projectWithTargets targetIds metadata =
    let
        target id =
            let
                base =
                    serverPublishing []

                osProps =
                    base.osProps
            in
            { base | osProps = { osProps | uuid = id, name = id } }
    in
    { project
        | servers =
            RDPP.RemoteDataPlusPlus
                (RDPP.DoHave (serverPublishing metadata :: List.map target targetIds) receivedAt)
                (RDPP.NotLoading Nothing)
    }


{-| A stored batch record as an earlier session left it, keyed to the fixture project + instance.
-}
storedBatch : List String -> ExtensionBatch
storedBatch remaining =
    { cloudUrl = "https://openstack.example/keystone/v3"
    , projectUuid = "project-uuid"
    , instanceUuid = "self"
    , batchId = "exo-cs-batch-1000"
    , remaining = remaining
    }


cloudShieldBatchPersistenceSuite : Test
cloudShieldBatchPersistenceSuite =
    let
        -- A three-target batch mid-drain: target 1's request is on the wire, 2 and 3 are parked.
        draining =
            writeRequestAt 1000 { subject = "i-1", batchId = Nothing } modelAfterStartScan

        -- A fresh page for the same instance, as a reload leaves it.
        afterReload =
            ServerDetail.init "self"

        adopt records targets =
            afterReload
                |> ServerDetail.adoptStoredExoextBatch project records
                |> ServerDetail.adoptRestoredExoextBatch (projectWithTargets targets [])

        oneTarget =
            storedBatch [ "i-2" ]
    in
    describe "ServerDetail batch-tail persistence (exoext.batch.v1)"
        [ test "a draining batch is stored with its shared id and undrained tail" <|
            \_ ->
                Expect.equal
                    (SharedMsg.RecordExtensionBatch (storedBatch [ "i-2", "i-3" ]))
                    (ServerDetail.exoextBatchSharedMsg project draining)
        , test "a drained batch drops the record, and so does having no batch at all" <|
            \_ ->
                Expect.equal
                    ( SharedMsg.ForgetExtensionBatch "self", SharedMsg.ForgetExtensionBatch "self" )
                    ( ServerDetail.exoextBatchSharedMsg project { draining | exoextBatch = Just { batchId = Just "b", remaining = [], awaitingWrite = False } }
                    , ServerDetail.exoextBatchSharedMsg project afterReload
                    )
        , test "a stopped batch drops the record too — a stop must not leave resumable work behind" <|
            \_ ->
                Expect.equal (SharedMsg.ForgetExtensionBatch "self")
                    (ServerDetail.exoextBatchSharedMsg project (ServerDetail.exoextCancelRequested "exo-cs-req-1000" draining))
        , test "a stored tail round-trips back into a live batch, ready to drain" <|
            \_ ->
                -- `awaitingWrite` comes back False by construction: it guards a window inside one
                -- session, and after a reload nothing is in flight.
                Expect.equal
                    (Just { batchId = Just "exo-cs-batch-1000", remaining = [ "i-2", "i-3" ], awaitingWrite = False })
                    (adopt [ storedBatch [ "i-2", "i-3" ] ] [ "i-2", "i-3" ]).exoextBatch
        , test "a record for another instance is ignored" <|
            \_ ->
                Expect.equal Nothing
                    (adopt [ { oneTarget | instanceUuid = "some-other-vm" } ] [ "i-2" ]).exoextBatch
        , test "a record for another project, or another cloud, is ignored" <|
            \_ ->
                Expect.equal ( Nothing, Nothing )
                    ( (adopt [ { oneTarget | projectUuid = "other-project" } ] [ "i-2" ]).exoextBatch
                    , (adopt [ { oneTarget | cloudUrl = "https://elsewhere.example/v3" } ] [ "i-2" ]).exoextBatch
                    )
        , test "targets that are no longer eligible instances are dropped from the tail" <|
            \_ ->
                Expect.equal (Just [ "i-3" ])
                    ((adopt [ storedBatch [ "i-2", "i-3" ] ] [ "i-3" ]).exoextBatch |> Maybe.map .remaining)
        , test "a record whose every target is gone is dropped whole, not adopted empty" <|
            \_ ->
                let
                    allGone =
                        adopt [ storedBatch [ "i-2", "i-3" ] ] [ "i-9" ]
                in
                Expect.equal ( Nothing, Nothing )
                    ( allGone.exoextBatch, allGone.exoextRestoredBatch )
        , test "an instance list that has not arrived yet DEFERS the decision, it does not discard" <|
            \_ ->
                -- Entering this page fetches the publishing VM first and its siblings later, so the
                -- earliest polls genuinely see a list of one. Reading that as "every stored target was
                -- deleted" would throw the tail away in exactly the reload case this record exists for.
                let
                    tooEarly =
                        adopt [ storedBatch [ "i-2", "i-3" ] ] []
                in
                Expect.equal
                    ( Nothing, Just { batchId = Just "exo-cs-batch-1000", remaining = [ "i-2", "i-3" ], awaitingWrite = False } )
                    ( tooEarly.exoextBatch, tooEarly.exoextRestoredBatch )
        , test "a deferred record is HELD in storage, not forgotten on that poll" <|
            \_ ->
                -- The other half of the same hazard: a Forget while the tail is still pending would
                -- delete it before anything could resume it.
                Expect.equal SharedMsg.NoOp
                    (ServerDetail.exoextBatchSharedMsg project (adopt [ storedBatch [ "i-2", "i-3" ] ] []))
        , test "the deferred decision resolves once the sibling instances arrive" <|
            \_ ->
                Expect.equal
                    (Just { batchId = Just "exo-cs-batch-1000", remaining = [ "i-2", "i-3" ], awaitingWrite = False })
                    (adopt [ storedBatch [ "i-2", "i-3" ] ] []
                        |> ServerDetail.adoptRestoredExoextBatch (projectWithTargets [ "i-2", "i-3" ] [])
                        |> .exoextBatch
                    )
        , test "adoption never displaces a batch this session is already draining" <|
            \_ ->
                -- Same precedence rule as run recovery: the live one is the newer truth.
                let
                    adopted =
                        draining
                            |> ServerDetail.adoptStoredExoextBatch project [ storedBatch [ "i-9" ] ]
                            |> ServerDetail.adoptRestoredExoextBatch (projectWithTargets [ "i-9" ] [])
                in
                Expect.equal ( draining.exoextBatch, Nothing )
                    ( adopted.exoextBatch, adopted.exoextRestoredBatch )
        , test "a restored tail resumes through the existing drain path, keeping the shared id" <|
            \_ ->
                -- The whole point of persisting it. The interrupted run had already finished by the
                -- time the page came back, so recovery has to hand the drain a tracker to settle.
                let
                    metadata =
                        runSlotFor 1000 "done" "i-1"

                    resumed =
                        adopt [ storedBatch [ "i-2", "i-3" ] ] [ "i-2", "i-3" ]
                            |> ServerDetail.recoverExoextRun pollTime metadata
                            |> ServerDetail.advanceExoextBatch metadata
                            |> Tuple.first
                in
                Expect.equal
                    ( Just { batchId = Just "exo-cs-batch-1000", remaining = [ "i-3" ], awaitingWrite = True }
                    , Just "done"
                    )
                    ( resumed.exoextBatch, Dict.get "i-1" resumed.exoextCard.scanState )
        , test "a restored tail shows its members as queued again, with the live row still live" <|
            \_ ->
                -- What the researcher sees one poll after reloading mid-batch: i-1 is on the wire and
                -- counting, i-2 and i-3 have not been written yet and are still coming. Their
                -- optimistic badges died with the session; the tail is what puts them back.
                let
                    metadata =
                        runSlotFor 1700 "running" "i-1"

                    projectHere =
                        projectWithTargets [ "i-1", "i-2", "i-3" ] metadata

                    restored =
                        afterReload
                            |> ServerDetail.adoptStoredExoextBatch project [ storedBatch [ "i-2", "i-3" ] ]
                            |> ServerDetail.adoptRestoredExoextBatch projectHere
                            |> ServerDetail.recoverExoextRun pollTime metadata

                    rendered =
                        Card.projection Time.utc
                            (ServerDetail.exoextViewConfig True projectHere restored (Time.millisToPosix 1700) (serverPublishing metadata))
                            ([ ( "i-1", "alpha" ), ( "i-2", "beta" ), ( "i-3", "gamma" ) ]
                                |> List.map (\( id, name ) -> { id = id, name = name, status = "ACTIVE" })
                            )
                            restored.exoextCard
                in
                Expect.equal (Ok [ "scanning · 0:00", "queued", "queued" ])
                    (Decode.decodeValue
                        (Decode.field "instances" (Decode.list (Decode.field "scanState" Decode.string)))
                        rendered
                        |> Result.mapError Decode.errorToString
                    )
        , test "with no stored record for this instance nothing is adopted and nothing changes" <|
            \_ ->
                Expect.equal ( Nothing, Nothing )
                    ( (adopt [] [ "i-2" ]).exoextBatch, (adopt [] [ "i-2" ]).exoextRestoredBatch )
        ]


cloudShieldCancelSuite : Test
cloudShieldCancelSuite =
    let
        -- A three-target batch mid-drain: target 1's request is on the wire, targets 2 and 3 are
        -- parked at their optimistic `queued` badges.
        draining =
            writeRequestAt 1700 { subject = "i-1", batchId = Nothing } modelAfterStartScan

        runningRow =
            Just { targetId = "i-1", state = "running" }
    in
    describe "ServerDetail cancel (the §I cancel channel)"
        [ test "an active run offers its row a stop, naming the request id" <|
            \_ ->
                Expect.equal (Just { targetId = "i-1", requestId = "exo-cs-req-1700" })
                    (ServerDetail.exoextCancellableRun pollTime (runSlotFor 1700 "running" "i-1") runningRow draining)
        , test "every non-terminal state is stoppable and every terminal one is not" <|
            \_ ->
                Expect.equal [ True, True, True, False, False, False, False ]
                    ([ "queued", "running", "scanning", "done", "error", "cancelled", "expired" ]
                        |> List.map
                            (\state ->
                                ServerDetail.exoextCancellableRun pollTime
                                    (runSlotFor 1700 state "i-1")
                                    (Just { targetId = "i-1", state = state })
                                    draining
                                    /= Nothing
                            )
                    )
        , test "a publisher reporting no requestId falls back to the id this host minted for the seq" <|
            \_ ->
                Expect.equal (Just { targetId = "i-1", requestId = "exo-cs-req-1700" })
                    (ServerDetail.exoextCancellableRun pollTime (runSlot 1700 "running") runningRow draining)
        , test "a run this host cannot name at all is not stoppable" <|
            \_ ->
                -- No wire requestId, no wire target and no tracker: a stop would name nothing. The
                -- tracker has to be absent for this to be the question — a host that IS tracking a
                -- request the wire never echoed names ITS OWN request instead, which is a different
                -- run and a different question (`cloudShieldPreEchoStopSuite`).
                Expect.equal Nothing
                    (ServerDetail.exoextCancellableRun pollTime (runSlot 9999 "running") Nothing (ServerDetail.init "self"))
        , test "a run whose target cannot be attributed to a row is not stoppable either" <|
            \_ ->
                Expect.equal Nothing
                    (ServerDetail.exoextCancellableRun pollTime (runSlot 1700 "running") Nothing draining)
        , test "the stop ends the batch and clears the badges of targets that will never run" <|
            \_ ->
                let
                    cancelled =
                        ServerDetail.exoextCancelRequested "exo-cs-req-1700" draining
                in
                Expect.equal
                    ( Just "exo-cs-req-1700", Nothing, [ Just "queued", Nothing, Nothing ] )
                    ( cancelled.exoextCancelRequestId
                    , cancelled.exoextBatch
                    , [ "i-1", "i-2", "i-3" ] |> List.map (\id -> Dict.get id cancelled.exoextCard.scanState)
                    )
        , test "a stopped run withdraws its own Cancel control, with no host-owned label involved" <|
            \_ ->
                Expect.equal Nothing
                    (ServerDetail.exoextCancellableRun pollTime
                        (runSlotFor 1700 "running" "i-1")
                        runningRow
                        (ServerDetail.exoextCancelRequested "exo-cs-req-1700" draining)
                    )
        , test "the NEXT request restores the control, because that write clears the channel" <|
            \_ ->
                let
                    restarted =
                        ServerDetail.exoextCancelRequested "exo-cs-req-1700" draining
                            |> writeRequestAt 2500 { subject = "i-1", batchId = Nothing }
                in
                Expect.equal ( Nothing, Just { targetId = "i-1", requestId = "exo-cs-req-2500" } )
                    ( restarted.exoextCancelRequestId
                    , ServerDetail.exoextCancellableRun pollTime (runSlotFor 2500 "running" "i-1") runningRow restarted
                    )
        , test "a stop for one request does not withdraw a DIFFERENT run's control" <|
            \_ ->
                Expect.equal (Just { targetId = "i-1", requestId = "exo-cs-req-1700" })
                    (ServerDetail.exoextCancellableRun pollTime
                        (runSlotFor 1700 "running" "i-1")
                        runningRow
                        (ServerDetail.exoextCancelRequested "exo-cs-req-9999" draining)
                    )
        ]


cloudShieldDismissSuite : Test
cloudShieldDismissSuite =
    let
        openSession model =
            ServerDetail.exoextEmbedProjection
                (embedResultMetadata (okEmbedBody expiresAtIso))
                (objectFor "b1")
                beforeExpiry
                Nothing
                Nothing
                model

        dismissed model =
            { model | exoextSessionDismissed = True }
    in
    describe "ServerDetail dismiss (closing an open result session)"
        [ test "an open session reports sessionOpen, so the manifest can offer a close" <|
            \_ ->
                Expect.equal True (openSession modelWithArchivedFindings).sessionOpen
        , test "dismissing unmounts the pane: no findings, no iframe url, sessionOpen False" <|
            \_ ->
                -- Unmounted rather than hidden: an embed left mounted keeps retrying auth.
                let
                    projection =
                        openSession (dismissed modelWithArchivedFindings)
                in
                Expect.equal ( Nothing, "", False )
                    ( projection.results, projection.embedUrl, projection.sessionOpen )
        , test "dismissing drops the now-viewing flag and nothing else about the row" <|
            \_ ->
                let
                    projection =
                        openSession (dismissed modelWithArchivedFindings)
                in
                Expect.equal ( Nothing, Card.EmbedIdle )
                    ( projection.activeResultId, projection.embedState )
        , test "asking for a session again clears the dismissal, so the same scan reopens" <|
            \_ ->
                let
                    ( reopened, _, _ ) =
                        ServerDetail.update
                            (ServerDetail.ExoextWriteEmbedRequest { resultId = "b1", batchId = "b1" } (Time.millisToPosix 3000))
                            project
                            (dismissed modelWithArchivedFindings)
                in
                Expect.equal False reopened.exoextSessionDismissed
        , test "starting a new scan clears it too — the new results supersede what was closed" <|
            \_ ->
                Expect.equal False
                    (writeRequestAt 3000 { subject = "i-1", batchId = Nothing } (dismissed modelAfterStartScan)).exoextSessionDismissed
        , test "a dismissal with nothing on screen leaves the empty state exactly as it was" <|
            \_ ->
                let
                    idle model =
                        ServerDetail.exoextEmbedProjection [] Nothing beforeExpiry Nothing Nothing model

                    base =
                        ServerDetail.init "self"
                in
                Expect.equal
                    ( (idle base).sessionOpen, (idle base).embedState )
                    ( (idle (dismissed base)).sessionOpen, (idle (dismissed base)).embedState )
        , test "dismissing retires the in-flight request, so no row is left reading Opening" <|
            \_ ->
                -- Bug 5. The pending marker is cleared only by a matching wire result, so closing the
                -- pane used to leave the row advertising an arrival nothing would then display.
                let
                    pending =
                        modelPendingEmbed (Time.millisToPosix 0)

                    rowState model =
                        (ServerDetail.exoextEmbedProjection [] Nothing (Time.millisToPosix 1000) Nothing Nothing model).pendingResultId
                in
                Expect.equal ( Just "b1", Nothing, Nothing )
                    ( rowState pending
                    , rowState (ServerDetail.exoextDismissSession pending)
                    , (ServerDetail.exoextDismissSession pending).exoextPendingEmbed
                    )
        , test "dismissing keeps the record of WHICH result the res slot holds" <|
            \_ ->
                -- `exoextEmbedResultId` outlives the request by design: it is what tells two §2.2
                -- siblings apart. Clearing it would degrade the per-row states to the shared batchId.
                let
                    recording =
                        modelRecording "exo-cs-req-100" "b1-run-2"
                in
                Expect.equal (Just { requestId = "exo-cs-req-100", resultId = "b1-run-2" })
                    (ServerDetail.exoextDismissSession recording).exoextEmbedResultId
        , test "dismissing with no request in flight only records the dismissal" <|
            \_ ->
                let
                    dismissedBase =
                        ServerDetail.exoextDismissSession (ServerDetail.init "self")
                in
                Expect.equal ( True, Nothing )
                    ( dismissedBase.exoextSessionDismissed, dismissedBase.exoextPendingEmbed )
        , test "a View after a dismissal still opens: the request re-arms both fields" <|
            \_ ->
                let
                    ( reopened, _, _ ) =
                        ServerDetail.update
                            (ServerDetail.ExoextWriteEmbedRequest { resultId = "b1", batchId = "b1" } (Time.millisToPosix 3000))
                            project
                            (ServerDetail.exoextDismissSession (modelPendingEmbed (Time.millisToPosix 0)))
                in
                Expect.equal
                    ( False, Just "b1", Just "exo-cs-req-3000" )
                    ( reopened.exoextSessionDismissed
                    , reopened.exoextPendingEmbed |> Maybe.map .subject
                    , reopened.exoextPendingEmbed |> Maybe.map .requestId
                    )
        ]


{-| The two §7.1 guards as the manifest sees them, through the whole host path: `exoextViewConfig`
projected by `CloudShield.Card.projection`. This is the wiring test — that `/scanBusy` is genuinely
`exoextScanBlocked` (the predicate that gates a SCAN press) and not the View guard beside it.
-}
cloudShieldBusyProjectionSuite : Test
cloudShieldBusyProjectionSuite =
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
    describe "ServerDetail /scanBusy (WP10 bug 4)"
        [ test "a live run makes both guards busy" <|
            \_ ->
                Expect.equal (Ok ( True, True )) (busyFlags (runSlot 1700 "running") draining)
        , test "between two siblings only the SCAN guard is busy, which is why it is its own flag" <|
            \_ ->
                -- The case that would be wrong if the manifest bound its Scan controls to
                -- `requestBusy`: the tracked run reads terminal, so the View guard has lifted, but
                -- the batch still has targets parked and a Scan press would strand them.
                Expect.equal (Ok ( True, False )) (busyFlags (runSlot 1700 "done") draining)
        , test "an idle card is busy on neither" <|
            \_ ->
                Expect.equal (Ok ( False, False ))
                    (busyFlags (runSlot 1700 "done") (ServerDetail.init "self"))
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


{-| The §7.1 cancel channel as the VM would see it after this host wrote a stop for `requestId`.
-}
cancelSlot : String -> List OSTypes.MetadataItem
cancelSlot requestId =
    [ { key = "exoext.v1.req.cancel", value = requestId } ]


cloudShieldStoppingSuite : Test
cloudShieldStoppingSuite =
    let
        -- The three-target batch of `cloudShieldCancelSuite`, mid-drain on target 1.
        draining =
            writeRequestAt 1700 { subject = "i-1", batchId = Nothing } modelAfterStartScan

        runningRow =
            Just { targetId = "i-1", state = "running" }

        -- A page that has just reloaded: no tracker, no batch, no durable row state. Everything the
        -- reader knows about the run has to come off the wire.
        afterReload =
            ServerDetail.init "self"

        liveRun =
            runSlotFor 1700 "running" "i-1"
    in
    describe "ServerDetail stopping (the wire-derived state, WP10 bugs 2+3)"
        [ test "a cancel on the wire makes the run it names stopping" <|
            \_ ->
                Expect.equal (Just "i-1")
                    (ServerDetail.exoextStoppingTarget pollTime (liveRun ++ cancelSlot "exo-cs-req-1700") runningRow draining)
        , test "a stopping run survives a reload: no session state, and the wire still says so" <|
            \_ ->
                -- The whole of bug 3. Every session-local record is gone, and the answer is
                -- unchanged, because the host wrote the channel it is now reading.
                Expect.equal ( Just "i-1", Nothing )
                    ( ServerDetail.exoextStoppingTarget pollTime (liveRun ++ cancelSlot "exo-cs-req-1700") Nothing afterReload
                    , ServerDetail.exoextCancellableRun pollTime (liveRun ++ cancelSlot "exo-cs-req-1700") Nothing afterReload
                    )
        , test "the same reload with no cancel on the wire reads as a plain live run" <|
            \_ ->
                Expect.equal ( Nothing, Just { targetId = "i-1", requestId = "exo-cs-req-1700" } )
                    ( ServerDetail.exoextStoppingTarget pollTime liveRun Nothing afterReload
                    , ServerDetail.exoextCancellableRun pollTime liveRun Nothing afterReload
                    )
        , test "a stopping run is not cancellable: a second press could only be a no-op" <|
            \_ ->
                Expect.equal Nothing
                    (ServerDetail.exoextCancellableRun pollTime (liveRun ++ cancelSlot "exo-cs-req-1700") runningRow draining)
        , test "the press is acknowledged before the write lands, from this session's own record" <|
            \_ ->
                -- The gap the session-local record covers: the cancel write is an HTTP round-trip and
                -- the wire says nothing until the NEXT poll. Without this the row would keep reading
                -- `scanning` for seconds after a press that plainly landed.
                Expect.equal ( Just "i-1", Nothing )
                    (( ServerDetail.exoextStoppingTarget pollTime liveRun runningRow
                     , ServerDetail.exoextCancellableRun pollTime liveRun runningRow
                     )
                        |> Tuple.mapBoth
                            (\f -> f (ServerDetail.exoextCancelRequested "exo-cs-req-1700" draining))
                            (\f -> f (ServerDetail.exoextCancelRequested "exo-cs-req-1700" draining))
                    )
        , test "a cancel naming a DIFFERENT request leaves this run alone" <|
            \_ ->
                Expect.equal ( Nothing, Just { targetId = "i-1", requestId = "exo-cs-req-1700" } )
                    ( ServerDetail.exoextStoppingTarget pollTime (liveRun ++ cancelSlot "exo-cs-req-9999") runningRow draining
                    , ServerDetail.exoextCancellableRun pollTime (liveRun ++ cancelSlot "exo-cs-req-9999") runningRow draining
                    )
        , test "the cleared channel names nothing, so a superseded stop cannot linger" <|
            \_ ->
                -- `reqSlotMetadata` clears the channel by writing `""`, which is how the NEXT
                -- request retires a stop. An empty value must never match a real request id.
                Expect.equal Nothing
                    (ServerDetail.exoextStoppingTarget pollTime (runSlotFor 1700 "running" "i-1" ++ cancelSlot "") Nothing afterReload)
        , test "a terminal run is never stopping, however the stop was recorded" <|
            \_ ->
                -- It has stopped, or it finished first. Either way its own terminal state is the
                -- truthful badge and `stopping` would be a run that is still going.
                Expect.equal []
                    ([ "done", "error", "cancelled", "expired" ]
                        |> List.filterMap
                            (\state ->
                                let
                                    metadata =
                                        runSlotFor 1700 state "i-1" ++ cancelSlot "exo-cs-req-1700"
                                in
                                ServerDetail.exoextStoppingTarget pollTime
                                    metadata
                                    (Just { targetId = "i-1", state = state })
                                    (ServerDetail.exoextCancelRequested "exo-cs-req-1700" draining)
                            )
                    )
        , test "a state this host does not recognize is still stoppable-in-progress" <|
            \_ ->
                -- `cancellableRunStates` is about what a stop can be OFFERED for and names only
                -- states this host knows; whether a stop is PENDING is a fact about the wire.
                Expect.equal ( Just "i-1", Nothing )
                    ( ServerDetail.exoextStoppingTarget pollTime (runSlotFor 1700 "snapshotting" "i-1" ++ cancelSlot "exo-cs-req-1700") Nothing afterReload
                    , ServerDetail.exoextCancellableRun pollTime (runSlotFor 1700 "snapshotting" "i-1") Nothing afterReload
                    )
        , test "the NEXT request clears the channel, so a restarted run is stoppable again" <|
            \_ ->
                let
                    restarted =
                        ServerDetail.exoextCancelRequested "exo-cs-req-1700" draining
                            |> writeRequestAt 2500 { subject = "i-1", batchId = Nothing }

                    -- The wire as `reqSlotMetadata` leaves it: the new run, and a cleared channel.
                    metadata =
                        runSlotFor 2500 "running" "i-1" ++ cancelSlot ""
                in
                Expect.equal ( Nothing, Just { targetId = "i-1", requestId = "exo-cs-req-2500" } )
                    ( ServerDetail.exoextStoppingTarget pollTime metadata (Just { targetId = "i-1", state = "running" }) restarted
                    , ServerDetail.exoextCancellableRun pollTime metadata (Just { targetId = "i-1", state = "running" }) restarted
                    )
        , test "a run the host cannot name is neither stoppable nor stopping" <|
            \_ ->
                Expect.equal ( Nothing, Nothing )
                    ( ServerDetail.exoextStoppingTarget pollTime (runSlot 1700 "running" ++ cancelSlot "exo-cs-req-1700") Nothing afterReload
                    , ServerDetail.exoextRunControl pollTime (runSlot 1700 "running") Nothing afterReload
                    )
        ]


cloudShieldTailStopSuite : Test
cloudShieldTailStopSuite =
    let
        -- Target 1's request is on the wire; targets 2 and 3 are parked at their optimistic badges.
        draining =
            writeRequestAt 1700 { subject = "i-1", batchId = Nothing } modelAfterStartScan

        stop req model =
            let
                ( updated, _, _ ) =
                    ServerDetail.exoextStopRequested project req model
            in
            updated

        -- What a reviewer would read off the batch and the rows after a stop.
        tailAndRows model =
            ( model.exoextBatch |> Maybe.map .remaining
            , [ "i-1", "i-2", "i-3" ] |> List.map (\id -> Dict.get id model.exoextCard.scanState)
            )
    in
    describe "ServerDetail stopping a QUEUED target (WP10 bug 1)"
        [ test "a queued target leaves the tail and drops its badge, and nothing else moves" <|
            \_ ->
                let
                    stopped =
                        stop { requestId = "", targetId = "i-2" } draining
                in
                Expect.equal
                    ( ( Just [ "i-3" ], [ Just "queued", Nothing, Just "queued" ] )
                    , ( Nothing, draining.exoextCard.pending )
                    )
                    ( tailAndRows stopped
                    , ( stopped.exoextCancelRequestId, stopped.exoextCard.pending )
                    )
        , test "the live run is untouched: still tracked, still stoppable, no cancel written" <|
            \_ ->
                -- The property that matters most here. The cancel channel names ONE request and the
                -- only request on the wire is the live run's, so writing one for a queued row would
                -- kill a different target's scan.
                let
                    stopped =
                        stop { requestId = "", targetId = "i-2" } draining

                    metadata =
                        runSlotFor 1700 "running" "i-1"
                in
                Expect.equal
                    ( Just { targetId = "i-1", requestId = "exo-cs-req-1700" }, Nothing )
                    ( ServerDetail.exoextCancellableRun pollTime metadata (Just { targetId = "i-1", state = "running" }) stopped
                    , ServerDetail.exoextStoppingTarget pollTime metadata (Just { targetId = "i-1", state = "running" }) stopped
                    )
        , test "removing the last tail member leaves the batch as a drained one does" <|
            \_ ->
                let
                    emptied =
                        draining
                            |> stop { requestId = "", targetId = "i-2" }
                            |> stop { requestId = "", targetId = "i-3" }
                in
                Expect.equal ( Nothing, [ Just "queued", Nothing, Nothing ] )
                    (tailAndRows emptied)
        , test "a batch emptied by stops no longer blocks a fresh scan, exactly like a drained one" <|
            \_ ->
                -- The observable consequence of "same state as a drained batch": `exoextScanBlocked`
                -- keys on the tail, so a batch left behind as an empty record would wedge Scan.
                let
                    emptied =
                        draining
                            |> stop { requestId = "", targetId = "i-2" }
                            |> stop { requestId = "", targetId = "i-3" }
                in
                Expect.equal False
                    (ServerDetail.exoextScanBlocked (projectPublishing (runSlot 1700 "done")) emptied)
        , test "a decided-but-unwritten continuation keeps its §7.1 guard when the tail empties" <|
            \_ ->
                -- `awaitingWrite` covers the window between deciding on a subject and issuing its
                -- write. Dropping the record there would open exactly the window it exists to close.
                let
                    emptied =
                        { draining | exoextBatch = Just { batchId = Nothing, remaining = [ "i-3" ], awaitingWrite = True } }
                            |> stop { requestId = "", targetId = "i-3" }
                in
                Expect.equal
                    ( Just { batchId = Nothing, remaining = [], awaitingWrite = True }, True )
                    ( emptied.exoextBatch
                    , ServerDetail.exoextScanBlocked (projectPublishing (runSlot 1700 "done")) emptied
                    )
        , test "a target the tail is not holding changes nothing at all" <|
            \_ ->
                let
                    unchanged =
                        stop { requestId = "", targetId = "i-9" } draining
                in
                Expect.equal ( draining.exoextBatch, draining.exoextCard.scanState )
                    ( unchanged.exoextBatch, unchanged.exoextCard.scanState )
        , test "a named request still takes the wire path: cancel recorded, whole batch ended" <|
            \_ ->
                -- The other branch, unchanged by WP10: stopping a live run stops the batch with it.
                let
                    stopped =
                        stop { requestId = "exo-cs-req-1700", targetId = "i-1" } draining
                in
                Expect.equal
                    ( Just "exo-cs-req-1700", Nothing, [ Just "queued", Nothing, Nothing ] )
                    ( stopped.exoextCancelRequestId
                    , stopped.exoextBatch
                    , [ "i-1", "i-2", "i-3" ] |> List.map (\id -> Dict.get id stopped.exoextCard.scanState)
                    )
        , test "exoextDropFromTail with no batch at all is a no-op" <|
            \_ ->
                let
                    dropped =
                        ServerDetail.exoextDropFromTail "i-2" (ServerDetail.init "self")
                in
                Expect.equal ( Nothing, Dict.empty )
                    ( dropped.exoextBatch, dropped.exoextCard.scanState )
        ]


cloudShieldPreEchoStopSuite : Test
cloudShieldPreEchoStopSuite =
    let
        -- The head of a batch, one poll after its request went out: the host tracks seq 1700 on
        -- `i-1`, and the publisher has not claimed the slot or written any `run.*` yet.
        headWritten =
            writeRequestAt 1700 { subject = "i-1", batchId = Nothing } modelAfterStartScan

        -- The wire as the SECOND leg of a batch finds it: the previous leg's run is still the only
        -- thing in the status slot, settled and correlated to a seq the host no longer tracks.
        previousLegSettled =
            runSlotFor 1600 "done" "i-0"
    in
    describe "ServerDetail stopping a request the wire has not echoed yet"
        [ test "an empty status slot still offers the tracked request a stop" <|
            \_ ->
                -- The reported bug in its simplest form: the request is on the wire, the publisher
                -- has not answered, and the one target actually about to be scanned had no control.
                Expect.equal (Just { targetId = "i-1", requestId = "exo-cs-req-1700" })
                    (ServerDetail.exoextCancellableRun pollTime [] Nothing headWritten)
        , test "the previous leg's settled run does not swallow the new head's stop" <|
            \_ ->
                -- Why the gate is "has the slot echoed MY seq" and not "is the slot empty": mid-batch
                -- the slot is never empty, it carries the leg that just finished. Reading that as the
                -- current run is what left the head reading `queued` with nothing to press.
                Expect.equal (Just { targetId = "i-1", requestId = "exo-cs-req-1700" })
                    (ServerDetail.exoextCancellableRun pollTime previousLegSettled Nothing headWritten)
        , test "the pre-echo state is queued, the same word an unreported request reads as" <|
            \_ ->
                Expect.equal (Just "queued")
                    (ServerDetail.exoextRunControl pollTime previousLegSettled Nothing headWritten
                        |> Maybe.map .state
                    )
        , test "the wire takes over the moment it echoes the seq, terminal states included" <|
            \_ ->
                -- The record covers the pre-echo window and nothing more: once the publisher reports
                -- the run, every answer comes off the wire, and a terminal run is not stoppable.
                Expect.equal [ True, True, False, False ]
                    ([ "queued", "running", "done", "cancelled" ]
                        |> List.map
                            (\state ->
                                ServerDetail.exoextCancellableRun pollTime
                                    (runSlotFor 1700 state "i-1")
                                    (Just { targetId = "i-1", state = state })
                                    headWritten
                                    /= Nothing
                            )
                    )
        , test "the press is acknowledged here too: the row reads stopping, the control withdraws" <|
            \_ ->
                -- The pre-echo run shares the WP10 acknowledgement, so a stop pressed before the
                -- publisher has answered does not look like it did nothing.
                let
                    stopped =
                        ServerDetail.exoextCancelRequested "exo-cs-req-1700" headWritten
                in
                Expect.equal ( Just "i-1", Nothing )
                    ( ServerDetail.exoextStoppingTarget pollTime previousLegSettled Nothing stopped
                    , ServerDetail.exoextCancellableRun pollTime previousLegSettled Nothing stopped
                    )
        , test "the cancel channel alone is enough, so the stop survives a reload of this window" <|
            \_ ->
                -- §7.1 describes a pre-claim cancel as a slot bump; this host writes the cancel
                -- CHANNEL instead, which is durable and names a deterministic request id. Nothing
                -- session-local is involved in reading it back.
                Expect.equal (Just "i-1")
                    (ServerDetail.exoextStoppingTarget pollTime
                        (previousLegSettled ++ cancelSlot "exo-cs-req-1700")
                        Nothing
                        headWritten
                    )
        , test "a stop press in this window names the request, so it writes the cancel channel" <|
            \_ ->
                -- `exoextStopRequested` routes on a non-empty requestId, so the pre-echo id has to
                -- reach it as a real id: an empty one would take the tail-removal path and leave the
                -- publisher never told.
                let
                    ( stopped, _, _ ) =
                        ServerDetail.exoextStopRequested project
                            { requestId = "exo-cs-req-1700", targetId = "i-1" }
                            headWritten
                in
                Expect.equal ( Just "exo-cs-req-1700", Nothing )
                    ( stopped.exoextCancelRequestId, stopped.exoextBatch )
        , test "a tracked request older than the stale bound offers nothing, same valve as the wire" <|
            \_ ->
                -- `seq` is the wall-clock moment of the write, so the record ages on the same clock
                -- the wire path uses. A request the host has stopped believing must not keep a live
                -- Stop control on a row that otherwise reads idle.
                Expect.equal Nothing
                    (ServerDetail.exoextCancellableRun
                        (Time.millisToPosix (1700 + Lifecycle.staleRunAfterMillis + 1))
                        previousLegSettled
                        Nothing
                        headWritten
                    )
        , test "no tracked request means no pre-echo run to offer" <|
            \_ ->
                Expect.equal Nothing
                    (ServerDetail.exoextRunControl pollTime [] Nothing (ServerDetail.init "self"))
        ]
