module Tests.CloudShield.ServerDetail exposing (cloudShieldEmbedProjectionSuite, cloudShieldPendingEmbedSuite, cloudShieldReadDecisionSuite, cloudShieldScanTimerSuite)

import CloudShield.Card as Card
import Expect
import Helpers.RemoteDataPlusPlus as RDPP
import ISO8601
import Json.Encode as Encode
import Page.ServerDetail as ServerDetail
import Test exposing (Test, describe, test)
import Time


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


{-| A model that has fetched the archived scan body for `etag-1` (a history pick's findings).
-}
modelWithArchivedFindings : ServerDetail.Model
modelWithArchivedFindings =
    modelWithResultRef "etag-1" "results/b1.json" """{"findings":[{"severity":"low"}]}"""


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
        , test "the same result past its expiry unmounts the iframe, reports EmbedExpired, and marks the row expired (not active) — the expiry fix" <|
            \_ ->
                let
                    projection =
                        ServerDetail.exoextEmbedProjection
                            (embedResultMetadata (okEmbedBody expiresAtIso))
                            afterExpiry
                            Nothing
                            Nothing
                            modelWithArchivedFindings
                in
                -- The expiry fix: an expired session no longer reads as active ("Now viewing"). The
                -- iframe unmounts (embedUrl ""), the state is EmbedExpired, and the previously-viewed
                -- row becomes `expiredBatchId` (a muted "Expired"/View row) instead of `activeBatchId`.
                Expect.equal ( "", Card.EmbedExpired, ( Nothing, Just "b1" ) )
                    ( projection.embedUrl, projection.embedState, ( projection.activeBatchId, projection.expiredBatchId ) )
        , test "an error embed result reports EmbedError with the unwrapped message and no iframe" <|
            \_ ->
                let
                    projection =
                        ServerDetail.exoextEmbedProjection
                            (embedResultMetadata errorEmbedBody)
                            beforeExpiry
                            Nothing
                            Nothing
                            (ServerDetail.init "self")
                in
                Expect.equal ( "", Card.EmbedError "remint failed" )
                    ( projection.embedUrl, projection.embedState )
        , test "a pending getEmbed with no matching result yet reports EmbedLoading and its batchId as pendingBatchId" <|
            \_ ->
                let
                    now =
                        Time.millisToPosix 1000000

                    projection =
                        ServerDetail.exoextEmbedProjection
                            [ { key = "exoext.v1.etag", value = "etag-1" } ]
                            now
                            Nothing
                            Nothing
                            (modelPendingEmbed now)
                in
                Expect.equal ( Card.EmbedLoading, Just "b1" )
                    ( projection.embedState, projection.pendingBatchId )
        , test "a pending getEmbed older than the timeout reports EmbedError and clears pendingBatchId (no stuck loading)" <|
            \_ ->
                let
                    now =
                        Time.millisToPosix 1000000

                    projection =
                        ServerDetail.exoextEmbedProjection
                            [ { key = "exoext.v1.etag", value = "etag-1" } ]
                            now
                            Nothing
                            Nothing
                            (modelPendingEmbed (Time.millisToPosix (1000000 - 31000)))
                in
                Expect.equal ( Card.EmbedError "the request timed out", Nothing )
                    ( projection.embedState, projection.pendingBatchId )
        , test "a matching error result reports EmbedError and clears pendingBatchId (no stuck loading)" <|
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
                            beforeExpiry
                            Nothing
                            Nothing
                            model
                in
                Expect.equal ( Card.EmbedError "remint failed", Nothing )
                    ( projection.embedState, projection.pendingBatchId )
        , test "a just-completed scan whose result batchId is null keys activeBatchId on its requestId (mirrors the index row so the row reads Now viewing)" <|
            \_ ->
                let
                    doneScanBody =
                        """{"schemaVersion":"1.0","requestId":"exo-cs-req-42","batchId":null,"completedAt":"2026-07-20T21:00:00.000Z","status":"ok","findings":[]}"""

                    projection =
                        ServerDetail.exoextEmbedProjection
                            [ { key = "exoext.v1.etag", value = "etag-1" } ]
                            beforeExpiry
                            (Just { targetId = "i-1", state = "done" })
                            (Just doneScanBody)
                            (ServerDetail.init "self")
                in
                Expect.equal (Just "exo-cs-req-42") projection.activeBatchId
        , test "a just-completed scan with a real result batchId keys activeBatchId on that batchId" <|
            \_ ->
                let
                    doneScanBody =
                        """{"schemaVersion":"1.0","requestId":"exo-cs-req-42","batchId":"batch-9","completedAt":"2026-07-20T21:00:00.000Z","status":"ok","findings":[]}"""

                    projection =
                        ServerDetail.exoextEmbedProjection
                            [ { key = "exoext.v1.etag", value = "etag-1" } ]
                            beforeExpiry
                            (Just { targetId = "i-1", state = "done" })
                            (Just doneScanBody)
                            (ServerDetail.init "self")
                in
                Expect.equal (Just "batch-9") projection.activeBatchId
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
