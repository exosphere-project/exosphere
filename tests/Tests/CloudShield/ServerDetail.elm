module Tests.CloudShield.ServerDetail exposing (cloudShieldEmbedProjectionSuite, cloudShieldPendingEmbedSuite, cloudShieldReadDecisionSuite)

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
        | cloudShieldManifest =
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
        | cloudShieldResultRef =
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
                    (ServerDetail.cloudShieldManifestNeedsFetch "etag-1" (ServerDetail.init "self"))
        , test "a matching manifest body is usable and does not refetch" <|
            \_ ->
                let
                    model =
                        modelWithManifest "etag-1" """{"ui":{}}"""
                in
                Expect.equal ( Just """{"ui":{}}""", False )
                    ( ServerDetail.cloudShieldManifestBodyForEtag "etag-1" model
                    , ServerDetail.cloudShieldManifestNeedsFetch "etag-1" model
                    )
        , test "a stale manifest body is ignored and needs a fetch for the new etag" <|
            \_ ->
                let
                    model =
                        modelWithManifest "etag-old" """{"ui":{"stale":true}}"""
                in
                Expect.equal ( Nothing, True )
                    ( ServerDetail.cloudShieldManifestBodyForEtag "etag-new" model
                    , ServerDetail.cloudShieldManifestNeedsFetch "etag-new" model
                    )
        , test "an in-flight manifest request suppresses a duplicate fetch for the same etag" <|
            \_ ->
                let
                    base =
                        ServerDetail.init "self"

                    model =
                        { base | cloudShieldManifestRequestEtag = Just "etag-1" }
                in
                Expect.equal False (ServerDetail.cloudShieldManifestNeedsFetch "etag-1" model)
        , test "inline result bodies are selected directly" <|
            \_ ->
                let
                    body =
                        """{"findings":[],"embedUrl":"https://example.test"}"""
                in
                Expect.equal (Just body)
                    (ServerDetail.effectiveCloudShieldResultBody "etag-1" body (ServerDetail.init "self"))
        , test "a ref result waits until the pointed object has been fetched" <|
            \_ ->
                Expect.equal Nothing
                    (ServerDetail.effectiveCloudShieldResultBody "etag-1" """{"ref":"results/run-1.json"}""" (ServerDetail.init "self"))
        , test "a ref result uses the matching fetched object body" <|
            \_ ->
                let
                    fetched =
                        """{"findings":[{"severity":"low"}]}"""
                in
                Expect.equal (Just fetched)
                    (ServerDetail.effectiveCloudShieldResultBody
                        "etag-1"
                        """{"ref":"results/run-1.json"}"""
                        (modelWithResultRef "etag-1" "results/run-1.json" fetched)
                    )
        , test "a ref result ignores a fetched object from a stale etag" <|
            \_ ->
                Expect.equal Nothing
                    (ServerDetail.effectiveCloudShieldResultBody
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
        | cloudShieldPendingEmbed =
            Just { requestId = "exo-cs-req-200", batchId = "b1", since = since }
    }


cloudShieldEmbedProjectionSuite : Test
cloudShieldEmbedProjectionSuite =
    describe "ServerDetail cloudShieldEmbedProjection (history-View reader)"
        [ test "an ok, unexpired embed result renders findings + the iframe url and reports EmbedReady" <|
            \_ ->
                let
                    projection =
                        ServerDetail.cloudShieldEmbedProjection
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
        , test "the same result past its expiry unmounts the iframe and reports EmbedExpired" <|
            \_ ->
                let
                    projection =
                        ServerDetail.cloudShieldEmbedProjection
                            (embedResultMetadata (okEmbedBody expiresAtIso))
                            afterExpiry
                            Nothing
                            Nothing
                            modelWithArchivedFindings
                in
                Expect.equal ( "", Card.EmbedExpired )
                    ( projection.embedUrl, projection.embedState )
        , test "an error embed result reports EmbedError with the unwrapped message and no iframe" <|
            \_ ->
                let
                    projection =
                        ServerDetail.cloudShieldEmbedProjection
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
                        ServerDetail.cloudShieldEmbedProjection
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
                        ServerDetail.cloudShieldEmbedProjection
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
                            | cloudShieldPendingEmbed =
                                Just { requestId = "exo-cs-req-100", batchId = "b1", since = beforeExpiry }
                        }

                    projection =
                        ServerDetail.cloudShieldEmbedProjection
                            (embedResultMetadata errorEmbedBody)
                            beforeExpiry
                            Nothing
                            Nothing
                            model
                in
                Expect.equal ( Card.EmbedError "remint failed", Nothing )
                    ( projection.embedState, projection.pendingBatchId )
        ]


cloudShieldPendingEmbedSuite : Test
cloudShieldPendingEmbedSuite =
    describe "ServerDetail clearResolvedPendingEmbed"
        [ test "a matching-requestId result clears the pending marker" <|
            \_ ->
                Expect.equal Nothing
                    (ServerDetail.clearResolvedPendingEmbed
                        (embedResultMetadata (okEmbedBody expiresAtIso))
                        (Just { requestId = "exo-cs-req-100", batchId = "b1", since = Time.millisToPosix 0 })
                    )
        , test "a result for a different requestId leaves the pending marker in place" <|
            \_ ->
                let
                    pending =
                        Just { requestId = "exo-cs-req-999", batchId = "b1", since = Time.millisToPosix 0 }
                in
                Expect.equal pending
                    (ServerDetail.clearResolvedPendingEmbed
                        (embedResultMetadata (okEmbedBody expiresAtIso))
                        pending
                    )
        ]
