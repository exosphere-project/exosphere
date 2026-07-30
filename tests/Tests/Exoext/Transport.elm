module Tests.Exoext.Transport exposing
    ( capBodySuite
    , countsLabelSuite
    , embedRequestSuite
    , embedResultSuite
    , getEmbedBlockedSuite
    , historyRefreshKeySuite
    , indexSuite
    , reqCancelSuite
    , resolveResultBodySuite
    , resultBodySuite
    , runStatusSuite
    )

import Exoext.Transport as Transport
import Expect
import Json.Decode as Decode
import Test exposing (Test, describe, test)


meta : List ( String, String ) -> List { key : String, value : String }
meta pairs =
    List.map (\( k, v ) -> { key = k, value = v }) pairs


capBodySuite : Test
capBodySuite =
    describe "capBody fails closed above a byte cap (phase-0-spec.md §5.5)"
        [ test "a body at exactly the cap is accepted" <|
            \_ ->
                Expect.equal (Ok "abc") (Transport.capBody 3 "abc")
        , test "a body one byte over the cap is rejected" <|
            \_ ->
                Expect.notEqual (Ok "abcd") (Transport.capBody 3 "abcd")
        , test "manifestCapBytes is 16 KB" <|
            \_ ->
                Expect.equal (16 * 1024) Transport.manifestCapBytes
        , test "resultCapBytes is 1 MB" <|
            \_ ->
                Expect.equal (1024 * 1024) Transport.resultCapBytes
        ]


runStatusSuite : Test
runStatusSuite =
    let
        requiredKeys =
            [ ( "exoext.v1.run.seq", "7" ), ( "exoext.v1.run.state", "running" ) ]

        descriptorKeys =
            [ ( "exoext.v1.run.target", "i-1" )
            , ( "exoext.v1.run.requestId", "exo-cs-req-7" )
            , ( "exoext.v1.run.batchId", "exo-cs-batch-7" )
            , ( "exoext.v1.run.phase", "cloning" )
            , ( "exoext.v1.run.pct", "40" )
            ]

        descriptors status =
            ( status.target, status.requestId, status.batchId )
    in
    describe "runStatusFromMetadata reads the §4.3 run slot, descriptors optional"
        [ test "the two required keys alone decode, with every descriptor Nothing" <|
            \_ ->
                Expect.equal
                    (Just ( ( 7, "running" ), ( Nothing, Nothing, Nothing ), ( Nothing, Nothing ) ))
                    (Transport.runStatusFromMetadata (meta requiredKeys)
                        |> Maybe.map (\s -> ( ( s.seq, s.state ), descriptors s, ( s.phase, s.pct ) ))
                    )
        , test "a publisher writing every descriptor decodes all of them" <|
            \_ ->
                Expect.equal
                    (Just ( ( Just "i-1", Just "exo-cs-req-7", Just "exo-cs-batch-7" ), ( Just "cloning", Just 40 ) ))
                    (Transport.runStatusFromMetadata (meta (requiredKeys ++ descriptorKeys))
                        |> Maybe.map (\s -> ( descriptors s, ( s.phase, s.pct ) ))
                    )
        , test "a missing state fails the read even with descriptors present" <|
            \_ ->
                Expect.equal Nothing
                    (Transport.runStatusFromMetadata (meta (( "exoext.v1.run.seq", "7" ) :: descriptorKeys)))
        , test "an unparseable seq fails the read" <|
            \_ ->
                Expect.equal Nothing
                    (Transport.runStatusFromMetadata (meta [ ( "exoext.v1.run.seq", "later" ), ( "exoext.v1.run.state", "running" ) ]))
        , test "an empty descriptor value is Nothing, never an identity that matches nothing" <|
            \_ ->
                Expect.equal (Just ( Nothing, Nothing, Nothing ))
                    (Transport.runStatusFromMetadata
                        (meta
                            (requiredKeys
                                ++ [ ( "exoext.v1.run.target", "" )
                                   , ( "exoext.v1.run.requestId", "" )
                                   , ( "exoext.v1.run.batchId", "" )
                                   ]
                            )
                        )
                        |> Maybe.map descriptors
                    )
        , test "a stringified Python None is Nothing, not a phantom target id" <|
            \_ ->
                Expect.equal (Just ( Nothing, Nothing, Nothing ))
                    (Transport.runStatusFromMetadata
                        (meta
                            (requiredKeys
                                ++ [ ( "exoext.v1.run.target", "None" )
                                   , ( "exoext.v1.run.requestId", "None" )
                                   , ( "exoext.v1.run.batchId", "None" )
                                   ]
                            )
                        )
                        |> Maybe.map descriptors
                    )
        , test "pct outside 0-100 and a non-numeric pct both read as absent" <|
            \_ ->
                Expect.equal [ Just 0, Just 100, Nothing, Nothing, Nothing ]
                    ([ "0", "100", "101", "-1", "half" ]
                        |> List.map
                            (\value ->
                                Transport.runStatusFromMetadata (meta (( "exoext.v1.run.pct", value ) :: requiredKeys))
                                    |> Maybe.andThen .pct
                            )
                    )
        ]


reqCancelSuite : Test
reqCancelSuite =
    describe "the exoext.v1.req.cancel channel"
        [ test "a cancel names the requestId to stop, and touches nothing else" <|
            \_ ->
                Expect.equal [ { key = "exoext.v1.req.cancel", value = "exo-cs-req-7" } ]
                    (Transport.reqCancelMetadata "exo-cs-req-7")
        , test "cancelling nothing writes the cleared value, which names no request" <|
            \_ ->
                Expect.equal [ { key = "exoext.v1.req.cancel", value = "" } ]
                    (Transport.reqCancelMetadata "")
        , test "writing a request slot clears the channel, so a stale stop cannot kill the new run" <|
            \_ ->
                Expect.equal (Just "")
                    (Transport.reqSlotMetadata 9 "{}"
                        |> List.filter (\item -> item.key == Transport.reqCancelKey)
                        |> List.head
                        |> Maybe.map .value
                    )
        ]


resolveResultBodySuite : Test
resolveResultBodySuite =
    describe "resolveResultBody distinguishes an inline result from a {\"ref\": ...} pointer"
        [ test "a plain result object resolves to InlineResult with the body untouched" <|
            \_ ->
                let
                    body =
                        """{"schemaVersion":"1.0","findings":[]}"""
                in
                Expect.equal (Transport.InlineResult body) (Transport.resolveResultBody body)
        , test "a {\"ref\": ...} pointer resolves to ResultRef with the referenced object name" <|
            \_ ->
                Expect.equal
                    (Transport.ResultRef "results/abc-123.json")
                    (Transport.resolveResultBody """{"ref":"results/abc-123.json"}""")
        , test "malformed JSON is not a ref, so it resolves to InlineResult (the renderer's own validation rejects it)" <|
            \_ ->
                Expect.equal (Transport.InlineResult "not json") (Transport.resolveResultBody "not json")
        ]


resultBodySuite : Test
resultBodySuite =
    describe "resultRefObjectName + resultBody: the two-step ref-pointer follow-up fetch"
        [ test "an inline result names no ref to follow up" <|
            \_ ->
                Expect.equal Nothing (Transport.resultRefObjectName (Transport.InlineResult "{}"))
        , test "a ref names the object to fetch next" <|
            \_ ->
                Expect.equal (Just "results/abc-123.json") (Transport.resultRefObjectName (Transport.ResultRef "results/abc-123.json"))
        , test "resultBody returns the inline body directly, ignoring the fetched-ref argument" <|
            \_ ->
                Expect.equal (Just "{}") (Transport.resultBody (Transport.InlineResult "{}") (Just "should be ignored"))
        , test "resultBody returns Nothing for a ref while its fetch is still in flight" <|
            \_ ->
                Expect.equal Nothing (Transport.resultBody (Transport.ResultRef "results/abc-123.json") Nothing)
        , test "resultBody returns the fetched body once a ref's follow-up fetch completes" <|
            \_ ->
                Expect.equal (Just "{\"findings\":[]}") (Transport.resultBody (Transport.ResultRef "results/abc-123.json") (Just "{\"findings\":[]}"))
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
                    (Transport.decodeIndex goodIndex |> List.map .batchId)
        , test "a row's counts are decoded" <|
            \_ ->
                case Transport.decodeIndex goodIndex of
                    first :: _ ->
                        Expect.equal ( 7, 14, 3 )
                            ( first.counts.high, first.counts.medium, first.counts.low )

                    [] ->
                        Expect.fail "expected at least one row"
        , test "a row's requestId is decoded as its per-run result identity" <|
            \_ ->
                Expect.equal [ Just "exo-cs-req-42" ]
                    (Transport.decodeIndex """[ { "batchId": "b-1", "requestId": "exo-cs-req-42" } ]"""
                        |> List.map .requestId
                    )
        , test "a legacy row with no requestId decodes as Nothing (the batchId fallback)" <|
            \_ ->
                Expect.equal [ Nothing ] (Transport.decodeIndex goodIndex |> List.map .requestId |> List.take 1)
        , test "an empty requestId is Nothing, never an id that identifies nothing" <|
            \_ ->
                Expect.equal [ Nothing ]
                    (Transport.decodeIndex """[ { "batchId": "b-1", "requestId": "" } ]"""
                        |> List.map .requestId
                    )
        , test "unknown fields are tolerated and missing counts default to zero" <|
            \_ ->
                let
                    rows =
                        Transport.decodeIndex """[ { "batchId": "b-9", "targetName": "gamma", "extra": "ignored" } ]"""
                in
                case rows of
                    row :: _ ->
                        Expect.equal ( "b-9", "gamma", 0 )
                            ( row.batchId, row.targetName, row.counts.high )

                    [] ->
                        Expect.fail "expected a tolerant row"
        , test "malformed JSON reads as no history" <|
            \_ ->
                Expect.equal [] (Transport.decodeIndex "not json at all")
        , test "a non-array top level reads as no history" <|
            \_ ->
                Expect.equal [] (Transport.decodeIndex """{"batchId":"b-1"}""")
        , test "an over-cap body reads as no history (fail-closed at indexCapBytes)" <|
            \_ ->
                let
                    -- A valid JSON array padded past the 64 KiB cap.
                    oversize =
                        "[" ++ String.repeat (Transport.indexCapBytes + 16) " " ++ "]"
                in
                Expect.equal [] (Transport.decodeIndex oversize)
        , test "indexCapBytes is 64 KiB" <|
            \_ ->
                Expect.equal (64 * 1024) Transport.indexCapBytes
        , test "indexObjectName appends results/index.json to the prefix without a separator" <|
            \_ ->
                Expect.equal "instance-abc/results/index.json"
                    (Transport.indexObjectName "instance-abc/")
        ]


countsLabelSuite : Test
countsLabelSuite =
    describe "countsLabel renders a human severity summary, omitting zeros"
        [ test "non-zero severities are joined in descending order" <|
            \_ ->
                Expect.equal "7 high · 14 medium · 3 low"
                    (Transport.countsLabel { critical = 0, high = 7, medium = 14, low = 3, info = 0 })
        , test "all-zero counts read as no findings" <|
            \_ ->
                Expect.equal "no findings"
                    (Transport.countsLabel { critical = 0, high = 0, medium = 0, low = 0, info = 0 })
        , test "critical and info are included when present" <|
            \_ ->
                Expect.equal "2 critical · 5 info"
                    (Transport.countsLabel { critical = 2, high = 0, medium = 0, low = 0, info = 5 })
        ]


embedRequestSuite : Test
embedRequestSuite =
    describe "embedRequestJson encodes the getEmbed request shape"
        [ test "it carries schemaVersion, action=getEmbed, batchId, resultId and createdAt" <|
            \_ ->
                let
                    json =
                        Transport.embedRequestJson
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
                case Transport.embedResultFromBody body of
                    Just embed ->
                        Expect.equal ( "b-1", "ok", "https://1-2-3-4.sslip.io/embed" )
                            ( embed.batchId, embed.status, embed.embedUrl )

                    Nothing ->
                        Expect.fail "expected an embed result"
        , test "an echoed resultId is decoded: the session's own §4.2 identity" <|
            \_ ->
                Transport.embedResultFromBody """{"kind":"embed","requestId":"r-1","batchId":"b-1","resultId":"exo-cs-req-7","status":"ok"}"""
                    |> Maybe.andThen .resultId
                    |> Expect.equal (Just "exo-cs-req-7")
        , test "a publisher echoing no resultId decodes as Nothing (the reader falls back)" <|
            \_ ->
                Transport.embedResultFromBody """{"kind":"embed","requestId":"r-1","batchId":"b-1","status":"ok"}"""
                    |> Maybe.andThen .resultId
                    |> Expect.equal Nothing
        , test "an empty resultId is Nothing, never an id that identifies nothing" <|
            \_ ->
                Transport.embedResultFromBody """{"kind":"embed","requestId":"r-1","batchId":"b-1","resultId":"","status":"ok"}"""
                    |> Maybe.andThen .resultId
                    |> Expect.equal Nothing
        , test "a scan-result body (no kind field) is not an embed result" <|
            \_ ->
                Expect.equal Nothing
                    (Transport.embedResultFromBody """{"schemaVersion":"1.0","findings":[],"embedUrl":"https://x"}""")
        , test "an error embed result carries its status" <|
            \_ ->
                Transport.embedResultFromBody """{"kind":"embed","batchId":"b-1","status":"error","error":{"code":"expired"}}"""
                    |> Maybe.map .status
                    |> Expect.equal (Just "error")
        ]


getEmbedBlockedSuite : Test
getEmbedBlockedSuite =
    describe "getEmbedBlocked: the scan-protection guard (only blocks a genuinely in-flight scan)"
        [ test "blocks while the run is queued (no pending scan needed)" <|
            \_ ->
                Expect.equal True
                    (Transport.getEmbedBlocked Nothing (meta [ ( "exoext.v1.run.state", "queued" ) ]))
        , test "blocks while the run is running (no pending scan needed)" <|
            \_ ->
                Expect.equal True
                    (Transport.getEmbedBlocked Nothing (meta [ ( "exoext.v1.run.state", "running" ) ]))
        , test "blocks while THIS reader's just-written scan is unclaimed (claimed missing)" <|
            \_ ->
                Expect.equal True
                    (Transport.getEmbedBlocked (Just 1700000000000)
                        (meta [ ( "exoext.v1.req.seq", "1700000000000" ) ])
                    )
        , test "blocks while THIS reader's scan is written but claimed lags its seq" <|
            \_ ->
                Expect.equal True
                    (Transport.getEmbedBlocked (Just 1700000000001)
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
                    (Transport.getEmbedBlocked (Just 1700000000000)
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
                    (Transport.getEmbedBlocked (Just 100)
                        (meta
                            [ ( "exoext.v1.req.seq", "1700000000200" )
                            , ( "exoext.v1.run.state", "done" )
                            ]
                        )
                    )
        , test "allows when an unclaimed req exists but the reader has no pending scan" <|
            \_ ->
                Expect.equal False
                    (Transport.getEmbedBlocked Nothing (meta [ ( "exoext.v1.req.seq", "1700000000000" ) ]))
        , test "allows on error and expired terminal states" <|
            \_ ->
                Expect.equal ( False, False )
                    ( Transport.getEmbedBlocked Nothing (meta [ ( "exoext.v1.run.state", "error" ) ])
                    , Transport.getEmbedBlocked Nothing (meta [ ( "exoext.v1.run.state", "expired" ) ])
                    )
        , test "allows when there is no run and no request slot" <|
            \_ ->
                Expect.equal False (Transport.getEmbedBlocked Nothing (meta [ ( "foo", "bar" ) ]))
        ]


historyRefreshKeySuite : Test
historyRefreshKeySuite =
    describe "historyRefreshKey composes etag + run.seq + run.state"
        [ test "it joins the three keys so a run-state transition changes it" <|
            \_ ->
                Expect.equal "d9d0b3c250c3:42:done"
                    (Transport.historyRefreshKey
                        (meta
                            [ ( "exoext.v1.etag", "d9d0b3c250c3" )
                            , ( "exoext.v1.run.seq", "42" )
                            , ( "exoext.v1.run.state", "done" )
                            ]
                        )
                    )
        , test "the etag alone does not change the key across a scan (run slot does)" <|
            \_ ->
                let
                    key state =
                        Transport.historyRefreshKey
                            (meta
                                [ ( "exoext.v1.etag", "d9d0b3c250c3" )
                                , ( "exoext.v1.run.seq", "42" )
                                , ( "exoext.v1.run.state", state )
                                ]
                            )
                in
                Expect.notEqual (key "running") (key "done")
        , test "missing keys render as empty segments" <|
            \_ ->
                Expect.equal "::" (Transport.historyRefreshKey (meta []))
        ]
