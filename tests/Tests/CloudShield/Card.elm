module Tests.CloudShield.Card exposing
    ( discoverySuite
    , embedSuite
    , historySuite
    , historyViewParamsSuite
    , manifestSuite
    , projectionSuite
    , rowTimerSuite
    , transportSuite
    )

{-| Unit tests for the CloudShield browser-side dynamic-UI integration (Phase 1).

Covers the pure host logic that carries over 100% to the Jetstream2 object-storage path:
the frozen manifest still validates fail-closed, the §1.2 host projection, the §3.1 discovery
sentinel + §7.1 metadata transport framing, and the §2.4 `$instances` eligibility filter.

-}

import CloudShield.Card as Card
import Dict
import Exoext.Discovery as Discovery
import Exoext.Transport as Transport
import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import JsonRender
import JsonRender.Expr as Expr
import JsonRender.Render as Render
import OpenStack.Types as OSTypes
import Set exposing (Set)
import Test exposing (Test, describe, test)
import Time



-- MANIFEST (the fail-closed catalog gate over the frozen card.json)


manifestSuite : Test
manifestSuite =
    describe "CloudShield embedded manifest"
        [ test "the embedded card.json validates fail-closed through the catalog" <|
            \_ ->
                case JsonRender.decodeString Card.cardJson of
                    Ok _ ->
                        Expect.pass

                    Err message ->
                        Expect.fail ("frozen card.json should validate, but: " ++ message)
        , test "an off-catalog component type is rejected (fail-closed)" <|
            \_ ->
                let
                    bad =
                        """{ "root": "x", "elements": { "x": { "type": "ScriptInjector", "props": {}, "children": [] } } }"""
                in
                case JsonRender.decodeString bad of
                    Ok _ ->
                        Expect.fail "a ScriptInjector component must be rejected"

                    Err _ ->
                        Expect.pass
        ]



-- PROJECTION (host-renderer-interface.md §1.2)


sampleModel : Set String -> Card.Model
sampleModel selection =
    { renderer = Render.init
    , selection = selection
    , selectAll = False
    , scanState = Dict.fromList [ ( "i-2", "queued" ) ]
    , seq = 0
    , pending = Nothing
    , showDemoIframe = False
    }


sampleInstances : List Card.Instance
sampleInstances =
    [ { id = "i-1", name = "alpha", status = "ACTIVE" }
    , { id = "i-2", name = "beta", status = "ACTIVE" }
    ]


rowScanState : Int -> Decode.Value -> Result String String
rowScanState index value =
    Decode.decodeValue
        (Decode.at [ "instances" ] (Decode.index index (Decode.field "scanState" Decode.string)))
        value
        |> Result.mapError Decode.errorToString


rowSelected : Int -> Decode.Value -> Result String Bool
rowSelected index value =
    Decode.decodeValue
        (Decode.field "instances" (Decode.index index (Decode.field "selected" Decode.bool)))
        value
        |> Result.mapError Decode.errorToString


loadedHistory : List Transport.IndexEntry -> { rows : List Transport.IndexEntry, loading : Bool, loaded : Bool }
loadedHistory rows =
    { rows = rows, loading = False, loaded = True }


loadingHistory : { rows : List Transport.IndexEntry, loading : Bool, loaded : Bool }
loadingHistory =
    { rows = [], loading = True, loaded = False }


projectionSuite : Test
projectionSuite =
    describe "CloudShield host projection (§1.2)"
        [ test "selected reflects the host selection set" <|
            \_ ->
                let
                    value =
                        Card.projection Time.utc Nothing Nothing Nothing (loadedHistory []) Nothing "" Nothing sampleInstances (sampleModel (Set.singleton "i-1"))
                in
                Expect.equal ( Ok True, Ok False )
                    ( rowSelected 0 value, rowSelected 1 value )
        , test "scanState comes from the local scanState map when no override" <|
            \_ ->
                let
                    value =
                        Card.projection Time.utc Nothing Nothing Nothing (loadedHistory []) Nothing "" Nothing sampleInstances (sampleModel Set.empty)
                in
                Expect.equal ( Ok "idle", Ok "queued" )
                    ( rowScanState 0 value, rowScanState 1 value )
        , test "a polled statusOverride wins for its target row only" <|
            \_ ->
                let
                    override =
                        Just { targetId = "i-1", state = "running" }

                    value =
                        Card.projection Time.utc Nothing Nothing Nothing (loadedHistory []) Nothing "" override sampleInstances (sampleModel Set.empty)
                in
                Expect.equal ( Ok "running", Ok "queued" )
                    ( rowScanState 0 value, rowScanState 1 value )
        , test "embedUrl is projected to the top-level /embedUrl render-state key" <|
            \_ ->
                let
                    value =
                        Card.projection Time.utc Nothing Nothing Nothing (loadedHistory []) Nothing "https://1-2-3-4.sslip.io/app" Nothing sampleInstances (sampleModel Set.empty)
                in
                Expect.equal (Ok "https://1-2-3-4.sslip.io/app")
                    (Decode.decodeValue (Decode.field "embedUrl" Decode.string) value
                        |> Result.mapError Decode.errorToString
                    )
        , test "the running scan's counting-up label projects onto its row, even with history present" <|
            \_ ->
                -- The host composes "scanning · m:ss" into the running target's override; the row must
                -- carry it regardless of a history pick being shown on the right (non-empty history).
                let
                    override =
                        Just { targetId = "i-1", state = "scanning · 0:23" }

                    value =
                        Card.projection Time.utc Nothing Nothing Nothing (loadedHistory [ historyEntry "b-1" ]) Nothing "" override sampleInstances (sampleModel Set.empty)
                in
                Expect.equal ( Ok "scanning · 0:23", Ok "queued" )
                    ( rowScanState 0 value, rowScanState 1 value )
        , test "history first-load state omits the count until rows have fetched" <|
            \_ ->
                let
                    value =
                        Card.projection Time.utc Nothing Nothing Nothing loadingHistory Nothing "" Nothing sampleInstances (sampleModel Set.empty)

                    stringField key =
                        Decode.decodeValue (Decode.field key Decode.string) value
                            |> Result.mapError Decode.errorToString
                in
                Expect.equal ( Ok "Scan history", Ok "" )
                    ( stringField "historyLabel", stringField "historyNote" )
        ]


{-| `scanningRowLabel` builds the left-column running row's counting-up elapsed from the run's
wall-clock start (the request seq) and the shared clock, consistent with the frozen completion time.
-}
rowTimerSuite : Test
rowTimerSuite =
    let
        start =
            1750000000000
    in
    describe "the scanning-row counting-up label (scanningRowLabel)"
        [ test "composes scanning · m:ss from the wall-clock elapsed" <|
            \_ ->
                Expect.equal "scanning · 0:23" (Card.scanningRowLabel start (start + 23000))
        , test "formats minutes past 60 seconds" <|
            \_ ->
                Expect.equal "scanning · 2:05" (Card.scanningRowLabel start (start + 125000))
        , test "reads 0:00 before the wall-clock start stamp lands (non-epoch guard)" <|
            \_ ->
                -- A tiny optimistic seq (not yet stamped with wall-clock millis) must not read as an
                -- absurd elapsed; it clamps to 0:00 until the real timestamp arrives.
                Expect.equal "scanning · 0:00" (Card.scanningRowLabel 5 (start + 23000))
        , test "reads 0:00 for a negative diff (clock skew)" <|
            \_ ->
                Expect.equal "scanning · 0:00" (Card.scanningRowLabel start (start - 1000))
        ]



-- DISCOVERY (§3.1 sentinel, §2.4 eligibility)


meta : List ( String, String ) -> List OSTypes.MetadataItem
meta pairs =
    List.map (\( k, v ) -> { key = k, value = v }) pairs


discoverySuite : Test
discoverySuite =
    describe "CloudShield discovery"
        [ test "readSentinel returns Nothing without the exoext.v1.kind key" <|
            \_ ->
                Expect.equal Nothing (Discovery.readSentinel (meta [ ( "foo", "bar" ) ]))
        , test "readSentinel reads kind + store + flags" <|
            \_ ->
                case Discovery.readSentinel (meta [ ( "exoext.v1.kind", "cloudshield" ), ( "exoext.v1.store", "metadata" ), ( "exoext.v1.flags", "scan,results" ) ]) of
                    Just s ->
                        Expect.equal ( "cloudshield", Discovery.StoreMetadata, [ "scan", "results" ] )
                            ( s.kind, s.store, s.flags )

                    Nothing ->
                        Expect.fail "sentinel should be present"
        , test "manifestBodyFromMetadata concatenates man.body chunks in order" <|
            \_ ->
                Expect.equal (Just "abcdEFGH")
                    (Discovery.manifestBodyFromMetadata
                        (meta [ ( "exoext.v1.man.body.0", "abcd" ), ( "exoext.v1.man.body.1", "EFGH" ) ])
                    )
        , test "manifestBodyFromMetadata stops at the first gap" <|
            \_ ->
                Expect.equal (Just "abcd")
                    (Discovery.manifestBodyFromMetadata
                        (meta [ ( "exoext.v1.man.body.0", "abcd" ), ( "exoext.v1.man.body.2", "XX" ) ])
                    )
        , test "manifestBodyFromMetadata honors the man.body.n count, ignoring orphans" <|
            \_ ->
                Expect.equal (Just "abcd")
                    (Discovery.manifestBodyFromMetadata
                        (meta [ ( "exoext.v1.man.body.n", "1" ), ( "exoext.v1.man.body.0", "abcd" ), ( "exoext.v1.man.body.1", "STALE" ) ])
                    )
        , test "manifestBodyFromMetadata is Nothing without man.body.0" <|
            \_ ->
                Expect.equal Nothing (Discovery.manifestBodyFromMetadata (meta [ ( "exoext.v1.man.body.1", "x" ) ]))
        , test "eligibleInstances keeps ACTIVE and excludes self" <|
            \_ ->
                let
                    servers =
                        [ { id = "self", name = "cloudshield-vm", status = OSTypes.ServerActive }
                        , { id = "i-1", name = "alpha", status = OSTypes.ServerActive }
                        , { id = "i-2", name = "building", status = OSTypes.ServerBuild }
                        ]
                in
                Expect.equal [ { id = "i-1", name = "alpha", status = "Active" } ]
                    (Discovery.eligibleInstances "self" servers)
        ]



-- TRANSPORT (§4.1 request, §7.1 framing, §4.3 status)


transportSuite : Test
transportSuite =
    describe "CloudShield POC transport (§7.1)"
        [ test "scanRequestJson encodes the §4.1 shape" <|
            \_ ->
                let
                    json =
                        Transport.scanRequestJson
                            { requestId = "exo-cs-req-1"
                            , batchId = Nothing
                            , createdAt = "2026-06-22T00:00:00Z"
                            , projectId = "proj-1"
                            , target = { instanceId = "i-1", instanceName = "alpha" }
                            , profile = "quick"
                            }

                    field path =
                        Decode.decodeString (Decode.at path Decode.string) json
                in
                Expect.equal
                    ( Ok "1.0", Ok "i-1", Ok "snapshot-clone" )
                    ( field [ "schemaVersion" ], field [ "target", "instanceId" ], field [ "scan", "method" ] )
        , test "reqSlotMetadata emits seq + chunk-count + chunked body" <|
            \_ ->
                let
                    items =
                        Transport.reqSlotMetadata 7 "hello"

                    keys =
                        List.map .key items
                in
                Expect.equal [ "exoext.v1.req.seq", "exoext.v1.req.body.n", "exoext.v1.req.body.0" ] keys
        , test "chunkString splits at the size boundary" <|
            \_ ->
                Expect.equal [ "abc", "de" ] (Transport.chunkString 3 "abcde")
        , test "chunkString keeps a short string whole" <|
            \_ ->
                Expect.equal [ "abc" ] (Transport.chunkString 255 "abc")
        , test "reqSlotMetadata chunks a >255-char body across body.0/body.1" <|
            \_ ->
                let
                    big =
                        String.repeat 300 "x"

                    keys =
                        Transport.reqSlotMetadata 1 big |> List.map .key
                in
                Expect.equal [ "exoext.v1.req.seq", "exoext.v1.req.body.n", "exoext.v1.req.body.0", "exoext.v1.req.body.1" ] keys
        , test "readChunkedBody honors the count and ignores a stale orphan chunk" <|
            \_ ->
                -- a later, shorter write left body.2 behind; the count (n=2) must win.
                Expect.equal (Just "abcdEFGH")
                    (Transport.readChunkedBody "exoext.v1.body."
                        (meta
                            [ ( "exoext.v1.body.n", "2" )
                            , ( "exoext.v1.body.0", "abcd" )
                            , ( "exoext.v1.body.1", "EFGH" )
                            , ( "exoext.v1.body.2", "STALE" )
                            ]
                        )
                    )
        , test "runStatusFromMetadata reads seq + state" <|
            \_ ->
                Expect.equal (Just { seq = 4, state = "running" })
                    (Transport.runStatusFromMetadata (meta [ ( "exoext.v1.run.seq", "4" ), ( "exoext.v1.run.state", "running" ) ]))
        , test "runStatusFromMetadata is Nothing when state is missing" <|
            \_ ->
                Expect.equal Nothing
                    (Transport.runStatusFromMetadata (meta [ ( "exoext.v1.run.seq", "4" ) ]))
        , test "resultBodyFromMetadata reassembles res.body chunks" <|
            \_ ->
                Expect.equal (Just "RESULT")
                    (Transport.resultBodyFromMetadata (meta [ ( "exoext.v1.res.body.0", "RES" ), ( "exoext.v1.res.body.1", "ULT" ) ]))
        ]



-- EMBED (getEmbed action, Phase B). The single-req-slot guard is host-side (see
-- Tests.Exoext.Transport getEmbedBlockedSuite); the card just emits the request.


idleModel : Card.Model
idleModel =
    let
        base =
            sampleModel Set.empty
    in
    { base | scanState = Dict.empty }


embedSuite : Test
embedSuite =
    describe "cloudshield.getEmbed emits EmbedRequested (guard is host-side)"
        [ test "a getEmbed with a batchId emits EmbedRequested regardless of card scan state" <|
            \_ ->
                -- sampleModel carries a stale "queued" scan for i-2; the card no longer guards on it.
                Expect.equal (Just (Card.EmbedRequested { batchId = "b-1" }))
                    (Card.requestEmbed (Just "b-1") (sampleModel Set.empty) |> Tuple.second)
        , test "a getEmbed with no batchId is a no-op" <|
            \_ ->
                Expect.equal Nothing
                    (Card.requestEmbed Nothing idleModel |> Tuple.second)
        ]



-- HISTORY (the /history projection, newest first)


historyEntry : String -> Transport.IndexEntry
historyEntry batchId =
    { batchId = batchId
    , targetId = "i-1"
    , targetName = "alpha"
    , completedAt = "2026-07-01T00:00:00Z"
    , status = "done"
    , counts = { critical = 0, high = 2, medium = 0, low = 0, info = 0 }
    }


historyBatchIds : Decode.Value -> Result String (List String)
historyBatchIds value =
    Decode.decodeValue
        (Decode.field "history" (Decode.list (Decode.field "batchId" Decode.string)))
        value
        |> Result.mapError Decode.errorToString


historyNoteOf : Decode.Value -> Result String String
historyNoteOf value =
    Decode.decodeValue (Decode.field "historyNote" Decode.string) value
        |> Result.mapError Decode.errorToString


{-| `n` history entries in append-only (oldest-first) file order, batchIds `b-01`..`b-<n>`.
-}
historyEntries : Int -> List Transport.IndexEntry
historyEntries n =
    List.range 1 n
        |> List.map (\i -> historyEntry ("b-" ++ String.padLeft 2 '0' (String.fromInt i)))


projectHistory : List Transport.IndexEntry -> Decode.Value
projectHistory entries =
    Card.projection Time.utc Nothing Nothing Nothing (loadedHistory entries) Nothing "" Nothing sampleInstances idleModel


{-| Project a single history row and read a string field from `/history/0`, given the active and
in-flight (`pendingBatchId`) batch ids. No embed-error batch (the common case).
-}
historyRowField : Maybe String -> Maybe String -> Transport.IndexEntry -> String -> Result String String
historyRowField activeBatchId pendingBatchId entry field =
    historyRowFieldE activeBatchId pendingBatchId Nothing entry field


{-| As `historyRowField`, but also supplying the `erroredBatchId` (the last getEmbed that failed /
timed out) so the new "Couldn't open" / "Retry" precedence can be exercised.
-}
historyRowFieldE : Maybe String -> Maybe String -> Maybe String -> Transport.IndexEntry -> String -> Result String String
historyRowFieldE activeBatchId pendingBatchId erroredBatchId entry field =
    Card.projection Time.utc activeBatchId pendingBatchId erroredBatchId (loadedHistory [ entry ]) Nothing "" Nothing sampleInstances idleModel
        |> Decode.decodeValue (Decode.field "history" (Decode.index 0 (Decode.field field Decode.string)))
        |> Result.mapError Decode.errorToString


erroredEntry : Transport.IndexEntry
erroredEntry =
    { batchId = "b-err"
    , targetId = "i-1"
    , targetName = "alpha"
    , completedAt = "2026-07-01T00:00:00Z"
    , status = "error"
    , counts = { critical = 0, high = 0, medium = 0, low = 0, info = 0 }
    }


historySuite : Test
historySuite =
    describe "the /history projection"
        [ test "rows are newest first (the append-only index is reversed)" <|
            \_ ->
                Expect.equal (Ok [ "b-3", "b-2", "b-1" ])
                    (historyBatchIds (projectHistory [ historyEntry "b-1", historyEntry "b-2", historyEntry "b-3" ]))
        , test "each row carries a human countsLabel" <|
            \_ ->
                let
                    label =
                        Decode.decodeValue
                            (Decode.field "history" (Decode.index 0 (Decode.field "countsLabel" Decode.string)))
                            (projectHistory [ historyEntry "b-1" ])
                            |> Result.mapError Decode.errorToString
                in
                Expect.equal (Ok "2 high") label
        , test "at or under the cap, all rows show and the note is empty" <|
            \_ ->
                let
                    value =
                        projectHistory (historyEntries 20)
                in
                Expect.equal ( Ok 20, Ok "" )
                    ( historyBatchIds value |> Result.map List.length
                    , historyNoteOf value
                    )
        , test "above the cap, only the latest 20 show (newest first) with the overflow note" <|
            \_ ->
                let
                    value =
                        projectHistory (historyEntries 25)

                    ids =
                        historyBatchIds value
                in
                Expect.equal ( Ok 20, Ok "b-25", Ok "b-06" )
                    ( ids |> Result.map List.length
                    , ids |> Result.map (List.head >> Maybe.withDefault "?")
                    , ids |> Result.map (List.reverse >> List.head >> Maybe.withDefault "?")
                    )
        , test "the overflow note wording names the cap and the total" <|
            \_ ->
                Expect.equal (Ok "Showing the latest 20 of 25 scans.")
                    (historyNoteOf (projectHistory (historyEntries 25)))
        , test "completedAt is humanized (local, 12-hour), not raw ISO" <|
            \_ ->
                Expect.equal (Ok "Jul 1, 2026 · 12:00 AM")
                    (Decode.decodeValue
                        (Decode.field "history" (Decode.index 0 (Decode.field "completedAt" Decode.string)))
                        (projectHistory [ historyEntry "b-1" ])
                        |> Result.mapError Decode.errorToString
                    )
        , test "a done row not being viewed is a plain View with an empty (hidden) rowState" <|
            \_ ->
                Expect.equal ( Ok "", Ok "View", Ok "alpha · #b-1" )
                    ( historyRowField Nothing Nothing (historyEntry "b-1") "rowState"
                    , historyRowField Nothing Nothing (historyEntry "b-1") "actionLabel"
                    , historyRowField Nothing Nothing (historyEntry "b-1") "subLabel"
                    )
        , test "the active batch row flips to Now viewing / Refresh" <|
            \_ ->
                Expect.equal ( Ok "Now viewing", Ok "Refresh" )
                    ( historyRowField (Just "b-1") Nothing (historyEntry "b-1") "rowState"
                    , historyRowField (Just "b-1") Nothing (historyEntry "b-1") "actionLabel"
                    )
        , test "the row whose getEmbed is in flight shows the Opening… loading state (button and badge)" <|
            \_ ->
                Expect.equal ( Ok "Opening…", Ok "Opening…" )
                    ( historyRowField Nothing (Just "b-1") (historyEntry "b-1") "rowState"
                    , historyRowField Nothing (Just "b-1") (historyEntry "b-1") "actionLabel"
                    )
        , test "a pending getEmbed suppresses Now viewing on the previously-active row (it drops to plain View)" <|
            \_ ->
                -- b-2's getEmbed is in flight while b-1 was the active pick: b-1 must NOT read as
                -- Now viewing; it falls back to an idle View row.
                Expect.equal ( Ok "", Ok "View" )
                    ( historyRowField (Just "b-1") (Just "b-2") (historyEntry "b-1") "rowState"
                    , historyRowField (Just "b-1") (Just "b-2") (historyEntry "b-1") "actionLabel"
                    )
        , test "the loading row wins over Now viewing when it is also the active batch" <|
            \_ ->
                Expect.equal ( Ok "Opening…", Ok "Opening…" )
                    ( historyRowField (Just "b-1") (Just "b-1") (historyEntry "b-1") "rowState"
                    , historyRowField (Just "b-1") (Just "b-1") (historyEntry "b-1") "actionLabel"
                    )
        , test "a failed scan is muted: failed state, nothing to view, no findings" <|
            \_ ->
                Expect.equal ( Ok "failed", Ok "", Ok "failed · no findings" )
                    ( historyRowField Nothing Nothing erroredEntry "rowState"
                    , historyRowField Nothing Nothing erroredEntry "actionLabel"
                    , historyRowField Nothing Nothing erroredEntry "subLabel"
                    )
        , test "an errored batch never reads as active even if it is the active batchId" <|
            \_ ->
                Expect.equal ( Ok "failed", Ok "" )
                    ( historyRowField (Just "b-err") Nothing erroredEntry "rowState"
                    , historyRowField (Just "b-err") Nothing erroredEntry "actionLabel"
                    )
        , test "an errored batch never reads as loading even if its getEmbed is pending" <|
            \_ ->
                Expect.equal ( Ok "failed", Ok "" )
                    ( historyRowField Nothing (Just "b-err") erroredEntry "rowState"
                    , historyRowField Nothing (Just "b-err") erroredEntry "actionLabel"
                    )
        , test "per-row findings are synthesized from counts so history pills match results pills" <|
            \_ ->
                Expect.equal (Ok [ "high", "high" ])
                    (Decode.decodeValue
                        (Decode.field "history" (Decode.index 0 (Decode.field "findings" (Decode.list (Decode.field "severity" Decode.string)))))
                        (projectHistory [ historyEntry "b-1" ])
                        |> Result.mapError Decode.errorToString
                    )
        , test "the errored-getEmbed batch row reads Couldn't open / Retry" <|
            \_ ->
                Expect.equal ( Ok "Couldn't open", Ok "Retry" )
                    ( historyRowFieldE Nothing Nothing (Just "b-1") (historyEntry "b-1") "rowState"
                    , historyRowFieldE Nothing Nothing (Just "b-1") (historyEntry "b-1") "actionLabel"
                    )
        , test "while an embed error is shown, the prior active row does NOT reclaim Now viewing" <|
            \_ ->
                -- b-1 was the active pick, then its getEmbed errored (erroredBatchId = b-1). b-1 must
                -- read as Couldn't open, never Now viewing.
                Expect.equal ( Ok "Couldn't open", Ok "Retry" )
                    ( historyRowFieldE (Just "b-1") Nothing (Just "b-1") (historyEntry "b-1") "rowState"
                    , historyRowFieldE (Just "b-1") Nothing (Just "b-1") (historyEntry "b-1") "actionLabel"
                    )
        , test "an embed error on one batch suppresses Now viewing on a DIFFERENT active row" <|
            \_ ->
                -- b-2's getEmbed errored while b-1 was the active pick: b-1 must NOT read Now viewing.
                Expect.equal ( Ok "", Ok "View" )
                    ( historyRowFieldE (Just "b-1") Nothing (Just "b-2") (historyEntry "b-1") "rowState"
                    , historyRowFieldE (Just "b-1") Nothing (Just "b-2") (historyEntry "b-1") "actionLabel"
                    )
        , test "a fresh getEmbed (pending) wins over a stale embed error on the same row" <|
            \_ ->
                -- Retry fired: pending = b-1 again while the old error for b-1 is cleared host-side; if
                -- both were somehow set, loading supersedes the error look for that row.
                Expect.equal ( Ok "Opening…", Ok "Opening…" )
                    ( historyRowFieldE Nothing (Just "b-1") Nothing (historyEntry "b-1") "rowState"
                    , historyRowFieldE Nothing (Just "b-1") Nothing (historyEntry "b-1") "actionLabel"
                    )
        , test "an errored SCAN is never overridden by an embed-error state (stays failed, no view)" <|
            \_ ->
                Expect.equal ( Ok "failed", Ok "" )
                    ( historyRowFieldE Nothing Nothing (Just "b-err") erroredEntry "rowState"
                    , historyRowFieldE Nothing Nothing (Just "b-err") erroredEntry "actionLabel"
                    )
        , test "the column headers carry a live count of targets and total scans" <|
            \_ ->
                let
                    value =
                        Card.projection Time.utc Nothing Nothing Nothing (loadedHistory (historyEntries 3)) Nothing "" Nothing sampleInstances idleModel

                    stringField key =
                        Decode.decodeValue (Decode.field key Decode.string) value
                            |> Result.mapError Decode.errorToString
                in
                -- sampleInstances has 2 targets; 3 history entries.
                Expect.equal ( Ok "Scan targets · 2", Ok "Scan history · 3" )
                    ( stringField "targetsLabel", stringField "historyLabel" )
        ]


{-| Regression pin for the history View button: its `params` must emit the batchId VALUE, not
the item PATH. A top-level `{"$item":"batchId"}` resolves (per `Expr.resolveTopLevelParam`) to
`"/history/0/batchId"` — the pointer string — which the bridge rejects. The fallback cardJson
uses `{"$template":"${batchId}"}`, which resolves to the row's batchId value.
-}
historyViewParamsSuite : Test
historyViewParamsSuite =
    describe "the history View button emits a batchId value, not a JSON Pointer"
        [ test "resolveParams in a /history row 0 childContext yields the item's batchId value" <|
            \_ ->
                case JsonRender.decodeString Card.cardJson of
                    Ok spec ->
                        case Dict.get "history-view-btn" spec.elements |> Maybe.andThen (\el -> Dict.get "press" el.on) |> Maybe.andThen List.head of
                            Just binding ->
                                let
                                    item =
                                        Encode.object
                                            [ ( "batchId", Encode.string "b-1" )
                                            , ( "targetName", Encode.string "alpha" )
                                            ]

                                    ctx =
                                        Expr.childContext "/history" 0 item (Expr.rootContext [] Encode.null)

                                    resolved =
                                        Expr.resolveParams ctx binding.params
                                in
                                Expect.equal "{\"batchId\":\"b-1\"}" (Encode.encode 0 resolved)

                            Nothing ->
                                Expect.fail "history-view-btn should carry a press binding"

                    Err message ->
                        Expect.fail ("the fallback cardJson should validate: " ++ message)
        ]
