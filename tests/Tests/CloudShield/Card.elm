module Tests.CloudShield.Card exposing
    ( discoverySuite
    , embedSuite
    , historySuite
    , manifestSuite
    , projectionSuite
    , transportSuite
    )

{-| Unit tests for the CloudShield browser-side dynamic-UI integration (Phase 1).

Covers the pure host logic that carries over 100% to the Jetstream2 object-storage path:
the frozen manifest still validates fail-closed, the §1.2 host projection, the §3.1 discovery
sentinel + §7.1 metadata transport framing, and the §2.4 `$instances` eligibility filter.

-}

import CloudShield.Card as Card
import CloudShield.Discovery as Discovery
import CloudShield.Transport as Transport
import Dict
import Expect
import Json.Decode as Decode
import JsonRender
import JsonRender.Render as Render
import OpenStack.Types as OSTypes
import Set exposing (Set)
import Test exposing (Test, describe, test)



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
    { optedIn = True
    , renderer = Render.init
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


projectionSuite : Test
projectionSuite =
    describe "CloudShield host projection (§1.2)"
        [ test "selected reflects the host selection set" <|
            \_ ->
                let
                    value =
                        Card.projection [] Nothing "" Nothing sampleInstances (sampleModel (Set.singleton "i-1"))
                in
                Expect.equal ( Ok True, Ok False )
                    ( rowSelected 0 value, rowSelected 1 value )
        , test "scanState comes from the local scanState map when no override" <|
            \_ ->
                let
                    value =
                        Card.projection [] Nothing "" Nothing sampleInstances (sampleModel Set.empty)
                in
                Expect.equal ( Ok "idle", Ok "queued" )
                    ( rowScanState 0 value, rowScanState 1 value )
        , test "a polled statusOverride wins for its target row only" <|
            \_ ->
                let
                    override =
                        Just { targetId = "i-1", state = "running" }

                    value =
                        Card.projection [] Nothing "" override sampleInstances (sampleModel Set.empty)
                in
                Expect.equal ( Ok "running", Ok "queued" )
                    ( rowScanState 0 value, rowScanState 1 value )
        , test "embedUrl is projected to the top-level /embedUrl render-state key" <|
            \_ ->
                let
                    value =
                        Card.projection [] Nothing "https://1-2-3-4.sslip.io/app" Nothing sampleInstances (sampleModel Set.empty)
                in
                Expect.equal (Ok "https://1-2-3-4.sslip.io/app")
                    (Decode.decodeValue (Decode.field "embedUrl" Decode.string) value
                        |> Result.mapError Decode.errorToString
                    )
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
-- Tests.CloudShield.Transport getEmbedBlockedSuite); the card just emits the request.


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


historySuite : Test
historySuite =
    describe "the /history projection"
        [ test "rows are newest first (the append-only index is reversed)" <|
            \_ ->
                let
                    -- index file order is oldest first
                    value =
                        Card.projection
                            [ historyEntry "b-1", historyEntry "b-2", historyEntry "b-3" ]
                            Nothing
                            ""
                            Nothing
                            sampleInstances
                            idleModel
                in
                Expect.equal (Ok [ "b-3", "b-2", "b-1" ]) (historyBatchIds value)
        , test "each row carries a human countsLabel" <|
            \_ ->
                let
                    value =
                        Card.projection [ historyEntry "b-1" ] Nothing "" Nothing sampleInstances idleModel

                    label =
                        Decode.decodeValue
                            (Decode.field "history" (Decode.index 0 (Decode.field "countsLabel" Decode.string)))
                            value
                            |> Result.mapError Decode.errorToString
                in
                Expect.equal (Ok "2 high") label
        ]
