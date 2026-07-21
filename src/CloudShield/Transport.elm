module CloudShield.Transport exposing
    ( Counts
    , EmbedRequest
    , EmbedResult
    , IndexEntry
    , ResolvedResult(..)
    , RunStatus
    , ScanRequest
    , capBody
    , chunkString
    , countsLabel
    , decodeIndex
    , embedRequestJson
    , embedResultFromBody
    , getEmbedBlocked
    , historyRefreshKey
    , indexCapBytes
    , indexObjectName
    , manifestCapBytes
    , readChunkedBody
    , reqSlotMetadata
    , resolveResultBody
    , resultBody
    , resultBodyFromMetadata
    , resultCapBytes
    , resultRefObjectName
    , runStatusFromMetadata
    , scanRequestJson
    )

{-| CloudShield POC wire transport over Nova server metadata (Phase 0 spec §4.1 / §7.1).

This is the **throwaway framing layer** for the no-Jetstream2 POC: it carries the §4.1 scan
request and the §4.3 status / §4.2 result over server metadata instead of object storage.
Per the spec, the request/result _JSON_ and the run _states_ are identical to the Jetstream2
path; only the `seq`/`claimed`/chunk framing here is POC-specific and is dropped when
`store=swift` lands (Phase 1b).

All functions are pure and string-in/string-out so they unit-test without a live cloud.

Wire layout (on the publishing CloudShield VM's own metadata):

  - **Request slot (Exosphere → VM):** `exoext.v1.req.seq` = monotonic integer; the §4.1
    request JSON chunked across `exoext.v1.req.body.0..N` (≤255 chars/value, §3.1 / D5).
  - **Status / result slot (VM → Exosphere):** `exoext.v1.run.seq` echoes the request seq;
    `exoext.v1.run.state` ∈ queued|running|done|error|cancelled|expired (§4.4); an optional
    small result summary is chunked across `exoext.v1.res.body.0..N` (§4.2 shape).

-}

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import OpenStack.Types as OSTypes



-- REQUEST (Exosphere -> VM)


{-| The host-resolved scan request (§4.1). `target` is the re-resolved real instance
(§5.4); `createdAt` is an ISO-8601 string supplied by the host (Phase 0 §4.1).
-}
type alias ScanRequest =
    { requestId : String
    , batchId : Maybe String
    , createdAt : String
    , projectId : String
    , target : { instanceId : String, instanceName : String }
    , profile : String
    }


{-| Encode a §4.1 scan-request object to a compact JSON string (the body that gets chunked
into the metadata req-slot, or written object-side in Phase 1b — same bytes either way).
-}
scanRequestJson : ScanRequest -> String
scanRequestJson req =
    Encode.encode 0 <|
        Encode.object
            [ ( "schemaVersion", Encode.string "1.0" )
            , ( "requestId", Encode.string req.requestId )
            , ( "batchId"
              , case req.batchId of
                    Just b ->
                        Encode.string b

                    Nothing ->
                        Encode.null
              )
            , ( "createdAt", Encode.string req.createdAt )
            , ( "requestedBy"
              , Encode.object
                    [ ( "source", Encode.string "exosphere" )
                    , ( "projectId", Encode.string req.projectId )
                    ]
              )
            , ( "target"
              , Encode.object
                    [ ( "instanceId", Encode.string req.target.instanceId )
                    , ( "instanceName", Encode.string req.target.instanceName )
                    ]
              )
            , ( "scan"
              , Encode.object
                    [ ( "profile", Encode.string req.profile )
                    , ( "method", Encode.string "snapshot-clone" )
                    ]
              )
            ]


{-| Build the §7.1 request-slot metadata key/value items for a request: the monotonic `seq`,
a **chunk count** `exoext.v1.req.body.n`, and the request JSON chunked across
`exoext.v1.req.body.0..N-1` (≤255 chars/value). Each item is written with one
`requestSetServerMetadata` call on the CloudShield VM.

The explicit count is what makes a re-write safe: Nova metadata POST **merges** keys and never
deletes, so a later, shorter request would otherwise leave a stale trailing `body.N` chunk that
a gapless reader would concatenate into corrupt JSON. The reader (`readChunkedBody`) honors the
count and reads exactly `n` chunks, ignoring any orphan.

-}
reqSlotMetadata : Int -> String -> List OSTypes.MetadataItem
reqSlotMetadata seq requestJson =
    let
        chunks =
            chunkString 255 requestJson
    in
    { key = "exoext.v1.req.seq", value = String.fromInt seq }
        :: { key = "exoext.v1.req.body.n", value = String.fromInt (List.length chunks) }
        :: List.indexedMap
            (\i chunk ->
                { key = "exoext.v1.req.body." ++ String.fromInt i, value = chunk }
            )
            chunks



-- STATUS / RESULT (VM -> Exosphere)


{-| The coarse, UI-facing run status read back from the VM's metadata (§4.3 `state`), with
the `seq` it corresponds to (§7.1 correlation).
-}
type alias RunStatus =
    { seq : Int
    , state : String
    }


{-| Read the §7.1 status slot from metadata: `exoext.v1.run.seq` + `exoext.v1.run.state`.
`Nothing` unless both are present and `seq` parses.
-}
runStatusFromMetadata : List OSTypes.MetadataItem -> Maybe RunStatus
runStatusFromMetadata metadata =
    let
        dict =
            toDict metadata
    in
    Maybe.map2 RunStatus
        (Dict.get "exoext.v1.run.seq" dict |> Maybe.andThen String.toInt)
        (Dict.get "exoext.v1.run.state" dict)


{-| Reassemble the small result summary (§4.2) from `exoext.v1.res.body.*` chunks.
-}
resultBodyFromMetadata : List OSTypes.MetadataItem -> Maybe String
resultBodyFromMetadata metadata =
    readChunkedBody "exoext.v1.res.body." metadata



-- OBJECT-STORAGE BODY CAPS + RESULT-POINTER RESOLUTION (store=swift, §5.5)


{-| The manifest body hard cap: **16 KB** (`phase-0-spec.md` §5.5). An object-storage manifest
fetch has no natural size bound (unlike the metadata path, which is bounded by the chunk-key
budget), so this is enforced fail-closed by `capBody` after the fetch.
-}
manifestCapBytes : Int
manifestCapBytes =
    16 * 1024


{-| The result object hard cap: **1 MB** (`phase-0-spec.md` §5.5, "result object ≤ 1 MB (host
refuses to bind a larger body)").
-}
resultCapBytes : Int
resultCapBytes =
    1024 * 1024


{-| Fail-closed size check on a fetched body, measured in UTF-8 bytes via `String.length` on the
already-decoded String (the wire-level `Bytes.width` check happens earlier, in
`Rest.Helpers.expectCappedStringWithErrorBody`; this is the second, pure gate the host applies
before binding the body into the card — e.g. after following a result `ref` pointer, whose
target was not covered by the original fetch's cap). `Err` names the cap that was exceeded so the
host can draw the "?" provenance truncation warning (§5.5) instead of silently truncating.
-}
capBody : Int -> String -> Result String String
capBody capBytes body =
    if String.length body > capBytes then
        Err ("body exceeds the " ++ String.fromInt capBytes ++ "-byte cap")

    else
        Ok body


{-| A result body is either the result JSON itself, or a small `{"ref": "<objectName>"}`
pointer to where the real result object lives (§3.2: a result can be written write-once under
`results/<requestId>.json` and referenced from the polled slot rather than duplicated inline).
-}
type ResolvedResult
    = InlineResult String
    | ResultRef String


{-| Resolve a fetched result body: a top-level `{"ref": "..."}` string field means the real
result lives at that object name (relative to the same container/prefix) and must be fetched
separately (fail-closed under `resultCapBytes` again); anything else is treated as the result
body itself. Not valid JSON at all still resolves to `InlineResult` — the renderer's own
fail-closed manifest/result validation is what rejects it, not this resolver.
-}
resolveResultBody : String -> ResolvedResult
resolveResultBody body =
    case Decode.decodeString (Decode.field "ref" Decode.string) body of
        Ok ref ->
            ResultRef ref

        Err _ ->
            InlineResult body


{-| The object name to fetch next, when a result body was a `{"ref": ...}` pointer rather than
the result itself (§3.2). `Nothing` when the body was already the result (`InlineResult`) — no
second fetch needed.
-}
resultRefObjectName : ResolvedResult -> Maybe String
resultRefObjectName resolved =
    case resolved of
        InlineResult _ ->
            Nothing

        ResultRef objectName ->
            Just objectName


{-| The caller-facing extraction of a `ResolvedResult`: the result body ready to hand to the
card, either taken directly (`InlineResult`) or fetched separately from the object name named
by `resultRefObjectName` and passed in here once available (`fetchedRefBody`, `Nothing` while
that fetch is in-flight).
-}
resultBody : ResolvedResult -> Maybe String -> Maybe String
resultBody resolved fetchedRefBody =
    case resolved of
        InlineResult body ->
            Just body

        ResultRef _ ->
            fetchedRefBody


{-| Reassemble a chunked body written under `<prefix>0..N-1`. Prefers an explicit
`<prefix>n` count (so a stale orphan chunk from an earlier, longer write is ignored — Nova
metadata never deletes keys); falls back to gapless concatenation stopping at the first
missing index for bodies written without a count. `Nothing` when no body is present.
-}
readChunkedBody : String -> List OSTypes.MetadataItem -> Maybe String
readChunkedBody prefix metadata =
    let
        dict =
            toDict metadata

        gaplessFrom index acc =
            case Dict.get (prefix ++ String.fromInt index) dict of
                Just chunk ->
                    gaplessFrom (index + 1) (acc ++ chunk)

                Nothing ->
                    acc
    in
    case Dict.get (prefix ++ "n") dict |> Maybe.andThen String.toInt of
        Just count ->
            List.range 0 (count - 1)
                |> List.filterMap (\i -> Dict.get (prefix ++ String.fromInt i) dict)
                |> String.concat
                |> Just

        Nothing ->
            case Dict.get (prefix ++ "0") dict of
                Just _ ->
                    Just (gaplessFrom 0 "")

                Nothing ->
                    Nothing



-- SCAN HISTORY (the append-only results/index.json the bridge archives, Phase B)


{-| One archived-scan row from `<prefix>results/index.json` (the append-only history index the
bridge writes in `store=swift` mode). Only the fields the reader renders are decoded; unknown
fields are tolerated (the bridge may add more over time).
-}
type alias IndexEntry =
    { batchId : String
    , targetId : String
    , targetName : String
    , completedAt : String
    , status : String
    , counts : Counts
    }


{-| Per-severity finding counts carried on an index row.
-}
type alias Counts =
    { critical : Int
    , high : Int
    , medium : Int
    , low : Int
    , info : Int
    }


{-| The history index object name for a per-instance prefix (§3.1 `exoext.v1.prefix`, which
already carries its trailing slash). Built the same way the manifest is fetched (`prefix ++
manifest`, no separator inserted): `prefix ++ "results/index.json"`.
-}
indexObjectName : String -> String
indexObjectName prefix =
    prefix ++ "results/index.json"


{-| The history index hard cap: **64 KiB**. Enforced fail-closed alongside the §5.5 caps: an
oversize index reads as no history rather than an error, so a runaway index can never wedge
the card.
-}
indexCapBytes : Int
indexCapBytes =
    64 * 1024


{-| Decode the history index body into rows, fail-closed. Malformed JSON, a non-array top
level, or a body over `indexCapBytes` all resolve to `[]` (no history) — never an error state.
Individual rows tolerate missing/unknown fields (string fields default to `""`, counts to 0).
-}
decodeIndex : String -> List IndexEntry
decodeIndex body =
    if String.length body > indexCapBytes then
        []

    else
        Decode.decodeString (Decode.list indexEntryDecoder) body
            |> Result.withDefault []


indexEntryDecoder : Decode.Decoder IndexEntry
indexEntryDecoder =
    Decode.map6 IndexEntry
        (optionalString "batchId")
        (optionalString "targetId")
        (optionalString "targetName")
        (optionalString "completedAt")
        (optionalString "status")
        (Decode.oneOf [ Decode.field "counts" countsDecoder, Decode.succeed emptyCounts ])


countsDecoder : Decode.Decoder Counts
countsDecoder =
    Decode.map5 Counts
        (optionalInt "critical")
        (optionalInt "high")
        (optionalInt "medium")
        (optionalInt "low")
        (optionalInt "info")


emptyCounts : Counts
emptyCounts =
    { critical = 0, high = 0, medium = 0, low = 0, info = 0 }


optionalString : String -> Decode.Decoder String
optionalString key =
    Decode.oneOf [ Decode.field key Decode.string, Decode.succeed "" ]


optionalInt : String -> Decode.Decoder Int
optionalInt key =
    Decode.oneOf [ Decode.field key Decode.int, Decode.succeed 0 ]


{-| The cache key that decides when to refetch the history index. The CloudShield `etag`
(`exoext.v1.etag`) is a content hash of the **static manifest UI body**, so it does NOT change
when a scan completes — keying a refetch on it alone would leave history stale until reload.
Instead compose it with the run slot (`run.seq` + `run.state`), which advances on every scan
state transition (queued → running → done) and on a getEmbed claim. Missing keys render as `""`.
So each transition triggers one cheap ≤`indexCapBytes` refetch; a steady state refetches nothing.
-}
historyRefreshKey : List OSTypes.MetadataItem -> String
historyRefreshKey metadata =
    let
        dict =
            toDict metadata

        get key =
            Dict.get key dict |> Maybe.withDefault ""
    in
    String.join ":"
        [ get "exoext.v1.etag"
        , get "exoext.v1.run.seq"
        , get "exoext.v1.run.state"
        ]


{-| A human-readable severity summary from a row's counts, omitting zero severities in
descending order, e.g. `"7 high · 14 medium · 3 low"`. All-zero ⇒ `"no findings"`.
-}
countsLabel : Counts -> String
countsLabel counts =
    let
        parts =
            [ ( "critical", counts.critical )
            , ( "high", counts.high )
            , ( "medium", counts.medium )
            , ( "low", counts.low )
            , ( "info", counts.info )
            ]
                |> List.filter (\( _, n ) -> n > 0)
                |> List.map (\( label, n ) -> String.fromInt n ++ " " ++ label)
    in
    if List.isEmpty parts then
        "no findings"

    else
        String.join " · " parts



-- EMBED (getEmbed request + embed result, Phase B)


{-| A `getEmbed` request: ask the bridge to mint a fresh, short-lived embed token/URL for an
already-archived scan. Written through the same §7.1 req slot as a scan request (via
`reqSlotMetadata`); the bridge claims the slot and writes a small embed result inline into the
res slot without touching `run.state`.
-}
type alias EmbedRequest =
    { requestId : String
    , batchId : String
    , createdAt : String
    }


{-| Encode a `getEmbed` request to a compact JSON string (the body that gets chunked into the
req slot). Shape: `{schemaVersion, requestId, action:"getEmbed", batchId, createdAt}`.
-}
embedRequestJson : EmbedRequest -> String
embedRequestJson req =
    Encode.encode 0 <|
        Encode.object
            [ ( "schemaVersion", Encode.string "1.0" )
            , ( "requestId", Encode.string req.requestId )
            , ( "action", Encode.string "getEmbed" )
            , ( "batchId", Encode.string req.batchId )
            , ( "createdAt", Encode.string req.createdAt )
            ]


{-| The bridge's embed result, written inline into the res slot in response to a `getEmbed`.
Distinguished from a scan result by `"kind":"embed"` (scan-result bodies carry no `kind`).
-}
type alias EmbedResult =
    { requestId : String
    , batchId : String
    , status : String
    , embedUrl : String
    , embedExpiresAt : String
    , error : Maybe String
    }


{-| Recognize a res-slot body as an embed result: `Just` only when the body decodes with
`"kind":"embed"`. A body without a `kind` field (a scan result) is `Nothing`, so scan-result
handling is untouched by this path.
-}
embedResultFromBody : String -> Maybe EmbedResult
embedResultFromBody body =
    case Decode.decodeString (Decode.field "kind" Decode.string) body of
        Ok "embed" ->
            Decode.decodeString embedResultDecoder body
                |> Result.toMaybe

        _ ->
            Nothing


embedResultDecoder : Decode.Decoder EmbedResult
embedResultDecoder =
    Decode.map6 EmbedResult
        (optionalString "requestId")
        (optionalString "batchId")
        (optionalString "status")
        (optionalString "embedUrl")
        (optionalString "embedExpiresAt")
        (Decode.oneOf
            [ Decode.field "error" (Decode.nullable Decode.value)
                |> Decode.map (Maybe.map (Encode.encode 0))
            , Decode.succeed Nothing
            ]
        )


{-| Whether issuing a `getEmbed` right now would cancel a genuinely in-flight **scan** and must be
blocked (§7.1, single req slot). Its only job is to protect a scan — NOT to block one View from
superseding another. Blocked when:

  - the run is active — `run.state ∈ {queued, running}`; or
  - the reader has a scan it just wrote (`pendingScanSeq` = `Just` its `pending.seq`) whose req is
    still **unclaimed** on the wire: `req.seq` equals that scan's seq and `req.claimed` does not
    equal it (the ≤10 s window before the bridge claims a request).

Writing the getEmbed req slot bumps `req.seq`, and the bridge's `is_cancelled` (compares `req.seq`
vs `run.seq`) would cancel a pending/running scan — hence the guard. It deliberately keys the
unclaimed branch on the scan's own seq: a req slot left unclaimed by a prior _getEmbed_ (e.g. after
a timeout) does NOT match `pendingScanSeq`, so it never blocks — a timed-out getEmbed cannot wedge
future clicks, and a new View always supersedes an in-flight one. A missing `req.claimed` counts as
unclaimed. Terminal runs (`done`/`error`/`expired`) with a claimed request do not block.

-}
getEmbedBlocked : Maybe Int -> List OSTypes.MetadataItem -> Bool
getEmbedBlocked pendingScanSeq metadata =
    let
        dict =
            toDict metadata

        runActive =
            case Dict.get "exoext.v1.run.state" dict of
                Just state ->
                    state == "queued" || state == "running"

                Nothing ->
                    False

        scanUnclaimed =
            case ( pendingScanSeq, Dict.get "exoext.v1.req.seq" dict ) of
                ( Just seq, Just wireSeq ) ->
                    -- The unclaimed req is this scan's (not a later getEmbed's) and not yet claimed.
                    (wireSeq == String.fromInt seq)
                        && (Dict.get "exoext.v1.req.claimed" dict /= Just wireSeq)

                _ ->
                    False
    in
    runActive || scanUnclaimed



-- HELPERS


{-| Split a string into chunks of at most `size` characters, in order. An empty string
yields a single empty chunk so the body is always representable as `body.0`.
-}
chunkString : Int -> String -> List String
chunkString size str =
    if size <= 0 then
        [ str ]

    else if String.length str <= size then
        [ str ]

    else
        String.left size str :: chunkString size (String.dropLeft size str)


toDict : List OSTypes.MetadataItem -> Dict String String
toDict metadata =
    metadata |> List.map (\item -> ( item.key, item.value )) |> Dict.fromList
