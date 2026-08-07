module Exoext.Transport exposing
    ( ResolvedResult(..)
    , RunStatus
    , capBody
    , chunkString
    , historyRefreshKey
    , identifierField
    , manifestCapBytes
    , readChunkedBody
    , reqCancelFromMetadata
    , reqCancelKey
    , reqCancelMetadata
    , reqSlotMetadata
    , resolveResultBody
    , resultBody
    , resultBodyFromMetadata
    , resultCapBytes
    , resultRefObjectName
    , runStatusFromMetadata
    )

{-| Exoext POC wire transport over Nova server metadata (Phase 0 spec §4.1 / §7.1).

This is the **throwaway framing layer** for the no-Jetstream2 POC: it carries the §4.1 request
and the §4.3 status / §4.2 result over server metadata instead of object storage. Per the spec,
the request/result _JSON_ and the run _states_ are identical to the Jetstream2 path; only the
`seq`/`claimed`/chunk framing here is POC-specific and is dropped when `store=swift` lands
(Phase 1b).

**This module is the envelope, never the contents.** Nothing here knows what a request is FOR:
bodies go in and come out as opaque strings, and the extension-specific payloads that fill them
live with the adapter that speaks them (`CloudShield.Wire` today, cited only as the example
consumer, the same convention `Exoext.Lifecycle` follows). That is what lets a second extension
reuse this file unchanged.

All functions are pure and string-in/string-out so they unit-test without a live cloud.

Wire layout (on the publishing extension VM's own metadata):

  - **Request slot (Exosphere → VM):** `exoext.v1.req.seq` = monotonic integer; the §4.1
    request JSON chunked across `exoext.v1.req.body.0..N` (≤255 chars/value, §3.1 / D5).
  - **Status / result slot (VM → Exosphere):** `exoext.v1.run.seq` echoes the request seq;
    `exoext.v1.run.state` ∈ queued|running|done|error|cancelled|expired (§4.4); an optional
    small result summary is chunked across `exoext.v1.res.body.0..N` (§4.2 shape). The run slot
    also carries the optional §4.3 descriptors that say WHICH run it is —
    `exoext.v1.run.target` / `.requestId` / `.batchId` / `.phase` / `.pct` — see
    [`RunStatus`](#RunStatus).
  - **Cancel channel (Exosphere → VM):** `exoext.v1.req.cancel` names the requestId the user
    asked to stop — see [`reqCancelMetadata`](#reqCancelMetadata). It is also read back
    ([`reqCancelFromMetadata`](#reqCancelFromMetadata)): the host wrote it, so the wire — not the
    browser session — is where "a stop is pending for this run" durably lives.

-}

import Dict exposing (Dict)
import Json.Decode as Decode
import OpenStack.Types as OSTypes



-- REQUEST (Exosphere -> VM)


{-| Build the §7.1 request-slot metadata key/value items for a request: the monotonic `seq`,
a **chunk count** `exoext.v1.req.body.n`, and the request JSON chunked across
`exoext.v1.req.body.0..N-1` (≤255 chars/value). Each item is written with one
`requestSetServerMetadata` call on the publishing extension VM.

The explicit count is what makes a re-write safe: Nova metadata POST **merges** keys and never
deletes, so a later, shorter request would otherwise leave a stale trailing `body.N` chunk that
a gapless reader would concatenate into corrupt JSON. The reader (`readChunkedBody`) honors the
count and reads exactly `n` chunks, ignoring any orphan.

Writing the slot also **clears the cancel channel** ([`reqCancelKey`](#reqCancelKey)). A cancel
names one requestId, so a cancel left on the wire from a previous run would otherwise be sitting
there when the publisher claims the NEXT request — and while today's ids differ per write, a host
that stops a run and immediately starts another must not be one id collision away from killing the
new one. Clearing belongs here rather than at each call site so no future request writer can forget
it. Nova metadata POST cannot delete a key, so "cleared" is the empty string, which can never equal
a real requestId.

-}
reqSlotMetadata : Int -> String -> List OSTypes.MetadataItem
reqSlotMetadata seq requestJson =
    let
        chunks =
            chunkString 255 requestJson
    in
    { key = "exoext.v1.req.seq", value = String.fromInt seq }
        :: { key = "exoext.v1.req.body.n", value = String.fromInt (List.length chunks) }
        :: { key = reqCancelKey, value = "" }
        :: List.indexedMap
            (\i chunk ->
                { key = "exoext.v1.req.body." ++ String.fromInt i, value = chunk }
            )
            chunks


{-| The cancel channel key. The host writes the `requestId` (§4.1) it wants stopped here; the
publisher treats a request whose id equals this value as cancelled, on top of the §7.1
seq-mismatch supersede rule.

It is a channel of its own rather than a seq bump because bumping `req.seq` is indistinguishable
from writing a NEW request — the publisher cannot tell "stop this" from "do that instead".

-}
reqCancelKey : String
reqCancelKey =
    "exoext.v1.req.cancel"


{-| The metadata item that asks the publisher to stop the run belonging to `requestId`. Written
with one `requestSetServerMetadata` call, the same atomic single-POST idiom as
[`reqSlotMetadata`](#reqSlotMetadata) — and deliberately WITHOUT touching the request slot, so a
stop never reads as a new request.

An empty `requestId` yields the cleared value, which names no request and is a publisher-side
no-op — so a caller with nothing to cancel cannot accidentally arm the channel.

-}
reqCancelMetadata : String -> List OSTypes.MetadataItem
reqCancelMetadata requestId =
    [ { key = reqCancelKey, value = requestId } ]


{-| Read the cancel channel BACK off the wire: the `requestId` (§4.1) a stop was asked for, or
`Nothing` when the channel names no request.

The channel is written by the host and read by the publisher, so reading it back looks redundant —
it is not. It is the only DURABLE record that a stop was asked for: the host's own memory of what it
wrote is session-local and a reload throws it away, after which a run that is already on its way out
reads as untouched and offers its stop again. The wire outlives the session, so the answer taken from
here survives a reload (`ServerDetail.exoextRunControl`).

"Names no request" is deliberately the same three ways [`identifierValue`](#identifierValue) reads
every other id: absent (no stop was ever asked for), `""` (the cleared value
[`reqSlotMetadata`](#reqSlotMetadata) writes when a new request supersedes the channel), and
`"None"`. None of them can equal a real requestId, so none of them can make a live run look stopped.

-}
reqCancelFromMetadata : List OSTypes.MetadataItem -> Maybe String
reqCancelFromMetadata metadata =
    identifierValue reqCancelKey (toDict metadata)



-- STATUS / RESULT (VM -> Exosphere)


{-| The coarse, UI-facing run status read back from the VM's metadata (§4.3 `state`), with
the `seq` it corresponds to (§7.1 correlation).

`seq` + `state` are the two required keys. Everything after them is the §4.3 descriptor set that
says WHICH run the slot is reporting, and every one of them is optional — absent reads as
`Nothing`. That is what makes the descriptors additive in both directions: an old publisher that
writes only `seq`/`state` still decodes, and a new publisher's extra keys are simply ignored by an
older reader.

  - `target` — the instance id the run is about. It is the key that makes run RECOVERY possible:
    without it a reader can only project a run whose request IT wrote this session, so a reload
    mid-run leaves the card looking idle.
  - `requestId` — the §4.1 request id of the run, i.e. the value a cancel has to name
    ([`reqCancelMetadata`](#reqCancelMetadata)). Self-describing, so it survives a reload where the
    host's own record of what it wrote does not.
  - `batchId` — the §2.2 batch the run belongs to, absent for a lone request.
  - `phase` / `pct` — optional coarse progress. Free-form token and 0-100 integer.

-}
type alias RunStatus =
    { seq : Int
    , state : String
    , target : Maybe String
    , requestId : Maybe String
    , batchId : Maybe String
    , phase : Maybe String
    , pct : Maybe Int
    }


{-| Read the §7.1 status slot from metadata: `exoext.v1.run.seq` + `exoext.v1.run.state`, plus the
optional §4.3 descriptors. `Nothing` unless the two required keys are present and `seq` parses; a
missing descriptor is `Nothing` on the record and never fails the read.
-}
runStatusFromMetadata : List OSTypes.MetadataItem -> Maybe RunStatus
runStatusFromMetadata metadata =
    let
        dict =
            toDict metadata
    in
    Maybe.map2
        (\seq state ->
            { seq = seq
            , state = state
            , target = identifierValue "exoext.v1.run.target" dict
            , requestId = identifierValue "exoext.v1.run.requestId" dict
            , batchId = identifierValue "exoext.v1.run.batchId" dict
            , phase = identifierValue "exoext.v1.run.phase" dict
            , pct = percentValue "exoext.v1.run.pct" dict
            }
        )
        (Dict.get "exoext.v1.run.seq" dict |> Maybe.andThen String.toInt)
        (Dict.get "exoext.v1.run.state" dict)


{-| A flat-metadata identifier value, read with the same discipline as
[`identifierField`](#identifierField) reads one out of a JSON body: absent and `""` both mean "the
publisher knows no value here", and neither may ever be adopted as an identity — an id that
identifies nothing collides with every other one.

Metadata values are plain strings, so it also folds `"None"`, which is what Python's `str(None)`
puts on the wire if a publisher stringifies a missing value instead of omitting the key. That can
never be a real UUID or request id, and letting it through would synthesize a run tracker pointing
at a row that does not exist.

-}
identifierValue : String -> Dict String String -> Maybe String
identifierValue key dict =
    Dict.get key dict
        |> Maybe.andThen emptyToNothing
        |> Maybe.andThen
            (\value ->
                if value == "None" then
                    Nothing

                else
                    Just value
            )


{-| A 0-100 percentage from flat metadata. Anything unparseable or out of range is `Nothing`
rather than clamped: a progress number the publisher cannot state correctly is better absent than
silently rewritten into something the UI presents as fact.
-}
percentValue : String -> Dict String String -> Maybe Int
percentValue key dict =
    Dict.get key dict
        |> Maybe.andThen String.toInt
        |> Maybe.andThen
            (\pct ->
                if pct >= 0 && pct <= 100 then
                    Just pct

                else
                    Nothing
            )


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


{-| An identifier field, read the same way wherever one appears on the wire. A publisher that
knows no value may say so three different ways, and all three mean the same thing here:

  - **`null`** — the shape-stable form the bridge writes (like `embedUrl` / `embedExpiresAt`, the
    key is always present so the object's shape never varies);
  - **absent** — an older publisher that predates the field;
  - **`""`** — folded in deliberately: an id that identifies nothing must never be adopted as an
    identity, or two records carrying it collide exactly the way a shared `batchId` does.

Anything else non-string is `Nothing` too, matching the tolerance of the fields around it.

-}
identifierField : String -> Decode.Decoder (Maybe String)
identifierField key =
    Decode.oneOf
        [ Decode.field key (Decode.nullable Decode.string)
        , Decode.succeed Nothing
        ]
        |> Decode.map (Maybe.andThen emptyToNothing)


emptyToNothing : String -> Maybe String
emptyToNothing value =
    if String.isEmpty value then
        Nothing

    else
        Just value


{-| The cache key that decides when a reader should refetch a body derived from PAST requests
(today: the archived-run history index). Generic on purpose: it composes only §3.1/§7.1 keys and
knows nothing about what is being refetched.

The exoext `etag` (`exoext.v1.etag`) is a content hash of the **static manifest UI body**, so it
does NOT change when a request settles — keying a refetch on it alone would leave a derived body
stale until reload. Composing it with the run slot (`run.seq` + `run.state`) fixes that: the slot
advances on every §4.4 state transition and on every request the publisher claims. Missing keys
render as `""`. So each transition triggers one cheap refetch, and a steady state refetches
nothing.

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
