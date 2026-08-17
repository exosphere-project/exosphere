module Exoext.Messages exposing
    ( ActionResult
    , Counts
    , DeleteRequest
    , EmbedRequest
    , EmbedResult
    , IndexEntry
    , ScanRequest
    , actionResultFromBody
    , countsLabel
    , decodeIndex
    , deleteRequestJson
    , embedRequestJson
    , embedResultFromBody
    , encodeRequestBody
    , getEmbedBlocked
    , indexCapBytes
    , indexObjectName
    , kindDeleteResult
    , kindOpenSession
    , kindScan
    , scanRequestJson
    , writeRequestKinds
    )

{-| The reference extension's own wire payloads: the bodies it puts inside the exoext envelope,
and the reads that make sense of what comes back.

The split against [`Exoext.Transport`](Exoext-Transport) is the whole point of this module
existing. Transport owns the **envelope** — the §7.1 request slot, the chunk framing, the run
status, the cancel channel, the §5.5 caps, the result-pointer plumbing — none of which knows what
a request is FOR. This module owns the **contents**: the §4.1 body that says `snapshot-clone`, the
`getEmbed` verb and its response, the severity vocabulary of an archived-scan row, and the object
name a scan history lives at. Those are the extension's, not the host's, and a second extension
would replace this module wholesale while reusing every line of Transport unchanged.

That boundary is what makes the request path extension-agnostic: the host stamps an
[`Exoext.Transport.RequestContext`](Exoext-Transport#RequestContext) (ids, timestamps, the §5.4
re-resolved subject) and asks THIS module to turn it into a body. The host never names a profile,
a method, or a verb.

-}

import Dict
import Exoext.Transport as Transport
import Json.Decode as Decode
import Json.Encode as Encode
import OpenStack.Types as OSTypes



-- REQUESTS (Exosphere -> the publishing VM)


{-| The wire request kind for a scan (§4.1). The token the manifest routes an
`exoext.writeRequest` press with, and the one this adapter answers for.
-}
kindScan : String
kindScan =
    "scan"


{-| The wire request kind for a `getEmbed`: mint a session for one already-archived result. It is
not a [`writeRequestKind`](#writeRequestKinds) — the manifest reaches it through the generic
`exoext.openSession` verb, not through the per-target request path — but it is written into the
same §7.1 slot, so it is encoded here alongside the scan.
-}
kindOpenSession : String
kindOpenSession =
    "getEmbed"


{-| The wire request kind for a `deleteResult`: remove one already-archived result from the
history. Like [`kindOpenSession`](#kindOpenSession) it is not a [`writeRequestKind`](#writeRequestKinds)
— the manifest reaches it through the generic `exoext.deleteResult` verb rather than through the
per-target request path — but it is written into the same §7.1 slot, so it is encoded here beside
the others.
-}
kindDeleteResult : String
kindDeleteResult =
    "deleteResult"


{-| The request kinds a manifest may route through `exoext.writeRequest`, i.e. the per-target
request path with its optimistic row states and its batch pacing.

The gate is an allowlist rather than "anything with a kind" on purpose: that path flips the
targeted rows to `queued` before anything reaches the wire, so a kind
[`encodeRequestBody`](#encodeRequestBody) would decline would leave rows advertising a request that
never gets written. Refusing at dispatch keeps the two answers in one place.

-}
writeRequestKinds : List String
writeRequestKinds =
    [ kindScan ]


{-| Turn a request `kind` plus the host's [`Exoext.Transport.RequestContext`](Exoext-Transport#RequestContext)
into the §7.1 request body, or `Nothing` for a kind this adapter does not speak.

**This is the host↔adapter request boundary.** The host mints the ids and timestamps (only it has
the wall clock and the §7.1 seq), re-resolves the subject against Exosphere's own instance list
(§5.4), and then knows nothing else about what it is about to write: it asks here for a body and
frames whatever comes back. Every word that is the extension's — `scan`, `snapshot-clone`, the
`quick` profile, `getEmbed` — is on this side of the call, and a second extension supplies its own
version of this one function.

`Nothing` is fail-closed and reaches the wire as no write at all: an unknown kind must never be
silently encoded as some other verb.

`profile` is fixed at `"quick"` here. §2.2 admits `"quick" | "full"` as a manifest param, and
plumbing that through is a manifest + params change, not a host change — which is now exactly the
point: it would be a change to this file only.

-}
encodeRequestBody : String -> Transport.RequestContext -> Maybe String
encodeRequestBody kind context =
    if kind == kindScan || String.isEmpty kind then
        -- An empty kind is a request this host is continuing without having been told the verb: a
        -- batch tail restored across a reload, whose §4.3 run slot carries no verb to recover one
        -- from (`Exoext.Lifecycle.recoverRun` mints `""` rather than inventing one). Resolving it
        -- to the scan is this ADAPTER's call to make, and it is sound because a batch is a group of
        -- §4.1 siblings and the scan is the only kind this adapter can make a batch of. An adapter
        -- with two batchable kinds would have to persist the kind alongside the tail instead.
        Just
            (scanRequestJson
                { requestId = context.requestId
                , batchId = context.batchId
                , createdAt = context.createdAt
                , projectId = context.projectId
                , target = { instanceId = context.subject.id, instanceName = context.subject.name }
                , profile = "quick"
                }
            )

    else if kind == kindDeleteResult then
        -- Same shape as a session request and for the same reason: the subject is the §4.2 result
        -- to act on, not an instance, so its re-resolved display name is unused. `batchId` rides
        -- along for a publisher that can only select by batch; it is optional on this verb, so an
        -- absent one is written as the empty string the publisher already reads as "not given".
        Just
            (deleteRequestJson
                { requestId = context.requestId
                , batchId = context.batchId |> Maybe.withDefault ""
                , resultId = context.subject.id
                , createdAt = context.createdAt
                }
            )

    else if kind == kindOpenSession then
        -- The subject of a session request is the §4.2 result to open, not an instance, so its
        -- re-resolved display name is unused. `batchId` is the selector of last resort for a
        -- publisher that predates `resultId`, and it is always present on this path.
        Just
            (embedRequestJson
                { requestId = context.requestId
                , batchId = context.batchId |> Maybe.withDefault ""
                , resultId = context.subject.id
                , createdAt = context.createdAt
                }
            )

    else
        Nothing


{-| The host-resolved scan request (§4.1). `target` is the re-resolved real instance
(§5.4); `createdAt` is an ISO-8601 string supplied by the host (Phase 0 §4.1).
-}
type alias ScanRequest =
    { requestId : String
    , batchId : Maybe String
    , createdAt : String
    , projectId : String
    , target :
        { instanceId : String
        , instanceName : String
        }
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


{-| A `getEmbed` request: ask the bridge to mint a fresh, short-lived embed token/URL for an
already-archived scan. Written through the same §7.1 req slot as a scan request (via
`Exoext.Transport.reqSlotMetadata`); the bridge claims the slot and writes a small embed result
inline into the res slot without touching `run.state`.
-}
type alias EmbedRequest =
    { requestId : String
    , batchId : String
    , resultId : String
    , createdAt : String
    }


{-| Encode a `getEmbed` request to a compact JSON string (the body that gets chunked into the
req slot). Shape: `{schemaVersion, requestId, action:"getEmbed", batchId, resultId, createdAt}`.

`resultId` names the ONE archived result to open (§4.2 keys results by requestId). `batchId` stays
on the wire for a publisher that predates `resultId` and selects by batch; it is the selector of
last resort, since §2.2 siblings share it.

-}
embedRequestJson : EmbedRequest -> String
embedRequestJson req =
    Encode.encode 0 <|
        Encode.object
            [ ( "schemaVersion", Encode.string "1.0" )
            , ( "requestId", Encode.string req.requestId )
            , ( "action", Encode.string "getEmbed" )
            , ( "batchId", Encode.string req.batchId )
            , ( "resultId", Encode.string req.resultId )
            , ( "createdAt", Encode.string req.createdAt )
            ]


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

It reasons entirely in this extension's terms — one of ITS verbs protecting one of ITS runs — which
is why it lives here and not with the §7.1 framing it reads. The host asks whether the press is
blocked; only the adapter knows why it would be.

-}
getEmbedBlocked : Maybe Int -> List OSTypes.MetadataItem -> Bool
getEmbedBlocked pendingScanSeq metadata =
    let
        dict =
            metadata |> List.map (\item -> ( item.key, item.value )) |> Dict.fromList

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


{-| A `deleteResult` request: ask the bridge to drop one already-archived scan from the mailbox —
its result object and its history row. Written through the same §7.1 req slot as a scan or a
`getEmbed`; the bridge claims the slot and answers with a small non-archived
[`ActionResult`](#ActionResult) without touching `run.state`.

Removal is the publisher's to perform, not the host's: the archive is in the researcher's own object
store but it is the extension that knows its layout, so the host asks and the publisher acts.

-}
type alias DeleteRequest =
    { requestId : String
    , batchId : String
    , resultId : String
    , createdAt : String
    }


{-| Encode a `deleteResult` request to a compact JSON string. Shape:
`{schemaVersion, requestId, action:"deleteResult", batchId, resultId, createdAt}` — deliberately the
`getEmbed` shape with a different `action`, so the publisher parses both through one path.
-}
deleteRequestJson : DeleteRequest -> String
deleteRequestJson req =
    Encode.encode 0 <|
        Encode.object
            [ ( "schemaVersion", Encode.string "1.0" )
            , ( "requestId", Encode.string req.requestId )
            , ( "action", Encode.string kindDeleteResult )
            , ( "batchId", Encode.string req.batchId )
            , ( "resultId", Encode.string req.resultId )
            , ( "createdAt", Encode.string req.createdAt )
            ]



-- EMBED RESULT (the publishing VM -> Exosphere)


{-| The bridge's embed result, written inline into the res slot in response to a `getEmbed`.
Distinguished from a scan result by `"kind":"embed"` (scan-result bodies carry no `kind`).

`resultId` is the §4.2 result the session was minted for, and it is the ONLY self-describing
identity on this response: `batchId` is shared by §2.2 siblings, so it cannot say which archived
run is on screen. The bridge always writes the key and sends JSON `null` when it does not know a
value (so the object's shape never varies); a publisher predating the field omits it entirely.
Either way it reads as `Nothing` and sends the reader to its fallbacks.

-}
type alias EmbedResult =
    { requestId : String
    , batchId : String
    , resultId : Maybe String
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
    Decode.map7 EmbedResult
        (optionalString "requestId")
        (optionalString "batchId")
        (Transport.identifierField "resultId")
        (optionalString "status")
        (optionalString "embedUrl")
        (optionalString "embedExpiresAt")
        (Decode.oneOf
            [ Decode.field "error" (Decode.nullable Decode.value)
                |> Decode.map (Maybe.map (Encode.encode 0))
            , Decode.succeed Nothing
            ]
        )


{-| The bridge's acknowledgement of a non-session action, written inline into the res slot the same
way an embed result is. Distinguished from both a scan result and an embed result by
`"kind":"action"`, and self-describing about WHICH action it answers (`action`) and what that action
was about (`resultId`).

`status` is `"ok"` or `"error"`; `error` carries the publisher's plain-language reason for the
latter. There is one shape for every action verb rather than one type per verb, because the host's
half of the round-trip is identical for all of them: match the requestId, read a status, show a
reason.

-}
type alias ActionResult =
    { requestId : String
    , action : String
    , resultId : Maybe String
    , status : String
    , error : Maybe String
    }


{-| Recognize a res-slot body as an action acknowledgement: `Just` only when the body decodes with
`"kind":"action"`. A scan result (no `kind`) and an embed result (`"kind":"embed"`) are both
`Nothing`, so neither of those paths is disturbed by this one.
-}
actionResultFromBody : String -> Maybe ActionResult
actionResultFromBody body =
    case Decode.decodeString (Decode.field "kind" Decode.string) body of
        Ok "action" ->
            Decode.decodeString actionResultDecoder body
                |> Result.toMaybe

        _ ->
            Nothing


actionResultDecoder : Decode.Decoder ActionResult
actionResultDecoder =
    Decode.map5 ActionResult
        (optionalString "requestId")
        (optionalString "action")
        (Transport.identifierField "resultId")
        (optionalString "status")
        (Decode.oneOf
            [ Decode.field "error" (Decode.nullable Decode.value)
                |> Decode.map (Maybe.map errorText)
            , Decode.succeed Nothing
            ]
        )


{-| The user-facing text of an action acknowledgement's `error`. The publisher may send a plain
string or a `{code, message}` object; a `message` field wins, then a plain string, and anything else
falls back to the raw JSON rather than to silence — an unreadable reason is still evidence.
-}
errorText : Encode.Value -> String
errorText value =
    case Decode.decodeValue (Decode.field "message" Decode.string) value of
        Ok message ->
            message

        Err _ ->
            case Decode.decodeValue Decode.string value of
                Ok text ->
                    text

                Err _ ->
                    Encode.encode 0 value



-- SCAN HISTORY (the append-only results/index.json the bridge archives, Phase B)


{-| One archived-scan row from `<prefix>results/index.json` (the append-only history index the
bridge writes in `store=swift` mode). Only the fields the reader renders are decoded; unknown
fields are tolerated (the bridge may add more over time).

`requestId` is the §4.1 request this row archives, and it is **optional**: rows written before the
per-run result identity landed carry only `batchId`, which §2.2 siblings SHARE — so `batchId` alone
cannot identify one run within a batch. `Nothing` for those legacy rows; the reader falls back to
`batchId` there.

-}
type alias IndexEntry =
    { batchId : String
    , requestId : Maybe String
    , targetId : String
    , targetName : String
    , completedAt : String
    , status : String
    , counts : Counts

    -- The publisher's short plain-language reason a `status == "error"` row failed. Optional in
    -- both directions: a row that succeeded carries none, and a row archived by a publisher older
    -- than the field carries none either, so "why did this fail?" has to degrade to a sentence the
    -- card supplies rather than to a blank.
    , error : Maybe String
    }


{-| Per-severity finding counts carried on an index row. The five names ARE the CVSS severity
vocabulary of a vulnerability scanner, which is why the whole history shape lives with the adapter
that speaks it.
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
the card. Unlike the §5.5 caps in `Exoext.Transport` this one is not in the spec — it bounds an
object this extension invented, so it is this extension's number to pick.
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
    Decode.map8 IndexEntry
        (optionalString "batchId")
        (Transport.identifierField "requestId")
        (optionalString "targetId")
        (optionalString "targetName")
        (optionalString "completedAt")
        (optionalString "status")
        (Decode.oneOf [ Decode.field "counts" countsDecoder, Decode.succeed emptyCounts ])
        (optionalMessage "error")


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


{-| A human-readable severity summary from a row's counts, omitting zero severities in
descending order, e.g. `"7 high · 14 medium · 3 low"`. All-zero ⇒ `"no findings"`.

A display string, and therefore an adapter's to own: "finding" is a scanner's noun and the
descending order IS the severity ranking. The host projects it into the render state and never
composes it.

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



-- HELPERS


optionalString : String -> Decode.Decoder String
optionalString key =
    Decode.oneOf [ Decode.field key Decode.string, Decode.succeed "" ]


{-| An optional free-text field: absent, `null`, a non-string, or `""` all read as "the publisher
said nothing here". Empty is folded because a blank sentence and a missing one are the same thing to
a reader, and the card has one fallback for both.
-}
optionalMessage : String -> Decode.Decoder (Maybe String)
optionalMessage key =
    Decode.oneOf
        [ Decode.field key (Decode.nullable Decode.string)
        , Decode.succeed Nothing
        ]
        |> Decode.map
            (Maybe.andThen
                (\value ->
                    if String.isEmpty value then
                        Nothing

                    else
                        Just value
                )
            )


optionalInt : String -> Decode.Decoder Int
optionalInt key =
    Decode.oneOf [ Decode.field key Decode.int, Decode.succeed 0 ]
