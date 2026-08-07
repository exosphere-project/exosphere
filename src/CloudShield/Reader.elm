module CloudShield.Reader exposing
    ( Projection
    , ReadContext
    , answeredRequestId
    , completionTimer
    , projection
    , resultId
    , resultObjectName
    , rowStatus
    , truncationWarning
    )

{-| The CloudShield extension's read side: what the wire says, turned into what its card shows.

The split against the host mirrors the one [`CloudShield.Wire`](CloudShield-Wire) draws on the
write side. The host owns the **framing** — polling the instance, pulling the §7.1 res slot out of
metadata, fetching a capped object, holding the etag and the client clock — and none of that knows
what a result is ABOUT. This module owns the **reading**: that an ok embed result is a history pick
which outranks the live run's own result, that a settled run auto-opens its findings, which
archived object a session names (§4.2 `results/<resultId>.json`), how long a scan has been going,
and which of those states earns which line of chrome. Those are CloudShield's judgements, and a
second extension would replace this module wholesale while reusing every line of host plumbing.

The boundary is [`ReadContext`](#ReadContext): the host stamps the wire reads and its own session
bookkeeping into one record and asks here for a [`Projection`](#Projection) it can hand to the card.
It never decides what wins, what is stale, or what to call anything.

-}

import CloudShield.Card as Card
import CloudShield.Wire as Wire
import Exoext.Lifecycle
import Exoext.Transport
import Helpers.Time
import ISO8601
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe.Extra
import Time



-- THE HOST↔ADAPTER READ BOUNDARY


{-| Everything the host has read or recorded that bears on what the results pane shows.

  - `resSlotBody` — the raw §7.1 res-slot body, exactly as it came off metadata.
  - `resultBody` — the same body once the host has confirmed it belongs to the current manifest
    etag (an in-flight instance switch can leave the two disagreeing).
  - `manifestEtag` — the §3.1 etag on this instance right now, one half of the cached-body gate.
  - `archivedObjectName` — the object the CURRENT result names ([`resultObjectName`](#resultObjectName)),
    the other half of that gate.
  - `fetchedArchive` — the capped object the host last fetched, if it has one.
  - `currentTime` — the shared client clock (expiry and elapsed are both read from it).
  - `runStatus` — the §7.1 run slot correlated to the tracked request, or `Nothing`.
  - `pendingSession` — the in-flight `getEmbed`, as the generic `Exoext.Lifecycle.PendingRequest`.
  - `sessionRequest` — the host's own record of which result the last `getEmbed` asked for.
  - `sessionDismissed` — whether the researcher closed the pane.

-}
type alias ReadContext =
    { resSlotBody : Maybe String
    , resultBody : Maybe String
    , manifestEtag : String
    , archivedObjectName : Maybe String
    , fetchedArchive :
        Maybe
            { etag : String
            , objectName : String
            , body : String
            }
    , currentTime : Time.Posix
    , runStatus :
        Maybe
            { targetId : String
            , state : String
            }
    , pendingSession : Maybe Exoext.Lifecycle.PendingRequest
    , sessionRequest :
        Maybe
            { requestId : String
            , resultId : String
            }
    , sessionDismissed : Bool
    }


{-| What the results pane is showing: the findings and iframe url bound for the renderer, the
host-side `embedState` line, and the per-row **result** ids (never batch ids — §2.2 siblings share a
batch, so a batch id flags every sibling row at once).
-}
type alias Projection =
    { results : Maybe Encode.Value
    , embedUrl : String
    , embedState : Card.EmbedState
    , activeResultId : Maybe String
    , pendingResultId : Maybe String
    , erroredResultId : Maybe String
    , expiredResultId : Maybe String
    , sessionOpen : Bool
    }


{-| How long a written getEmbed waits for its result before the pending marker is treated as a
timeout error. getEmbed normally resolves in ~10s (one poll after the bridge claims the slot); 30s
leaves headroom for a slow mint + CloudShield without leaving a dead "Opening…" row up so long it
reads as broken.
-}
sessionTimeoutMillis : Int
sessionTimeoutMillis =
    30 * 1000


{-| The reader projection for the history-View embed flow, decided purely from the
[`ReadContext`](#ReadContext) the host stamps.

An ok embed result in the res slot is a history pick and wins over the live scan result for both
the findings table and the iframe; otherwise the scan-result path is used. `embedUrl` is gated on
`embedState`: only `EmbedReady` carries the live URL, so an expired (`embedExpiresAt <= currentTime`),
errored, or in-flight embed emits `""` and the origin-pinned Iframe self-hides. That unmount is the
resource-drain fix (an expired CloudShield+Clerk app kept mounted spins forever on auth retries).

-}
projection : ReadContext -> Projection
projection context =
    let
        rawEmbedResult =
            context.resSlotBody
                |> Maybe.andThen Wire.embedResultFromBody

        -- A history pick to show: an ok embed result in the res slot (fresh OR expired). A non-ok
        -- embed result never binds findings/iframe (it falls to the scan path).
        maybeEmbedResult =
            rawEmbedResult
                |> Maybe.andThen
                    (\embed ->
                        if embed.status == "ok" then
                            Just embed

                        else
                            Nothing
                    )

        -- Normalize the wire embed result into the generic `Exoext.Lifecycle.ResultInput` (unwrap
        -- the error message; parse the ISO expiry once — unparseable/absent = no expiry = Fresh).
        -- The generic `sessionState` machine then classifies opening / open / stale / failed / idle.
        resultInput =
            rawEmbedResult
                |> Maybe.map
                    (\embed ->
                        { requestId = embed.requestId

                        -- The session's subject is the §4.2 RESULT it views, resolved from the
                        -- host's own request record (see `resultId`) — the response's
                        -- `batchId` cannot tell two siblings apart.
                        , subject = resultId context.sessionRequest embed
                        , status = embed.status
                        , url = embed.embedUrl
                        , expiresAt =
                            ISO8601.fromString embed.embedExpiresAt
                                |> Result.toMaybe
                                |> Maybe.map ISO8601.toPosix
                        , message = errorMessage embed
                        }
                    )

        session =
            Exoext.Lifecycle.sessionState sessionTimeoutMillis context.currentTime context.pendingSession resultInput

        -- The host-side embed line + iframe gate, mapped 1:1 from the generic session token.
        embedState =
            case session of
                Exoext.Lifecycle.NoSession ->
                    Card.EmbedIdle

                Exoext.Lifecycle.Opening _ ->
                    Card.EmbedLoading

                Exoext.Lifecycle.Open _ ->
                    Card.EmbedReady

                Exoext.Lifecycle.OpenStale _ ->
                    Card.EmbedExpired

                Exoext.Lifecycle.Failed { message } ->
                    Card.EmbedError message

        -- The expiry fix: an ok-but-expired session is `OpenStale`, so the previously-viewed row
        -- becomes `expiredResultId` (a muted "Expired" / plain-View row) INSTEAD of
        -- `activeResultId` ("Now viewing" / Refresh). A fresh ok session stays active as before.
        expiredResultId =
            case session of
                Exoext.Lifecycle.OpenStale expiredSession ->
                    Just expiredSession.resultId

                _ ->
                    Nothing

        -- A getEmbed the user just pressed is in flight, so whatever it opens supersedes whatever is
        -- on screen now. The manual-session path gets this for free — its `results`/`embedUrl` come
        -- from `session`, which is already `Opening` — but the auto-opened live-scan view below is
        -- read straight off the run's result body and has to be told.
        embedRequestInFlight =
            context.pendingSession /= Nothing

        ( results, embedUrl ) =
            case maybeEmbedResult of
                Just _ ->
                    -- Findings for the selected history scan, parsed from the archived body the host
                    -- fetched. TWO gates, both required. The etag rejects a body left over from a
                    -- different instance/manifest (it gives no scan-freshness — it is a constant
                    -- manifest hash). The object name is what pins the body to THIS scan: the host
                    -- holds whatever object was fetched last, and a fetch in flight deliberately
                    -- keeps the previous body, so without this check pressing View on one sibling
                    -- while another's body was cached bound the OLD scan's findings under the new
                    -- scan's "Now viewing" row and iframe — three UI elements showing two different
                    -- scans. A mismatch reads as not-yet-loaded (`Nothing`), never as another
                    -- object's data.
                    let
                        archivedFindings =
                            case context.fetchedArchive of
                                Just fetched ->
                                    if fetched.etag == context.manifestEtag && Just fetched.objectName == context.archivedObjectName then
                                        Decode.decodeString (Decode.field "findings" Decode.value) fetched.body
                                            |> Result.toMaybe

                                    else
                                        Nothing

                                Nothing ->
                                    Nothing
                    in
                    ( archivedFindings
                    , case session of
                        Exoext.Lifecycle.Open openSession ->
                            -- Only a fresh, open session mounts the iframe (its live URL from the
                            -- generic session). Expired/loading keep the findings but unmount it.
                            openSession.url

                        _ ->
                            ""
                    )

                Nothing ->
                    -- The scan-result path: when the in-flight run is `done`, bind the §4.2 result
                    -- body's `findings[]` into `/results` and its `embedUrl` into `/embedUrl`. Gated
                    -- on the correlated `done` state so a stale prior-run result can't show — and on
                    -- no getEmbed being in flight, so pressing View on another row closes this
                    -- auto-opened view AT THE PRESS rather than ~10s later when the bridge answers.
                    -- Left mounted it read as "View won't close", and it put a live `embedUrl` on
                    -- screen outside `EmbedReady`, against the invariant the rest of this function
                    -- keeps. Yielding nothing here hands the pane to the same Opening presentation
                    -- the manual path shows.
                    if embedRequestInFlight then
                        ( Nothing, "" )

                    else
                        let
                            atDone decoder =
                                case context.runStatus of
                                    Just override ->
                                        if override.state == "done" then
                                            context.resultBody |> Maybe.andThen decoder

                                        else
                                            Nothing

                                    Nothing ->
                                        Nothing
                        in
                        ( atDone (Decode.decodeString (Decode.field "findings" Decode.value) >> Result.toMaybe)
                        , atDone (Decode.decodeString (Decode.field "embedUrl" Decode.string) >> Result.toMaybe)
                            |> Maybe.withDefault ""
                        )

        -- The resultId whose findings/embed are on screen, so the card can flag exactly one
        -- history row as "Now viewing": the picked history row wins (only while its session is not
        -- expired — an expired session becomes `expiredResultId` above), else the just-completed
        -- live scan (read from the result body only when the correlated run is `done`).
        activeResultId =
            case maybeEmbedResult of
                Just embed ->
                    if expiredResultId == Nothing then
                        Just (resultId context.sessionRequest embed)

                    else
                        Nothing

                Nothing ->
                    case context.runStatus of
                        Just override ->
                            if override.state == "done" && not embedRequestInFlight then
                                -- The just-completed live scan, keyed exactly as its fresh history
                                -- row keys itself (`requestId`, falling back to `batchId` for a
                                -- publisher that archives no per-run id). Get this wrong and the new
                                -- row sits on "View" instead of "Now viewing". Dropped while a
                                -- getEmbed is in flight for the same reason the bindings above are:
                                -- one source of truth here, rather than leaning on `historyRow` to
                                -- suppress the chip downstream via `pendingResultId`.
                                context.resultBody
                                    |> Maybe.andThen
                                        (\body ->
                                            let
                                                stringField key =
                                                    Decode.decodeString (Decode.field key Decode.string) body |> Result.toMaybe
                                            in
                                            Maybe.Extra.or (stringField "requestId") (stringField "batchId")
                                        )

                            else
                                Nothing

                        Nothing ->
                            Nothing

        -- The in-flight getEmbed's result (`Opening`) for the per-row "Opening…" loading state, and
        -- the last failed getEmbed's result (`Failed`) for the per-row "Couldn't open" + Retry
        -- state. Both come straight from the generic session token, so each is `Just` only in its
        -- state and clears the moment the request resolves — no row is ever wedged loading/errored.
        pendingResultId =
            case session of
                Exoext.Lifecycle.Opening { subject } ->
                    Just subject

                _ ->
                    Nothing

        erroredResultId =
            case session of
                Exoext.Lifecycle.Failed { subject } ->
                    Just subject

                _ ->
                    Nothing

        -- Whether the results pane has anything on it. Not `embedState == EmbedReady`: a history pick
        -- whose session has expired still shows its findings with the iframe unmounted, and that is
        -- still a session on screen and still closeable.
        paneShowsSession =
            (results /= Nothing) || embedUrl /= ""

        showing =
            { results = results
            , embedUrl = embedUrl
            , embedState = embedState
            , activeResultId = activeResultId
            , pendingResultId = pendingResultId
            , erroredResultId = erroredResultId
            , expiredResultId = expiredResultId
            , sessionOpen = paneShowsSession
            }
    in
    if paneShowsSession && context.sessionDismissed then
        -- The researcher closed this session, so blank the PANE fields and nothing else. The iframe
        -- goes by way of an empty `embedUrl`, which unmounts it rather than hiding it — a live embed
        -- left mounted keeps retrying auth. `activeResultId` goes because it is the "now viewing"
        -- flag and nothing is being viewed; `pendingResultId` / `erroredResultId` /
        -- `expiredResultId` stay, because they are per-row history and closing a pane never claims a
        -- scan did not happen.
        { showing
            | results = Nothing
            , embedUrl = ""
            , embedState = Card.EmbedIdle
            , activeResultId = Nothing
            , sessionOpen = False
        }

    else
        showing


{-| The user-facing text of an embed result's `error`. `EmbedResult.error` holds the error field
re-encoded as a JSON string (the bridge may send a string or an object), so unwrap a plain-string
error back to its text for a clean line, falling back to the raw JSON for a structured error.
-}
errorMessage : Wire.EmbedResult -> String
errorMessage embed =
    case embed.error of
        Just encoded ->
            Decode.decodeString Decode.string encoded |> Result.withDefault encoded

        Nothing ->
            "the request failed"



-- RESULT IDENTITY (§4.2)


{-| The archived result an embed result is about, resolved in three steps:

1.  the response's own `resultId` — self-describing, and the only source that survives a page
    reload, so it wins whenever the publisher echoes it;
2.  the host's record of what it asked for (`sessionRequest`), when this response answers that
    request — this covers a publisher that predates the echoed `resultId`;
3.  the response's `batchId` — last resort. §2.2 siblings SHARE it, so it names a whole batch
    rather than one run; it is right only for a legacy, batch-keyed archive.

§4.2 keys result objects by requestId, so this same value also names the object to fetch.

Steps 1 and 2 are what keep the "Now viewing" flag on exactly one row: with only step 3, an open
session on either sibling of a batch flagged both.

-}
resultId : Maybe { requestId : String, resultId : String } -> Wire.EmbedResult -> String
resultId sessionRequest embed =
    let
        recordedResultId =
            sessionRequest
                |> Maybe.andThen
                    (\record ->
                        if record.requestId == embed.requestId then
                            Just record.resultId

                        else
                            Nothing
                    )
    in
    Maybe.Extra.or embed.resultId recordedResultId
        |> Maybe.withDefault embed.batchId


{-| The object to fetch for a res-slot body, if any: a `{"ref": ...}` scan-result pointer names its
object directly; an embed result (`kind == "embed"`, `status == "ok"`) names the archived scan body
`<prefix>results/<resultId>.json` (§4.2) — the same capped ref-fetch path serves both. An inline
scan result or a non-ok embed result names nothing.

The §4.2 layout is this extension's, which is why the name is composed here and not by the host
that fetches it. `prefix` is §3.1 `exoext.v1.prefix`, which already carries its trailing slash.

-}
resultObjectName : Maybe { requestId : String, resultId : String } -> String -> String -> Maybe String
resultObjectName sessionRequest prefix body =
    case Wire.embedResultFromBody body of
        Just embed ->
            if embed.status == "ok" then
                Just (prefix ++ "results/" ++ resultId sessionRequest embed ++ ".json")

            else
                Nothing

        Nothing ->
            Exoext.Transport.resultRefObjectName (Exoext.Transport.resolveResultBody body)


{-| The §4.1 `requestId` a res-slot body answers, if it answers one at all.

Only an embed result is a reply TO a request; a scan result is a run's output rather than a
response, so it answers nothing and yields `Nothing`. The host holds the in-flight marker and does
the comparison — recognizing a body as a reply, and finding the id inside it, is this extension's
to do, since a second extension's responses would be shaped differently.

-}
answeredRequestId : String -> Maybe String
answeredRequestId body =
    Wire.embedResultFromBody body
        |> Maybe.map .requestId


{-| The truncation notice for a result body the publisher had to cut down, or `Nothing` when it did
not.

`truncated` / `truncatedReason` are fields of THIS extension's result body, not of the exoext
envelope (§5.5 caps the envelope; what a publisher does when its own payload will not fit is its
own business), so both the decode and the sentence belong here. The host only finds a slot to put
the line in.

-}
truncationWarning : String -> Maybe String
truncationWarning body =
    case Decode.decodeString (Decode.field "truncated" Decode.bool) body of
        Ok True ->
            let
                suffix =
                    Decode.decodeString (Decode.field "truncatedReason" Decode.string) body
                        |> Result.toMaybe
                        |> Maybe.map (\reason -> ": " ++ reason)
                        |> Maybe.withDefault ""
            in
            Just
                {- @nonlocalized -} ("Full results are too large for this cloud's metadata transport" ++ suffix)

        _ ->
            Nothing



-- ELAPSED TIME


{-| The scan-completion timer descriptor, derived purely from the tracked run's wall-clock start
(`Just` the request seq, else `Nothing` when no scan is tracked), the correlated live `state`, and
the fetched result body. Independent of any history view — a scan that is queued/running/done keeps
its descriptor while a past scan is viewed on the right (its running progress renders on the row).

Shapes:

  - no tracked run (`Nothing` start) ⇒ `Nothing` (no descriptor).
  - `done` ⇒ freeze to the full wall-clock flow, `completedAt - startMillis` (minutes; snapshot →
    boot clone → scan). It never falls back to `summary.durationSec` (the scanner-only ~seconds,
    which reads as "faster than reality"); if `completedAt` is missing/unparseable the frozen
    duration is `Nothing` (no line) rather than a misleadingly small number.
  - a terminal non-`done` state (`error`/`cancelled`/`expired`) ⇒ `Nothing` (drop the line).
  - otherwise (queued/running) ⇒ a live descriptor with `doneDurationSec = Nothing`; the running
    elapsed itself is drawn on the scanning row, not here.

-}
completionTimer : Maybe Int -> String -> Maybe String -> Maybe { startMillis : Int, doneDurationSec : Maybe Int }
completionTimer maybeStartMillis state resultBody =
    maybeStartMillis
        |> Maybe.andThen
            (\startMillis ->
                case state of
                    "done" ->
                        Just
                            { startMillis = startMillis
                            , doneDurationSec =
                                resultBody
                                    |> Maybe.andThen
                                        (\body ->
                                            Decode.decodeString (Decode.field "completedAt" Decode.string) body
                                                |> Result.toMaybe
                                        )
                                    |> Maybe.andThen (Helpers.Time.iso8601StringToPosix >> Result.toMaybe)
                                    |> Maybe.map (\completedAt -> max 0 ((Time.posixToMillis completedAt - startMillis) // 1000))
                            }

                    "error" ->
                        Nothing

                    "cancelled" ->
                        Nothing

                    "expired" ->
                        Nothing

                    _ ->
                        Just { startMillis = startMillis, doneDurationSec = Nothing }
            )


{-| The row display state, composed from the raw §7.1 status slot. Raw `runStatus` drives the reader
logic above (embed projection, completion timer); here, while the run is `running`, the counting-up
elapsed is composed into its state (`"scanning · m:ss"`) so the scanning row is the live progress
signal. `queued`/`done`/terminal states pass through raw.

`startMillis` is the tracked request's seq (wall-clock millis) and `nowMillis` the shared client
clock; with either absent there is nothing to count from and the raw state passes through.

-}
rowStatus : Maybe { targetId : String, state : String } -> Maybe Int -> Int -> Maybe { targetId : String, state : String }
rowStatus runStatus maybeStartMillis nowMillis =
    case ( runStatus, maybeStartMillis ) of
        ( Just override, Just startMillis ) ->
            if override.state == "running" then
                Just
                    { targetId = override.targetId
                    , state = Card.scanningRowLabel startMillis nowMillis
                    }

            else
                Just override

        ( other, _ ) ->
            other
