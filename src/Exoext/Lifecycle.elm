module Exoext.Lifecycle exposing
    ( PendingRequest
    , Session
    , Freshness(..)
    , sessionFreshness
    , ResultInput
    , SessionState(..)
    , sessionState
    , terminalRunStates
    , isTerminalRunState
    , correlatedRunState
    , requestStillPending
    , RunRecovery(..)
    , recoverRun
    , Batch
    , BatchStep(..)
    , advanceBatch
    , verbWriteRequest
    , verbCancelRequest
    , verbOpenSession
    , verbDismissSession
    , verbRefresh
    , VerbAlias
    , resolveVerb
    )

{-| The extension-agnostic request / response LIFECYCLE and result SESSION model for the
`exoext.v1` wire contract. Everything here is generic: it knows only about wire requests
(one in flight per §7.1, correlated by seq / requestId), the polled run status slot, and an
open "result session" (a time-boxed, single-resource view minted in response to a request).
It names no extension — CloudShield is only ever cited as the example consumer in doc
comments. `CloudShield.Card` (the adapter) maps its own manifest verbs and wire result shapes
onto these types.

Two things live here that used to be duplicated in `Page.ServerDetail` and `CloudShield.Card`:

1.  **Request tracking + run correlation** — the single in-flight [`PendingRequest`](#PendingRequest),
    the run-status correlation ([`correlatedRunState`](#correlatedRunState)), and the terminal-state
    vocabulary. A `kind == "scan"` request is correlated to the run slot by seq; a
    `kind == "getEmbed"` request is correlated to its result by requestId and times out client-side.

2.  **The result-session state machine** — [`sessionState`](#sessionState) derives one generic
    [`SessionState`](#SessionState) token from (the pending request, the wire result, the current
    time, and the request timeout). The adapter maps that token onto its display strings and its
    per-row/per-region visuals. This is the single source of the "opening / open / stale / failed"
    decision (today: the CloudShield history-View embed flow).


# Requests

@docs PendingRequest


# Result sessions

@docs Session
@docs Freshness
@docs sessionFreshness
@docs ResultInput
@docs SessionState
@docs sessionState


# Run status

@docs terminalRunStates
@docs isTerminalRunState
@docs correlatedRunState
@docs requestStillPending
@docs RunRecovery
@docs recoverRun


# Request batches

@docs Batch
@docs BatchStep
@docs advanceBatch


# Declarative verbs

@docs verbWriteRequest
@docs verbCancelRequest
@docs verbOpenSession
@docs verbDismissSession
@docs verbRefresh
@docs VerbAlias
@docs resolveVerb

-}

import Exoext.Transport
import OpenStack.Types as OSTypes
import Time



-- REQUESTS


{-| A single in-flight wire request (§7.1 allows exactly one at a time per publishing VM). It
generalizes the two trackers this replaces:

  - the scan tracker (was `CloudShield.Card.Model.pending : Maybe { seq, targetId }`):
    `kind == "scan"`, `subject` = the target instance id, correlated to the run slot by `seq`.
  - the session-request tracker (was `ServerDetail.Model.cloudShieldPendingEmbed :
    Maybe { requestId, batchId, since }`): `kind == "getEmbed"`, `subject` = the archived
    result / batch id, correlated to its wire result by `requestId` and timed out client-side
    against `since`.

`seq` is the monotonic §7.1 req-slot sequence; `requestId` is the wire request's id (used for
result correlation on session requests; unused — empty — for scans, which correlate by seq);
`kind` is the wire request kind; `subject` is what the request is about; `since` is the
wall-clock time the request was written (used for the client-side session timeout).

-}
type alias PendingRequest =
    { seq : Int
    , requestId : String
    , kind : String
    , subject : String
    , since : Time.Posix
    }



-- RESULT SESSIONS


{-| An open result session parsed once from a wire result: a single-resource, time-boxed view
(today: a CloudShield embed URL minted for one archived scan). `resultId` is the resource this
session views (the batch id); `url` is where it is served; `expiresAt` is the session's hard
expiry, `Nothing` when the wire result carried no parseable expiry (treated as non-expiring —
see [`sessionFreshness`](#sessionFreshness)).
-}
type alias Session =
    { resultId : String
    , url : String
    , expiresAt : Maybe Time.Posix
    }


{-| Whether an open [`Session`](#Session) is still within its expiry at a given clock time.
An absent or unparseable expiry is treated as `Fresh` (never hide a possibly-valid session on a
missing / malformed timestamp — the current behavior).
-}
type Freshness
    = Fresh
    | Stale


{-| Classify a [`Session`](#Session)'s freshness against the current client clock. `Stale`
exactly at or after `expiresAt`; `Fresh` strictly before it, or when there is no expiry.
-}
sessionFreshness : Time.Posix -> Session -> Freshness
sessionFreshness now session =
    case session.expiresAt of
        Just expiresAt ->
            if Time.posixToMillis expiresAt <= Time.posixToMillis now then
                Stale

            else
                Fresh

        Nothing ->
            Fresh


{-| The normalized wire result an adapter feeds to [`sessionState`](#sessionState). The adapter
parses its extension-specific result body into this shape (today: `Exoext.Transport.EmbedResult`
→ `ResultInput`, with `message` the already-unwrapped error text). `requestId` correlates the
result to the pending request; `subject` is the result's resource / batch id; `status` is the
wire status (`"ok"` / `"error"` / anything else = still settling); `url` and `expiresAt` describe
the session an `"ok"` result opens; `message` is the human-facing error for an `"error"` result.
-}
type alias ResultInput =
    { requestId : String
    , subject : String
    , status : String
    , url : String
    , expiresAt : Maybe Time.Posix
    , message : String
    }


{-| The generic vocabulary the UI maps from — the state of a result-session request/response
round-trip, derived by [`sessionState`](#sessionState):

  - `NoSession` — nothing in flight and no session result to show (idle).
  - `Opening { subject }` — a request is in flight for `subject` (pending marker set, no matching
    result yet, not timed out).
  - `Open session` — an `"ok"`, still-fresh result: the session is live.
  - `OpenStale session` — an `"ok"` result whose session has expired. Distinct from `Open` so the
    UI can drop the "now viewing" affordance (the resource is no longer served) while still
    indicating which resource it was — the fix for an expired session still reading as active.
  - `Failed { subject, message }` — the request's result reported an error, or the request timed
    out client-side.

-}
type SessionState
    = NoSession
    | Opening { subject : String }
    | Open Session
    | OpenStale Session
    | Failed { subject : String, message : String }


{-| Derive the generic [`SessionState`](#SessionState) from the pending request, the wire result
currently sitting in the response slot, the current client clock, and the client-side request
timeout (millis). This centralizes what used to be the `embedState` / `activeBatchId` /
`pendingBatchId` / `erroredBatchId` cluster in `ServerDetail.cloudShieldEmbedProjection`:

  - With a pending request whose `requestId` matches the slot result → resolve that result.
  - With a pending request and no matching result yet → `Opening`, unless `now - since` has passed
    the timeout, in which case `Failed` ("the request timed out"), attributed to the request's
    `subject`.
  - With no pending request → resolve whatever result sits in the slot (a session opened earlier,
    or nothing → `NoSession`).

A resolved `"ok"` result is `Open` while fresh and `OpenStale` once expired (per
[`sessionFreshness`](#sessionFreshness)); an `"error"` result is `Failed`; any other status is
still-settling and reads as `NoSession`.

-}
sessionState : Int -> Time.Posix -> Maybe PendingRequest -> Maybe ResultInput -> SessionState
sessionState timeoutMillis now pending result =
    let
        sessionOf r =
            { resultId = r.subject, url = r.url, expiresAt = r.expiresAt }

        resolved r =
            if r.status == "ok" then
                let
                    session =
                        sessionOf r
                in
                case sessionFreshness now session of
                    Fresh ->
                        Open session

                    Stale ->
                        OpenStale session

            else if r.status == "error" then
                Failed { subject = r.subject, message = r.message }

            else
                NoSession

        loadingOrTimeout p =
            if Time.posixToMillis now - Time.posixToMillis p.since > timeoutMillis then
                Failed { subject = p.subject, message = "the request timed out" }

            else
                Opening { subject = p.subject }
    in
    case pending of
        Just p ->
            case result of
                Just r ->
                    if r.requestId == p.requestId then
                        resolved r

                    else
                        -- The slot still holds an earlier result; our request is in flight.
                        loadingOrTimeout p

                Nothing ->
                    loadingOrTimeout p

        Nothing ->
            case result of
                Just r ->
                    resolved r

                Nothing ->
                    NoSession



-- RUN STATUS


{-| The terminal §7.1 run states: once the correlated run reaches one of these, the request is no
longer worth polling for.
-}
terminalRunStates : List String
terminalRunStates =
    [ "done", "error", "cancelled", "expired" ]


{-| Whether a run state is terminal (a member of [`terminalRunStates`](#terminalRunStates)).
-}
isTerminalRunState : String -> Bool
isTerminalRunState state =
    List.member state terminalRunStates


{-| The live run state correlated to a tracked request `seq`, read from the §7.1 status slot.
Defaults to `"queued"` when the status slot is absent or its `seq` does not match the tracked
request — the request was just written and the publisher may not have claimed / advanced it yet.
-}
correlatedRunState : Int -> List OSTypes.MetadataItem -> String
correlatedRunState seq metadata =
    Exoext.Transport.runStatusFromMetadata metadata
        |> Maybe.andThen
            (\status ->
                if status.seq == seq then
                    Just status.state

                else
                    Nothing
            )
        |> Maybe.withDefault "queued"


{-| Whether a tracked run-correlated request is still pending — present and not yet terminal.
`Nothing` (no tracked request) is not pending. This is the run-slot half of the fast-poll gate
(the session half is a non-`Nothing` pending session request).
-}
requestStillPending : Maybe PendingRequest -> List OSTypes.MetadataItem -> Bool
requestStillPending pending metadata =
    case pending of
        Just p ->
            not (isTerminalRunState (correlatedRunState p.seq metadata))

        Nothing ->
            False



-- RUN RECOVERY


{-| What a reader should adopt from the §4.3 run slot when it is tracking nothing itself — the
answer to "the page was reloaded mid-run, now what?".

  - `NoRecovery` — adopt nothing. Either the reader already tracks a request (its own tracker is
    always the truth, see [`recoverRun`](#recoverRun)) or the wire says nothing adoptable.
  - `RecoverPending request` — adopt `request` as the tracked request. It is a real §4.3 run the
    reader simply did not write, so every projection keyed on a tracked request (the row's live
    state, the elapsed timer, the fast poll, the batch pacing) engages exactly as it would for a
    request this session issued.
  - `RecoverSettled { subject, state }` — the run is over. There is nothing to track and nothing to
    poll for, only a finished per-subject state to commit so the row shows what happened instead of
    reverting to idle.

-}
type RunRecovery
    = NoRecovery
    | RecoverPending PendingRequest
    | RecoverSettled { subject : String, state : String }


{-| Decide what to adopt from the §4.3 run slot. This is the whole fix for "reload during a run and
the card reads idle": a reader can only project a run it can NAME, and before the run slot carried
`run.target` the only name available was the reader's own session-local record of what it wrote.

The precedence is deliberate and total, in this order:

1.  **A tracked request always wins.** `tracked` being `Just` ⇒ `NoRecovery`, unconditionally.
    Recovery only ever fills a gap; it must never overwrite a live tracker. The two disagree
    routinely and harmlessly — a request written moments ago is not yet echoed by the run slot, and
    a batch's continuation deliberately runs ahead of it — and in every one of those cases the
    reader's own record is the newer truth. Overwriting it would rewind the tracker to the previous
    run and strand the batch.
2.  **The wire must name a run.** No status slot, or a status slot with no `target`, ⇒
    `NoRecovery`: a run that cannot be attributed to a row is not adoptable, and guessing a row is
    worse than showing none.
3.  **A live run becomes a tracked request** (`RecoverPending`). `since` is taken from the run's own
    `seq`, which the host writes as wall-clock millis — the moment the request was actually written,
    which is both truthful and stable across reloads (unlike "now", which would restart the
    elapsed clock on every reload).
4.  **A finished run settles** (`RecoverSettled`) — with ONE exception: when `tailPending` says an
    undrained batch tail is waiting on this run, a finished run is adopted as a tracked request
    instead. Draining the tail is driven by [`advanceBatch`](#advanceBatch), which needs a tracked
    request to settle before it can pop the next subject; without this the restored tail of a batch
    whose run had already finished at reload would never resume.

The recovered request's `kind` is `"scan"` because the run slot is the RUN-correlated request channel
and a run request is the only thing that appears in it — a session request (`getEmbed`) is correlated
by requestId and never touches `run.state`. Like every run-correlated request's `kind`, it is inert:
correlation is by `seq`.

-}
recoverRun :
    { tracked : Maybe PendingRequest
    , tailPending : Bool
    , metadata : List OSTypes.MetadataItem
    }
    -> RunRecovery
recoverRun { tracked, tailPending, metadata } =
    case tracked of
        Just _ ->
            NoRecovery

        Nothing ->
            case Exoext.Transport.runStatusFromMetadata metadata of
                Nothing ->
                    NoRecovery

                Just status ->
                    case status.target of
                        Nothing ->
                            NoRecovery

                        Just target ->
                            if isTerminalRunState status.state && not tailPending then
                                RecoverSettled { subject = target, state = status.state }

                            else
                                RecoverPending
                                    { seq = status.seq
                                    , requestId = status.requestId |> Maybe.withDefault ""
                                    , kind = "scan"
                                    , subject = target
                                    , since = Time.millisToPosix status.seq
                                    }



-- REQUEST BATCHES


{-| A sequenced group of requests sharing one wire `batchId` (§4.1 siblings), paced one at a time
through the single §7.1 request slot. `remaining` is the subjects not yet written, in order.
`batchId` is `Nothing` for a single-subject request (the wire field is null then).

`awaitingWrite` covers the gap between deciding on a subject's request and issuing that write: the
subject has already been popped off `remaining`, but nothing has reached the wire and no tracked
seq has moved, so the metadata still reads exactly as it did. §7.1 admits one request at a time,
and that invariant must not rest on how the runtime interleaves the two messages — see
[`advanceBatch`](#advanceBatch).

-}
type alias Batch =
    { batchId : Maybe String
    , remaining : List String
    , awaitingWrite : Bool
    }


{-| What a fresh metadata poll says the host should do about the tracked request + batch.
-}
type BatchStep
    = BatchWaiting
    | BatchSettled
        { subject : String
        , state : String
        , next : Maybe String
        , remaining : List String
        }


{-| Decide the next pacing step from the tracked request, the batch, and fresh §7.1 metadata.

`BatchWaiting` when nothing is tracked, when a decided-on write has not been issued yet, or when
the correlated run is not yet terminal. `BatchSettled` when the correlated run reached a terminal
state: `subject`/`state` are the finished subject and its final run state (commit these to durable
per-subject state), `next` is the subject whose request should be written now (`Nothing` when the
batch is exhausted), and `remaining` is what is left after popping `next`.

Two guards keep exactly one request in flight, covering the two halves of a continuation:

  - **Before the write** — deciding on `next` and issuing its write are separate steps, and the
    metadata is unchanged in between, so a second poll landing in that gap would settle the same
    subject again and pop another. `awaitingWrite` closes that window, and is checked first.
  - **After the write** — the write sets `pending.seq` to N+1 while the status slot still echoes
    run N; [`correlatedRunState`](#correlatedRunState) reads that seq mismatch as `"queued"`, so
    the run is non-terminal again until the publisher claims the new request.

-}
advanceBatch : Maybe PendingRequest -> Maybe Batch -> List OSTypes.MetadataItem -> BatchStep
advanceBatch pending batch metadata =
    case pending of
        Nothing ->
            BatchWaiting

        Just p ->
            if batch |> Maybe.map .awaitingWrite |> Maybe.withDefault False then
                BatchWaiting

            else if requestStillPending (Just p) metadata then
                BatchWaiting

            else
                let
                    remaining =
                        batch |> Maybe.map .remaining |> Maybe.withDefault []
                in
                BatchSettled
                    { subject = p.subject
                    , state = correlatedRunState p.seq metadata
                    , next = List.head remaining
                    , remaining = List.drop 1 remaining
                    }



-- DECLARATIVE VERBS


{-| The generic verb that frames and writes a §4.1 request (params: `{ kind, … }`). Always
confirm-gated by the catalog's confirm rule. Today's `cloudshield.startScan` aliases to it.
-}
verbWriteRequest : String
verbWriteRequest =
    "exoext.writeRequest"


{-| The generic verb that stops a request the publisher has not finished (params:
`{ requestId }` — the §4.1 id to stop, which the host projects per row as `cancelRequestId`).
Always confirm-gated by the catalog's confirm rule: stopping discards work in progress.

It names the request rather than just saying "stop": there is one request slot per publishing VM
(§7.1) and the reader may be a reload behind, so an unnamed stop could land on a run the user never
saw. A cancel for a request the publisher no longer has is a no-op by construction.

-}
verbCancelRequest : String
verbCancelRequest =
    "exoext.cancelRequest"


{-| The generic verb that opens a result session for an existing result (params: `{ resultId }`).
Today's `cloudshield.getEmbed` aliases to it.
-}
verbOpenSession : String
verbOpenSession =
    "exoext.openSession"


{-| The generic verb that closes the open result session (no params). Host-local by definition: a
session is a view, so dismissing one writes nothing to the wire, cancels no request, and discards no
archived result — the same resource can be re-opened with [`verbOpenSession`](#verbOpenSession).
-}
verbDismissSession : String
verbDismissSession =
    "exoext.dismissSession"


{-| The generic verb that refreshes the current session / view. No manifest verb aliases to it
yet (the "Refresh" action on the active row re-opens the session via [`verbOpenSession`](#verbOpenSession)).
-}
verbRefresh : String
verbRefresh =
    "exoext.refresh"


{-| One entry of an adapter's action-name → generic-verb alias table. `name` is the manifest
action name the (frozen) manifest still emits (e.g. `"cloudshield.startScan"`); `verb` is the
generic verb it maps to (one of the `verb*` constants above); `kind` is the wire request kind
carried into a [`verbWriteRequest`](#verbWriteRequest) (e.g. `"scan"`), `""` for verbs that do
not write a request. The table is pure data supplied by the adapter — there are no
extension-named dispatch functions.
-}
type alias VerbAlias =
    { name : String
    , verb : String
    , kind : String
    }


{-| Look up a manifest action name in an alias table, returning the matched [`VerbAlias`](#VerbAlias)
(and thus the generic verb + kind) or `Nothing` for an off-table name (fail-closed: an unaliased
action is ignored by the caller).
-}
resolveVerb : List VerbAlias -> String -> Maybe VerbAlias
resolveVerb table name =
    table |> List.filter (\alias -> alias.name == name) |> List.head
