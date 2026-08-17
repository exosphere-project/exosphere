module Exoext.Lifecycle exposing
    ( PendingRequest
    , Session
    , Freshness(..)
    , sessionFreshness
    , ResultInput
    , SessionState(..)
    , sessionState
    , requestTimeoutMillis
    , requestTimedOut
    , requestTimedOutMessage
    , terminalRunStates
    , isTerminalRunState
    , correlatedRunState
    , runStopping
    , requestStillPending
    , staleRunAfterMillis
    , runStale
    , RunRecovery(..)
    , recoverRun
    , Batch
    , BatchStep(..)
    , advanceBatch
    , verbWriteRequest
    , verbCancelRequest
    , verbOpenSession
    , verbDismissSession
    , verbDeleteResult
    , verbNavigate
    , verbShowDetail
    , verbRefresh
    , VerbAlias
    , resolveVerb
    )

{-| The extension-agnostic request / response LIFECYCLE and result SESSION model for the
`exoext.v1` wire contract. Everything here is generic: it knows only about wire requests
(one in flight per §7.1, correlated by seq / requestId), the polled run status slot, and an
open "result session" (a time-boxed, single-resource view minted in response to a request).
It names no extension — the reference extension is only ever cited as the example consumer in
doc comments. `Exoext.Card` (the adapter) maps its own manifest verbs and wire result shapes
onto these types.

Two things live here that used to be duplicated in `Page.ServerDetail` and `Exoext.Card`:

1.  **Request tracking + run correlation** — the single in-flight [`PendingRequest`](#PendingRequest),
    the run-status correlation ([`correlatedRunState`](#correlatedRunState)), and the terminal-state
    vocabulary. A `kind == "scan"` request is correlated to the run slot by seq; a
    `kind == "getEmbed"` request is correlated to its result by requestId and times out client-side.

2.  **The result-session state machine** — [`sessionState`](#sessionState) derives one generic
    [`SessionState`](#SessionState) token from (the pending request, the wire result, the current
    time, and the request timeout). The adapter maps that token onto its display strings and its
    per-row/per-region visuals. This is the single source of the "opening / open / stale / failed"
    decision (today: the reference extension's history-View embed flow).


# Requests

@docs PendingRequest


# Result sessions

@docs Session
@docs Freshness
@docs sessionFreshness
@docs ResultInput
@docs SessionState
@docs sessionState
@docs requestTimeoutMillis
@docs requestTimedOut
@docs requestTimedOutMessage


# Run status

@docs terminalRunStates
@docs isTerminalRunState
@docs correlatedRunState
@docs runStopping
@docs requestStillPending
@docs staleRunAfterMillis
@docs runStale
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
@docs verbDeleteResult
@docs verbNavigate
@docs verbShowDetail
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

  - the scan tracker (was `Exoext.Card.Model.pending : Maybe { seq, targetId }`):
    `kind == "scan"`, `subject` = the target instance id, correlated to the run slot by `seq`.
  - the session-request tracker (was `ServerDetail.Model.extensionPendingEmbed :
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
(today: an embed URL minted for one archived scan). `resultId` is the resource this
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
parses its extension-specific result body into this shape, with `message` the already-unwrapped
error text. `requestId` correlates the
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
`pendingBatchId` / `erroredBatchId` cluster in `ServerDetail.extensionEmbedProjection`:

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
                Failed { subject = p.subject, message = requestTimedOutMessage }

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


{-| How long a written request may wait for its acknowledgement before the host stops believing one
is coming: 30 seconds.

It applies to the request kinds the publisher answers **inline in the response slot** — a session
mint, a removal — rather than to a run, which reports its own §4.4 states and has its own much wider
bounds. Those inline answers normally arrive in one poll after the publisher claims the slot (~10 s),
so 30 s leaves headroom for a slow publisher without leaving a dead "Opening…" / "Removing…" row up
long enough to read as broken.

The bound is not cosmetic. A marker that never clears is a request that is in flight forever, and
with one §7.1 slot per publisher that is a wedge: every later press is refused to protect a request
that is not coming back.

-}
requestTimeoutMillis : Int
requestTimeoutMillis =
    30 * 1000


{-| Whether a written request has waited past [`requestTimeoutMillis`](#requestTimeoutMillis) for an
answer, measured from the moment it was written (`since`).

Only meaningful for the inline-answer kinds. A scan correlates by `seq` against the run slot and
carries no `since`, so this must never be asked about one.

-}
requestTimedOut : Time.Posix -> PendingRequest -> Bool
requestTimedOut now pending =
    Time.posixToMillis now - Time.posixToMillis pending.since > requestTimeoutMillis


{-| What a request that never came back is reported as. One sentence, shared by every path that can
time one out, so a timeout reads the same wherever the researcher meets it.
-}
requestTimedOutMessage : String
requestTimedOutMessage =
    "the request timed out"



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


{-| Whether a run is **stopping**: a stop has been asked for and the publisher has not answered it
yet. Two conditions, and both are read off the §7.1 wire rather than off session state:

1.  the cancel channel ([`Exoext.Transport.reqCancelFromMetadata`](Exoext-Transport#reqCancelFromMetadata))
    names THIS run's `requestId` (§4.3 `run.requestId`, or the id the host minted for the run's seq);
2.  the run has not reached a terminal state (§4.4). A terminal run is never stopping — it has
    already stopped, or it finished before the stop arrived, and either way there is nothing left
    in flight to report.

This is a distinct state token, not a display string: the host projects `"stopping"` and the manifest
owns the word. It exists because withdrawing the stop control was the ONLY acknowledgement a press
got, which leaves the row reading its previous state (`scanning · 0:54`) for as long as the publisher
takes to reach a phase boundary — long enough that researchers press stop again.

Deriving it from the wire rather than from the press is what makes it survive a reload: the host
wrote the channel, so the channel is the durable record of the press, and a reader that comes back
mid-stop sees the same thing the reader that pressed does.

-}
runStopping : { requestId : String, state : String } -> List OSTypes.MetadataItem -> Bool
runStopping run metadata =
    (Exoext.Transport.reqCancelFromMetadata metadata == Just run.requestId)
        && not (isTerminalRunState run.state)


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



-- STALE RUNS


{-| How long a NON-terminal §7.1 run may sit on the wire before the host stops believing it: six
hours, in millis. Past this the run is STALE ([`runStale`](#runStale)) and the host treats it as
absent rather than as live.

**Why the host needs a bound at all.** The run slot is written by the publisher and settled by the
publisher, and nothing in the contract obliges a publisher to survive its own restart. One that
dies mid-run leaves `run.state` non-terminal forever, and a host that believes the wire without
qualification then reports a scan that will never finish AND withholds every Scan affordance,
because a run is correctly in flight as far as it can tell. Two individually correct behaviors
compose into a trap with no move left in it. That is the shape a defensive bound exists for: the
host cannot assume every publisher is healthy, and it must never be the reason a researcher is
stuck.

**Why six hours.** The host does not know any given publisher's timeouts and must not pretend it
does, so the number is derived from the REFERENCE publisher's documented bounds and then given a
wide margin. §4.4 sets `SCAN_DEADLINE` at 45 min (a run past it is aborted to `error`) and
`REQUEST_TTL` at 60 min (a request past it is ignored and marked `expired`). Six hours is ~8x the
deadline and 6x the TTL, so a publisher an operator tuned several times more generously than the
reference — a very large disk, a slow scanner — still settles its runs well inside it. It is also
short enough that a run wedged in the morning is not still blocking the researcher the next day.

**Which direction to be wrong in.** The two errors are not symmetric, and the constant is sized so
that only the cheap one is reachable.

  - Too GENEROUS costs waiting. The trap stays shut longer, on a page that is otherwise fully
    usable: results, history and every other control still work, only NEW scans are held. The cost
    is bounded, it is recoverable (a wedged run stays wedged, so the valve opens later), and it
    degrades to exactly today's behavior.
  - Too EAGER costs work. The host would call a genuinely running scan dead, drop it from the
    display, and invite the researcher to start a replacement — whose request supersedes a scan
    that was minutes from finishing, discarding the work and spending the clone/snapshot quota
    twice. Worse, it would do this preferentially on the slowest, largest targets, i.e. the ones
    least distinguishable from a failure and most expensive to redo. A safety valve that
    manufactures that regression is worse than the trap it was added for.

-}
staleRunAfterMillis : Int
staleRunAfterMillis =
    6 * 60 * 60 * 1000


{-| Whether a §7.1 run has been non-terminal for implausibly long — see
[`staleRunAfterMillis`](#staleRunAfterMillis) for the bound and the reasoning behind it.

The age needs no new wire field. The host mints a request's `req.seq` as the wall-clock millis at
which it wrote that request (`Page.ServerDetail`'s `ExoextWriteRequest` handler, and the
`exoextRequestId` minted from it), and §7.1 has the publisher echo that value back as `run.seq`.
So the slot already carries the moment the run began, and `now - seq` is the run's age.

Only a NON-terminal run can be stale. A `done`/`error`/`cancelled`/`expired` slot is a settled
record of the LAST run and is allowed to be arbitrarily old — ageing terminal runs out would break
the one place an old terminal run is still load-bearing, an undrained batch tail adopting it in
[`recoverRun`](#recoverRun).

**Why dropping a stale run is safe under the contract.** Not because the host may cancel whenever
it likes — §7.1 is narrower than that. §7.1 has the host bump `req.seq` to cancel _before_ the
publisher has claimed the slot, and otherwise says the host "won't reuse the slot until the run is
terminal"; §4.4 adds that a `running` run is not interruptible in v1. So "a seq mismatch supersedes
anything" is NOT the rule, and this code does not rest on it.

What makes a stale run different is §4.4's own guards, which are the publisher's obligations rather
than the host's: a request older than `REQUEST_TTL` must be ignored and marked `expired`, and a run
past `SCAN_DEADLINE` must be aborted, cleaned up and written `error`. Both are terminal. A run many
multiples of those bounds old is therefore one the publisher's OWN rules say cannot still be live.
The host is not overriding a live run; it is declining to believe a slot the contract says should
have settled long ago.

That framing is also why staleness writes nothing. It only stops the host blocking, handing the
decision back to the researcher. If they then start a new scan, the resulting seq bump lands on a
publisher that is either gone (nothing to supersede) or back — holding a request `staleRunAfterMillis`
old, which §4.4 requires it to treat as `expired` regardless of what the host did.

-}
runStale : Time.Posix -> { seq : Int, state : String } -> Bool
runStale now run =
    not (isTerminalRunState run.state)
        && (Time.posixToMillis now - run.seq > staleRunAfterMillis)



-- RUN RECOVERY


{-| What a reader should adopt from the §4.3 run slot when it is tracking nothing itself — the
answer to "the page was reloaded mid-run, now what?".

  - `NoRecovery` — adopt nothing. Either the reader already tracks a request (its own tracker is
    always the truth, see [`recoverRun`](#recoverRun)), or the wire says nothing adoptable, or the
    run the wire names is already over (a finished run is not something that is happening).
  - `RecoverPending request` — adopt `request` as the tracked request. It is a real §4.3 run the
    reader simply did not write, so every projection keyed on a tracked request (the row's live
    state, the elapsed timer, the fast poll, the batch pacing) engages exactly as it would for a
    request this session issued.

-}
type RunRecovery
    = NoRecovery
    | RecoverPending PendingRequest


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
4.  **Unless that "live" run is [stale](#runStale)**, in which case `NoRecovery`. This is the reload
    half of the safety valve, and it has to be here rather than downstream: adopting a run and then
    discarding it would still let it exist as a tracked request for an instant, and a run the host
    has decided is not live must never be resurrected into the tracker at all. Note the ordering
    against rule 5 — staleness applies only to the non-terminal branch, so an old terminal run with
    a tail waiting on it is still adopted, because that adoption is about draining the batch and
    not about believing the run.
5.  **A finished run is adopted only when a tail is waiting on it.** The §7.1 run slot is
    overwritten in place and never cleared, so it is a record of the LAST run, not of a CURRENT one.
    A reader that adopts a terminal run therefore re-adopts the same finished run on every later
    page load, forever — which is exactly how a completed scan came to read as still-finishing-now
    on a page the user opened days later. A finished run is history, and history belongs to the
    archived-result view (which carries its timestamp), so `tailPending == False` ⇒ `NoRecovery`.
    The ONE exception is `tailPending == True`: an undrained batch tail is waiting on this run, and
    draining it is driven by [`advanceBatch`](#advanceBatch), which needs a tracked request to
    settle before it can pop the next subject. Without that, the restored tail of a batch whose run
    had already finished at reload would never resume. Adopting a terminal run is correct there
    because it is not being adopted as a display state at all — it is being adopted as the token
    that lets the batch take its next step.

The deliberate consequence: WITHIN one session a finished row still shows its terminal state,
because the reader itself observed that run and committed it (the host's own per-subject record).
ACROSS a reload it does not, because the reader only trusts the wire for what is happening NOW and
the wire cannot distinguish "just finished" from "finished last week". That asymmetry is intended:
session-local knowledge is dated by construction, and a run slot is not.

The recovered request's `kind` is `""`, and empty is the only honest value. §4.3 carries no verb, so
the wire never says what the run was a request FOR; a reader that filled the field in would be
stating something it was not told. It costs nothing to leave empty because a run-correlated request
correlates by `seq` — nothing reads this `kind` — and it is the field an adapter would have to trust
if anything ever did. Naming a verb here was the generic layer speaking one extension's vocabulary.

-}
recoverRun :
    { now : Time.Posix
    , tracked : Maybe PendingRequest
    , tailPending : Bool
    , metadata : List OSTypes.MetadataItem
    }
    -> RunRecovery
recoverRun { now, tracked, tailPending, metadata } =
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
                                -- A last-run record with nothing waiting on it: not adoptable.
                                NoRecovery

                            else if runStale now { seq = status.seq, state = status.state } then
                                -- Non-terminal but implausibly old (§4.4 TTL/deadline say it
                                -- cannot still be running): not live, so not adoptable either.
                                NoRecovery

                            else
                                RecoverPending
                                    { seq = status.seq
                                    , requestId = status.requestId |> Maybe.withDefault ""
                                    , kind = ""
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
confirm-gated by the catalog's confirm rule. The reference extension's `startScan` aliases to it.
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
The reference extension's `getEmbed` aliases to it.
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


{-| The generic verb that removes one existing result (params: `{ resultId, batchId? }`). Always
confirm-gated by the catalog's confirm rule: unlike [`verbDismissSession`](#verbDismissSession),
which closes a view, this one destroys the thing being viewed.

It is a request like any other and goes through the same single §7.1 slot, because only the
publisher can act on its own archive. The host's half is entirely generic — write a request that
names a result id, then read one acknowledgement back — and that is deliberately all the host knows:
what "removing a result" costs, and whether it can be undone, are the publisher's to state in the
confirm text its manifest supplies.

-}
verbDeleteResult : String
verbDeleteResult =
    "exoext.deleteResult"


{-| The generic verb that asks the HOST to navigate to one of its own instances (params:
`{ instanceId }`).

This is the first verb through which an extension moves the app rather than describing it, so it is
deliberately the narrowest one that is useful. It names an INSTANCE, not a URL and not a route: the
host resolves the id against its own project and navigates to that instance's page, so the set of
places an extension can send a researcher is exactly the set of pages they could have reached by
clicking around the project themselves. An id the host does not recognize is ignored rather than
guessed at.

That framing is what keeps it benign. Navigation cannot exfiltrate, cannot write, and cannot be made
to point outside the app, because there is nowhere in the grammar to put an outside address.

-}
verbNavigate : String
verbNavigate =
    "exoext.navigate"


{-| The generic verb that asks the HOST to show a longer piece of text the extension already has
on screen in short form (params: `{ title, text }`, both plain strings the manifest resolves like
any other param).

Host-local by definition: it writes nothing, requests nothing, and names no resource — it is the
extension saying "there is more to read here" and Exosphere deciding how reading it looks. That
split is the whole point. The extension owns the words, so a failure reason, a truncation notice or
a policy note all reach the researcher in the publisher's own vocabulary; the host owns the surface,
so every one of them is the same dismissible, bounded, keyboard-closable dialog drawn in Exosphere's
chrome, and a manifest can neither style it nor make it unclosable.

The text is publisher-authored and is rendered as plain text, never as markup.

-}
verbShowDetail : String
verbShowDetail =
    "exoext.showDetail"


{-| The generic verb that refreshes the current session / view. No manifest verb aliases to it
yet (the "Refresh" action on the active row re-opens the session via [`verbOpenSession`](#verbOpenSession)).
-}
verbRefresh : String
verbRefresh =
    "exoext.refresh"


{-| One entry of an adapter's action-name → generic-verb alias table. `name` is the manifest
action name the (frozen) manifest still emits (e.g. `"<publisher>.startScan"`); `verb` is the
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
