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
    , verbWriteRequest
    , verbCancelRequest
    , verbOpenSession
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


# Declarative verbs

@docs verbWriteRequest
@docs verbCancelRequest
@docs verbOpenSession
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



-- DECLARATIVE VERBS


{-| The generic verb that frames and writes a §4.1 request (params: `{ kind, … }`). Always
confirm-gated by the catalog's confirm rule. Today's `cloudshield.startScan` aliases to it.
-}
verbWriteRequest : String
verbWriteRequest =
    "exoext.writeRequest"


{-| The generic verb that cancels the in-flight request. No manifest verb aliases to it yet.
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
