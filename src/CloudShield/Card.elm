module CloudShield.Card exposing (EmbedState(..), Instance, ManifestSource(..), Model, Msg, OutMsg(..), ViewConfig, abandonScanState, cancelTargetOf, dispatchVerb, headerTitle, init, projection, requestEmbed, resolveAction, resultIdOf, rollbackScanRequest, scanningRowLabel, sentinelKind, settleScanState, transportChip, update, view)

{-| Host wiring for the CloudShield dynamic-UI card (Phase 1, browser side).

This is the Exosphere-side **host** for the vendored native Elm json-render renderer
(`JsonRender.*`). It owns the json-render `state`, projects it for the renderer
(`bnr/spike/renderer/host-renderer-interface.md` §1.2), handles the renderer's `Effect`s
(start scans, apply checkbox write-backs), and draws the host-only trust chrome — the
provenance "?" marker (§5.2) and the opt-in affordance (§5.3) — around the rendered card.

**Manifest source.** The card's json-render manifest arrives ONLY over the wire — a
`store=swift` object fetch or metadata-mode chunks — surfaced to the view as a
[`ManifestSource`](#ManifestSource). There is no built-in fallback manifest: while the body is
still being fetched the card region shows quiet host loading chrome, and an unresolvable /
undecodable body shows the fail-closed notice, never a stale embedded card. Selection,
select-all, and the confirm dialog are fully live; a confirmed write-request flips the targeted
rows to `queued` locally so the action round-trip is visible.

Everything here is gated by `context.experimentalFeaturesEnabled` at the call site
(`Page.ServerDetail`), so normal operation is unaffected.

-}

import Color
import Dict exposing (Dict)
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Exoext.Lifecycle as Lifecycle
import Exoext.Transport as Transport
import Helpers.Time
import Html
import Html.Attributes
import Json.Decode as Decode
import Json.Encode as Encode
import JsonRender
import JsonRender.Render as Render
import JsonRender.Spec as Spec
import Maybe.Extra
import Set exposing (Set)
import Style.Helpers as SH
import Style.Types exposing (ExoPalette)
import Style.Widgets.Code as Code
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import Time



-- IDENTITY


{-| The §3.1 `exoext.v1.kind` value this adapter is the view for. The host matches a published
sentinel's `kind` against it to decide whether it recognizes the publisher at all, so the token
lives here with the adapter that speaks it rather than in the host that merely compares it.
-}
sentinelKind : String
sentinelKind =
    "cloudshield"


{-| What the host draws in the card header for [`sentinelKind`](#sentinelKind). A display name is
not derivable from a wire token, and it is the adapter, not the host, that knows how its own
extension is spelled.
-}
headerTitle : String
headerTitle =
    "CloudShield (extension)"



-- MODEL


{-| A scannable instance, as the host hands it to the card. In M1 this is a fixture; in M2
it is the eligibility-filtered projection of `project.servers`.
-}
type alias Instance =
    { id : String
    , name : String
    , status : String
    }


type alias Model =
    { renderer : Render.Model
    , selection : Set String
    , selectAll : Bool

    -- per-instance scan state: id -> "idle" | "queued" | "running" | "done" | "error".
    -- Absent => "idle". Host-owned local optimistic state; the authoritative live state
    -- comes from the polled status object (applied via ViewConfig.statusOverride).
    , scanState : Dict String String

    -- monotonic request seq for the §7.1 metadata req-slot (per CloudShield VM).
    , seq : Int

    -- the in-flight scan request (§7.1 single-in-flight), as the generic
    -- `Exoext.Lifecycle.PendingRequest` (`kind == "scan"`, `subject` = the target instance id):
    -- which seq maps to which target, so the host can project the polled run.state onto the right
    -- row. A scan correlates by `seq`, so its `requestId` / `since` are inert here (the host owns
    -- the getEmbed tracker, the other view of `PendingRequest`).
    , pending : Maybe Lifecycle.PendingRequest

    -- DEMO-ONLY: whether the embedded CloudShield live-UI iframe is expanded. This is a raw,
    -- unpinned host-chrome embed, distinct from the catalog's origin-pinned Iframe element.
    -- Off by default; toggled by a link, only when ViewConfig.demoIframeUrl is set.
    , showDemoIframe : Bool

    -- whether the raw decoder diagnostic under a rejected manifest is expanded. Collapsed by
    -- default: the researcher gets a sentence, and the wall of decoder prose is one click away
    -- for whoever is actually debugging the publisher.
    , showManifestErrorDetail : Bool
    }


type Msg
    = GotApprove
    | GotForget
    | GotToggleDemoIframe
    | GotToggleManifestErrorDetail
    | GotRetryEmbed String
    | RendererMsg Render.Msg


{-| What the card asks its parent (`Page.ServerDetail`) to do. The card itself never issues
OpenStack Cmds; the parent owns the Nova metadata write (it has `Rest.Nova` + the project).

  - `ScanRequested { seq, targetIds }` — a confirmed `cloudshield.startScan`. The parent
    re-resolves the targets (§5.4), encodes the §4.1 request, and writes the §7.1 req-slot
    on the CloudShield VM's metadata.

  - `EmbedRequested { resultId, batchId }` — a `cloudshield.getEmbed` on a history row. The parent
    stamps a wall-clock seq, encodes the `getEmbed` request, and writes the same §7.1 req-slot.
    `resultId` is the row's own §4.2 result id (the selector); `batchId` is the shared §2.2 batch
    id, kept on the wire for a publisher that only knows how to select by batch.

-}
type OutMsg
    = ScanRequested { seq : Int, targetIds : List String }
    | EmbedRequested { resultId : String, batchId : String }
      -- `CancelRequested { requestId, targetId }` — a confirmed `exoext.cancelRequest` on a row the
      -- host marked stoppable. BOTH ids ride along because the two stoppable cases are stopped in
      -- different ways: a run on the wire is stopped by writing the cancel channel
      -- (`exoext.v1.req.cancel`) with its `requestId`, while a target still queued behind the single
      -- §7.1 request slot has no request to name and is stopped by taking it out of the tail
      -- host-side. The card decides neither — it holds no run-state and no batch knowledge — it just
      -- carries what the row was projected with; see `cancelTargetOf`.
    | CancelRequested { requestId : String, targetId : String }
      -- `SessionDismissed` — the researcher closed the open results session. Purely host-local
      -- display state: nothing is written to the wire and no archived scan is touched.
    | SessionDismissed
      -- The trust decisions. The card holds no approval state of its own; it asks the host to
      -- grant (persist an approval for the publishing instance) or forget (remove it). The host
      -- owns the persisted `exoext.approval.v1` record and stamps the wall-clock `approvedAt`.
    | ApprovalGranted
    | ApprovalForgotten


init : Model
init =
    { renderer = Render.init
    , selection = Set.empty
    , selectAll = False
    , scanState = Dict.empty
    , seq = 0
    , pending = Nothing
    , showDemoIframe = False
    , showManifestErrorDetail = False
    }



-- UPDATE


update : List Instance -> Msg -> Model -> ( Model, Maybe OutMsg )
update instances msg model =
    case msg of
        GotApprove ->
            ( model, Just ApprovalGranted )

        GotForget ->
            ( model, Just ApprovalForgotten )

        GotToggleDemoIframe ->
            ( { model | showDemoIframe = not model.showDemoIframe }, Nothing )

        GotToggleManifestErrorDetail ->
            ( { model | showManifestErrorDetail = not model.showManifestErrorDetail }, Nothing )

        GotRetryEmbed resultId ->
            -- The results-region "Retry" affordance re-fires getEmbed for the last-attempted result.
            -- Same OutMsg the history-row press emits, so it flows through the host's single-req-slot
            -- guard exactly like a fresh View. Only the result id survives into the host's errored
            -- state, so it doubles as the batch selector: the publisher prefers `resultId`, and where
            -- it does not (a row with no per-run id) the two ids are the same value anyway.
            requestEmbed (Just { resultId = resultId, batchId = resultId }) model

        RendererMsg rmsg ->
            let
                ( renderer, effect ) =
                    Render.update rmsg model.renderer
            in
            applyEffect instances effect { model | renderer = renderer }


applyEffect : List Instance -> Maybe Render.Effect -> Model -> ( Model, Maybe OutMsg )
applyEffect instances effect model =
    case effect of
        Nothing ->
            ( model, Nothing )

        Just (Render.EmitAction action) ->
            -- Resolve the manifest's action name to a generic `Exoext.Lifecycle` verb, then
            -- dispatch. Manifest v2 emits the generic verbs directly (`exoext.writeRequest`,
            -- `exoext.openSession`); manifest v1 emits the `cloudshield.*` names carried by the
            -- pure-data alias table. Both resolve here; an off-table / off-verb action is ignored
            -- (fail-closed).
            dispatchVerb (resolveAction action.verb action.params) action.params model

        Just (Render.EmitStateChange change) ->
            ( applyStateChange instances change model, Nothing )


{-| The CloudShield adapter's action-name → generic-verb alias table (pure data). The frozen
manifest emits `cloudshield.startScan` (a §4.1 request, kind `"scan"`) and `cloudshield.getEmbed`
(open a result session for an archived scan); both map onto `Exoext.Lifecycle`'s generic verbs.
There are no `cloudshield.*`-named dispatch FUNCTIONS — only this table.
-}
actionAliases : List Lifecycle.VerbAlias
actionAliases =
    [ { name = "cloudshield.startScan", verb = Lifecycle.verbWriteRequest, kind = "scan" }
    , { name = "cloudshield.getEmbed", verb = Lifecycle.verbOpenSession, kind = "" }
    , { name = "cloudshield.cancelScan", verb = Lifecycle.verbCancelRequest, kind = "" }
    ]


{-| Resolve a manifest action name to a generic [`Lifecycle.VerbAlias`](Exoext-Lifecycle#VerbAlias).
Two ways in: the v1 [`actionAliases`](#actionAliases) table (the `cloudshield.*` names), and — for
manifest v2 and later — a name that IS already a generic verb (`exoext.writeRequest`,
`exoext.openSession`, `exoext.cancelRequest`, `exoext.dismissSession`), accepted directly with its
`kind` read from the emitted params. Anything else is `Nothing` (fail-closed). Keeping the alias
table means a v1 manifest still dispatches against this host until the demo VM redeploys.
-}
resolveAction : String -> Encode.Value -> Maybe Lifecycle.VerbAlias
resolveAction name params =
    case Lifecycle.resolveVerb actionAliases name of
        Just alias ->
            Just alias

        Nothing ->
            if name == Lifecycle.verbWriteRequest then
                Just { name = name, verb = Lifecycle.verbWriteRequest, kind = kindOf params }

            else if List.member name [ Lifecycle.verbOpenSession, Lifecycle.verbCancelRequest, Lifecycle.verbDismissSession ] then
                Just { name = name, verb = name, kind = "" }

            else
                Nothing


{-| The generic `exoext.writeRequest` carries its wire request `kind` in params (v1 carried it in
the alias table). Read it, defaulting to `""` when absent.
-}
kindOf : Encode.Value -> String
kindOf params =
    Decode.decodeValue (Decode.field "kind" Decode.string) params
        |> Result.withDefault ""


{-| The generic verb dispatcher: interpret a resolved [`Lifecycle.VerbAlias`](Exoext-Lifecycle#VerbAlias)
against this adapter's request/session handlers. `verbWriteRequest` frames + writes a scan request
(targets resolved §5.4) — but only for `kind == "scan"`, the one request kind this adapter
handles; any other (or missing) kind is a no-op, so a manifest cannot route an unknown request
kind onto the scan path. `verbOpenSession` opens a result session for the pressed row's result.
`verbCancelRequest` asks the host to stop the named request. `verbDismissSession` closes the open
session. `Nothing` (an unaliased action) and any not-yet-wired verb are no-ops (fail-closed).
-}
dispatchVerb : Maybe Lifecycle.VerbAlias -> Encode.Value -> Model -> ( Model, Maybe OutMsg )
dispatchVerb maybeAlias params model =
    case maybeAlias of
        Just alias ->
            if alias.verb == Lifecycle.verbWriteRequest && alias.kind == "scan" then
                requestScan (targetsOf model params) model

            else if alias.verb == Lifecycle.verbOpenSession then
                requestEmbed (resultIdOf params) model

            else if alias.verb == Lifecycle.verbCancelRequest then
                requestCancel (cancelTargetOf params) model

            else if alias.verb == Lifecycle.verbDismissSession then
                ( model, Just SessionDismissed )

            else
                ( model, Nothing )

        Nothing ->
            ( model, Nothing )


{-| A confirmed `startScan`: bump the seq, flip the targeted rows to `queued` optimistically
(dedup-aware), record the in-flight correlation (first target — §7.1 single-in-flight), and
ask the parent to write the §7.1 req-slot. The full target list rides along: §7.1 allows one
request at a time, so the parent writes the head now and paces the tail as each run settles.
No targets ⇒ no-op.
-}
requestScan : List String -> Model -> ( Model, Maybe OutMsg )
requestScan requested model =
    let
        -- Dedup at the source (§4.4 one-active-run / §7.1 single-in-flight): drop targets
        -- that already have an active run, so a re-click does not re-emit a request, bump the
        -- seq, or overwrite `pending` for an in-flight target.
        targets =
            List.filter (\id -> not (isActive (localScanState model id))) requested
    in
    case targets of
        [] ->
            ( model, Nothing )

        firstTarget :: _ ->
            let
                seq =
                    model.seq + 1
            in
            ( { model
                | seq = seq
                , pending =
                    Just
                        { seq = seq
                        , requestId = ""
                        , kind = "scan"
                        , subject = firstTarget
                        , since = Time.millisToPosix 0
                        }
                , scanState = startScan targets model.scanState
              }
            , Just (ScanRequested { seq = seq, targetIds = targets })
            )


{-| A `cloudshield.getEmbed` on a history row: ask the parent to mint a fresh embed URL for the
named archived scan. The card holds no run-state knowledge, so it does not guard here — the §7.1
single-req-slot guard (don't disturb an active or unclaimed scan) is applied host-side in
`Page.ServerDetail`, derived from the live wire metadata. Missing ids ⇒ no-op.
-}
requestEmbed : Maybe { resultId : String, batchId : String } -> Model -> ( Model, Maybe OutMsg )
requestEmbed maybeIds model =
    case maybeIds of
        Just ids ->
            ( model, Just (EmbedRequested ids) )

        Nothing ->
            ( model, Nothing )


{-| A confirmed stop: ask the parent to stop what the pressed row named. The card optimistically
changes NOTHING — no row state, no tracker. Stopping is the publisher's decision to honor (§7.1), and
the run's own state is what reports it; a locally invented badge would either need a display string
of its own (the manifest owns those) or would lie when the publisher had already finished. The
acknowledgement the researcher sees is the host's, from the wire: the row's `"stopping"` state token,
and the stop control withdrawing with it.

The full target list a stopped batch had left to scan is dropped host-side, not here — the card does
not own the batch.

-}
requestCancel : Maybe { requestId : String, targetId : String } -> Model -> ( Model, Maybe OutMsg )
requestCancel maybeTarget model =
    case maybeTarget of
        Just target ->
            ( model, Just (CancelRequested target) )

        Nothing ->
            ( model, Nothing )


{-| Resolve a stop press's params: `requestId` — the §4.1 id of the run to stop, projected onto the
row as `cancelRequestId` — and `targetId`, the instance the press is about, projected as
`cancelTargetId`. Each defaults to `""` when absent, so a manifest that emits only one of them still
dispatches (the frozen v4 manifest emits only `requestId`).

`Nothing` only when NEITHER is present, which is fail-closed on the one thing that matters: a press
that names nothing at all is a press the host could not attribute to any run or any row, so it must
not reach the dispatcher. Beyond that the emptiness of each id is MEANINGFUL rather than invalid, and
the host reads it as such — an empty `requestId` with a real `targetId` is exactly a queued target,
which has no request on the wire yet and is stopped by leaving the queue rather than by writing the
cancel channel. Only the host knows which of the two a press is, because only the host owns the batch.

-}
cancelTargetOf : Encode.Value -> Maybe { requestId : String, targetId : String }
cancelTargetOf params =
    let
        stringParam key =
            Decode.decodeValue (Decode.field key Decode.string) params
                |> Result.withDefault ""

        requestId =
            stringParam "requestId"

        targetId =
            stringParam "targetId"
    in
    if String.isEmpty requestId && String.isEmpty targetId then
        Nothing

    else
        Just { requestId = requestId, targetId = targetId }


{-| Resolve the pressed history row's session ids from the renderer's already-resolved params.
The manifest emits `resultId` (the row's own §4.2 result id, unique per run) and `batchId` (its
§2.2 batch id, SHARED by siblings). Each falls back to the other, so a manifest that still emits
only `batchId` keeps dispatching, and a legacy row with no per-run id of its own resolves both to
the same value. `Nothing` when neither param is present (fail-closed).
-}
resultIdOf : Encode.Value -> Maybe { resultId : String, batchId : String }
resultIdOf params =
    let
        stringParam key =
            Decode.decodeValue (Decode.field key Decode.string) params
                |> Result.toMaybe
    in
    Maybe.map2 (\resultId batchId -> { resultId = resultId, batchId = batchId })
        (Maybe.Extra.or (stringParam "resultId") (stringParam "batchId"))
        (Maybe.Extra.or (stringParam "batchId") (stringParam "resultId"))


{-| Resolve a `cloudshield.startScan` `targetInstanceIds` param: an empty array means "use
the current selection" (the pinned host convention, host-renderer-interface.md §2.1); a
non-empty array is the explicit per-row id(s).
-}
targetsOf : Model -> Encode.Value -> List String
targetsOf model params =
    case Decode.decodeValue (Decode.field "targetInstanceIds" (Decode.list Decode.string)) params of
        Ok [] ->
            Set.toList model.selection

        Ok ids ->
            ids

        Err _ ->
            []


{-| Flip each targeted row to `queued`, skipping rows already queued/running (dedup, §4.4).
-}
startScan : List String -> Dict String String -> Dict String String
startScan ids scanState =
    let
        trigger id acc =
            if isActive (Dict.get id acc |> Maybe.withDefault "idle") then
                acc

            else
                Dict.insert id "queued" acc
    in
    List.foldl trigger scanState ids


{-| Undo [`requestScan`](#update)'s optimistic mutation on a request the host refused, given the
card as it was before the press and as it is after. The card decodes the press, so it has already
updated by the time the host's §7.1 guard gets to say no; a refused request must then leave no
scan-tracking trace, or `pending` points at a seq the wire will never carry and the live run's
correlation is lost. Exactly the three fields `requestScan` writes are restored — `seq`, `pending`,
`scanState`.

Everything else the press did deliberately stands. `startScan` is confirm-gated, and accepting the
dialog is the same step that emits the action, so restoring the renderer would leave the confirm
dialog open on a press that does nothing.

-}
rollbackScanRequest : Model -> Model -> Model
rollbackScanRequest before after =
    { after
        | seq = before.seq
        , pending = before.pending
        , scanState = before.scanState
    }


{-| Commit one instance's terminal run state into the card's durable `scanState`. The live run
projection (`ViewConfig.statusOverride`) covers exactly one row — the tracked request's subject —
so while a batch drains, each finished row must be written here before the tracker retargets, or
it falls back to its optimistic `queued` badge. The host calls this as each run settles.
-}
settleScanState : String -> String -> Model -> Model
settleScanState id state model =
    { model | scanState = Dict.insert id state model.scanState }


{-| Drop the optimistic state of rows whose requests will now never be written — the targets left in
a batch that was stopped. `requestScan` flips every selected row to `queued` up front so the press
reads as accepted; if the batch is then abandoned, those rows have no run coming and no terminal
state ever arrives to overwrite them, so they would sit on a spinning `queued` badge forever.

Removing the key (rather than writing an "idle" state) returns each row to the absent-means-idle
default, which is also what makes its Scan action pressable again.

-}
abandonScanState : List String -> Model -> Model
abandonScanState ids model =
    { model | scanState = List.foldl Dict.remove model.scanState ids }


isActive : String -> Bool
isActive state =
    state == "queued" || state == "running"


{-| Apply a renderer write-back from a two-way checkbox to the host-owned selection state.
The renderer reports the toggle at an absolute JSON Pointer; the host is the source of truth.
-}
applyStateChange : List Instance -> { path : String, value : Encode.Value } -> Model -> Model
applyStateChange instances change model =
    let
        bool =
            Decode.decodeValue Decode.bool change.value |> Result.withDefault False
    in
    case String.split "/" change.path of
        [ "", "selectAll" ] ->
            { model
                | selectAll = bool
                , selection =
                    if bool then
                        instances |> List.map .id |> Set.fromList

                    else
                        Set.empty
            }

        [ "", "instances", indexStr, "selected" ] ->
            case String.toInt indexStr |> Maybe.andThen (instanceIdAt instances) of
                Just id ->
                    let
                        selection =
                            if bool then
                                Set.insert id model.selection

                            else
                                Set.remove id model.selection
                    in
                    { model
                        | selection = selection
                        , selectAll = allSelected instances selection
                    }

                Nothing ->
                    model

        _ ->
            model


instanceIdAt : List Instance -> Int -> Maybe String
instanceIdAt instances index =
    instances |> List.drop index |> List.head |> Maybe.map .id


{-| Whether every (eligible) instance is currently selected. Membership-based, not a size
comparison, so a stale id in `selection` (left after a poll shrinks/reorders the list) cannot
spuriously report "all selected". Empty list ⇒ not all-selected.
-}
allSelected : List Instance -> Set String -> Bool
allSelected instances selection =
    not (List.isEmpty instances) && List.all (\i -> Set.member i.id selection) instances



-- THE RENDERER-FACING PROJECTION (host-renderer-interface.md §1.2)


{-| How many history rows the card projects into `/history`. Older scans beyond this are
summarized by `/historyNote` rather than rendered, so the card stays bounded no matter how
long the archived index grows.
-}
historyDisplayCap : Int
historyDisplayCap =
    20


{-| The whole render state the manifest binds against, projected from the host's
[`ViewConfig`](#ViewConfig). It takes the config wholesale rather than a dozen positional signals:
every input here is already a config field, and each new per-row signal (WP8 added two) was one more
`Nothing` in a row of interchangeable `Maybe String`s at every call site.
-}
projection :
    Time.Zone
    -> ViewConfig
    -> List Instance
    -> Model
    -> Encode.Value
projection zone config instances model =
    let
        history =
            config.history

        total =
            List.length history.rows

        -- Newest first (the index is append-only, oldest first), capped to the latest N.
        shown =
            List.reverse history.rows |> List.take historyDisplayCap

        historyNote =
            if not history.loaded || total <= historyDisplayCap then
                ""

            else
                "Showing the latest "
                    ++ String.fromInt historyDisplayCap
                    ++ " of "
                    ++ String.fromInt total
                    ++ " scans."

        -- The two columns each carry a header with a live count, so the researcher can tell at a
        -- glance how many targets / scans exist even though each column is height-bounded and
        -- scrolls internally (counts unbounded; "unlimited" instances and scans).
        targetsLabel =
            "Scan targets · " ++ String.fromInt (List.length instances)

        historyLabel =
            if history.loaded then
                "Scan history · " ++ String.fromInt total

            else
                "Scan history"
    in
    Encode.object
        [ ( "selectAll", Encode.bool model.selectAll )
        , ( "results", Maybe.withDefault Encode.null config.results )
        , ( "embedUrl", Encode.string config.embedUrl )

        -- Not display copy: this is the §2.4 state path the manifest binds against
        -- (`$instances`), so it is contract data and localizing it would break the wire.
        , {- @nonlocalized -} ( "instances", Encode.list (instanceProjection config model) instances )
        , ( "history", Encode.list (historyRow zone config.activeResultId config.pendingResultId config.erroredResultId config.expiredResultId) shown )
        , ( "historyNote", Encode.string historyNote )

        -- Whether the host would SWALLOW an `exoext.openSession` press right now (the §7.1
        -- single-req-slot guard: a scan is active or its request is still unclaimed). The manifest
        -- binds it to the row action's `disabled`, so the guard is visible instead of silent.
        , ( "requestBusy", Encode.bool config.requestBusy )

        -- The same idea for the scan side: whether the host would swallow an `exoext.writeRequest`
        -- press right now. A separate key because it is a separate guard on separate controls —
        -- see `ViewConfig.scanBusy`.
        , ( "scanBusy", Encode.bool config.scanBusy )

        -- Whether a result session is on screen right now, so the manifest can offer its close
        -- affordance only when there is something to close (an `exoext.dismissSession` press with
        -- no open session is a no-op, and a control that does nothing reads as broken).
        , ( "sessionOpen", Encode.bool config.sessionOpen )

        -- Old display-string header keys (manifest v1). Kept alongside the new count/flag keys
        -- below so a v1 manifest still renders against this host until the demo VM redeploys v2.
        , ( "targetsLabel", Encode.string targetsLabel )
        , ( "historyLabel", Encode.string historyLabel )

        -- New wire-owned keys (manifest v2): the manifest composes the header strings itself from
        -- these counts/flag, so no display string lives in the host. `historyLoaded` gates the
        -- history count the same way `historyLabel` did above (one source, two projections).
        , ( "historyLoaded", Encode.bool history.loaded )
        , ( "historyCount", Encode.int total )
        , ( "targetsCount", Encode.int (List.length instances) )
        ]


{-| Project one history row for the `/history` render state. The index is append-only
(oldest first), so the render state reverses it to newest-first.

The host computes every per-row visual signal here, so the renderer stays a pure catalog with
no conditionals (host-renderer-interface.md §1.2):

  - `completedAt` — humanized local timestamp ("Jul 20, 2026 · 10:52 PM"), not raw ISO.
  - `subLabel` — the target + short batch id ("alpha · #84a1c6"), on every row including a failed
    one (the batch id is dropped when the scan had none).
  - `findings` — a findings-shaped array synthesized from the aggregate counts, bound into the
    row's `FindingsTable` so history pills match the live `/results` pills.
  - `rowState` — the one active/failed/loading hook the stylesheet keys on: "Opening…" for the
    row whose getEmbed is in flight (wins), "Now viewing" for the row the embed/results are
    showing, "Expired" for the row whose session has expired, "failed" for an errored scan,
    "" (hidden badge) otherwise.
  - `actionLabel` — the View/Refresh/Opening flip ("Opening…" while this row's getEmbed is in
    flight, "Refresh" for the active row, "View" for an expired row (reopen), "" for a failed scan
    that has nothing to view).

Every one of those signals is matched on the row's **`resultId`** — its own §4.2 result id
(`entry.requestId`), falling back to `batchId` for a legacy row that carries none. §2.2 siblings
SHARE a `batchId`, so matching on it marked every sibling of a batch "Now viewing" at once; the
result id is unique per run, so exactly one row can ever match.

`pendingResultId` is the getEmbed that is genuinely in flight right now: its row shows the loading
state, and while it is set NO row shows "Now viewing" (the clicked row supersedes the prior one).

`erroredResultId` is the last getEmbed that failed / timed out (host-tracked, retained until a new
getEmbed starts or one succeeds). That row reads "Couldn't open" and its action flips to "Retry"
(re-firing the same `getEmbed` press for its result), and — crucially — while it is set the prior
active row does NOT falsely reclaim "Now viewing".

`expiredResultId` is the row whose result SESSION has expired (its embed URL is no longer served).
The host derives it from `Exoext.Lifecycle`'s `OpenStale` token instead of leaving that row
`activeResultId`. The row reverts from "Now viewing" / "Refresh" to a muted "Expired" badge and a
plain "View" action (reopen), while keeping a faint highlight so the researcher still sees which
scan they were on. This is the expired-session fix.

Precedence for a row's state: **failed scan (dead scan, never View-able) → embed error (this result
just failed) → loading (in-flight) → expired (session lapsed) → now viewing (resolved ok, fresh) →
idle**. The two "error" concepts never co-occur on one row (a failed scan is never
getEmbed-clickable, so `erroredResultId` can never name a failed-scan result), so keying `isError`
first keeps the failed-scan row pristine; `expiredResultId` and `activeResultId` are mutually
exclusive (`OpenStale` sets one, `Open` the other), so they never contend for a row.

`countsLabel` is retained (unbound by the current manifest) as a stable plain-text fallback.

-}
historyRow : Time.Zone -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Transport.IndexEntry -> Encode.Value
historyRow zone activeResultId pendingResultId erroredResultId expiredResultId entry =
    let
        -- The row's identity for every session match below, and the `resultId` the View press
        -- sends back as the §4.2 selector. `batchId` is the fallback for a legacy index row.
        resultId =
            Maybe.withDefault entry.batchId entry.requestId

        isError =
            entry.status == "error"

        -- This row's last getEmbed failed / timed out: it shows a danger "Couldn't open" state and
        -- its action flips to "Retry" (the same press re-fires getEmbed). Never on a failed scan.
        isEmbedErrorRow =
            (erroredResultId == Just resultId) && not isError

        -- This row's getEmbed is in flight: it shows the loading state and its button de-emphasizes.
        -- An error on this same result supersedes the loading look (it can only be one at a time).
        isLoadingRow =
            (pendingResultId == Just resultId) && not isError && not isEmbedErrorRow

        -- This row's result session has expired: it reverts to a muted "Expired" / plain-View row
        -- (never on a failed scan, and superseded by an in-flight / errored getEmbed on this result).
        isExpiredRow =
            (expiredResultId == Just resultId) && not isError && not isEmbedErrorRow && not isLoadingRow

        -- While ANY getEmbed is pending, OR while this result's getEmbed just errored, no row reads
        -- as "Now viewing": the clicked (loading/errored) row supersedes the previously-active one,
        -- so the old row drops its active state instead of falsely reclaiming it.
        isActiveRow =
            (pendingResultId == Nothing)
                && (erroredResultId == Nothing)
                && (activeResultId == Just resultId)
                && not isError

        completedAtLabel =
            case Helpers.Time.iso8601StringToPosix entry.completedAt of
                Ok posix ->
                    Helpers.Time.humanReadableDateAndTimeCompact zone posix

                Err _ ->
                    entry.completedAt

        -- Every row names its target, a failed one included: "which instance failed?" is the first
        -- question a failure raises, and the row used to answer it with a host-owned "failed · no
        -- findings" that said neither. That the scan failed is already carried by `state` /
        -- `rowState` (the badge) and by the row's empty findings, so dropping the words loses
        -- nothing and takes one more display string out of the host. The short batch id is appended
        -- only when there is one: §4.1 leaves `batchId` null for a single-target scan, and a bare
        -- "#" is noise.
        subLabel =
            if entry.batchId == "" then
                entry.targetName

            else
                entry.targetName ++ " · #" ++ String.left 6 entry.batchId

        rowState =
            if isError then
                "failed"

            else if isEmbedErrorRow then
                "Couldn't open"

            else if isLoadingRow then
                "Opening…"

            else if isExpiredRow then
                "Expired"

            else if isActiveRow then
                "Now viewing"

            else
                ""

        actionLabel =
            if isError then
                ""

            else if isEmbedErrorRow then
                "Retry"

            else if isLoadingRow then
                "Opening…"

            else if isExpiredRow then
                "View"

            else if isActiveRow then
                "Refresh"

            else
                "View"

        -- The SAME precedence chain as `rowState`, projected as a stable token instead of a
        -- display string. Manifest v2 keys its badge `variant` (→ `data-state`) and its per-row
        -- `$cond` text/action chains on this token, so the host owns no CloudShield display
        -- strings. `rowState`/`actionLabel` above stay projected for manifest v1 until the demo VM
        -- redeploys v2 — one source of truth, two projections, so they can never disagree.
        state =
            if isError then
                "failed"

            else if isEmbedErrorRow then
                "error"

            else if isLoadingRow then
                "opening"

            else if isExpiredRow then
                "expired"

            else if isActiveRow then
                "viewing"

            else
                "idle"
    in
    Encode.object
        [ -- The row's unique §4.2 result id: `repeat.key`, the View press's selector, and what
          -- every session state above matches on. `batchId` stays projected beside it for the
          -- shared-batch subLabel and for a manifest that still keys on it.
          ( "resultId", Encode.string resultId )
        , ( "batchId", Encode.string entry.batchId )
        , ( "targetName", Encode.string entry.targetName )
        , ( "completedAt", Encode.string completedAtLabel )
        , ( "subLabel", Encode.string subLabel )
        , ( "status", Encode.string entry.status )
        , ( "countsLabel", Encode.string (Transport.countsLabel entry.counts) )
        , ( "findings", findingsFromCounts entry.counts )
        , ( "rowState", Encode.string rowState )
        , ( "actionLabel", Encode.string actionLabel )
        , ( "state", Encode.string state )
        ]


{-| Synthesize a findings-shaped array from a row's aggregate `counts` so the per-row
`FindingsTable` renders the same palette-driven severity pills as the live `/results` view: one
`{ "severity": <sev> }` object per counted finding, which the table groups and counts back into
pills. This keeps history pills byte-consistent with results pills without a second pill
renderer or any renderer conditional. Cost is O(total findings) per row; history is display-
capped and scan counts are modest, so the projected state stays small.
-}
findingsFromCounts : Transport.Counts -> Encode.Value
findingsFromCounts counts =
    let
        entriesFor severity n =
            List.repeat n (Encode.object [ ( "severity", Encode.string severity ) ])
    in
    Encode.list identity
        (entriesFor "critical" counts.critical
            ++ entriesFor "high" counts.high
            ++ entriesFor "medium" counts.medium
            ++ entriesFor "low" counts.low
            ++ entriesFor "info" counts.info
        )


{-| Project one scan-target row. Besides its display state, the row carries the three signals a stop
action needs, all decided host-side from the live wire state:

  - `cancellable` — this row can be stopped. The manifest binds the stop control's presence /
    `disabled` to it rather than re-deriving the answer from `scanState` strings, which is
    display-composed (`"scanning · 1:07"`) and would be guesswork.
  - `cancelRequestId` — the §4.1 request id that a stop has to name, `""` on every row without a run
    on the wire (`cancelTargetOf` treats an empty id as "no request", which is what routes a queued
    row's press to the tail-removal path instead of the cancel channel).
  - `cancelTargetId` — the instance the press is ABOUT. It exists because a target still sitting in
    the batch tail has no request on the wire to name, and is therefore unnameable by
    `cancelRequestId` alone: that is the whole reason a queued row could not be stopped at all.
    `""` on a row that cannot be stopped, the same fail-closed idiom.

The row's display state has four sources, and the precedence between them is a statement about time:
a pending stop refines what the wire says, the wire says what is happening NOW, the batch tail says
what is about to happen, and the card's own `scanState` says what already happened.

1.  **A pending stop wins** (`stoppingTargetId`). It is not a competing state but a REFINEMENT of the
    live one: the publisher is still running the run the wire describes, and has additionally been
    asked to abandon it. It outranks the live state because that is the more specific fact, and
    because leaving the live state on top is precisely the bug — the row kept reading
    `scanning · 0:54` through a stop the user had already pressed, so the user pressed it again.
2.  **Then the live run** (`statusOverride`, which the host projects onto exactly the tracked
    request's row). It is the only one of these read from the wire this poll.
3.  **Then the undrained tail** (`queuedTargets`): a target whose request has not been written yet
    is `queued`, whoever decided that — this session or the batch record a reload restored. This
    beats `scanState` deliberately: a target that is in the tail AND carries a terminal state from
    an earlier run is about to be scanned again, so `queued` is the truthful badge and its finished
    run is in the history panel either way.
4.  **Then the card's durable state**, which is `settleScanState`'s record of runs this session
    watched finish, defaulting to `idle`.

-}
instanceProjection : ViewConfig -> Model -> Instance -> Encode.Value
instanceProjection config model instance =
    let
        stopping =
            config.stoppingTargetId == Just instance.id

        -- This row is in the undrained batch tail: its request is decided but unwritten.
        queuedInTail =
            List.member instance.id config.queuedTargets

        scanState =
            if stopping then
                "stopping"

            else
                case liveStateOf config instance of
                    Just state ->
                        state

                    Nothing ->
                        if queuedInTail then
                            "queued"

                        else
                            localScanState model instance.id

        cancelRequestId =
            case config.cancellableRun of
                Just run ->
                    if run.targetId == instance.id then
                        run.requestId

                    else
                        ""

                Nothing ->
                    ""

        -- Stoppable either because a live run on this row can still be abandoned, or because the row
        -- is waiting its turn at the single §7.1 request slot and can simply be taken out of the
        -- queue. A `stopping` row is neither: the host drops it from `cancellableRun` the moment the
        -- stop is pending, and a tail member never has a run to stop in the first place.
        cancellable =
            cancelRequestId /= "" || queuedInTail
    in
    Encode.object
        [ ( "id", Encode.string instance.id )
        , ( "name", Encode.string instance.name )
        , ( "selected", Encode.bool (Set.member instance.id model.selection) )
        , ( "scanState", Encode.string scanState )
        , ( "cancellable", Encode.bool cancellable )
        , ( "cancelRequestId", Encode.string cancelRequestId )
        , ( "cancelTargetId"
          , Encode.string
                (if cancellable then
                    instance.id

                 else
                    ""
                )
          )
        ]


{-| The live run's state as it applies to ONE row: the host projects `statusOverride` onto exactly
the tracked request's target, so every other row reads `Nothing` and falls through to the rest of the
precedence chain in [`instanceProjection`](#instanceProjection).
-}
liveStateOf : ViewConfig -> Instance -> Maybe String
liveStateOf config instance =
    config.statusOverride
        |> Maybe.andThen
            (\override ->
                if override.targetId == instance.id then
                    Just override.state

                else
                    Nothing
            )


localScanState : Model -> String -> String
localScanState model id =
    Dict.get id model.scanState |> Maybe.withDefault "idle"



-- VIEW


{-| The card's json-render manifest as resolved by the host from the wire. There is no built-in
fallback manifest (the frozen `cardJson` is gone): the body arrives only over a `store=swift`
object fetch or metadata-mode chunks.

  - `ManifestLoading` — the sentinel is present but the body has not resolved yet (the object
    fetch is in flight / not yet requested). The card region shows quiet host loading chrome.
  - `ManifestReady body` — a resolved manifest body to decode (fail-closed by the renderer; an
    undecodable body still shows the error notice, never a partial tree).
  - `ManifestUnavailable` — the body could not be resolved (fetch errored, or over the transport
    cap). The card region shows a muted "unavailable" line, never a blank region.

-}
type ManifestSource
    = ManifestLoading
    | ManifestReady String
    | ManifestUnavailable


{-| What the host hands the card view: the publishing instance's display name (for the
provenance marker, §5.2), the wire-resolved manifest ([`ManifestSource`](#ManifestSource)), a
short transport label for the header chip, and the scan-timer descriptor for the
active/completed elapsed line.
-}
type alias ViewConfig =
    { -- Whether the host has a persisted approval for the publishing instance (matched by its
      -- UUID). When False the card shows only its opt-in affordance; when True it renders the
      -- extension. The card keeps no approval state of its own — this is the whole gate.
      approved : Bool
    , sourceName : String
    , manifest : ManifestSource

    -- a short, non-technical label for how the manifest was transported (e.g. "server
    -- metadata"), rendered as a muted chip in the card *header* by the host, not in the body.
    -- `Nothing` hides the chip.
    , transportLabel : Maybe String

    -- the tracked scan's completion descriptor. The *running* elapsed now lives on the scanning
    -- target row (projected into `statusOverride` as `"scanning · m:ss"`), so this only drives the
    -- brief "Scan completed in m:ss" confirmation line: while the run is active `doneDurationSec` is
    -- `Nothing` (nothing drawn here — the row carries the progress); once the run is `done` it holds
    -- the full wall-clock flow duration (`completedAt - startMillis`), never the scanner-only
    -- `summary.durationSec` (which reads faster than the flow the user watched). `startMillis` is the
    -- run's wall-clock start (the request seq). `Nothing`, or a `Nothing` `doneDurationSec`, draws no
    -- confirmation line.
    , scanTimer :
        Maybe
            { startMillis : Int
            , doneDurationSec : Maybe Int
            }

    -- A muted host-drawn line for transport warnings/errors outside the sandboxed manifest.
    , transportWarning : Maybe String

    -- the display state of the in-flight run, projected onto its target row (left column),
    -- overriding the optimistic local scanState. Read from the polled status object (§4.3); while
    -- the run is `running` the host composes the counting-up elapsed into it (`"scanning · m:ss"`),
    -- so the scanning row itself is the primary live-progress signal — independent of whether a
    -- history row is being viewed on the right. `queued`/`done`/terminal states pass through raw.
    , statusOverride :
        Maybe
            { targetId : String
            , state : String
            }

    -- the ids of targets whose request the host has decided on but not yet written: the undrained
    -- tail of the batch being paced through the single §7.1 request slot. They read `queued`, which
    -- is what a session that started the batch shows optimistically anyway — the difference is that
    -- this survives a reload, because the tail is durable and the optimistic state is not. Empty
    -- when no batch is draining.
    , queuedTargets : List String

    -- the §4.2 result's `findings[]` array (host-parsed from the polled result object),
    -- bound into the `FindingsTable` at `/results`. `Nothing` until a run is `done`.
    , results : Maybe Encode.Value

    -- the archived-scan history rows (the bridge's `results/index.json`) plus the first-fetch
    -- state. Once `loaded` is true the stale rows stay visible during refreshes; first-fetch
    -- loading hides the count and lets host CSS draw the loading line in the history column.
    , history :
        { rows : List Transport.IndexEntry
        , loading : Bool
        , loaded : Bool
        }

    -- the §4.2 resultId whose results/embed are currently on screen (the picked history row, else
    -- the just-completed live scan). The row with this resultId renders as the single "Now viewing"
    -- state (accent stripe + tint, action button flips View -> Refresh). `Nothing` when nothing
    -- is being viewed. All four of these are resultIds, never batch ids: §2.2 siblings share a
    -- batch, so a batch id would flag every sibling row at once.
    , activeResultId : Maybe String

    -- the resultId of the getEmbed that is genuinely in flight right now (`EmbedLoading`), so its
    -- history row shows the "Opening…" loading state and its action button de-emphasizes. It wins
    -- over `activeResultId`: while a getEmbed is pending NO row shows "Now viewing" (the clicked row
    -- supersedes the previously-active one immediately). `Nothing` once the request resolves,
    -- errors, or times out, so no row is ever wedged in the loading state.
    , pendingResultId : Maybe String

    -- the resultId of the last getEmbed that failed or timed out (`EmbedError`), retained until a
    -- new getEmbed starts or one succeeds. Its history row reads a danger "Couldn't open" state
    -- with a "Retry" action (re-firing getEmbed for it), and while it is set NO row falsely shows
    -- "Now viewing". Also drives the results-region "Retry" affordance. `Nothing` when no embed is
    -- in an error state.
    , erroredResultId : Maybe String

    -- the resultId whose result session has EXPIRED (`EmbedExpired`, from `Lifecycle.OpenStale`).
    -- Its history row reverts from "Now viewing"/"Refresh" to a muted "Expired" badge + plain
    -- "View" (reopen), keeping only a faint highlight. The host sets this INSTEAD of
    -- `activeResultId` for the expired result, so the expired session no longer reads as active.
    -- `Nothing` when the open session (if any) is still fresh. This is the expired-session fix.
    , expiredResultId : Maybe String

    -- whether the host would SWALLOW an `exoext.openSession` press right now — the §7.1
    -- single-req-slot guard (`ServerDetail.exoextGetEmbedBlocked`) protecting an active or
    -- unclaimed scan. Projected to `/requestBusy` so the manifest can disable the row action
    -- instead of the press vanishing with no feedback.
    , requestBusy : Bool

    -- whether the host would SWALLOW an `exoext.writeRequest` (scan) press right now — the OTHER
    -- §7.1 guard (`ServerDetail.exoextScanBlocked`: a tracked run that has not settled, or a batch
    -- still draining). Projected to `/scanBusy` so the Scan controls grey instead of the press
    -- disappearing. Kept distinct from `requestBusy` above, which is the View guard: the two are
    -- true at different moments, so one flag driving both controls would grey a working button and
    -- leave a dead one live.
    , scanBusy : Bool

    -- the ONE target row whose run can still be stopped, and the §4.1 request id a stop must name
    -- (`ServerDetail.exoextCancellableRun`). §7.1 admits one request per publishing VM, so there is
    -- at most one. Projected per row as `cancellable` / `cancelRequestId`; `Nothing` when no run is
    -- stoppable, which also covers a run whose cancel this host has already written.
    , cancellableRun :
        Maybe
            { targetId : String
            , requestId : String
            }

    -- the row whose stop has been asked for and not yet answered
    -- (`ServerDetail.exoextStoppingTarget`, derived from the wire's cancel channel so it survives a
    -- reload). Projected as that row's `scanState` token `"stopping"`, which the manifest turns into
    -- a word; the host states only that the run is between the press and the publisher's answer.
    -- `Nothing` when no stop is outstanding.
    , stoppingTargetId : Maybe String

    -- whether a result session is currently ON SCREEN (findings and/or a mounted iframe), so the
    -- manifest can show its close affordance only when there is something to close. False once
    -- dismissed, which is what makes the close control disappear along with the pane it closed.
    , sessionOpen : Bool

    -- the iframe origin allowlist, derived host-side from the instance's own floating IPs.
    -- The renderer emits an `<iframe>` only for a `src` whose origin is an exact member of
    -- this list; it is the whole safety boundary for the catalog's origin-pinned Iframe.
    , allowedIframeOrigins : List String

    -- the bridge result body's `embedUrl`, projected into the render state at top-level
    -- `/embedUrl` so the catalog `Iframe` can bind its `src` to it. `""` (self-hiding
    -- placeholder) until a run is `done` and the result carries an embed URL.
    , embedUrl : String

    -- the host-computed state of the history-View embed flow, drawn as a quiet affordance line
    -- beside the results/iframe area (same slot discipline as `scanTimer`/`transportWarning`).
    -- It gates whether the iframe is mounted: only `EmbedReady` carries a live `embedUrl`; every
    -- other state emits `embedUrl == ""` so the origin-pinned Iframe self-hides (the expired-token
    -- resource-drain fix). `EmbedIdle`/`EmbedReady` draw no line.
    , embedState : EmbedState

    -- DEMO-ONLY: when set, the card shows a collapsible panel embedding the *real* CloudShield
    -- web UI from this URL in a raw, unpinned host-chrome iframe (no origin allowlist). This is
    -- separate from the catalog's origin-pinned Iframe element and is for the program-officer
    -- demo only. `Nothing` disables the panel entirely.
    , demoIframeUrl : Maybe String
    }


{-| The host-computed state of a history-View embed round-trip, decided in
`Page.ServerDetail.exoextEmbedProjection` from (the pending getEmbed marker, the res-slot
embed result, and the shared client clock). It drives the quiet affordance line and gates the
iframe mount.

  - `EmbedIdle` — nothing in flight and no embed result to show; draws no line.
  - `EmbedLoading` — a getEmbed is in flight (pending set, no matching result yet).
  - `EmbedError` — the matching result reported `status == "error"`, or the request timed out.
  - `EmbedExpired` — an ok result whose `embedExpiresAt` is at/before the client clock; the
    iframe is unmounted so an expired CloudShield+Clerk app can't keep spinning on auth retries.
  - `EmbedReady` — an ok, unexpired result; the iframe shows it.

-}
type EmbedState
    = EmbedIdle
    | EmbedLoading
    | EmbedError String
    | EmbedExpired
    | EmbedReady


{-| Render the card with its host trust chrome. The whole thing is mounted inside
Exosphere's elm-ui tree via `Element.html`.
-}
view : ExoPalette -> Time.Zone -> ViewConfig -> List Instance -> Model -> Element.Element Msg
view palette zone config instances model =
    if config.approved then
        -- The card is now a two-column desktop layout (scan targets | scan history, with the
        -- results region full-width below), so it wants room. Cap the whole column at ~1300px so it
        -- doesn't stretch absurdly on ultra-wide displays; below that it fills the page column and
        -- the CSS grid stacks to one column on narrow widths. The provenance bar, rendered manifest,
        -- and the muted host lines all share this one contained surface.
        Element.column
            [ Element.width (Element.fill |> Element.maximum 1300), Element.spacing spacer.px8 ]
            [ provenanceMarker palette config.sourceName
            , rendererView palette zone config instances model
            , embedStateView palette config.embedState config.erroredResultId
            , transportWarningView palette config.transportWarning
            , scanTimerView palette config.scanTimer
            , demoIframePanel palette config.demoIframeUrl model.showDemoIframe
            , disableAffordance palette
            ]

    else
        optInAffordance palette config.sourceName


transportWarningView : ExoPalette -> Maybe String -> Element.Element Msg
transportWarningView palette warning =
    case warning of
        Just label ->
            Element.el
                [ Text.fontSize Text.Small
                , Font.color (SH.toElementColor palette.neutral.text.subdued)
                ]
                (Text.body label)

        Nothing ->
            Element.none


{-| The host-drawn state line for the full-width results region below the columns. It owns the
empty / loading / error affordance for the embed flow — the catalog `Iframe` now renders NOTHING
for an empty `src` (`renderIframe`), so this line is the single source of truth for "no iframe yet"
and never the renderer's old fail-closed "Embedded content is unavailable" placeholder.

  - `EmbedIdle` — a gentle muted hint ("Select a scan to view its results.") so an untouched card
    reads as ready, not broken.
  - `EmbedLoading` — a spinner + "Opening scan results…" (in addition to the per-row "Opening…"
    badge), so the region itself shows progress rather than sitting blank.
  - `EmbedError` — a danger-toned "Couldn't open these results." with a **Retry** affordance that
    re-fires getEmbed for the last-attempted result (`erroredResultId`).
  - `EmbedExpired` — a gentle prompt to reopen (the iframe was unmounted so the dead CloudShield
    app stops spinning on auth retries).
  - `EmbedReady` — nothing; the iframe is on screen.

-}
embedStateView : ExoPalette -> EmbedState -> Maybe String -> Element.Element Msg
embedStateView palette embedState erroredResultId =
    let
        mutedLine tone label =
            Element.el
                [ Text.fontSize Text.Small
                , Font.color (SH.toElementColor tone)
                ]
                (Text.body label)
    in
    case embedState of
        EmbedIdle ->
            mutedLine palette.neutral.text.subdued
                "Select a scan to view its results."

        EmbedReady ->
            Element.none

        EmbedLoading ->
            Element.row
                [ Element.spacing spacer.px8 ]
                [ spinner palette
                , mutedLine palette.neutral.text.subdued "Opening scan results…"
                ]

        EmbedError message ->
            Element.row
                [ Element.spacing spacer.px8 ]
                [ mutedLine palette.danger.textOnNeutralBG
                    ("Couldn't open these results: " ++ message ++ ".")
                , case erroredResultId of
                    Just resultId ->
                        linkButton palette "Retry" (GotRetryEmbed resultId)

                    Nothing ->
                        Element.none
                ]

        EmbedExpired ->
            mutedLine palette.neutral.text.subdued
                "This results session expired. Click View to reopen."


{-| A small CSS-driven spinner ring for the results-region loading line, tinted to the subdued
neutral text so it reads as quiet host chrome (matches the per-row "Opening…" ring idiom).
The raw span is wrapped in a sized `Element.el` because elm-ui's row `spacing` only applies
between its own wrapped children — a bare `Element.html` child gets no gap (and no measured
box), which rendered the ring flush against the label.
-}
spinner : ExoPalette -> Element.Element msg
spinner palette =
    Element.el
        [ Element.width (Element.px 12)
        , Element.height (Element.px 12)
        ]
    <|
        Element.html
            (Html.node "span"
                [ Html.Attributes.style "display" "inline-block"
                , Html.Attributes.style "width" "12px"
                , Html.Attributes.style "height" "12px"
                , Html.Attributes.style "border" ("2px solid " ++ Color.toCssString palette.neutral.text.subdued)
                , Html.Attributes.style "border-top-color" "transparent"
                , Html.Attributes.style "border-radius" "50%"
                , Html.Attributes.style "animation" "jr-badge-spin 0.7s linear infinite"
                ]
                []
            )


{-| A muted, host-drawn confirmation line under the rendered manifest, shown only once the run is
`done`: `Scan completed in m:ss`, frozen to the full wall-clock flow. The _running_ progress lives
on the scanning target row (see `scanningRowLabel`), so this draws nothing while a run is active or
when no frozen duration is available (`doneDurationSec == Nothing`).
-}
scanTimerView : ExoPalette -> Maybe { startMillis : Int, doneDurationSec : Maybe Int } -> Element.Element Msg
scanTimerView palette timer =
    case timer |> Maybe.andThen .doneDurationSec of
        Just secs ->
            Element.el
                [ Text.fontSize Text.Small
                , Font.color (SH.toElementColor palette.neutral.text.subdued)
                ]
                (Text.body ("Scan completed in " ++ formatElapsed secs))

        Nothing ->
            Element.none


{-| The scanning target row's counting-up label, `"scanning · m:ss"`, elapsed since the run's
wall-clock start (`startMillis`, the request seq stamped with `Time.posixToMillis`). The host
composes this into the row's `scanState` while the run is `running`, so the live progress reads on
the row and is never suppressed by a concurrent history view. Wall-clock, so it stays consistent
with the frozen `Scan completed in …` value (`completedAt - startMillis`).
-}
scanningRowLabel : Int -> Int -> String
scanningRowLabel startMillis nowMillis =
    "scanning · " ++ formatElapsed (elapsedSeconds startMillis nowMillis)


{-| Whole seconds between a wall-clock `startMillis` and `nowMillis`. `startMillis` is the request
seq, which the host stamps with wall-clock millis. For the ~1 frame before that stamp lands it is
still the small optimistic counter, which would read as an absurd elapsed; treat any non-epoch
start (or a negative diff) as 0 until the real timestamp arrives.
-}
elapsedSeconds : Int -> Int -> Int
elapsedSeconds startMillis nowMillis =
    let
        diffMs =
            nowMillis - startMillis
    in
    if startMillis < 1000000000000 || diffMs < 0 then
        0

    else
        diffMs // 1000


{-| Format a whole-second count as `m:ss` (e.g. 125 -> "2:05").
-}
formatElapsed : Int -> String
formatElapsed totalSeconds =
    let
        minutes =
            totalSeconds // 60

        seconds =
            modBy 60 totalSeconds
    in
    String.fromInt minutes ++ ":" ++ String.padLeft 2 '0' (String.fromInt seconds)


{-| A small, muted "transport" chip for the card header (host chrome, drawn outside the
sandboxed manifest). Polymorphic in the message type because it carries no events.
-}
transportChip : ExoPalette -> String -> Element.Element msg
transportChip palette label =
    Element.el
        [ Element.centerY
        , Element.paddingXY spacer.px8 spacer.px4
        , Background.color (SH.toElementColor palette.neutral.background.frontLayer)
        , Border.width 1
        , Border.color (SH.toElementColor palette.neutral.border)
        , Border.rounded 4
        , Text.fontSize Text.Tiny
        , Font.color (SH.toElementColor palette.neutral.text.subdued)
        ]
        (Element.text label)


rendererView :
    ExoPalette
    -> Time.Zone
    -> ViewConfig
    -> List Instance
    -> Model
    -> Element.Element Msg
rendererView palette zone config instances model =
    case config.manifest of
        ManifestReady manifestJson ->
            -- Decode per render is fine for the small card; the fail-closed decoder is the security
            -- gate (an off-catalog or oversized manifest yields the error stub, never a partial tree).
            case JsonRender.decodeString manifestJson of
                Ok spec ->
                    -- Fill the card width: `Element.html` shrinks to content by default, which squeezes the
                    -- rendered manifest (and the results iframe) into a narrow column. `width fill` on the
                    -- elm-ui wrapper plus `width: 100%` on the div lets it use the full available width.
                    Element.el [ Element.width Element.fill ]
                        (Element.html
                            (Html.div
                                [ Html.Attributes.style "width" "100%"
                                , Html.Attributes.attribute "data-exoext-history-loading"
                                    (if config.history.loading && not config.history.loaded then
                                        "true"

                                     else
                                        "false"
                                    )
                                ]
                                [ rendererStyle palette
                                , Html.map RendererMsg (Render.view config.allowedIframeOrigins spec (projection zone config instances model) model.renderer)
                                ]
                            )
                        )

                Err message ->
                    manifestErrorView palette config.sourceName message model.showManifestErrorDetail

        ManifestLoading ->
            -- The sentinel is present but the manifest body has not resolved yet: quiet host chrome
            -- (the same sized-spinner idiom as the results-region loading line), never a blank card.
            Element.el [ Element.width Element.fill ]
                (Element.row
                    [ Element.spacing spacer.px8 ]
                    [ spinner palette
                    , Element.el
                        [ Text.fontSize Text.Small
                        , Font.color (SH.toElementColor palette.neutral.text.subdued)
                        ]
                        (Text.body "Loading extension UI…")
                    ]
                )

        ManifestUnavailable ->
            -- The manifest body could not be resolved (fetch errored, or over the transport cap; the
            -- specific reason rides the separate host transportWarning line). A muted line here keeps
            -- the region explained and non-alarming, never blank and never a stale built-in card.
            Element.el [ Element.width Element.fill ]
                (Element.el
                    [ Text.fontSize Text.Small
                    , Font.color (SH.toElementColor palette.neutral.text.subdued)
                    ]
                    (Text.body "Extension UI is unavailable.")
                )


{-| What the researcher sees when a manifest is REFUSED by the fail-closed decoder.

The decoder's diagnostic is written for whoever is debugging the publisher: a JSON dump wrapped in
decoder prose. Putting that on screen told a researcher nothing they could act on and read as the
product breaking, so it is now two tiers. The plain-language sentence names the one thing that
differs between the two failures a researcher can do anything about, and the raw diagnostic moves
behind a collapsed "Technical details" toggle so it stays one click from whoever wants it.

  - [`Spec.UnknownCatalogSurface`](JsonRender-Spec#ErrorKind) — the manifest asked for catalog
    surface this build does not have, so this Exosphere is simply older than the extension.
    Updating is a real, honest thing to suggest.
  - [`Spec.Malformed`](JsonRender-Spec#ErrorKind) — nothing suggests a newer renderer would help,
    so the message says the refusal is deliberate and points at the publisher.

The toggle follows this file's own expand idiom (`demoIframePanel`): a `linkButton` over a `Bool`
in the card model, not a `<details>` island. Exosphere ships no disclosure widget in
`Style.Widgets`, and the surrounding card chrome is elm-ui, so this keeps one idiom in one file.

-}
manifestErrorView : ExoPalette -> String -> String -> Bool -> Element.Element Msg
manifestErrorView palette sourceName message detailExpanded =
    let
        ( title, body ) =
            case Spec.errorKind message of
                Spec.UnknownCatalogSurface ->
                    ( "This extension needs a newer Exosphere"
                    , "The \""
                        ++ sourceName
                        ++ "\" VM published interface features this version of Exosphere doesn't support yet. Updating Exosphere may fix this."
                    )

                Spec.Malformed ->
                    ( "This extension published an interface Exosphere can't render"
                    , "Refusing to display it is a safety feature. The extension may have a bug; consider notifying its publisher."
                    )
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing spacer.px8
        , Element.padding spacer.px12
        , Border.width 1
        , Border.color (SH.toElementColor palette.neutral.border)
        , Border.rounded 4
        ]
        [ Element.paragraph [ Font.bold ] [ Text.body title ]
        , Element.paragraph
            [ Text.fontSize Text.Small
            , Font.color (SH.toElementColor palette.neutral.text.subdued)
            ]
            [ Text.body body ]
        , linkButton palette
            ((if detailExpanded then
                "▾ "

              else
                "▸ "
             )
                ++ "Technical details"
            )
            GotToggleManifestErrorDetail
        , if detailExpanded then
            -- The raw decoder output, in the codebase's monospace-code idiom. Height-bounded and
            -- scrollable so a long diagnostic cannot push the rest of the page away, and a
            -- `paragraph` so it wraps: elm-ui puts `white-space: pre` on a bare `el`, which would
            -- send one long decoder line off the side of the card.
            Element.el
                ([ Element.width Element.fill
                 , Element.height (Element.shrink |> Element.maximum 220)
                 , Element.scrollbarY
                 , Element.padding spacer.px8
                 ]
                    ++ Code.codeAttrs palette
                )
                (Element.paragraph [ Text.fontSize Text.Small ] [ Element.text message ])

          else
            Element.none
        ]


{-| §5.2 provenance marker — host-drawn, naming the source instance and stating that the UI
was published by a VM, not by Exosphere. Its text comes from the host/envelope, never the
manifest's `ui` body, and it is not suppressible by the manifest.
-}
provenanceMarker : ExoPalette -> String -> Element.Element Msg
provenanceMarker palette sourceName =
    Element.row
        [ Element.spacing spacer.px8
        , Element.padding spacer.px8
        , Element.width Element.fill
        , Background.color (SH.toElementColor palette.neutral.background.frontLayer)
        , Border.width 1
        , Border.color (SH.toElementColor palette.neutral.border)
        , Border.rounded 4
        ]
        [ Element.el
            [ Element.padding spacer.px4
            , Border.width 1
            , Border.color (SH.toElementColor palette.neutral.border)
            , Border.rounded 999
            , Font.bold
            , Text.fontSize Text.Small
            ]
            (Element.text "?")
        , Element.paragraph
            [ Text.fontSize Text.Small
            , Font.color (SH.toElementColor palette.neutral.text.subdued)
            ]
            [ Text.body ("Published by the \"" ++ sourceName ++ "\" VM — not verified by Exosphere.")
            ]
        ]


{-| The §5.3 opt-in gate: nothing a publisher wrote is rendered until the researcher says so.

Host chrome, and therefore extension-agnostic on purpose. This sentence is Exosphere speaking to
its own user about a decision Exosphere is asking them to make, and it is shown before the manifest
has been trusted at all — so it must not repeat any name the VM chose for itself. The publishing
VM's name is the one identifier here, and it is quoted because it is untrusted data.

-}
optInAffordance : ExoPalette -> String -> Element.Element Msg
optInAffordance palette sourceName =
    Element.column
        [ Element.spacing spacer.px8
        , Element.padding spacer.px12
        , Element.width Element.fill
        , Border.width 1
        , Border.color (SH.toElementColor palette.neutral.border)
        , Border.rounded 4
        ]
        [ Element.paragraph []
            [ Text.body
                ("The VM “" ++ sourceName ++ "” offers an extension UI. Extensions are off until you enable them. Enabling is remembered for this VM; you can forget it any time.")
            ]
        , linkButton palette "Enable this extension" GotApprove
        ]


{-| DEMO-ONLY panel embedding the real CloudShield web UI in a raw, unpinned iframe.

This is a clearly-marked demo embed for the program-officer visit: it embeds a host-provided
URL directly with no origin allowlist, so it lives in host chrome rather than the sandboxed
manifest, and is gated behind `ViewConfig.demoIframeUrl` (set only on the experimental-flag
path). The catalog's own `Iframe` element is origin-pinned; this demo panel deliberately is
not, which is why it stays out of the manifest.

-}
demoIframePanel : ExoPalette -> Maybe String -> Bool -> Element.Element Msg
demoIframePanel palette maybeUrl expanded =
    case maybeUrl of
        Nothing ->
            Element.none

        Just url ->
            Element.column
                [ Element.width Element.fill, Element.spacing spacer.px8 ]
                (linkButton palette
                    ((if expanded then
                        "▾ Hide"

                      else
                        "▸ Show"
                     )
                        ++ " CloudShield live UI (demo only, raw unpinned iframe outside the sandboxed manifest)"
                    )
                    GotToggleDemoIframe
                    :: (if expanded then
                            [ Element.el
                                [ Text.fontSize Text.Small
                                , Font.color (SH.toElementColor palette.warning.textOnNeutralBG)
                                ]
                                (Text.body ("DEMO ONLY, not production. Embedding " ++ url ++ " directly in a raw, unpinned host-chrome iframe (no origin allowlist), separate from the catalog's origin-pinned Iframe element."))
                            , Element.html
                                (Html.iframe
                                    [ Html.Attributes.src url
                                    , Html.Attributes.style "width" "100%"
                                    , Html.Attributes.style "height" "520px"
                                    , Html.Attributes.style "border" "1px solid rgba(255,255,255,0.2)"
                                    , Html.Attributes.style "border-radius" "4px"
                                    , Html.Attributes.attribute "sandbox" "allow-scripts allow-same-origin allow-forms"
                                    , Html.Attributes.title "CloudShield live UI (demo)"
                                    ]
                                    []
                                )
                            ]

                        else
                            []
                       )
                )


disableAffordance : ExoPalette -> Element.Element Msg
disableAffordance palette =
    Element.el [ Element.alignRight ]
        (linkButton palette "Forget this extension" GotForget)


linkButton : ExoPalette -> String -> Msg -> Element.Element Msg
linkButton palette label msg =
    Element.el
        [ Font.color (SH.toElementColor palette.primary)
        , Font.underline
        , Element.pointer
        , Element.padding spacer.px4
        , Element.htmlAttribute (Html.Attributes.style "cursor" "pointer")
        , Element.htmlAttribute (Html.Attributes.attribute "role" "button")
        , Element.Events.onClick msg
        ]
        (Text.body label)



-- A self-contained stylesheet for the renderer's jr-* classes, generated from the active
-- `ExoPalette` so the vendored renderer looks native inside Exosphere in BOTH light and dark
-- themes (colors are pulled from the palette's neutral/state families, not hardcoded). Scoped
-- to .jr-* names.


rendererStyle : ExoPalette -> Html.Html Msg
rendererStyle palette =
    let
        c =
            Color.toCssString

        text =
            c palette.neutral.text.default

        muted =
            c palette.neutral.text.subdued

        border =
            c palette.neutral.border

        frontBg =
            c palette.neutral.background.frontLayer

        primary =
            c palette.primary

        -- State families: the palette's own tinted `background`/`border`/`textOnColoredBG`
        -- read correctly on both light and dark pages, so badges/pills stay legible either way.
        infoBg =
            c palette.info.background

        infoText =
            c palette.info.textOnColoredBG

        infoBorder =
            c palette.info.border

        successBg =
            c palette.success.background

        successText =
            c palette.success.textOnColoredBG

        successBorder =
            c palette.success.border

        dangerBg =
            c palette.danger.background

        dangerText =
            c palette.danger.textOnColoredBG

        dangerBorder =
            c palette.danger.border

        warningBg =
            c palette.warning.background

        warningText =
            c palette.warning.textOnColoredBG

        warningBorder =
            c palette.warning.border

        -- Solid severity-dot colors for the findings pills.
        dangerDot =
            c palette.danger.default

        warningDot =
            c palette.warning.default

        infoDot =
            c palette.info.default

        neutralDot =
            c palette.muted.default

        -- Interactive/active accent, derived ENTIRELY from the Exosphere primary (never a
        -- hardcoded hue): a faint fill, a stronger fill, and a hairline. `color-mix` keeps these
        -- correct in both themes because `primary` itself flips with the palette.
        primaryTint =
            "color-mix(in srgb, " ++ primary ++ " 10%, transparent)"

        primaryTintStrong =
            "color-mix(in srgb, " ++ primary ++ " 16%, transparent)"

        primaryLine =
            "color-mix(in srgb, " ++ primary ++ " 55%, transparent)"

        -- Danger accent for the embed-error ("Couldn't open") history row, mirroring the primary
        -- tint/line idiom so the failed-to-open surface reads in the same visual language as the
        -- active/loading rows, just danger-toned. `color-mix` keeps both correct in either theme.
        dangerTint =
            "color-mix(in srgb, " ++ dangerDot ++ " 9%, transparent)"

        dangerLine =
            "color-mix(in srgb, " ++ dangerDot ++ " 55%, transparent)"
    in
    Html.node "style"
        []
        [ Html.text
            (String.join "\n"
                [ ".jr-root { font-family: inherit; color: " ++ text ++ "; }"
                , ".jr-card { display: flex; flex-direction: column; gap: 10px; padding: 4px 0; }"
                , ".jr-card__title { font-size: 1.05em; margin: 0 0 4px 0; font-weight: 600; }"
                , ".jr-stack { display: flex; gap: 10px; }"
                , ".jr-stack--row { flex-direction: row; align-items: center; }"
                , ".jr-stack--col { flex-direction: column; align-items: stretch; }"
                , ".jr-text { }"
                , ".jr-button { padding: 4px 12px; border: 1px solid " ++ border ++ "; border-radius: 4px; background: " ++ frontBg ++ "; color: " ++ text ++ "; cursor: pointer; font-size: 0.9em; }"
                , ".jr-button:hover { border-color: " ++ primary ++ "; color: " ++ primary ++ "; }"

                -- A `disabled` action (today: a row's View while the §7.1 request slot is busy).
                -- Dimmed and not-allowed rather than hidden, so the control stays where the eye
                -- expects it and the press reads as temporarily unavailable, not missing. The hover
                -- rule is re-neutralized because `:hover` still fires over a disabled button.
                , ".jr-button--disabled, .jr-button--disabled:hover { opacity: 0.45; cursor: not-allowed; border-color: " ++ border ++ "; color: " ++ muted ++ "; }"
                , ".jr-checkbox { display: inline-flex; align-items: center; gap: 6px; }"
                , ".jr-checkbox input { accent-color: " ++ primary ++ "; }"
                , ".jr-badge { padding: 1px 9px; border-radius: 999px; font-size: 0.8em; border: 1px solid transparent; }"

                -- In-progress badges (queued/running/scanning/stopping) get a small spinning ring
                -- before the label so an active scan reads as moving. `currentColor` inherits the
                -- badge tone color. `stopping` belongs here for a reason the others do not have: it
                -- is the ONLY feedback a stop press gets, because the stop control withdraws with
                -- the press. Drawn still, the row would read as already stopped while the publisher
                -- is in fact still winding the run down, and a still-running row that looks finished
                -- is what gets pressed a second time. Prefix selectors, so a state carrying a
                -- display suffix (`scanning · 0:15`) or an ellipsis (`stopping…`) still matches.
                --
                -- `flex: none` is what keeps the ring ROUND. Every round box in this stylesheet is a
                -- fixed-size pseudo-element sitting inside a badge or row that is itself a flex
                -- container, so it is a flex ITEM: its default `flex-shrink: 1` lets the main axis
                -- (here the width) be compressed when the line runs out of room, while the cross
                -- axis keeps its declared size, and a circle compressed on one axis is an oval.
                -- Nothing about `border-radius: 50%` prevents that — 50% of an oval is an oval.
                -- `.jr-findings__dot` already carries this for the same reason; the rings did not,
                -- and they are the elements the researcher watches while a scan runs.
                , ".jr-badge[data-state^=\"queued\"]::before, .jr-badge[data-state^=\"running\"]::before, .jr-badge[data-state^=\"scanning\"]::before, .jr-badge[data-state^=\"stopping\"]::before { content: \"\"; display: inline-block; flex: none; width: 10px; height: 10px; margin-right: 5px; vertical-align: -1px; border: 2px solid currentColor; border-top-color: transparent; border-radius: 50%; animation: jr-badge-spin 0.7s linear infinite; }"
                , "@keyframes jr-badge-spin { to { transform: rotate(360deg); } }"

                -- `scanning` is THIS extension's word for the phase where the clone is being read,
                -- composed by `scanningRowLabel`. The renderer's tone table knows only the §4.4 run
                -- states, so the word falls through to the neutral tone there — which would read as
                -- settled on the one row that is actively working. The adapter tones its own
                -- vocabulary here instead, matching `.jr-badge--info` exactly, which is the whole
                -- reason the generic layer does not need an arm for it.
                , ".jr-badge[data-state^=\"scanning\"] { background: " ++ infoBg ++ "; color: " ++ infoText ++ "; border-color: " ++ infoBorder ++ "; }"
                , ".jr-badge--neutral { background: " ++ frontBg ++ "; color: " ++ muted ++ "; border-color: " ++ border ++ "; }"
                , ".jr-badge--info { background: " ++ infoBg ++ "; color: " ++ infoText ++ "; border-color: " ++ infoBorder ++ "; }"
                , ".jr-badge--success { background: " ++ successBg ++ "; color: " ++ successText ++ "; border-color: " ++ successBorder ++ "; }"
                , ".jr-badge--danger { background: " ++ dangerBg ++ "; color: " ++ dangerText ++ "; border-color: " ++ dangerBorder ++ "; }"

                -- Findings summary: a single clean row of severity pills (dot + count + label),
                -- ordered by severity in the renderer; the iframe below is the rich view.
                , ".jr-findings { display: flex; flex-wrap: wrap; align-items: center; gap: 8px; }"
                , ".jr-findings--empty { color: " ++ muted ++ "; font-size: 0.9em; font-style: italic; }"
                , ".jr-findings__total { color: " ++ muted ++ "; font-size: 0.85em; font-weight: 600; margin-right: 2px; }"
                , ".jr-findings__pill { display: inline-flex; align-items: center; gap: 6px; padding: 2px 10px; border-radius: 999px; background: " ++ frontBg ++ "; border: 1px solid " ++ border ++ "; font-size: 0.85em; }"
                , ".jr-findings__dot { width: 8px; height: 8px; border-radius: 50%; background: " ++ neutralDot ++ "; flex: none; }"
                , ".jr-findings__count { font-weight: 700; color: " ++ text ++ "; }"
                , ".jr-findings__label { color: " ++ muted ++ "; text-transform: capitalize; }"
                , ".jr-findings__pill--critical .jr-findings__dot, .jr-findings__pill--high .jr-findings__dot { background: " ++ dangerDot ++ "; }"
                , ".jr-findings__pill--medium .jr-findings__dot { background: " ++ warningDot ++ "; }"
                , ".jr-findings__pill--low .jr-findings__dot { background: " ++ infoDot ++ "; }"
                , ".jr-findings__pill--info .jr-findings__dot { background: " ++ neutralDot ++ "; }"

                -- Table: a plain data grid. Header rule is heavier than row rules; all lines use
                -- the neutral border so it reads as quiet structure, not a colored callout.
                , ".jr-table { width: 100%; border-collapse: collapse; font-size: 0.85em; }"
                , ".jr-table__header { text-align: left; font-weight: 600; color: " ++ muted ++ "; padding: 6px 10px; border-bottom: 2px solid " ++ border ++ "; }"
                , ".jr-table__cell { padding: 6px 10px; color: " ++ text ++ "; border-bottom: 1px solid " ++ border ++ "; }"

                -- Alert: a tinted callout box, one tone per severity. Same bg/text/border palette
                -- families as the badges so tones stay consistent across the card in both themes.
                , ".jr-alert { padding: 10px 12px; border-radius: 6px; border: 1px solid transparent; font-size: 0.9em; }"
                , ".jr-alert__title { display: block; font-weight: 600; margin-bottom: 3px; }"
                , ".jr-alert__message { display: block; line-height: 1.45; }"
                , ".jr-alert--info { background: " ++ infoBg ++ "; color: " ++ infoText ++ "; border-color: " ++ infoBorder ++ "; }"
                , ".jr-alert--warning { background: " ++ warningBg ++ "; color: " ++ warningText ++ "; border-color: " ++ warningBorder ++ "; }"
                , ".jr-alert--danger { background: " ++ dangerBg ++ "; color: " ++ dangerText ++ "; border-color: " ++ dangerBorder ++ "; }"

                -- Iframe chrome: the provenance bar reads as quiet host chrome (subdued text on
                -- the front layer), and the frame carries a distinct-but-neutral border so the
                -- embed is visibly framed as third-party without alarming. Works in both themes.
                , ".jr-iframe { display: flex; flex-direction: column; }"
                , ".jr-iframe__provenance { padding: 5px 10px; font-size: 0.78em; color: " ++ muted ++ "; background: " ++ frontBg ++ "; border: 1px solid " ++ border ++ "; border-bottom: 0; border-radius: 6px 6px 0 0; }"
                , ".jr-iframe__frame { border: 1px solid " ++ border ++ "; border-radius: 0 0 6px 6px; overflow: hidden; }"

                -- REDESIGN v2 — a TWO-COLUMN desktop layout. Provenance (above) and the results
                -- region (below) are host chrome outside the manifest; INSIDE the manifest the card
                -- is a two-column row (scan targets | scan history) followed by the full-width
                -- results findings + iframe. Everything below styles a FIXED renderer DOM (no
                -- manifest class hooks); the one per-row hook is the rowState Badge's `data-state`,
                -- which `:has()` reads to style the whole row.
                --
                -- Selector anchoring — the columns row, the toolbar, and the history rows are ALL
                -- Stack rows, so we key strictly on depth from `.jr-card`:
                --   COLUMNS = `.jr-card > .jr-stack--row`  (the ONLY row that is a direct card child)
                --   TARGETS = COLUMNS `> .jr-stack--col:nth-child(1)`
                --   HISTORY = COLUMNS `> .jr-stack--col:nth-child(2)`
                --   scroll  = TARGETS/HISTORY `> .jr-stack--col`  (the repeat container in a column)
                , ".jr-card { gap: 16px; }"

                -- The two-column grid: targets narrower than history (history rows carry more —
                -- date, target, pills, state, action). On a narrow card it collapses to one column
                -- (see the @media at the end). A hairline divider runs down the gutter.
                , ".jr-card > .jr-stack--row { display: grid; grid-template-columns: 5fr 7fr; gap: 0; align-items: stretch; }"
                , ".jr-card > .jr-stack--row > .jr-stack--col { min-width: 0; gap: 8px; }"
                , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(1) { padding-right: 26px; }"
                , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) { padding-left: 26px; border-left: 1px solid " ++ border ++ "; }"

                -- Column headers: an uppercase muted rubric carrying the live count. The history
                -- overflow note ("showing latest 20 of M") is the history column's 2nd Text child.
                , ".jr-card > .jr-stack--row > .jr-stack--col > .jr-text:first-child { text-transform: uppercase; letter-spacing: 0.07em; font-size: 0.72em; font-weight: 700; color: " ++ muted ++ "; }"
                , ".jr-card > .jr-stack--row > .jr-stack--col > .jr-text:nth-child(2) { font-size: 0.78em; color: " ++ muted ++ "; margin-top: -2px; }"

                -- The scroll areas: BOTH the targets list and the history rows are height-bounded and
                -- scroll internally, so the two columns stay EQUAL height and can never be lopsided
                -- regardless of item counts. Grid `align-items: stretch` grows the shorter column's
                -- scroll area to match the taller; the taller one caps at ~6 rows and scrolls. This
                -- is the key requirement (the researcher may have "unlimited" instances and scans).
                , ".jr-card > .jr-stack--row > .jr-stack--col > .jr-stack--col { flex: 1 1 auto; max-height: 16rem; overflow-y: auto; gap: 2px; }"
                , "[data-exoext-history-loading=\"true\"] .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col { min-height: 3rem; display: flex; flex-direction: row; align-items: center; gap: 8px; padding: 9px 10px; color: " ++ muted ++ "; background: " ++ frontBg ++ "; border-radius: 8px; }"
                , "[data-exoext-history-loading=\"true\"] .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col::before { content: \"\"; display: inline-block; flex: none; width: 12px; height: 12px; border: 2px solid currentColor; border-top-color: transparent; border-radius: 50%; animation: jr-badge-spin 0.7s linear infinite; }"
                , "[data-exoext-history-loading=\"true\"] .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col::after { content: \"Loading history…\"; font-size: 0.9em; }"

                -- Scan-target rows (TARGETS scroll rows): the name grows; the Scan button sits right.
                , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(1) > .jr-stack--col > .jr-stack--row { align-items: center; gap: 10px; padding: 7px 10px; border-radius: 8px; }"
                , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(1) > .jr-stack--col > .jr-stack--row:hover { background: " ++ frontBg ++ "; }"
                , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(1) > .jr-stack--col > .jr-stack--row > .jr-text { flex: 1; min-width: 0; font-weight: 500; }"

                -- Toolbar (Select all | Scan selected): the targets column's own row child; the
                -- button pushes to the right edge.
                , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(1) > .jr-stack--row { align-items: center; }"
                , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(1) > .jr-stack--row .jr-button { margin-left: auto; }"

                -- History rows (HISTORY scroll rows): a two-line main block (when + sub) that grows,
                -- then pills, then the state badge, then the action.
                , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row { align-items: center; gap: 12px; padding: 9px 10px; border-radius: 9px; border-left: 3px solid transparent; }"
                , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:hover { background: " ++ frontBg ++ "; }"
                , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row > .jr-stack--col { flex: 1; min-width: 0; gap: 1px; }"
                , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row > .jr-stack--col > .jr-text:first-child { font-size: 0.95em; font-weight: 600; color: " ++ text ++ "; }"
                , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row > .jr-stack--col > .jr-text:last-child { font-size: 0.82em; color: " ++ muted ++ "; }"
                , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) .jr-findings { flex: 0 0 auto; }"
                , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) .jr-findings__total { display: none; }"

                -- The single per-row state hook. Empty rowState => hidden badge (the common case,
                -- manifest v1). Manifest v2 projects the idle token instead of an empty string, so
                -- the twin hides the idle history badge too — scoped to the history column so it
                -- can't hide a targets-column "idle" scanState badge (which stays visible).
                , ".jr-badge[data-state=\"\"] { display: none; }"
                , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row > .jr-badge[data-state=\"idle\"] { display: none; }"

                -- Active / now-viewing row: accent stripe + primary tint + a pulsing flag, action
                -- flips to Refresh. Keyed on the history Badge's data-state. Each rule carries BOTH
                -- selectors: the manifest-v1 display string ("Now viewing") and the manifest-v2
                -- token ("viewing", from the Badge `variant`). The token twins are history-scoped
                -- (never a bare global) so they cannot collide with a targets-column scanState. Both
                -- selector sets live side by side until the demo VM redeploys v2.
                , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"Now viewing\"]), .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"viewing\"]) { background: " ++ primaryTint ++ "; border-left-color: " ++ primary ++ "; box-shadow: inset 0 0 0 1px " ++ primaryLine ++ "; }"
                , ".jr-badge[data-state=\"Now viewing\"], .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row > .jr-badge[data-state=\"viewing\"] { display: inline-flex; align-items: center; gap: 5px; background: " ++ primaryTintStrong ++ "; color: " ++ primary ++ "; border-color: " ++ primaryLine ++ "; text-transform: uppercase; letter-spacing: 0.05em; font-size: 0.7em; font-weight: 700; }"
                , ".jr-badge[data-state=\"Now viewing\"]::before, .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row > .jr-badge[data-state=\"viewing\"]::before { content: \"\"; flex: none; width: 6px; height: 6px; border-radius: 50%; background: " ++ primary ++ "; animation: jr-viewing-pulse 2s ease-in-out infinite; }"
                , "@keyframes jr-viewing-pulse { 0%, 100% { opacity: 1; } 50% { opacity: 0.4; } }"
                , "@media (prefers-reduced-motion: reduce) { .jr-badge[data-state=\"Now viewing\"]::before, .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row > .jr-badge[data-state=\"viewing\"]::before { animation: none; } }"
                , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"Now viewing\"]) .jr-button, .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"viewing\"]) .jr-button { background: transparent; border-color: " ++ primaryLine ++ "; color: " ++ primary ++ "; }"

                -- Opening / loading row: this row's getEmbed is in flight. Same accent surface as
                -- "Now viewing" so the clicked row reads as the one taking over, but the badge carries
                -- a spinning ring (the codebase's in-progress idiom, shared with queued/running) and
                -- the action button is de-emphasized and non-interactive (pointer-events: none) while
                -- the bridge mints the fresh embed.
                , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"Opening…\"]), .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"opening\"]) { background: " ++ primaryTint ++ "; border-left-color: " ++ primaryLine ++ "; }"
                , ".jr-badge[data-state=\"Opening…\"], .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row > .jr-badge[data-state=\"opening\"] { display: inline-flex; align-items: center; gap: 6px; background: " ++ primaryTintStrong ++ "; color: " ++ primary ++ "; border-color: " ++ primaryLine ++ "; text-transform: uppercase; letter-spacing: 0.05em; font-size: 0.7em; font-weight: 700; }"
                , ".jr-badge[data-state=\"Opening…\"]::before, .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row > .jr-badge[data-state=\"opening\"]::before { content: \"\"; display: inline-block; flex: none; width: 9px; height: 9px; border: 2px solid currentColor; border-top-color: transparent; border-radius: 50%; animation: jr-badge-spin 0.7s linear infinite; }"
                , "@media (prefers-reduced-motion: reduce) { .jr-badge[data-state=\"Opening…\"]::before, .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row > .jr-badge[data-state=\"opening\"]::before { animation: none; } }"
                , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"Opening…\"]) .jr-button, .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"opening\"]) .jr-button { background: transparent; border-color: " ++ primaryLine ++ "; color: " ++ primary ++ "; opacity: 0.6; pointer-events: none; }"

                -- Embed-error row: this batch's last getEmbed failed / timed out. Danger-toned badge
                -- + stripe/tint (mirroring the primary active/loading idiom, just danger-colored), and
                -- the action flips to a danger-outlined \"Retry\" that re-fires getEmbed. Distinct from
                -- a FAILED SCAN below (which has nothing to view): here the scan is fine, only the
                -- results session failed to open, so the button stays and reads \"Retry\".
                , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"Couldn't open\"]), .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"error\"]) { background: " ++ dangerTint ++ "; border-left-color: " ++ dangerDot ++ "; }"
                , ".jr-badge[data-state=\"Couldn't open\"], .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row > .jr-badge[data-state=\"error\"] { display: inline-flex; align-items: center; gap: 5px; background: " ++ dangerBg ++ "; color: " ++ dangerText ++ "; border-color: " ++ dangerBorder ++ "; text-transform: uppercase; letter-spacing: 0.04em; font-size: 0.7em; font-weight: 700; }"
                , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"Couldn't open\"]) .jr-button, .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"error\"]) .jr-button { background: transparent; border-color: " ++ dangerLine ++ "; color: " ++ dangerDot ++ "; }"
                , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"Couldn't open\"]) .jr-button:hover, .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"error\"]) .jr-button:hover { border-color: " ++ dangerDot ++ "; }"

                -- Failed scan row: de-emphasized, a small danger 'failed' pill, and no pills/action
                -- (nothing to view). The host also blanks its actionLabel; this hides the button.
                , ".jr-badge[data-state=\"failed\"] { background: " ++ dangerBg ++ "; color: " ++ dangerText ++ "; border-color: " ++ dangerBorder ++ "; text-transform: uppercase; letter-spacing: 0.04em; font-size: 0.7em; font-weight: 700; }"
                , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"failed\"]) { opacity: 0.72; }"
                , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"failed\"]) .jr-button { display: none; }"
                , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"failed\"]) .jr-findings { display: none; }"

                -- Expired session row (the expired-session fix): the row's result session lapsed, so
                -- the host reverts it from "Now viewing"/"Refresh" to a muted neutral "Expired" badge
                -- and a plain View (reopen). It keeps a FAINT highlight — the same palette-derived
                -- primary accent as the active row, just at a lower alpha — so the researcher still
                -- sees which scan they were on. Neutral toned (like the failed pill but not danger).
                , ".jr-badge[data-state=\"Expired\"], .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row > .jr-badge[data-state=\"expired\"] { background: " ++ frontBg ++ "; color: " ++ muted ++ "; border-color: " ++ border ++ "; text-transform: uppercase; letter-spacing: 0.05em; font-size: 0.7em; font-weight: 700; }"
                , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"Expired\"]), .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"expired\"]) { background: color-mix(in srgb, " ++ primary ++ " 5%, transparent); border-left-color: " ++ primaryLine ++ "; }"
                , ".jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"Expired\"]) .jr-button, .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"expired\"]) .jr-button { background: transparent; border-color: " ++ border ++ "; color: " ++ muted ++ "; }"

                -- Responsive: on a narrow card the two columns stack (targets, then history), each
                -- keeping its own bounded scroll. The divider moves from a left border to a top one.
                , "@media (max-width: 720px) { .jr-card > .jr-stack--row { grid-template-columns: 1fr; } .jr-card > .jr-stack--row > .jr-stack--col:nth-child(1) { padding-right: 0; padding-bottom: 14px; } .jr-card > .jr-stack--row > .jr-stack--col:nth-child(2) { padding-left: 0; border-left: 0; border-top: 1px solid " ++ border ++ "; padding-top: 14px; } }"
                , ".jr-confirm { position: fixed; inset: 0; background: rgba(0,0,0,0.55); display: flex; align-items: center; justify-content: center; z-index: 1000; }"

                -- `white-space: normal` re-establishes wrapping inside the dialog: the card is raw
                -- HTML embedded in an elm-ui tree, and elm-ui sets `white-space: pre` on every `el`,
                -- which inherits into the overlay — an unwrapped message overflows the box.
                , ".jr-confirm__box { background: " ++ frontBg ++ "; color: " ++ text ++ "; padding: 20px 22px; border-radius: 8px; max-width: 380px; white-space: normal; border: 1px solid " ++ border ++ "; box-shadow: 0 8px 40px rgba(0,0,0,0.5); }"
                , ".jr-confirm__title { margin: 0 0 8px 0; font-size: 1.1em; font-weight: 600; }"
                , ".jr-confirm__message { margin: 0; color: " ++ muted ++ "; line-height: 1.45; }"
                , ".jr-confirm__actions { display: flex; gap: 8px; justify-content: flex-end; margin-top: 18px; }"
                , ".jr-confirm__cancel, .jr-confirm__confirm { padding: 6px 16px; border-radius: 4px; cursor: pointer; font-size: 0.9em; }"
                , ".jr-confirm__cancel { background: transparent; color: " ++ text ++ "; border: 1px solid " ++ border ++ "; }"
                , ".jr-confirm__cancel:hover { background: " ++ frontBg ++ "; }"
                , ".jr-confirm__confirm { background: " ++ primary ++ "; color: #fff; border: 1px solid " ++ primary ++ "; }"
                , ".jr-confirm__confirm:hover { filter: brightness(1.08); }"
                , ".jr-error, .jr-error-stub { border: 1px solid " ++ dangerBorder ++ "; padding: 8px 10px; border-radius: 4px; color: " ++ dangerText ++ "; background: " ++ dangerBg ++ "; }"
                ]
            )
        ]
