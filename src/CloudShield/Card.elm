module CloudShield.Card exposing (EmbedState(..), Instance, Model, Msg, OutMsg(..), ViewConfig, cardJson, init, projection, requestEmbed, transportChip, update, view)

{-| Host wiring for the CloudShield dynamic-UI card (Phase 1, browser side).

This is the Exosphere-side **host** for the vendored native Elm json-render renderer
(`JsonRender.*`). It owns the json-render `state`, projects it for the renderer
(`bnr/spike/renderer/host-renderer-interface.md` §1.2), handles the renderer's `Effect`s
(start scans, apply checkbox write-backs), and draws the host-only trust chrome — the
provenance "?" marker (§5.2) and the opt-in affordance (§5.3) — around the rendered card.

**Milestone status (M1).** The card is rendered from a hand-fed, frozen `card.json`
(`cardJson` below) against a hand-fed fixture instance list. Selection, select-all, and the
confirm dialog are fully live; a confirmed `startScan` flips the targeted rows to `queued`
locally so the action round-trip is visible. Real discovery (read the `exoext.v1.kind`
metadata sentinel + project the eligible `$instances` from `project.servers`) is M2, and the
real metadata/console POC transport (write the scan-request, poll status) is M3. Those swaps
do not touch the renderer wiring here.

Everything here is gated by `context.experimentalFeaturesEnabled` at the call site
(`Page.ServerDetail`), so normal operation is unaffected.

-}

import CloudShield.Transport as Transport
import Color
import Dict exposing (Dict)
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Helpers.Time
import Html
import Html.Attributes
import Json.Decode as Decode
import Json.Encode as Encode
import JsonRender
import JsonRender.Render as Render
import Set exposing (Set)
import Style.Helpers as SH
import Style.Types exposing (ExoPalette)
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Spinner as Spinner
import Style.Widgets.Text as Text
import Time



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

    -- the in-flight request (§7.1 single-in-flight): which seq maps to which target, so the
    -- host can project the polled run.state onto the right row.
    , pending : Maybe { seq : Int, targetId : String }

    -- DEMO-ONLY: whether the embedded CloudShield live-UI iframe is expanded. This is a raw,
    -- unpinned host-chrome embed, distinct from the catalog's origin-pinned Iframe element.
    -- Off by default; toggled by a link, only when ViewConfig.demoIframeUrl is set.
    , showDemoIframe : Bool
    }


type Msg
    = GotApprove
    | GotForget
    | GotToggleDemoIframe
    | RendererMsg Render.Msg


{-| What the card asks its parent (`Page.ServerDetail`) to do. The card itself never issues
OpenStack Cmds; the parent owns the Nova metadata write (it has `Rest.Nova` + the project).

  - `ScanRequested { seq, targetIds }` — a confirmed `cloudshield.startScan`. The parent
    re-resolves the targets (§5.4), encodes the §4.1 request, and writes the §7.1 req-slot
    on the CloudShield VM's metadata.

  - `EmbedRequested { batchId }` — a `cloudshield.getEmbed` on a history row. The parent
    stamps a wall-clock seq, encodes the `getEmbed` request, and writes the same §7.1 req-slot.

-}
type OutMsg
    = ScanRequested { seq : Int, targetIds : List String }
    | EmbedRequested { batchId : String }
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
            -- The host re-checks the verb against its own allowlist; an off-list verb is
            -- ignored (fail-closed).
            if action.verb == "cloudshield.startScan" then
                requestScan (targetsOf model action.params) model

            else if action.verb == "cloudshield.getEmbed" then
                requestEmbed (batchIdOf action.params) model

            else
                ( model, Nothing )

        Just (Render.EmitStateChange change) ->
            ( applyStateChange instances change model, Nothing )


{-| A confirmed `startScan`: bump the seq, flip the targeted rows to `queued` optimistically
(dedup-aware), record the in-flight correlation (first target — §7.1 single-in-flight), and
ask the parent to write the §7.1 req-slot. No targets ⇒ no-op.
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
                , pending = Just { seq = seq, targetId = firstTarget }
                , scanState = startScan targets model.scanState
              }
            , Just (ScanRequested { seq = seq, targetIds = targets })
            )


{-| A `cloudshield.getEmbed` on a history row: ask the parent to mint a fresh embed URL for the
named archived scan. The card holds no run-state knowledge, so it does not guard here — the §7.1
single-req-slot guard (don't disturb an active or unclaimed scan) is applied host-side in
`Page.ServerDetail`, derived from the live wire metadata. A missing `batchId` ⇒ no-op.
-}
requestEmbed : Maybe String -> Model -> ( Model, Maybe OutMsg )
requestEmbed maybeBatchId model =
    case maybeBatchId of
        Just batchId ->
            ( model, Just (EmbedRequested { batchId = batchId }) )

        Nothing ->
            ( model, Nothing )


{-| Resolve a `cloudshield.getEmbed` `batchId` param from the renderer's already-resolved
params (the `$item.batchId` of the pressed history row).
-}
batchIdOf : Encode.Value -> Maybe String
batchIdOf params =
    Decode.decodeValue (Decode.field "batchId" Decode.string) params
        |> Result.toMaybe


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


projection : Time.Zone -> Maybe String -> List Transport.IndexEntry -> Maybe Encode.Value -> String -> Maybe { targetId : String, state : String } -> List Instance -> Model -> Encode.Value
projection zone activeBatchId history results embedUrl statusOverride instances model =
    let
        total =
            List.length history

        -- Newest first (the index is append-only, oldest first), capped to the latest N.
        shown =
            List.reverse history |> List.take historyDisplayCap

        historyNote =
            if total <= historyDisplayCap then
                ""

            else
                "Showing the latest "
                    ++ String.fromInt historyDisplayCap
                    ++ " of "
                    ++ String.fromInt total
                    ++ " scans."
    in
    Encode.object
        [ ( "selectAll", Encode.bool model.selectAll )
        , ( "results", Maybe.withDefault Encode.null results )
        , ( "embedUrl", Encode.string embedUrl )
        , ( "instances", Encode.list (instanceProjection statusOverride model) instances )
        , ( "history", Encode.list (historyRow zone activeBatchId) shown )
        , ( "historyNote", Encode.string historyNote )
        ]


{-| Project one history row for the `/history` render state. The index is append-only
(oldest first), so the render state reverses it to newest-first.

The host computes every per-row visual signal here, so the renderer stays a pure catalog with
no conditionals (host-renderer-interface.md §1.2):

  - `completedAt` — humanized local timestamp ("Jul 20, 2026 · 10:52 PM"), not raw ISO.
  - `subLabel` — the target + short batch id ("alpha · #84a1c6"), or "failed · no findings" for
    an errored scan.
  - `findings` — a findings-shaped array synthesized from the aggregate counts, bound into the
    row's `FindingsTable` so history pills match the live `/results` pills.
  - `rowState` — the one active/failed hook the stylesheet keys on: "Now viewing" for the row
    the embed/results are showing, "failed" for an errored scan, "" (hidden badge) otherwise.
  - `actionLabel` — the View/Refresh flip ("Refresh" for the active row, "" for a failed scan
    that has nothing to view).

`countsLabel` is retained (unbound by the current manifest) as a stable plain-text fallback.

-}
historyRow : Time.Zone -> Maybe String -> Transport.IndexEntry -> Encode.Value
historyRow zone activeBatchId entry =
    let
        isError =
            entry.status == "error"

        isActiveRow =
            (activeBatchId == Just entry.batchId) && not isError

        completedAtLabel =
            case Helpers.Time.iso8601StringToPosix entry.completedAt of
                Ok posix ->
                    Helpers.Time.humanReadableDateAndTimeCompact zone posix

                Err _ ->
                    entry.completedAt

        subLabel =
            if isError then
                "failed · no findings"

            else
                entry.targetName ++ " · #" ++ String.left 6 entry.batchId

        rowState =
            if isError then
                "failed"

            else if isActiveRow then
                "Now viewing"

            else
                ""

        actionLabel =
            if isError then
                ""

            else if isActiveRow then
                "Refresh"

            else
                "View"
    in
    Encode.object
        [ ( "batchId", Encode.string entry.batchId )
        , ( "targetName", Encode.string entry.targetName )
        , ( "completedAt", Encode.string completedAtLabel )
        , ( "subLabel", Encode.string subLabel )
        , ( "status", Encode.string entry.status )
        , ( "countsLabel", Encode.string (Transport.countsLabel entry.counts) )
        , ( "findings", findingsFromCounts entry.counts )
        , ( "rowState", Encode.string rowState )
        , ( "actionLabel", Encode.string actionLabel )
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


instanceProjection : Maybe { targetId : String, state : String } -> Model -> Instance -> Encode.Value
instanceProjection statusOverride model instance =
    let
        scanState =
            case statusOverride of
                Just override ->
                    if override.targetId == instance.id then
                        override.state

                    else
                        localScanState model instance.id

                Nothing ->
                    localScanState model instance.id
    in
    Encode.object
        [ ( "id", Encode.string instance.id )
        , ( "name", Encode.string instance.name )
        , ( "selected", Encode.bool (Set.member instance.id model.selection) )
        , ( "scanState", Encode.string scanState )
        ]


localScanState : Model -> String -> String
localScanState model id =
    Dict.get id model.scanState |> Maybe.withDefault "idle"



-- VIEW


{-| What the host hands the card view: the publishing instance's display name (for the
provenance marker, §5.2), the resolved manifest JSON (from POC transport when discovered,
else the embedded frozen `cardJson` fallback), a short transport label for the header chip,
and the scan-timer descriptor for the active/completed elapsed line.
-}
type alias ViewConfig =
    { -- Whether the host has a persisted approval for the publishing instance (matched by its
      -- UUID). When False the card shows only its opt-in affordance; when True it renders the
      -- extension. The card keeps no approval state of its own — this is the whole gate.
      approved : Bool
    , sourceName : String
    , manifestJson : String

    -- a short, non-technical label for how the manifest was transported (e.g. "server
    -- metadata"), rendered as a muted chip in the card *header* by the host, not in the body.
    -- `Nothing` hides the chip.
    , transportLabel : Maybe String

    -- the tracked scan's elapsed-timer descriptor. `startMillis` is the run's wall-clock start
    -- (the request seq, which the host sets to `Time.posixToMillis`); while the run is active
    -- `doneDurationSec` is `Nothing` (the timer counts up from `startMillis` against the shared
    -- clock); once the run is `done` it holds the authoritative scan duration (from the result
    -- body's `summary.durationSec`) so the line freezes. `Nothing` hides the line entirely.
    , scanTimer : Maybe { startMillis : Int, doneDurationSec : Maybe Int }

    -- A muted host-drawn line for transport warnings/errors outside the sandboxed manifest.
    , transportWarning : Maybe String

    -- the authoritative live state of the in-flight run, read from the polled status object
    -- (§4.3) and projected onto its target row, overriding the optimistic local scanState.
    , statusOverride : Maybe { targetId : String, state : String }

    -- the §4.2 result's `findings[]` array (host-parsed from the polled result object),
    -- bound into the `FindingsTable` at `/results`. `Nothing` until a run is `done`.
    , results : Maybe Encode.Value

    -- the archived-scan history rows (the bridge's `results/index.json`), projected to the
    -- render state at `/history` (newest first). Empty when there is no history (metadata
    -- store, or nothing archived yet).
    , history : List Transport.IndexEntry

    -- the batchId whose results/embed are currently on screen (the picked history row, else the
    -- just-completed live scan). The row with this batchId renders as the single "Now viewing"
    -- state (accent stripe + tint, action button flips View -> Refresh). `Nothing` when nothing
    -- is being viewed.
    , activeBatchId : Maybe String

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
`Page.ServerDetail.cloudShieldEmbedProjection` from (the pending getEmbed marker, the res-slot
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
view : ExoPalette -> Time.Zone -> Time.Posix -> ViewConfig -> List Instance -> Model -> Element.Element Msg
view palette zone currentTime config instances model =
    if config.approved then
        Element.column
            [ Element.width Element.fill, Element.spacing spacer.px8 ]
            [ provenanceMarker palette config.sourceName
            , rendererView palette zone config.activeBatchId config.allowedIframeOrigins config.manifestJson config.history config.results config.embedUrl config.statusOverride instances model
            , embedStateView palette config.embedState
            , transportWarningView palette config.transportWarning
            , scanTimerView palette currentTime config.scanTimer
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


{-| A quiet, host-drawn line beside the results/iframe area that gives the history-View flow
visible feedback: a spinner while a getEmbed is in flight, an error-toned line if it failed or
timed out, and a gentle prompt when an embed token has expired (the iframe having been
unmounted by the host so the dead CloudShield app stops spinning). `EmbedReady`/`EmbedIdle`
draw nothing. Same muted-line idiom as `transportWarningView`/`scanTimerView`.
-}
embedStateView : ExoPalette -> EmbedState -> Element.Element Msg
embedStateView palette embedState =
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
            Element.none

        EmbedReady ->
            Element.none

        EmbedLoading ->
            Element.row
                [ Element.spacing spacer.px8 ]
                [ Element.el [ Element.centerY ] (Spinner.sized 16 palette)
                , mutedLine palette.neutral.text.subdued "Opening scan results…"
                ]

        EmbedError message ->
            mutedLine palette.danger.textOnNeutralBG
                ("Couldn't open these results: " ++ message ++ ". Try again.")

        EmbedExpired ->
            mutedLine palette.neutral.text.subdued
                "This results session expired. Click View to reopen."


{-| A muted, host-drawn line under the rendered manifest that gives the scan visible progress:
`Scanning… m:ss` while a run is active (counting up from the run's start against the shared
clock), frozen to `Scan completed in m:ss` once the run is `done`. `Nothing` hides it.
-}
scanTimerView : ExoPalette -> Time.Posix -> Maybe { startMillis : Int, doneDurationSec : Maybe Int } -> Element.Element Msg
scanTimerView palette currentTime timer =
    case timer of
        Just { startMillis, doneDurationSec } ->
            let
                label =
                    case doneDurationSec of
                        Just secs ->
                            "Scan completed in " ++ formatElapsed secs

                        Nothing ->
                            let
                                diffMs =
                                    Time.posixToMillis currentTime - startMillis

                                -- `startMillis` is the request seq, which the host stamps with
                                -- wall-clock millis. For the ~1 frame before that stamp lands it
                                -- is still the small optimistic counter, which would read as an
                                -- absurd elapsed; treat any non-epoch start (or a negative diff)
                                -- as 0:00 until the real timestamp arrives.
                                elapsedSec =
                                    if startMillis < 1000000000000 || diffMs < 0 then
                                        0

                                    else
                                        diffMs // 1000
                            in
                            "Scanning… " ++ formatElapsed elapsedSec
            in
            Element.el
                [ Text.fontSize Text.Small
                , Font.color (SH.toElementColor palette.neutral.text.subdued)
                ]
                (Text.body label)

        Nothing ->
            Element.none


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


rendererView : ExoPalette -> Time.Zone -> Maybe String -> List String -> String -> List Transport.IndexEntry -> Maybe Encode.Value -> String -> Maybe { targetId : String, state : String } -> List Instance -> Model -> Element.Element Msg
rendererView palette zone activeBatchId allowedIframeOrigins manifestJson history results embedUrl statusOverride instances model =
    -- Decode per render is fine for the small card; the fail-closed decoder is the security
    -- gate (an off-catalog or oversized manifest yields the error stub, never a partial tree).
    case JsonRender.decodeString manifestJson of
        Ok spec ->
            -- Fill the card width: `Element.html` shrinks to content by default, which squeezes the
            -- rendered manifest (and the results iframe) into a narrow column. `width fill` on the
            -- elm-ui wrapper plus `width: 100%` on the div lets it use the full available width.
            Element.el [ Element.width Element.fill ]
                (Element.html
                    (Html.div [ Html.Attributes.style "width" "100%" ]
                        [ rendererStyle palette
                        , Html.map RendererMsg (Render.view allowedIframeOrigins spec (projection zone activeBatchId history results embedUrl statusOverride instances model) model.renderer)
                        ]
                    )
                )

        Err message ->
            Element.el [ Element.width Element.fill ] (Element.html (JsonRender.errorStub message))


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
                ("The VM “" ++ sourceName ++ "” offers a CloudShield extension UI. Extensions are off until you enable them. Enabling is remembered for this instance; you can forget it any time.")
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
                , ".jr-checkbox { display: inline-flex; align-items: center; gap: 6px; }"
                , ".jr-checkbox input { accent-color: " ++ primary ++ "; }"
                , ".jr-badge { padding: 1px 9px; border-radius: 999px; font-size: 0.8em; border: 1px solid transparent; }"

                -- In-progress badges (queued/running) get a small spinning ring before the label
                -- so an active scan reads as moving. `currentColor` inherits the badge tone color.
                , ".jr-badge[data-state=\"queued\"]::before, .jr-badge[data-state=\"running\"]::before { content: \"\"; display: inline-block; width: 10px; height: 10px; margin-right: 5px; vertical-align: -1px; border: 2px solid currentColor; border-top-color: transparent; border-radius: 50%; animation: jr-badge-spin 0.7s linear infinite; }"
                , "@keyframes jr-badge-spin { to { transform: rotate(360deg); } }"
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

                -- Disclosure: a native <details> styled as quiet host chrome. The summary reads as a
                -- muted, clickable section label (no default triangle fuss beyond the browser marker);
                -- the body gets a modest top gap so expanded content is not flush against the summary.
                , ".jr-disclosure { border: 0; }"
                , ".jr-disclosure__summary { cursor: pointer; font-size: 0.85em; font-weight: 600; color: " ++ muted ++ "; padding: 2px 0; user-select: none; }"
                , ".jr-disclosure__summary:hover { color: " ++ text ++ "; }"
                , ".jr-disclosure__body { margin-top: 8px; }"

                -- REDESIGN — a composed card surface, not a flat list. Everything below styles a
                -- FIXED renderer DOM (no manifest class hooks); the one per-row hook is the
                -- rowState Badge's `data-state`, which `:has()` reads to style the whole row.
                --
                -- Section rubrics: the card's standalone Text child ("Scan targets") and the
                -- history disclosure summary read as uppercase muted labels.
                , ".jr-card { gap: 14px; }"
                , ".jr-card > .jr-text { text-transform: uppercase; letter-spacing: 0.07em; font-size: 0.72em; font-weight: 700; color: " ++ muted ++ "; margin: 2px 0 -4px; }"
                , ".jr-disclosure__summary { text-transform: uppercase; letter-spacing: 0.07em; font-size: 0.72em; font-weight: 700; }"

                -- Scan-target rows: the name grows; the primary-filled Scan button sits at the right.
                , ".jr-card > .jr-stack--col > .jr-stack--row { align-items: center; gap: 10px; padding: 7px 10px; border-radius: 8px; }"
                , ".jr-card > .jr-stack--col > .jr-stack--row:hover { background: " ++ frontBg ++ "; }"
                , ".jr-card > .jr-stack--col > .jr-stack--row > .jr-text { flex: 1; min-width: 0; font-weight: 500; }"
                , ".jr-card > .jr-stack--col > .jr-stack--row .jr-button { background: " ++ primary ++ "; border-color: " ++ primary ++ "; color: #fff; }"
                , ".jr-card > .jr-stack--col > .jr-stack--row .jr-button:hover { filter: brightness(1.08); color: #fff; }"

                -- Toolbar (Select all | Scan selected): push the button to the right edge.
                , ".jr-card > .jr-stack--row { align-items: center; }"
                , ".jr-card > .jr-stack--row .jr-button { margin-left: auto; }"

                -- History rows: a two-line main block (when + sub) that grows, then pills, then action.
                , ".jr-disclosure__body > .jr-stack--col { gap: 2px; }"
                , ".jr-disclosure__body > .jr-stack--col > .jr-stack--row { align-items: center; gap: 12px; padding: 9px 10px; border-radius: 9px; border-left: 3px solid transparent; }"
                , ".jr-disclosure__body > .jr-stack--col > .jr-stack--row:hover { background: " ++ frontBg ++ "; }"
                , ".jr-disclosure__body .jr-stack--row > .jr-stack--col { flex: 1; min-width: 0; gap: 1px; }"
                , ".jr-disclosure__body .jr-stack--row > .jr-stack--col > .jr-text:first-child { font-size: 0.95em; font-weight: 600; color: " ++ text ++ "; }"
                , ".jr-disclosure__body .jr-stack--row > .jr-stack--col > .jr-text:last-child { font-size: 0.82em; color: " ++ muted ++ "; }"
                , ".jr-disclosure__body .jr-stack--row .jr-findings { flex: 0 0 auto; }"
                , ".jr-disclosure__body .jr-findings__total { display: none; }"

                -- The single per-row state hook. Empty rowState => hidden badge (the common case).
                , ".jr-badge[data-state=\"\"] { display: none; }"

                -- Active / now-viewing row: accent stripe + primary tint + a pulsing flag, action
                -- flips to Refresh. Keyed on the rowState Badge value the host projects.
                , ".jr-disclosure__body > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"Now viewing\"]) { background: " ++ primaryTint ++ "; border-left-color: " ++ primary ++ "; box-shadow: inset 0 0 0 1px " ++ primaryLine ++ "; }"
                , ".jr-badge[data-state=\"Now viewing\"] { display: inline-flex; align-items: center; gap: 5px; background: " ++ primaryTintStrong ++ "; color: " ++ primary ++ "; border-color: " ++ primaryLine ++ "; text-transform: uppercase; letter-spacing: 0.05em; font-size: 0.7em; font-weight: 700; }"
                , ".jr-badge[data-state=\"Now viewing\"]::before { content: \"\"; width: 6px; height: 6px; border-radius: 50%; background: " ++ primary ++ "; animation: jr-viewing-pulse 2s ease-in-out infinite; }"
                , "@keyframes jr-viewing-pulse { 0%, 100% { opacity: 1; } 50% { opacity: 0.4; } }"
                , "@media (prefers-reduced-motion: reduce) { .jr-badge[data-state=\"Now viewing\"]::before { animation: none; } }"
                , ".jr-disclosure__body > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"Now viewing\"]) .jr-button { background: transparent; border-color: " ++ primaryLine ++ "; color: " ++ primary ++ "; }"

                -- Failed scan row: de-emphasized, a small danger 'failed' pill, and no pills/action
                -- (nothing to view). The host also blanks its actionLabel; this hides the button.
                , ".jr-badge[data-state=\"failed\"] { background: " ++ dangerBg ++ "; color: " ++ dangerText ++ "; border-color: " ++ dangerBorder ++ "; text-transform: uppercase; letter-spacing: 0.04em; font-size: 0.7em; font-weight: 700; }"
                , ".jr-disclosure__body > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"failed\"]) { opacity: 0.72; }"
                , ".jr-disclosure__body > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"failed\"]) .jr-button { display: none; }"
                , ".jr-disclosure__body > .jr-stack--col > .jr-stack--row:has(.jr-badge[data-state=\"failed\"]) .jr-findings { display: none; }"
                , ".jr-confirm { position: fixed; inset: 0; background: rgba(0,0,0,0.55); display: flex; align-items: center; justify-content: center; z-index: 1000; }"
                , ".jr-confirm__box { background: " ++ frontBg ++ "; color: " ++ text ++ "; padding: 20px 22px; border-radius: 8px; max-width: 380px; border: 1px solid " ++ border ++ "; box-shadow: 0 8px 40px rgba(0,0,0,0.5); }"
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



-- THE FROZEN MANIFEST (bnr/spike/renderer/card.json) — hand-fed in M1; in M2 this comes
-- from discovery (metadata sentinel -> POC transport body).


cardJson : String
cardJson =
    """
{
  "root": "card",
  "elements": {
    "card": {
      "type": "Card",
      "props": {},
      "children": ["targets-label", "toolbar", "list", "history", "results", "results-embed"]
    },
    "targets-label": {
      "type": "Text",
      "props": { "value": "Scan targets" },
      "children": []
    },
    "toolbar": {
      "type": "Stack",
      "props": { "direction": "row", "gap": 2 },
      "children": ["select-all", "scan-selected"]
    },
    "select-all": {
      "type": "Checkbox",
      "props": {
        "label": "Select all",
        "checked": { "$bindState": "/selectAll" }
      },
      "children": []
    },
    "scan-selected": {
      "type": "Button",
      "props": { "label": "Scan selected" },
      "on": {
        "press": {
          "action": "cloudshield.startScan",
          "params": { "targetInstanceIds": [] },
          "confirm": {
            "title": "Scan selected instances?",
            "message": "Queue CloudShield scans for the currently selected instances.",
            "variant": "default"
          }
        }
      },
      "children": []
    },
    "list": {
      "type": "Stack",
      "props": { "direction": "col", "gap": 1 },
      "repeat": { "statePath": "/instances", "key": "id" },
      "children": ["row"]
    },
    "row": {
      "type": "Stack",
      "props": { "direction": "row", "gap": 2 },
      "children": ["row-select", "row-name", "row-status", "row-scan-btn"]
    },
    "row-select": {
      "type": "Checkbox",
      "props": {
        "checked": { "$bindItem": "selected" }
      },
      "children": []
    },
    "row-name": {
      "type": "Text",
      "props": {
        "value": { "$item": "name" }
      },
      "children": []
    },
    "row-status": {
      "type": "Badge",
      "props": {
        "value": { "$item": "scanState" }
      },
      "children": []
    },
    "row-scan-btn": {
      "type": "Button",
      "props": { "label": "Scan" },
      "on": {
        "press": {
          "action": "cloudshield.startScan",
          "params": { "targetInstanceIds": [{ "$item": "id" }] },
          "confirm": {
            "title": "Start scan",
            "message": { "$template": "Run a snapshot-clone scan of \\"${name}\\"?" },
            "variant": "default"
          }
        }
      },
      "children": []
    },
    "history": {
      "type": "Disclosure",
      "props": { "label": "Scan history", "open": true },
      "children": ["history-note", "history-rows"]
    },
    "history-note": {
      "type": "Text",
      "props": { "value": { "$state": "/historyNote" } },
      "children": []
    },
    "history-rows": {
      "type": "Stack",
      "props": { "direction": "col", "gap": 1 },
      "repeat": { "statePath": "/history", "key": "batchId" },
      "children": ["history-row"]
    },
    "history-row": {
      "type": "Stack",
      "props": { "direction": "row", "gap": 2 },
      "children": ["history-main", "history-pills", "history-state", "history-view-btn"]
    },
    "history-main": {
      "type": "Stack",
      "props": { "direction": "col", "gap": 0 },
      "children": ["history-when", "history-sub"]
    },
    "history-when": {
      "type": "Text",
      "props": { "value": { "$item": "completedAt" } },
      "children": []
    },
    "history-sub": {
      "type": "Text",
      "props": { "value": { "$item": "subLabel" } },
      "children": []
    },
    "history-pills": {
      "type": "FindingsTable",
      "props": {
        "bind": { "$item": "findings" },
        "groupBy": "severity"
      },
      "children": []
    },
    "history-state": {
      "type": "Badge",
      "props": { "value": { "$item": "rowState" } },
      "children": []
    },
    "history-view-btn": {
      "type": "Button",
      "props": { "label": { "$item": "actionLabel" } },
      "on": {
        "press": {
          "action": "cloudshield.getEmbed",
          "params": { "batchId": { "$template": "${batchId}" } }
        }
      },
      "children": []
    },
    "results": {
      "type": "FindingsTable",
      "props": {
        "bind": { "$state": "/results" },
        "groupBy": "severity"
      },
      "children": []
    },
    "results-embed": {
      "type": "Iframe",
      "props": {
        "src": { "$state": "/embedUrl" },
        "title": "CloudShield scan results"
      },
      "children": []
    }
  },
  "state": {
    "selectAll": false,
    "instances": [],
    "results": null,
    "history": [],
    "historyNote": ""
  }
}
"""
