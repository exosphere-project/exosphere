module CloudShield.Card exposing (Instance, Model, Msg, OutMsg(..), ViewConfig, cardJson, init, projection, transportChip, update, view)

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

import Dict exposing (Dict)
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
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
    { optedIn : Bool
    , renderer : Render.Model
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
    = GotEnable
    | GotDisable
    | GotToggleDemoIframe
    | RendererMsg Render.Msg


{-| What the card asks its parent (`Page.ServerDetail`) to do. The card itself never issues
OpenStack Cmds; the parent owns the Nova metadata write (it has `Rest.Nova` + the project).

  - `ScanRequested { seq, targetIds }` — a confirmed `cloudshield.startScan`. The parent
    re-resolves the targets (§5.4), encodes the §4.1 request, and writes the §7.1 req-slot
    on the CloudShield VM's metadata.

-}
type OutMsg
    = ScanRequested { seq : Int, targetIds : List String }


init : Model
init =
    { optedIn = False
    , renderer = Render.init
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
        GotEnable ->
            ( { model | optedIn = True }, Nothing )

        GotDisable ->
            ( { model | optedIn = False }, Nothing )

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


projection : Maybe Encode.Value -> String -> Maybe { targetId : String, state : String } -> List Instance -> Model -> Encode.Value
projection results embedUrl statusOverride instances model =
    Encode.object
        [ ( "selectAll", Encode.bool model.selectAll )
        , ( "results", Maybe.withDefault Encode.null results )
        , ( "embedUrl", Encode.string embedUrl )
        , ( "instances", Encode.list (instanceProjection statusOverride model) instances )
        ]


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
    { sourceName : String
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

    -- the authoritative live state of the in-flight run, read from the polled status object
    -- (§4.3) and projected onto its target row, overriding the optimistic local scanState.
    , statusOverride : Maybe { targetId : String, state : String }

    -- the §4.2 result's `findings[]` array (host-parsed from the polled result object),
    -- bound into the `FindingsTable` at `/results`. `Nothing` until a run is `done`.
    , results : Maybe Encode.Value

    -- the iframe origin allowlist, derived host-side from the instance's own floating IPs.
    -- The renderer emits an `<iframe>` only for a `src` whose origin is an exact member of
    -- this list; it is the whole safety boundary for the catalog's origin-pinned Iframe.
    , allowedIframeOrigins : List String

    -- the bridge result body's `embedUrl`, projected into the render state at top-level
    -- `/embedUrl` so the catalog `Iframe` can bind its `src` to it. `""` (self-hiding
    -- placeholder) until a run is `done` and the result carries an embed URL.
    , embedUrl : String

    -- DEMO-ONLY: when set, the card shows a collapsible panel embedding the *real* CloudShield
    -- web UI from this URL in a raw, unpinned host-chrome iframe (no origin allowlist). This is
    -- separate from the catalog's origin-pinned Iframe element and is for the program-officer
    -- demo only. `Nothing` disables the panel entirely.
    , demoIframeUrl : Maybe String
    }


{-| Render the card with its host trust chrome. The whole thing is mounted inside
Exosphere's elm-ui tree via `Element.html`.
-}
view : ExoPalette -> Time.Posix -> ViewConfig -> List Instance -> Model -> Element.Element Msg
view palette currentTime config instances model =
    if model.optedIn then
        Element.column
            [ Element.width Element.fill, Element.spacing spacer.px8 ]
            [ provenanceMarker palette config.sourceName
            , rendererView config.allowedIframeOrigins config.manifestJson config.results config.embedUrl config.statusOverride instances model
            , scanTimerView palette currentTime config.scanTimer
            , demoIframePanel palette config.demoIframeUrl model.showDemoIframe
            , disableAffordance palette
            ]

    else
        optInAffordance palette config.sourceName


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


rendererView : List String -> String -> Maybe Encode.Value -> String -> Maybe { targetId : String, state : String } -> List Instance -> Model -> Element.Element Msg
rendererView allowedIframeOrigins manifestJson results embedUrl statusOverride instances model =
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
                        [ rendererStyle
                        , Html.map RendererMsg (Render.view allowedIframeOrigins spec (projection results embedUrl statusOverride instances model) model.renderer)
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
        , Element.paragraph [ Text.fontSize Text.Small ]
            [ Text.body ("Published by the \"" ++ sourceName ++ "\" VM.")
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
                ("The VM “" ++ sourceName ++ "” offers a CloudShield extension UI. Extensions are off until you enable them.")
            ]
        , linkButton palette "Enable CloudShield extension" GotEnable
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
        (linkButton palette "Disable extension" GotDisable)


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



-- A self-contained minimal stylesheet for the renderer's jr-* classes, so the vendored
-- renderer is legible inside Exosphere without depending on app CSS. Scoped to .jr-* names.


rendererStyle : Html.Html Msg
rendererStyle =
    Html.node "style"
        []
        [ Html.text """
/* Dark theme matching Exosphere (accent #2680c2 / #53b7e2). Scoped to .jr-* names. */
.jr-root { font-family: inherit; color: inherit; }
.jr-card { display: flex; flex-direction: column; gap: 10px; padding: 4px 0; }
.jr-card__title { font-size: 1.05em; margin: 0 0 4px 0; font-weight: 600; }
.jr-stack { display: flex; gap: 10px; }
.jr-stack--row { flex-direction: row; align-items: center; }
.jr-stack--col { flex-direction: column; align-items: stretch; }
.jr-text { }
.jr-button { padding: 4px 12px; border: 1px solid rgba(255,255,255,0.25); border-radius: 4px; background: rgba(255,255,255,0.06); color: inherit; cursor: pointer; font-size: 0.9em; }
.jr-button:hover { background: rgba(83,183,226,0.18); border-color: #53b7e2; }
.jr-checkbox { display: inline-flex; align-items: center; gap: 6px; }
.jr-checkbox input { accent-color: #53b7e2; }
.jr-badge { padding: 1px 9px; border-radius: 999px; font-size: 0.8em; border: 1px solid transparent; }
/* In-progress badges (queued/running) get a small spinning ring before the label so an
   active scan reads as moving. `currentColor` inherits the badge's per-tone text color. */
.jr-badge[data-state="queued"]::before,
.jr-badge[data-state="running"]::before {
  content: "";
  display: inline-block;
  width: 10px;
  height: 10px;
  margin-right: 5px;
  vertical-align: -1px;
  border: 2px solid currentColor;
  border-top-color: transparent;
  border-radius: 50%;
  animation: jr-badge-spin 0.7s linear infinite;
}
@keyframes jr-badge-spin { to { transform: rotate(360deg); } }
.jr-badge--neutral { background: rgba(255,255,255,0.10); color: #c9cdd4; }
.jr-badge--info { background: rgba(83,183,226,0.18); color: #9fd6ef; border-color: rgba(83,183,226,0.35); }
.jr-badge--success { background: rgba(80,200,120,0.18); color: #7fdc9b; border-color: rgba(80,200,120,0.30); }
.jr-badge--danger { background: rgba(226,80,80,0.18); color: #f0a0a0; border-color: rgba(226,80,80,0.35); }
.jr-findings--empty { color: #8b9099; font-size: 0.9em; }
.jr-findings__group { display: flex; gap: 6px; align-items: center; }
.jr-findings__count { color: #b9bdc4; }
.jr-confirm { position: fixed; inset: 0; background: rgba(0,0,0,0.55); display: flex; align-items: center; justify-content: center; z-index: 1000; }
.jr-confirm__box { background: #1d2027; color: #e6e8eb; padding: 20px 22px; border-radius: 8px; max-width: 380px; border: 1px solid rgba(255,255,255,0.12); box-shadow: 0 8px 40px rgba(0,0,0,0.5); }
.jr-confirm__title { margin: 0 0 8px 0; font-size: 1.1em; font-weight: 600; }
.jr-confirm__message { margin: 0; color: #b9bdc4; line-height: 1.45; }
.jr-confirm__actions { display: flex; gap: 8px; justify-content: flex-end; margin-top: 18px; }
.jr-confirm__cancel, .jr-confirm__confirm { padding: 6px 16px; border-radius: 4px; cursor: pointer; font-size: 0.9em; }
.jr-confirm__cancel { background: transparent; color: #e6e8eb; border: 1px solid rgba(255,255,255,0.25); }
.jr-confirm__cancel:hover { background: rgba(255,255,255,0.08); }
.jr-confirm__confirm { background: #2680c2; color: #fff; border: 1px solid #2680c2; }
.jr-confirm__confirm:hover { background: #3690d2; border-color: #3690d2; }
.jr-error, .jr-error-stub { border: 1px solid #b3504f; padding: 8px 10px; border-radius: 4px; color: #f0a0a0; background: rgba(226,80,80,0.08); }
""" ]



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
      "props": { "title": "CloudShield — scan instances" },
      "children": ["toolbar", "list", "results"]
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
            "title": "Scan this instance?",
            "message": "Queue a CloudShield scan for this instance?",
            "variant": "default"
          }
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
    }
  },
  "state": {
    "selectAll": false,
    "instances": [],
    "results": null
  }
}
"""
