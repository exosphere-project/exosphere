module CloudShield.Card exposing (Instance, Model, Msg, OutMsg(..), ViewConfig, cardJson, init, projection, update, view)

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
    }


type Msg
    = GotEnable
    | GotDisable
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
    }



-- UPDATE


update : List Instance -> Msg -> Model -> ( Model, Maybe OutMsg )
update instances msg model =
    case msg of
        GotEnable ->
            ( { model | optedIn = True }, Nothing )

        GotDisable ->
            ( { model | optedIn = False }, Nothing )

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
requestScan targets model =
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
                        , selectAll = Set.size selection == List.length instances
                    }

                Nothing ->
                    model

        _ ->
            model


instanceIdAt : List Instance -> Int -> Maybe String
instanceIdAt instances index =
    instances |> List.drop index |> List.head |> Maybe.map .id



-- THE RENDERER-FACING PROJECTION (host-renderer-interface.md §1.2)


projection : Maybe { targetId : String, state : String } -> List Instance -> Model -> Encode.Value
projection statusOverride instances model =
    Encode.object
        [ ( "selectAll", Encode.bool model.selectAll )
        , ( "results", Encode.null )
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
else the embedded frozen `cardJson` fallback), and an optional discovery note describing how
the manifest was resolved.
-}
type alias ViewConfig =
    { sourceName : String
    , manifestJson : String
    , discoveryNote : Maybe String

    -- the authoritative live state of the in-flight run, read from the polled status object
    -- (§4.3) and projected onto its target row, overriding the optimistic local scanState.
    , statusOverride : Maybe { targetId : String, state : String }
    }


{-| Render the card with its host trust chrome. The whole thing is mounted inside
Exosphere's elm-ui tree via `Element.html`.
-}
view : ExoPalette -> ViewConfig -> List Instance -> Model -> Element.Element Msg
view palette config instances model =
    if model.optedIn then
        Element.column
            [ Element.width Element.fill, Element.spacing spacer.px8 ]
            [ provenanceMarker palette config.sourceName
            , discoveryNoteView palette config.discoveryNote
            , rendererView config.manifestJson config.statusOverride instances model
            , disableAffordance palette
            ]

    else
        optInAffordance palette config.sourceName


discoveryNoteView : ExoPalette -> Maybe String -> Element.Element Msg
discoveryNoteView palette note =
    case note of
        Just text ->
            Element.el
                [ Text.fontSize Text.Small
                , Font.color (SH.toElementColor palette.neutral.text.subdued)
                ]
                (Text.body text)

        Nothing ->
            Element.none


rendererView : String -> Maybe { targetId : String, state : String } -> List Instance -> Model -> Element.Element Msg
rendererView manifestJson statusOverride instances model =
    -- Decode per render is fine for the small card; the fail-closed decoder is the security
    -- gate (an off-catalog or oversized manifest yields the error stub, never a partial tree).
    case JsonRender.decodeString manifestJson of
        Ok spec ->
            Element.html
                (Html.div []
                    [ rendererStyle
                    , Html.map RendererMsg (Render.view spec (projection statusOverride instances model) model.renderer)
                    ]
                )

        Err message ->
            Element.html (JsonRender.errorStub message)


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
            [ Text.body ("Published by the VM “" ++ sourceName ++ "”, not by Exosphere.")
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
.jr-root { font-family: inherit; }
.jr-card { display: flex; flex-direction: column; gap: 8px; padding: 8px 0; }
.jr-card__title { font-size: 1.05em; margin: 0 0 4px 0; }
.jr-stack { display: flex; gap: 8px; }
.jr-stack--row { flex-direction: row; align-items: center; }
.jr-stack--col { flex-direction: column; }
.jr-text { }
.jr-button { padding: 4px 10px; border: 1px solid #888; border-radius: 4px; background: #f4f4f4; cursor: pointer; }
.jr-button:hover { background: #e8e8e8; }
.jr-checkbox { display: inline-flex; align-items: center; gap: 4px; }
.jr-badge { padding: 1px 8px; border-radius: 999px; font-size: 0.85em; border: 1px solid transparent; }
.jr-badge--neutral { background: #ececec; color: #333; }
.jr-badge--info { background: #d7e9fb; color: #0b4a82; }
.jr-badge--success { background: #d6f3df; color: #14622f; }
.jr-badge--danger { background: #fbdcdc; color: #8a1f1f; }
.jr-findings--empty { color: #777; font-size: 0.9em; }
.jr-confirm { position: fixed; inset: 0; background: rgba(0,0,0,0.35); display: flex; align-items: center; justify-content: center; z-index: 1000; }
.jr-confirm__box { background: #fff; padding: 16px; border-radius: 6px; max-width: 360px; box-shadow: 0 4px 24px rgba(0,0,0,0.25); }
.jr-confirm__actions { display: flex; gap: 8px; justify-content: flex-end; margin-top: 12px; }
.jr-confirm__cancel, .jr-confirm__confirm { padding: 4px 12px; border-radius: 4px; border: 1px solid #888; cursor: pointer; }
.jr-confirm__confirm { background: #1769aa; color: #fff; border-color: #1769aa; }
.jr-error-stub { border: 1px solid #c00; padding: 8px; border-radius: 4px; color: #8a1f1f; }
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
