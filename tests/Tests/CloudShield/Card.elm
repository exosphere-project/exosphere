module Tests.CloudShield.Card exposing
    ( cancelDismissVerbSuite
    , discoverySuite
    , embedSuite
    , historySuite
    , historyViewParamsSuite
    , manifestSuite
    , projectionSuite
    , rollbackScanRequestSuite
    , rowTimerSuite
    , transportSuite
    )

{-| Unit tests for the CloudShield browser-side dynamic-UI integration (Phase 1).

Covers the pure host logic that carries over 100% to the Jetstream2 object-storage path:
the frozen manifest still validates fail-closed, the §1.2 host projection, the §3.1 discovery
sentinel + §7.1 metadata transport framing, and the §2.4 `$instances` eligibility filter.

-}

import CloudShield.Card as Card
import CloudShield.Wire as Wire
import Dict
import Exoext.Discovery as Discovery
import Exoext.Lifecycle as Lifecycle
import Exoext.Transport as Transport
import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import JsonRender
import JsonRender.Expr as Expr
import JsonRender.Render as Render
import JsonRender.Spec as Spec exposing (Props(..))
import OpenStack.Types as OSTypes
import Set exposing (Set)
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Tests.CloudShield.Fixtures exposing (cardViewConfig)
import Time



-- MANIFEST v3 (the wire-owned card.json — the manifest owns every display string)


{-| Manifest v3 as a MANUALLY-SYNCED copy of the bridge's authoritative card.json (the source of
truth). Re-sync whenever the bridge card changes:

    sed 's/\\\\/\\\\\\\\/g' ~/dev/cloudshield-bridge/cloudshield_bridge/card.json

(the only transform is doubling each JSON backslash for Elm's triple-quoted string literal.)

The contract test below ties this manifest to the host: the SAME per-row `state` token and the
SAME `/historyLoaded` + `/historyCount` + `/targetsCount` keys that `CloudShield.Card.projection`
/ `historyRow` project drive the manifest's `$cond` text/action chains and the badge `variant`, so
the two sides cannot silently disagree. `cardJson` is no longer embedded in the host — this fixture
is the only in-repo copy of the manifest, and it must decode fail-closed.

-}
cardJsonV3 : String
cardJsonV3 =
    """
{
  "root": "card",
  "elements": {
    "card": {
      "type": "Card",
      "props": {},
      "children": [
        "columns",
        "results",
        "results-embed"
      ]
    },
    "columns": {
      "type": "Stack",
      "props": {
        "direction": "row",
        "gap": 3
      },
      "children": [
        "targets-col",
        "history-col"
      ]
    },
    "targets-col": {
      "type": "Stack",
      "props": {
        "direction": "col",
        "gap": 2
      },
      "children": [
        "targets-label",
        "toolbar",
        "list"
      ]
    },
    "targets-label": {
      "type": "Text",
      "props": {
        "value": {
          "$template": "Scan targets · ${/targetsCount}"
        }
      },
      "children": []
    },
    "toolbar": {
      "type": "Stack",
      "props": {
        "direction": "row",
        "gap": 2
      },
      "children": [
        "select-all",
        "scan-selected"
      ]
    },
    "select-all": {
      "type": "Checkbox",
      "props": {
        "label": "Select all",
        "checked": {
          "$bindState": "/selectAll"
        }
      },
      "children": []
    },
    "scan-selected": {
      "type": "Button",
      "props": {
        "label": "Scan selected"
      },
      "on": {
        "press": {
          "action": "exoext.writeRequest",
          "params": {
            "kind": "scan",
            "targetInstanceIds": []
          },
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
      "props": {
        "direction": "col",
        "gap": 1
      },
      "repeat": {
        "statePath": "/instances",
        "key": "id"
      },
      "children": [
        "row"
      ]
    },
    "row": {
      "type": "Stack",
      "props": {
        "direction": "row",
        "gap": 2
      },
      "children": [
        "row-select",
        "row-name",
        "row-status",
        "row-scan-btn"
      ]
    },
    "row-select": {
      "type": "Checkbox",
      "props": {
        "checked": {
          "$bindItem": "selected"
        }
      },
      "children": []
    },
    "row-name": {
      "type": "Text",
      "props": {
        "value": {
          "$item": "name"
        }
      },
      "children": []
    },
    "row-status": {
      "type": "Badge",
      "props": {
        "value": {
          "$item": "scanState"
        }
      },
      "children": []
    },
    "row-scan-btn": {
      "type": "Button",
      "props": {
        "label": "Scan"
      },
      "on": {
        "press": {
          "action": "exoext.writeRequest",
          "params": {
            "kind": "scan",
            "targetInstanceIds": [
              {
                "$item": "id"
              }
            ]
          },
          "confirm": {
            "title": "Start scan",
            "message": {
              "$template": "Run a snapshot-clone scan of \\"${name}\\"?"
            },
            "variant": "default"
          }
        }
      },
      "children": []
    },
    "history-col": {
      "type": "Stack",
      "props": {
        "direction": "col",
        "gap": 2
      },
      "children": [
        "history-label",
        "history-note",
        "history-rows"
      ]
    },
    "history-label": {
      "type": "Text",
      "props": {
        "value": {
          "$cond": {
            "$state": "/historyLoaded"
          },
          "$then": {
            "$template": "Scan history · ${/historyCount}"
          },
          "$else": "Scan history"
        }
      },
      "children": []
    },
    "history-note": {
      "type": "Text",
      "props": {
        "value": {
          "$state": "/historyNote"
        }
      },
      "children": []
    },
    "history-rows": {
      "type": "Stack",
      "props": {
        "direction": "col",
        "gap": 1
      },
      "repeat": {
        "statePath": "/history",
        "key": "resultId"
      },
      "children": [
        "history-row"
      ]
    },
    "history-row": {
      "type": "Stack",
      "props": {
        "direction": "row",
        "gap": 2
      },
      "children": [
        "history-main",
        "history-pills",
        "history-state",
        "history-view-btn"
      ]
    },
    "history-main": {
      "type": "Stack",
      "props": {
        "direction": "col",
        "gap": 0
      },
      "children": [
        "history-when",
        "history-sub"
      ]
    },
    "history-when": {
      "type": "Text",
      "props": {
        "value": {
          "$item": "completedAt"
        }
      },
      "children": []
    },
    "history-sub": {
      "type": "Text",
      "props": {
        "value": {
          "$item": "subLabel"
        }
      },
      "children": []
    },
    "history-pills": {
      "type": "FindingsTable",
      "props": {
        "bind": {
          "$item": "findings"
        },
        "groupBy": "severity",
        "emptyLabel": {
          "$cond": {
            "$item": "status",
            "eq": "ok"
          },
          "$then": "No vulnerabilities found",
          "$else": ""
        }
      },
      "children": []
    },
    "history-state": {
      "type": "Badge",
      "props": {
        "value": {
          "$cond": {
            "$item": "state",
            "eq": "viewing"
          },
          "$then": "Now viewing",
          "$else": {
            "$cond": {
              "$item": "state",
              "eq": "opening"
            },
            "$then": "Opening…",
            "$else": {
              "$cond": {
                "$item": "state",
                "eq": "expired"
              },
              "$then": "Expired",
              "$else": {
                "$cond": {
                  "$item": "state",
                  "eq": "error"
                },
                "$then": "Couldn't open",
                "$else": {
                  "$cond": {
                    "$item": "state",
                    "eq": "failed"
                  },
                  "$then": "failed",
                  "$else": ""
                }
              }
            }
          }
        },
        "variant": {
          "$item": "state"
        }
      },
      "children": []
    },
    "history-view-btn": {
      "type": "Button",
      "props": {
        "label": {
          "$cond": {
            "$item": "state",
            "eq": "viewing"
          },
          "$then": "Refresh",
          "$else": {
            "$cond": {
              "$item": "state",
              "eq": "opening"
            },
            "$then": "Opening…",
            "$else": {
              "$cond": {
                "$item": "state",
                "eq": "error"
              },
              "$then": "Retry",
              "$else": {
                "$cond": {
                  "$item": "state",
                  "eq": "failed"
                },
                "$then": "",
                "$else": "View"
              }
            }
          }
        },
        "disabled": {
          "$state": "/requestBusy"
        }
      },
      "on": {
        "press": {
          "action": "exoext.openSession",
          "params": {
            "resultId": {
              "$template": "${resultId}"
            },
            "batchId": {
              "$template": "${batchId}"
            }
          }
        }
      },
      "children": []
    },
    "results": {
      "type": "FindingsTable",
      "props": {
        "bind": {
          "$state": "/results"
        },
        "groupBy": "severity",
        "emptyLabel": {
          "$cond": {
            "$state": "/results"
          },
          "$then": "No vulnerabilities found",
          "$else": ""
        }
      },
      "children": []
    },
    "results-embed": {
      "type": "Iframe",
      "props": {
        "src": {
          "$state": "/embedUrl"
        },
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
    "historyNote": "",
    "requestBusy": false,
    "targetsLabel": "Scan targets",
    "historyLabel": "Scan history"
  }
}
    """


{-| Resolve an element's single display expression (Text `value`, Button `label`, or Badge
`value`) against a context, or `""` when the element / prop is absent.
-}
displayOf : String -> Expr.Context -> String
displayOf id ctx =
    case propsOf id of
        Just (TextP p) ->
            Expr.resolveDisplay ctx p.value

        Just (ButtonP p) ->
            Expr.resolveDisplay ctx p.label

        Just (BadgeP p) ->
            Expr.resolveDisplay ctx p.value

        _ ->
            ""


{-| Resolve a Badge's `variant` (→ `data-state`) against a context, or `""` when absent.
-}
variantOf : String -> Expr.Context -> String
variantOf id ctx =
    case propsOf id of
        Just (BadgeP p) ->
            p.variant |> Maybe.map (Expr.resolveDisplay ctx) |> Maybe.withDefault ""

        _ ->
            ""


propsOf : String -> Maybe Spec.Props
propsOf id =
    JsonRender.decodeString cardJsonV3
        |> Result.toMaybe
        |> Maybe.andThen (\spec -> Dict.get id spec.elements)
        |> Maybe.map .props


pressBinding : String -> Maybe Spec.ActionBinding
pressBinding id =
    JsonRender.decodeString cardJsonV3
        |> Result.toMaybe
        |> Maybe.andThen (\spec -> Dict.get id spec.elements)
        |> Maybe.andThen (\el -> Dict.get "press" el.on)
        |> Maybe.andThen List.head


{-| A history-row `$item` context carrying the given `state` token (plus the row ids so the View
button's params resolve, and a `status` so the pills' `emptyLabel` resolves). Mirrors
`Expr.childContext "/history" 0 …`.
-}
rowContext : String -> Expr.Context
rowContext token =
    rowContextWith
        [ ( "state", Encode.string token )
        , ( "resultId", Encode.string "r-1" )
        , ( "batchId", Encode.string "b-1" )
        , ( "status", Encode.string "done" )
        ]


rowContextWith : List ( String, Encode.Value ) -> Expr.Context
rowContextWith fields =
    Expr.childContext "/history" 0 (Encode.object fields) (Expr.rootContext [] Encode.null)


{-| The (badge text, badge data-state, action label) the manifest renders for a row `state` token.
This is the whole host↔manifest contract for a history row: the manifest reproduces exactly the
old rowState/actionLabel strings from the token the host projects.
-}
rowTriple : String -> ( String, String, String )
rowTriple token =
    let
        ctx =
            rowContext token
    in
    ( displayOf "history-state" ctx, variantOf "history-state" ctx, displayOf "history-view-btn" ctx )


manifestSuite : Test
manifestSuite =
    describe "CloudShield manifest v3 (wire contract, synced from the bridge card.json)"
        [ test "(a) the v2 manifest decodes fail-closed clean through the catalog" <|
            \_ ->
                case JsonRender.decodeString cardJsonV3 of
                    Ok _ ->
                        Expect.pass

                    Err message ->
                        Expect.fail ("card.json v3 should validate, but: " ++ message)
        , test "an off-catalog component type is rejected (fail-closed)" <|
            \_ ->
                let
                    bad =
                        """{ "root": "x", "elements": { "x": { "type": "ScriptInjector", "props": {}, "children": [] } } }"""
                in
                case JsonRender.decodeString bad of
                    Ok _ ->
                        Expect.fail "a ScriptInjector component must be rejected"

                    Err _ ->
                        Expect.pass
        , test "(b) the history header reads 'Scan history · N' once loaded" <|
            \_ ->
                let
                    state =
                        Card.projection Time.utc { sampleConfig | history = loadedHistory (historyEntries 3) } sampleInstances idleModel
                in
                Expect.equal "Scan history · 3" (displayOf "history-label" (Expr.rootContext [] state))
        , test "(b) the history header reads 'Scan history' before the first load resolves" <|
            \_ ->
                let
                    state =
                        Card.projection Time.utc { sampleConfig | history = loadingHistory } sampleInstances idleModel
                in
                Expect.equal "Scan history" (displayOf "history-label" (Expr.rootContext [] state))
        , test "(b) the targets header reads 'Scan targets · N' from /targetsCount" <|
            \_ ->
                let
                    state =
                        Card.projection Time.utc sampleConfig sampleInstances idleModel
                in
                Expect.equal "Scan targets · 2" (displayOf "targets-label" (Expr.rootContext [] state))
        , test "(c) each state token yields the right badge text + data-state + action label" <|
            \_ ->
                Expect.equal
                    [ ( "Now viewing", "viewing", "Refresh" )
                    , ( "Opening…", "opening", "Opening…" )
                    , ( "Expired", "expired", "View" )
                    , ( "Couldn't open", "error", "Retry" )
                    , ( "failed", "failed", "" )
                    , ( "", "idle", "View" )
                    ]
                    (List.map rowTriple [ "viewing", "opening", "expired", "error", "failed", "idle" ])
        , test "(d) the scan button dispatches the generic write-request verb with kind=scan" <|
            \_ ->
                case pressBinding "scan-selected" of
                    Just binding ->
                        Card.resolveAction binding.action binding.params
                            |> Expect.equal (Just { name = "exoext.writeRequest", verb = Lifecycle.verbWriteRequest, kind = "scan" })

                    Nothing ->
                        Expect.fail "scan-selected should carry a press binding"
        , test "(d) a write-request with a non-scan kind is a fail-closed no-op" <|
            \_ ->
                let
                    params =
                        Encode.object
                            [ ( "kind", Encode.string "backup" )
                            , ( "targetInstanceIds", Encode.list Encode.string [ "i-1" ] )
                            ]
                in
                Card.dispatchVerb (Card.resolveAction "exoext.writeRequest" params) params idleModel
                    |> Tuple.second
                    |> Expect.equal Nothing
        , test "(d) the scan button still carries its confirm (the confirm flow fires)" <|
            \_ ->
                pressBinding "scan-selected"
                    |> Maybe.andThen .confirm
                    |> Maybe.map (\c -> Expr.resolveDisplay (Expr.rootContext [] Encode.null) c.title)
                    |> Expect.equal (Just "Scan selected instances?")
        , test "(d) the View button dispatches openSession and emits the embed request" <|
            \_ ->
                case pressBinding "history-view-btn" of
                    Just binding ->
                        let
                            resolvedVerb =
                                Card.resolveAction binding.action binding.params
                                    |> Maybe.map .verb

                            emitted =
                                Expr.resolveParams (rowContext "expired") binding.params
                                    |> Card.resultIdOf
                                    |> (\ids -> Card.requestEmbed ids idleModel)
                                    |> Tuple.second
                        in
                        Expect.equal
                            ( Just Lifecycle.verbOpenSession
                            , Just (Card.EmbedRequested { resultId = "r-1", batchId = "b-1" })
                            )
                            ( resolvedVerb, emitted )

                    Nothing ->
                        Expect.fail "history-view-btn should carry a press binding"
        , test "(e) the row action's disabled binds the host's /requestBusy guard flag" <|
            \_ ->
                -- The busy guard must be VISIBLE: the same host predicate that swallows the press
                -- also greys the button, so a press during a scan reads as unavailable, not broken.
                let
                    isDisabled busy =
                        case propsOf "history-view-btn" of
                            Just (ButtonP p) ->
                                p.disabled
                                    |> Maybe.map
                                        (Expr.resolveBool
                                            (Expr.rootContext []
                                                (Encode.object [ ( "requestBusy", Encode.bool busy ) ])
                                            )
                                        )
                                    |> Maybe.withDefault False

                            _ ->
                                False
                in
                Expect.equal ( True, False ) ( isDisabled True, isDisabled False )
        , test "(f) a clean OK row says so definitively; a failed row says nothing" <|
            \_ ->
                -- Defect #2: an all-zero completed scan must not read "No findings yet". The failed
                -- row stays silent because its badge already says "failed" (no double-messaging).
                let
                    emptyLabelFor status =
                        case propsOf "history-pills" of
                            Just (FindingsTableP p) ->
                                p.emptyLabel
                                    |> Maybe.map (Expr.resolveDisplay (rowContextWith [ ( "status", Encode.string status ) ]))
                                    |> Maybe.withDefault "No findings yet"

                            _ ->
                                ""
                in
                Expect.equal ( "No vulnerabilities found", "" )
                    ( emptyLabelFor "ok", emptyLabelFor "error" )
        ]



-- PROJECTION (host-renderer-interface.md §1.2)


sampleModel : Set String -> Card.Model
sampleModel selection =
    { renderer = Render.init
    , selection = selection
    , selectAll = False
    , scanState = Dict.fromList [ ( "i-2", "queued" ) ]
    , seq = 0
    , pending = Nothing
    , showDemoIframe = False
    , showManifestErrorDetail = False
    }


sampleInstances : List Card.Instance
sampleInstances =
    [ { id = "i-1", name = "alpha", status = "ACTIVE" }
    , { id = "i-2", name = "beta", status = "ACTIVE" }
    ]


rowScanState : Int -> Decode.Value -> Result String String
rowScanState index value =
    Decode.decodeValue
        (Decode.at [ "instances" ] (Decode.index index (Decode.field "scanState" Decode.string)))
        value
        |> Result.mapError Decode.errorToString


rowSelected : Int -> Decode.Value -> Result String Bool
rowSelected index value =
    Decode.decodeValue
        (Decode.field "instances" (Decode.index index (Decode.field "selected" Decode.bool)))
        value
        |> Result.mapError Decode.errorToString


loadedHistory : List Wire.IndexEntry -> { rows : List Wire.IndexEntry, loading : Bool, loaded : Bool }
loadedHistory rows =
    { rows = rows, loading = False, loaded = True }


loadingHistory : { rows : List Wire.IndexEntry, loading : Bool, loaded : Bool }
loadingHistory =
    { rows = [], loading = True, loaded = False }


{-| The host's `ViewConfig` in its quiet default (see `Tests.CloudShield.Fixtures`). Each projection
test overrides only the fields it is actually about, so a test reads as the one situation it pins
rather than a row of interchangeable `Nothing`s.
-}
sampleConfig : Card.ViewConfig
sampleConfig =
    cardViewConfig


projectionSuite : Test
projectionSuite =
    describe "CloudShield host projection (§1.2)"
        [ test "selected reflects the host selection set" <|
            \_ ->
                let
                    value =
                        Card.projection Time.utc sampleConfig sampleInstances (sampleModel (Set.singleton "i-1"))
                in
                Expect.equal ( Ok True, Ok False )
                    ( rowSelected 0 value, rowSelected 1 value )
        , test "scanState comes from the local scanState map when no override" <|
            \_ ->
                let
                    value =
                        Card.projection Time.utc sampleConfig sampleInstances (sampleModel Set.empty)
                in
                Expect.equal ( Ok "idle", Ok "queued" )
                    ( rowScanState 0 value, rowScanState 1 value )
        , test "a polled statusOverride wins for its target row only" <|
            \_ ->
                let
                    override =
                        Just { targetId = "i-1", state = "running" }

                    value =
                        Card.projection Time.utc { sampleConfig | statusOverride = override } sampleInstances (sampleModel Set.empty)
                in
                Expect.equal ( Ok "running", Ok "queued" )
                    ( rowScanState 0 value, rowScanState 1 value )
        , test "a target in the undrained tail reads queued, so a restored batch looks unchanged" <|
            \_ ->
                -- After a reload the card's optimistic badges are gone (idleModel), but the batch
                -- record survived: its untouched targets are still coming and must say so.
                let
                    value =
                        Card.projection Time.utc
                            { sampleConfig | queuedTargets = [ "i-2" ] }
                            sampleInstances
                            idleModel
                in
                Expect.equal ( Ok "idle", Ok "queued" )
                    ( rowScanState 0 value, rowScanState 1 value )
        , test "the live run still wins on its own row while the rest of the tail reads queued" <|
            \_ ->
                -- The precedence that matters on a restored batch: the wire says what is happening
                -- now, the tail only says what is about to happen.
                let
                    value =
                        Card.projection Time.utc
                            { sampleConfig
                                | statusOverride = Just { targetId = "i-1", state = "scanning · 0:12" }
                                , queuedTargets = [ "i-2" ]
                            }
                            sampleInstances
                            idleModel
                in
                Expect.equal ( Ok "scanning · 0:12", Ok "queued" )
                    ( rowScanState 0 value, rowScanState 1 value )
        , test "a live run wins even over its own row being in the tail" <|
            \_ ->
                Expect.equal (Ok "running")
                    (rowScanState 0
                        (Card.projection Time.utc
                            { sampleConfig
                                | statusOverride = Just { targetId = "i-1", state = "running" }
                                , queuedTargets = [ "i-1" ]
                            }
                            sampleInstances
                            idleModel
                        )
                    )
        , test "a settled row that is queued again reads queued; one that is not keeps its state" <|
            \_ ->
                -- i-1 finished earlier this session AND is in the tail: it is about to be scanned
                -- again, so the tail wins. i-2 finished and is not in the tail, so it stays done.
                let
                    settled =
                        { idleModel | scanState = Dict.fromList [ ( "i-1", "done" ), ( "i-2", "done" ) ] }

                    value =
                        Card.projection Time.utc
                            { sampleConfig | queuedTargets = [ "i-1" ] }
                            sampleInstances
                            settled
                in
                Expect.equal ( Ok "queued", Ok "done" )
                    ( rowScanState 0 value, rowScanState 1 value )
        , test "embedUrl is projected to the top-level /embedUrl render-state key" <|
            \_ ->
                let
                    value =
                        Card.projection Time.utc { sampleConfig | embedUrl = "https://1-2-3-4.sslip.io/app" } sampleInstances (sampleModel Set.empty)
                in
                Expect.equal (Ok "https://1-2-3-4.sslip.io/app")
                    (Decode.decodeValue (Decode.field "embedUrl" Decode.string) value
                        |> Result.mapError Decode.errorToString
                    )
        , test "the running scan's counting-up label projects onto its row, even with history present" <|
            \_ ->
                -- The host composes "scanning · m:ss" into the running target's override; the row must
                -- carry it regardless of a history pick being shown on the right (non-empty history).
                let
                    override =
                        Just { targetId = "i-1", state = "scanning · 0:23" }

                    value =
                        Card.projection Time.utc { sampleConfig | history = loadedHistory [ historyEntry "b-1" ], statusOverride = override } sampleInstances (sampleModel Set.empty)
                in
                Expect.equal ( Ok "scanning · 0:23", Ok "queued" )
                    ( rowScanState 0 value, rowScanState 1 value )
        , test "history first-load state omits the count until rows have fetched" <|
            \_ ->
                let
                    value =
                        Card.projection Time.utc { sampleConfig | history = loadingHistory } sampleInstances (sampleModel Set.empty)

                    stringField key =
                        Decode.decodeValue (Decode.field key Decode.string) value
                            |> Result.mapError Decode.errorToString
                in
                Expect.equal ( Ok "Scan history", Ok "" )
                    ( stringField "historyLabel", stringField "historyNote" )
        , test "requestBusy is projected top-level so the manifest can disable the swallowed action" <|
            \_ ->
                let
                    busyFlag requestBusy =
                        Card.projection Time.utc { sampleConfig | requestBusy = requestBusy } sampleInstances idleModel
                            |> Decode.decodeValue (Decode.field "requestBusy" Decode.bool)
                            |> Result.mapError Decode.errorToString
                in
                Expect.equal ( Ok True, Ok False ) ( busyFlag True, busyFlag False )
        , test "sessionOpen is projected top-level so a close affordance appears only with a session" <|
            \_ ->
                let
                    openFlag sessionOpen =
                        Card.projection Time.utc { sampleConfig | sessionOpen = sessionOpen } sampleInstances idleModel
                            |> Decode.decodeValue (Decode.field "sessionOpen" Decode.bool)
                            |> Result.mapError Decode.errorToString
                in
                Expect.equal ( Ok True, Ok False ) ( openFlag True, openFlag False )
        , test "scanBusy is projected top-level, separately from requestBusy" <|
            \_ ->
                -- §I keeps the two guards distinct: `requestBusy` greys the View controls,
                -- `scanBusy` the Scan ones. Setting either must not move the other.
                let
                    flags config =
                        Card.projection Time.utc config sampleInstances idleModel
                            |> Decode.decodeValue
                                (Decode.map2 Tuple.pair
                                    (Decode.field "scanBusy" Decode.bool)
                                    (Decode.field "requestBusy" Decode.bool)
                                )
                            |> Result.mapError Decode.errorToString
                in
                Expect.equal
                    ( Ok ( True, False ), Ok ( False, True ), Ok ( False, False ) )
                    ( flags { sampleConfig | scanBusy = True }
                    , flags { sampleConfig | requestBusy = True }
                    , flags sampleConfig
                    )
        , test "the stoppable run's row carries cancellable + its requestId + its target; other rows carry none" <|
            \_ ->
                let
                    value =
                        Card.projection Time.utc
                            { sampleConfig | cancellableRun = Just { targetId = "i-1", requestId = "exo-cs-req-7" } }
                            sampleInstances
                            idleModel
                in
                Expect.equal
                    ( ( Ok True, Ok "exo-cs-req-7", Ok "i-1" ), ( Ok False, Ok "", Ok "" ) )
                    ( ( rowCancellable 0 value, rowCancelRequestId 0 value, rowCancelTargetId 0 value )
                    , ( rowCancellable 1 value, rowCancelRequestId 1 value, rowCancelTargetId 1 value )
                    )
        , test "with no stoppable run no row is cancellable, and none can name a request" <|
            \_ ->
                -- The empty ids are what make an errant press inert: `cancelTargetOf` rejects a
                -- press that names neither a request nor a target.
                let
                    value =
                        Card.projection Time.utc sampleConfig sampleInstances idleModel
                in
                Expect.equal ( Ok False, Ok "", Ok "" )
                    ( rowCancellable 0 value, rowCancelRequestId 0 value, rowCancelTargetId 0 value )
        , test "a target parked in the undrained tail is stoppable by TARGET, having no request yet" <|
            \_ ->
                -- Bug 1: the queued row has nothing on the wire to name, so `cancelRequestId` stays
                -- empty and `cancelTargetId` is the whole identity of the press.
                let
                    value =
                        Card.projection Time.utc
                            { sampleConfig | queuedTargets = [ "i-2" ] }
                            sampleInstances
                            idleModel
                in
                Expect.equal
                    ( ( Ok True, Ok "", Ok "i-2" ), Ok False )
                    ( ( rowCancellable 1 value, rowCancelRequestId 1 value, rowCancelTargetId 1 value )
                    , rowCancellable 0 value
                    )
        , test "a stopping row reads the stopping token and offers no second press" <|
            \_ ->
                -- Bugs 2+3 at the projection: the host says `stopping` and the manifest owns the
                -- word. The stop control goes because the host withdraws `cancellableRun`, which is
                -- what a stopping run looks like from here.
                let
                    value =
                        Card.projection Time.utc
                            { sampleConfig
                                | statusOverride = Just { targetId = "i-1", state = "scanning · 0:54" }
                                , stoppingTargetId = Just "i-1"
                            }
                            sampleInstances
                            idleModel
                in
                Expect.equal ( Ok "stopping", Ok False, Ok "" )
                    ( rowScanState 0 value, rowCancellable 0 value, rowCancelTargetId 0 value )
        , test "stopping outranks the live wire state, the tail, and the durable state alike" <|
            \_ ->
                -- The full precedence chain, exercised on one row that qualifies for every level at
                -- once: a live run, a place in the tail, and a committed terminal state.
                let
                    stateOf config =
                        Card.projection Time.utc config sampleInstances (settledModel "i-1" "done")
                            |> rowScanState 0

                    live =
                        { sampleConfig
                            | statusOverride = Just { targetId = "i-1", state = "running" }
                            , queuedTargets = [ "i-1" ]
                        }
                in
                Expect.equal ( Ok "stopping", Ok "running" )
                    ( stateOf { live | stoppingTargetId = Just "i-1" }, stateOf live )
        , test "stopping applies to its own row only" <|
            \_ ->
                let
                    value =
                        Card.projection Time.utc
                            { sampleConfig | stoppingTargetId = Just "i-1" }
                            sampleInstances
                            (settledModel "i-2" "done")
                in
                Expect.equal ( Ok "stopping", Ok "done" )
                    ( rowScanState 0 value, rowScanState 1 value )
        ]


{-| The card with one row's terminal run committed to its durable `scanState` — the bottom of the
row-state precedence chain.
-}
settledModel : String -> String -> Card.Model
settledModel id state =
    Card.settleScanState id state idleModel


rowCancellable : Int -> Decode.Value -> Result String Bool
rowCancellable index value =
    Decode.decodeValue
        (Decode.field "instances" (Decode.index index (Decode.field "cancellable" Decode.bool)))
        value
        |> Result.mapError Decode.errorToString


rowCancelRequestId : Int -> Decode.Value -> Result String String
rowCancelRequestId index value =
    rowStringField "cancelRequestId" index value


rowCancelTargetId : Int -> Decode.Value -> Result String String
rowCancelTargetId index value =
    rowStringField "cancelTargetId" index value


rowStringField : String -> Int -> Decode.Value -> Result String String
rowStringField key index value =
    Decode.decodeValue
        (Decode.field "instances" (Decode.index index (Decode.field key Decode.string)))
        value
        |> Result.mapError Decode.errorToString


{-| The two verbs WP8 added, at the dispatch boundary: what the manifest emits, what it resolves to,
and what the card asks the host to do about it.
-}
cancelDismissVerbSuite : Test
cancelDismissVerbSuite =
    let
        dispatch name params =
            Card.dispatchVerb (Card.resolveAction name params) params idleModel |> Tuple.second

        cancelParams requestId =
            Encode.object [ ( "requestId", Encode.string requestId ) ]

        stopParams requestId targetId =
            Encode.object
                [ ( "requestId", Encode.string requestId )
                , ( "targetId", Encode.string targetId )
                ]
    in
    describe "CloudShield cancel + dismiss verbs"
        [ test "a cancel press asks the host to stop the named request" <|
            \_ ->
                Expect.equal (Just (Card.CancelRequested { requestId = "exo-cs-req-7", targetId = "i-1" }))
                    (dispatch Lifecycle.verbCancelRequest (stopParams "exo-cs-req-7" "i-1"))
        , test "the v1 cloudshield.* alias reaches the same verb" <|
            \_ ->
                Expect.equal (Just Lifecycle.verbCancelRequest)
                    (Card.resolveAction "cloudshield.cancelScan" Encode.null |> Maybe.map .verb)
        , test "a cancel naming nothing at all is swallowed (both ids empty, and absent alike)" <|
            \_ ->
                -- A non-stoppable row projects BOTH ids empty, so this is the press that would
                -- otherwise reach the host with nothing it could attribute the press to.
                Expect.equal ( Nothing, Nothing )
                    ( dispatch Lifecycle.verbCancelRequest (stopParams "" "")
                    , dispatch Lifecycle.verbCancelRequest (Encode.object [])
                    )
        , test "a press naming only a target still dispatches: that is a queued row's stop" <|
            \_ ->
                -- Bug 1. An empty `requestId` is meaningful, not malformed — it says "no request on
                -- the wire yet". Only the host owns the batch, so only the host can route it.
                Expect.equal (Just (Card.CancelRequested { requestId = "", targetId = "i-2" }))
                    (dispatch Lifecycle.verbCancelRequest (stopParams "" "i-2"))
        , test "the frozen v4 manifest, which emits only requestId, still dispatches" <|
            \_ ->
                Expect.equal (Just (Card.CancelRequested { requestId = "exo-cs-req-7", targetId = "" }))
                    (dispatch Lifecycle.verbCancelRequest (cancelParams "exo-cs-req-7"))
        , test "cancelTargetOf reads both ids, and rejects only the press that names neither" <|
            \_ ->
                Expect.equal
                    ( Just { requestId = "exo-cs-req-7", targetId = "i-1" }
                    , Just { requestId = "", targetId = "i-2" }
                    , Nothing
                    )
                    ( Card.cancelTargetOf (stopParams "exo-cs-req-7" "i-1")
                    , Card.cancelTargetOf (stopParams "" "i-2")
                    , Card.cancelTargetOf (stopParams "" "")
                    )
        , test "a dismiss press asks the host to close the session, and needs no params" <|
            \_ ->
                Expect.equal (Just Card.SessionDismissed)
                    (dispatch Lifecycle.verbDismissSession Encode.null)
        , test "an off-verb action still resolves to nothing (fail-closed)" <|
            \_ ->
                Expect.equal Nothing (Card.resolveAction "exoext.deleteEverything" Encode.null)
        ]


{-| `scanningRowLabel` builds the left-column running row's counting-up elapsed from the run's
wall-clock start (the request seq) and the shared clock, consistent with the frozen completion time.
-}
rowTimerSuite : Test
rowTimerSuite =
    let
        start =
            1750000000000
    in
    describe "the scanning-row counting-up label (scanningRowLabel)"
        [ test "composes scanning · m:ss from the wall-clock elapsed" <|
            \_ ->
                Expect.equal "scanning · 0:23" (Card.scanningRowLabel start (start + 23000))
        , test "formats minutes past 60 seconds" <|
            \_ ->
                Expect.equal "scanning · 2:05" (Card.scanningRowLabel start (start + 125000))
        , test "reads 0:00 before the wall-clock start stamp lands (non-epoch guard)" <|
            \_ ->
                -- A tiny optimistic seq (not yet stamped with wall-clock millis) must not read as an
                -- absurd elapsed; it clamps to 0:00 until the real timestamp arrives.
                Expect.equal "scanning · 0:00" (Card.scanningRowLabel 5 (start + 23000))
        , test "reads 0:00 for a negative diff (clock skew)" <|
            \_ ->
                Expect.equal "scanning · 0:00" (Card.scanningRowLabel start (start - 1000))
        ]



-- DISCOVERY (§3.1 sentinel, §2.4 eligibility)


meta : List ( String, String ) -> List OSTypes.MetadataItem
meta pairs =
    List.map (\( k, v ) -> { key = k, value = v }) pairs


discoverySuite : Test
discoverySuite =
    describe "CloudShield discovery"
        [ test "readSentinel returns Nothing without the exoext.v1.kind key" <|
            \_ ->
                Expect.equal Nothing (Discovery.readSentinel (meta [ ( "foo", "bar" ) ]))
        , test "readSentinel reads kind + store + flags" <|
            \_ ->
                case Discovery.readSentinel (meta [ ( "exoext.v1.kind", "cloudshield" ), ( "exoext.v1.store", "metadata" ), ( "exoext.v1.flags", "scan,results" ) ]) of
                    Just s ->
                        Expect.equal ( "cloudshield", Discovery.StoreMetadata, [ "scan", "results" ] )
                            ( s.kind, s.store, s.flags )

                    Nothing ->
                        Expect.fail "sentinel should be present"
        , test "manifestBodyFromMetadata concatenates man.body chunks in order" <|
            \_ ->
                Expect.equal (Just "abcdEFGH")
                    (Discovery.manifestBodyFromMetadata
                        (meta [ ( "exoext.v1.man.body.0", "abcd" ), ( "exoext.v1.man.body.1", "EFGH" ) ])
                    )
        , test "manifestBodyFromMetadata stops at the first gap" <|
            \_ ->
                Expect.equal (Just "abcd")
                    (Discovery.manifestBodyFromMetadata
                        (meta [ ( "exoext.v1.man.body.0", "abcd" ), ( "exoext.v1.man.body.2", "XX" ) ])
                    )
        , test "manifestBodyFromMetadata honors the man.body.n count, ignoring orphans" <|
            \_ ->
                Expect.equal (Just "abcd")
                    (Discovery.manifestBodyFromMetadata
                        (meta [ ( "exoext.v1.man.body.n", "1" ), ( "exoext.v1.man.body.0", "abcd" ), ( "exoext.v1.man.body.1", "STALE" ) ])
                    )
        , test "manifestBodyFromMetadata is Nothing without man.body.0" <|
            \_ ->
                Expect.equal Nothing (Discovery.manifestBodyFromMetadata (meta [ ( "exoext.v1.man.body.1", "x" ) ]))
        , test "eligibleInstances keeps ACTIVE and excludes self" <|
            \_ ->
                let
                    servers =
                        [ candidate "self" "cloudshield-vm" OSTypes.ServerActive []
                        , candidate "i-1" "alpha" OSTypes.ServerActive []
                        , candidate "i-2" "building" OSTypes.ServerBuild []
                        ]
                in
                Expect.equal [ { id = "i-1", name = "alpha", status = "Active" } ]
                    (Discovery.eligibleInstances "self" servers)
        , test "eligibleInstances excludes an instance the publisher marked as its own machinery" <|
            \_ ->
                -- Bug 6. The marked instance is ACTIVE and not self, so every other eligibility rule
                -- admits it; only the publisher's own statement about it keeps it off the list.
                let
                    servers =
                        [ candidate "i-1" "alpha" OSTypes.ServerActive []
                        , candidate "i-9" "machinery" OSTypes.ServerActive [ ( Discovery.transientKey, "true" ) ]
                        ]
                in
                Expect.equal [ { id = "i-1", name = "alpha", status = "Active" } ]
                    (Discovery.eligibleInstances "self" servers)
        , test "only the exact `true` excludes: any other value leaves the instance scannable" <|
            \_ ->
                -- Fail-OPEN here on purpose, and it is the safe direction: the cost of admitting an
                -- instance is a row the researcher can ignore, while the cost of dropping one on a
                -- value the host merely failed to recognize is a machine that silently cannot be
                -- scanned at all.
                Expect.equal [ "i-1", "i-2", "i-3" ]
                    ([ candidate "i-1" "alpha" OSTypes.ServerActive [ ( Discovery.transientKey, "false" ) ]
                     , candidate "i-2" "beta" OSTypes.ServerActive [ ( Discovery.transientKey, "" ) ]
                     , candidate "i-3" "gamma" OSTypes.ServerActive [ ( "unrelated.key", "true" ) ]
                     ]
                        |> Discovery.eligibleInstances "self"
                        |> List.map .id
                    )
        ]


{-| One server as `eligibleInstances` takes it: identity, status, and its own Nova metadata (which
the §2.4 filter now reads one key off).
-}
candidate :
    String
    -> String
    -> OSTypes.ServerStatus
    -> List ( String, String )
    -> { id : String, name : String, status : OSTypes.ServerStatus, metadata : List OSTypes.MetadataItem }
candidate id name status metadata =
    { id = id, name = name, status = status, metadata = meta metadata }



-- TRANSPORT (§4.1 request, §7.1 framing, §4.3 status)


transportSuite : Test
transportSuite =
    describe "CloudShield POC transport (§7.1)"
        -- The §4.1 request bodies themselves are pinned as exact bytes in Tests.CloudShield.Wire;
        -- what matters here is the framing they are chunked into.
        [ test "reqSlotMetadata emits seq + chunk-count + chunked body" <|
            \_ ->
                let
                    items =
                        Transport.reqSlotMetadata 7 "hello"

                    keys =
                        List.map .key items
                in
                -- The cancel channel is cleared by the same atomic write, so a stop aimed at the
                -- previous run can never survive into this one.
                Expect.equal [ "exoext.v1.req.seq", "exoext.v1.req.body.n", "exoext.v1.req.cancel", "exoext.v1.req.body.0" ] keys
        , test "chunkString splits at the size boundary" <|
            \_ ->
                Expect.equal [ "abc", "de" ] (Transport.chunkString 3 "abcde")
        , test "chunkString keeps a short string whole" <|
            \_ ->
                Expect.equal [ "abc" ] (Transport.chunkString 255 "abc")
        , test "reqSlotMetadata chunks a >255-char body across body.0/body.1" <|
            \_ ->
                let
                    big =
                        String.repeat 300 "x"

                    keys =
                        Transport.reqSlotMetadata 1 big |> List.map .key
                in
                Expect.equal [ "exoext.v1.req.seq", "exoext.v1.req.body.n", "exoext.v1.req.cancel", "exoext.v1.req.body.0", "exoext.v1.req.body.1" ] keys
        , test "readChunkedBody honors the count and ignores a stale orphan chunk" <|
            \_ ->
                -- a later, shorter write left body.2 behind; the count (n=2) must win.
                Expect.equal (Just "abcdEFGH")
                    (Transport.readChunkedBody "exoext.v1.body."
                        (meta
                            [ ( "exoext.v1.body.n", "2" )
                            , ( "exoext.v1.body.0", "abcd" )
                            , ( "exoext.v1.body.1", "EFGH" )
                            , ( "exoext.v1.body.2", "STALE" )
                            ]
                        )
                    )
        , test "runStatusFromMetadata reads seq + state" <|
            \_ ->
                -- The §4.3 descriptors are covered in Tests.Exoext.Transport; here only the two
                -- required keys matter.
                Expect.equal (Just ( 4, "running" ))
                    (Transport.runStatusFromMetadata (meta [ ( "exoext.v1.run.seq", "4" ), ( "exoext.v1.run.state", "running" ) ])
                        |> Maybe.map (\status -> ( status.seq, status.state ))
                    )
        , test "runStatusFromMetadata is Nothing when state is missing" <|
            \_ ->
                Expect.equal Nothing
                    (Transport.runStatusFromMetadata (meta [ ( "exoext.v1.run.seq", "4" ) ]))
        , test "resultBodyFromMetadata reassembles res.body chunks" <|
            \_ ->
                Expect.equal (Just "RESULT")
                    (Transport.resultBodyFromMetadata (meta [ ( "exoext.v1.res.body.0", "RES" ), ( "exoext.v1.res.body.1", "ULT" ) ]))
        ]



-- EMBED (getEmbed action, Phase B). The single-req-slot guard is host-side (see
-- Tests.CloudShield.Wire getEmbedBlockedSuite); the card just emits the request.


idleModel : Card.Model
idleModel =
    let
        base =
            sampleModel Set.empty
    in
    { base | scanState = Dict.empty }


embedSuite : Test
embedSuite =
    describe "cloudshield.getEmbed emits EmbedRequested (guard is host-side)"
        [ test "a getEmbed with a resultId emits EmbedRequested regardless of card scan state" <|
            \_ ->
                -- sampleModel carries a stale "queued" scan for i-2; the card no longer guards on it.
                Expect.equal (Just (Card.EmbedRequested { resultId = "r-1", batchId = "b-1" }))
                    (Card.requestEmbed (Just { resultId = "r-1", batchId = "b-1" }) (sampleModel Set.empty) |> Tuple.second)
        , test "a getEmbed with no ids is a no-op" <|
            \_ ->
                Expect.equal Nothing
                    (Card.requestEmbed Nothing idleModel |> Tuple.second)
        , test "the params reader prefers resultId and keeps batchId beside it" <|
            \_ ->
                Expect.equal (Just { resultId = "r-1", batchId = "b-1" })
                    (Card.resultIdOf
                        (Encode.object
                            [ ( "resultId", Encode.string "r-1" )
                            , ( "batchId", Encode.string "b-1" )
                            ]
                        )
                    )
        , test "a manifest that emits only batchId still resolves both ids from it (fallback)" <|
            \_ ->
                Expect.equal (Just { resultId = "b-1", batchId = "b-1" })
                    (Card.resultIdOf (Encode.object [ ( "batchId", Encode.string "b-1" ) ]))
        , test "a press carrying only resultId mirrors it into batchId" <|
            \_ ->
                Expect.equal (Just { resultId = "r-1", batchId = "r-1" })
                    (Card.resultIdOf (Encode.object [ ( "resultId", Encode.string "r-1" ) ]))
        , test "neither id present is Nothing (fail-closed)" <|
            \_ ->
                Expect.equal Nothing
                    (Card.resultIdOf (Encode.object [ ( "targetName", Encode.string "alpha" ) ]))
        ]



-- HISTORY (the /history projection, newest first)


{-| A LEGACY index row: no per-run `requestId`, so its resultId falls back to the batchId. Most of
the suite below uses it, which keeps that fallback covered; `siblingEntry` covers the new path.
-}
historyEntry : String -> Wire.IndexEntry
historyEntry batchId =
    { batchId = batchId
    , requestId = Nothing
    , targetId = "i-1"
    , targetName = "alpha"
    , completedAt = "2026-07-01T00:00:00Z"
    , status = "done"
    , counts = { critical = 0, high = 2, medium = 0, low = 0, info = 0 }
    }


{-| One §2.2 sibling: its own `requestId` (hence its own resultId) under a batchId it SHARES with
the other siblings — the live shape that made BOTH rows of a batch read "Now viewing".
-}
siblingEntry : String -> String -> Wire.IndexEntry
siblingEntry batchId requestId =
    let
        base =
            historyEntry batchId
    in
    { base | requestId = Just requestId }


historyBatchIds : Decode.Value -> Result String (List String)
historyBatchIds value =
    Decode.decodeValue
        (Decode.field "history" (Decode.list (Decode.field "batchId" Decode.string)))
        value
        |> Result.mapError Decode.errorToString


historyNoteOf : Decode.Value -> Result String String
historyNoteOf value =
    Decode.decodeValue (Decode.field "historyNote" Decode.string) value
        |> Result.mapError Decode.errorToString


{-| `n` history entries in append-only (oldest-first) file order, batchIds `b-01`..`b-<n>`.
-}
historyEntries : Int -> List Wire.IndexEntry
historyEntries n =
    List.range 1 n
        |> List.map (\i -> historyEntry ("b-" ++ String.padLeft 2 '0' (String.fromInt i)))


projectHistory : List Wire.IndexEntry -> Decode.Value
projectHistory entries =
    Card.projection Time.utc { sampleConfig | history = loadedHistory entries } sampleInstances idleModel


{-| Project a single history row and read a string field from `/history/0`, given the active and
in-flight (`pendingResultId`) batch ids. No embed-error batch (the common case).
-}
historyRowField : Maybe String -> Maybe String -> Wire.IndexEntry -> String -> Result String String
historyRowField activeResultId pendingResultId entry field =
    historyRowFieldE activeResultId pendingResultId Nothing entry field


{-| As `historyRowField`, but also supplying the `erroredResultId` (the last getEmbed that failed /
timed out) so the "Couldn't open" / "Retry" precedence can be exercised.
-}
historyRowFieldE : Maybe String -> Maybe String -> Maybe String -> Wire.IndexEntry -> String -> Result String String
historyRowFieldE activeResultId pendingResultId erroredResultId entry field =
    Card.projection Time.utc
        { sampleConfig
            | history = loadedHistory [ entry ]
            , activeResultId = activeResultId
            , pendingResultId = pendingResultId
            , erroredResultId = erroredResultId
        }
        sampleInstances
        idleModel
        |> Decode.decodeValue (Decode.field "history" (Decode.index 0 (Decode.field field Decode.string)))
        |> Result.mapError Decode.errorToString


{-| Project a single history row supplying the `expiredResultId` (a row whose result session has
expired), so the expired-session row state ("Expired" / plain "View", faint highlight) can be
exercised.
-}
historyRowFieldX : Maybe String -> Maybe String -> Wire.IndexEntry -> String -> Result String String
historyRowFieldX activeResultId expiredResultId entry field =
    Card.projection Time.utc
        { sampleConfig
            | history = loadedHistory [ entry ]
            , activeResultId = activeResultId
            , expiredResultId = expiredResultId
        }
        sampleInstances
        idleModel
        |> Decode.decodeValue (Decode.field "history" (Decode.index 0 (Decode.field field Decode.string)))
        |> Result.mapError Decode.errorToString


erroredEntry : Wire.IndexEntry
erroredEntry =
    { batchId = "b-err"
    , requestId = Nothing
    , targetId = "i-1"
    , targetName = "alpha"
    , completedAt = "2026-07-01T00:00:00Z"
    , status = "error"
    , counts = { critical = 0, high = 0, medium = 0, low = 0, info = 0 }
    }


historySuite : Test
historySuite =
    describe "the /history projection"
        [ test "rows are newest first (the append-only index is reversed)" <|
            \_ ->
                Expect.equal (Ok [ "b-3", "b-2", "b-1" ])
                    (historyBatchIds (projectHistory [ historyEntry "b-1", historyEntry "b-2", historyEntry "b-3" ]))
        , test "each row carries a human countsLabel" <|
            \_ ->
                let
                    label =
                        Decode.decodeValue
                            (Decode.field "history" (Decode.index 0 (Decode.field "countsLabel" Decode.string)))
                            (projectHistory [ historyEntry "b-1" ])
                            |> Result.mapError Decode.errorToString
                in
                Expect.equal (Ok "2 high") label
        , test "at or under the cap, all rows show and the note is empty" <|
            \_ ->
                let
                    value =
                        projectHistory (historyEntries 20)
                in
                Expect.equal ( Ok 20, Ok "" )
                    ( historyBatchIds value |> Result.map List.length
                    , historyNoteOf value
                    )
        , test "above the cap, only the latest 20 show (newest first) with the overflow note" <|
            \_ ->
                let
                    value =
                        projectHistory (historyEntries 25)

                    ids =
                        historyBatchIds value
                in
                Expect.equal ( Ok 20, Ok "b-25", Ok "b-06" )
                    ( ids |> Result.map List.length
                    , ids |> Result.map (List.head >> Maybe.withDefault "?")
                    , ids |> Result.map (List.reverse >> List.head >> Maybe.withDefault "?")
                    )
        , test "the overflow note wording names the cap and the total" <|
            \_ ->
                Expect.equal (Ok "Showing the latest 20 of 25 scans.")
                    (historyNoteOf (projectHistory (historyEntries 25)))
        , test "completedAt is humanized (local, 12-hour), not raw ISO" <|
            \_ ->
                Expect.equal (Ok "Jul 1, 2026 · 12:00 AM")
                    (Decode.decodeValue
                        (Decode.field "history" (Decode.index 0 (Decode.field "completedAt" Decode.string)))
                        (projectHistory [ historyEntry "b-1" ])
                        |> Result.mapError Decode.errorToString
                    )
        , test "a done row not being viewed is a plain View with an empty (hidden) rowState" <|
            \_ ->
                Expect.equal ( Ok "", Ok "View", Ok "alpha · #b-1" )
                    ( historyRowField Nothing Nothing (historyEntry "b-1") "rowState"
                    , historyRowField Nothing Nothing (historyEntry "b-1") "actionLabel"
                    , historyRowField Nothing Nothing (historyEntry "b-1") "subLabel"
                    )
        , test "an expired session row reverts to a muted Expired badge and a plain View (the expiry fix)" <|
            \_ ->
                -- b-1's session expired: the host sets expiredResultId (not activeResultId), so the row
                -- reads "Expired" / "View" (reopen) instead of "Now viewing" / "Refresh".
                Expect.equal ( Ok "Expired", Ok "View" )
                    ( historyRowFieldX Nothing (Just "b-1") (historyEntry "b-1") "rowState"
                    , historyRowFieldX Nothing (Just "b-1") (historyEntry "b-1") "actionLabel"
                    )
        , test "the active batch row flips to Now viewing / Refresh" <|
            \_ ->
                Expect.equal ( Ok "Now viewing", Ok "Refresh" )
                    ( historyRowField (Just "b-1") Nothing (historyEntry "b-1") "rowState"
                    , historyRowField (Just "b-1") Nothing (historyEntry "b-1") "actionLabel"
                    )
        , test "the row whose getEmbed is in flight shows the Opening… loading state (button and badge)" <|
            \_ ->
                Expect.equal ( Ok "Opening…", Ok "Opening…" )
                    ( historyRowField Nothing (Just "b-1") (historyEntry "b-1") "rowState"
                    , historyRowField Nothing (Just "b-1") (historyEntry "b-1") "actionLabel"
                    )
        , test "a pending getEmbed suppresses Now viewing on the previously-active row (it drops to plain View)" <|
            \_ ->
                -- b-2's getEmbed is in flight while b-1 was the active pick: b-1 must NOT read as
                -- Now viewing; it falls back to an idle View row.
                Expect.equal ( Ok "", Ok "View" )
                    ( historyRowField (Just "b-1") (Just "b-2") (historyEntry "b-1") "rowState"
                    , historyRowField (Just "b-1") (Just "b-2") (historyEntry "b-1") "actionLabel"
                    )
        , test "the loading row wins over Now viewing when it is also the active batch" <|
            \_ ->
                Expect.equal ( Ok "Opening…", Ok "Opening…" )
                    ( historyRowField (Just "b-1") (Just "b-1") (historyEntry "b-1") "rowState"
                    , historyRowField (Just "b-1") (Just "b-1") (historyEntry "b-1") "actionLabel"
                    )
        , test "a failed scan is muted but still NAMES its target, like every other row" <|
            \_ ->
                -- "which instance failed?" is the first question a failure raises, and the badge
                -- (rowState / state) already says that it failed.
                Expect.equal ( Ok "failed", Ok "", Ok "alpha · #b-err" )
                    ( historyRowField Nothing Nothing erroredEntry "rowState"
                    , historyRowField Nothing Nothing erroredEntry "actionLabel"
                    , historyRowField Nothing Nothing erroredEntry "subLabel"
                    )
        , test "a scan with no batch id (§4.1 single-target) drops the id rather than showing a bare #" <|
            \_ ->
                let
                    doneEntry =
                        historyEntry "b-1"
                in
                Expect.equal ( Ok "alpha", Ok "alpha" )
                    ( historyRowField Nothing Nothing { erroredEntry | batchId = "" } "subLabel"
                    , historyRowField Nothing Nothing { doneEntry | batchId = "" } "subLabel"
                    )
        , test "an errored batch never reads as active even if it is the active batchId" <|
            \_ ->
                Expect.equal ( Ok "failed", Ok "" )
                    ( historyRowField (Just "b-err") Nothing erroredEntry "rowState"
                    , historyRowField (Just "b-err") Nothing erroredEntry "actionLabel"
                    )
        , test "an errored batch never reads as loading even if its getEmbed is pending" <|
            \_ ->
                Expect.equal ( Ok "failed", Ok "" )
                    ( historyRowField Nothing (Just "b-err") erroredEntry "rowState"
                    , historyRowField Nothing (Just "b-err") erroredEntry "actionLabel"
                    )
        , test "per-row findings are synthesized from counts so history pills match results pills" <|
            \_ ->
                Expect.equal (Ok [ "high", "high" ])
                    (Decode.decodeValue
                        (Decode.field "history" (Decode.index 0 (Decode.field "findings" (Decode.list (Decode.field "severity" Decode.string)))))
                        (projectHistory [ historyEntry "b-1" ])
                        |> Result.mapError Decode.errorToString
                    )
        , test "the errored-getEmbed batch row reads Couldn't open / Retry" <|
            \_ ->
                Expect.equal ( Ok "Couldn't open", Ok "Retry" )
                    ( historyRowFieldE Nothing Nothing (Just "b-1") (historyEntry "b-1") "rowState"
                    , historyRowFieldE Nothing Nothing (Just "b-1") (historyEntry "b-1") "actionLabel"
                    )
        , test "while an embed error is shown, the prior active row does NOT reclaim Now viewing" <|
            \_ ->
                -- b-1 was the active pick, then its getEmbed errored (erroredResultId = b-1). b-1 must
                -- read as Couldn't open, never Now viewing.
                Expect.equal ( Ok "Couldn't open", Ok "Retry" )
                    ( historyRowFieldE (Just "b-1") Nothing (Just "b-1") (historyEntry "b-1") "rowState"
                    , historyRowFieldE (Just "b-1") Nothing (Just "b-1") (historyEntry "b-1") "actionLabel"
                    )
        , test "an embed error on one batch suppresses Now viewing on a DIFFERENT active row" <|
            \_ ->
                -- b-2's getEmbed errored while b-1 was the active pick: b-1 must NOT read Now viewing.
                Expect.equal ( Ok "", Ok "View" )
                    ( historyRowFieldE (Just "b-1") Nothing (Just "b-2") (historyEntry "b-1") "rowState"
                    , historyRowFieldE (Just "b-1") Nothing (Just "b-2") (historyEntry "b-1") "actionLabel"
                    )
        , test "a fresh getEmbed (pending) wins over a stale embed error on the same row" <|
            \_ ->
                -- Retry fired: pending = b-1 again while the old error for b-1 is cleared host-side; if
                -- both were somehow set, loading supersedes the error look for that row.
                Expect.equal ( Ok "Opening…", Ok "Opening…" )
                    ( historyRowFieldE Nothing (Just "b-1") Nothing (historyEntry "b-1") "rowState"
                    , historyRowFieldE Nothing (Just "b-1") Nothing (historyEntry "b-1") "actionLabel"
                    )
        , test "an errored SCAN is never overridden by an embed-error state (stays failed, no view)" <|
            \_ ->
                Expect.equal ( Ok "failed", Ok "" )
                    ( historyRowFieldE Nothing Nothing (Just "b-err") erroredEntry "rowState"
                    , historyRowFieldE Nothing Nothing (Just "b-err") erroredEntry "actionLabel"
                    )
        , test "the column headers carry a live count of targets and total scans" <|
            \_ ->
                let
                    value =
                        Card.projection Time.utc { sampleConfig | history = loadedHistory (historyEntries 3) } sampleInstances idleModel

                    stringField key =
                        Decode.decodeValue (Decode.field key Decode.string) value
                            |> Result.mapError Decode.errorToString
                in
                -- sampleInstances has 2 targets; 3 history entries.
                Expect.equal ( Ok "Scan targets · 2", Ok "Scan history · 3" )
                    ( stringField "targetsLabel", stringField "historyLabel" )
        , test "a row's resultId is its requestId, falling back to batchId on a legacy row" <|
            \_ ->
                Expect.equal (Ok [ "r-2", "b-1" ])
                    (Decode.decodeValue
                        (Decode.field "history" (Decode.list (Decode.field "resultId" Decode.string)))
                        (projectHistory [ historyEntry "b-1", siblingEntry "b-9" "r-2" ])
                        |> Result.mapError Decode.errorToString
                    )
        , test "exactly ONE of two siblings sharing a batchId reads as viewing (the live regression)" <|
            \_ ->
                -- The observed failure: both rows of a WP5 batch showed "Now viewing" because the
                -- session matched on the SHARED batchId. Matching on the per-run resultId fixes it.
                let
                    rows =
                        [ siblingEntry "b-shared" "r-1", siblingEntry "b-shared" "r-2" ]

                    states =
                        Card.projection Time.utc { sampleConfig | history = loadedHistory rows, activeResultId = Just "r-2" } sampleInstances idleModel
                            |> Decode.decodeValue
                                (Decode.field "history" (Decode.list (Decode.field "state" Decode.string)))
                            |> Result.mapError Decode.errorToString
                in
                -- Newest first, so r-2 (the active one) leads.
                Expect.equal (Ok [ "viewing", "idle" ]) states
        ]


{-| Regression pin for the history View button: its `params` must emit the id VALUES, not the item
PATHS. A top-level `{"$item":"batchId"}` resolves (per `Expr.resolveTopLevelParam`) to
`"/history/0/batchId"` — the pointer string — which the bridge rejects. The manifest uses
`{"$template":"${…}"}`, which resolves to the row's value.
-}
historyViewParamsSuite : Test
historyViewParamsSuite =
    describe "the history View button emits id values, not JSON Pointers"
        [ test "resolveParams in a /history row 0 childContext yields the item's resultId + batchId values" <|
            \_ ->
                case JsonRender.decodeString cardJsonV3 of
                    Ok spec ->
                        case Dict.get "history-view-btn" spec.elements |> Maybe.andThen (\el -> Dict.get "press" el.on) |> Maybe.andThen List.head of
                            Just binding ->
                                let
                                    item =
                                        Encode.object
                                            [ ( "resultId", Encode.string "r-1" )
                                            , ( "batchId", Encode.string "b-1" )
                                            , ( "targetName", Encode.string "alpha" )
                                            ]

                                    ctx =
                                        Expr.childContext "/history" 0 item (Expr.rootContext [] Encode.null)
                                in
                                Expr.resolveParams ctx binding.params
                                    |> Card.resultIdOf
                                    |> Expect.equal (Just { resultId = "r-1", batchId = "b-1" })

                            Nothing ->
                                Expect.fail "history-view-btn should carry a press binding"

                    Err message ->
                        Expect.fail ("card.json v3 should validate: " ++ message)
        ]



-- BLOCKED-SCAN ROLLBACK


{-| A minimal confirm-gated Scan button. Reaching a renderer state with an OPEN confirm dialog is
only possible through a real press — `Render.Msg` is opaque — so this is rendered and clicked.
-}
confirmGatedScanManifest : String
confirmGatedScanManifest =
    """
    { "root": "scan-btn"
    , "elements":
        { "scan-btn":
            { "type": "Button"
            , "props": { "label": "Scan selected" }
            , "on":
                { "press":
                    { "action": "exoext.writeRequest"
                    , "params": { "kind": "scan", "targetInstanceIds": [] }
                    , "confirm":
                        { "title": "Scan selected instances?"
                        , "message": "This starts a scan."
                        , "confirmLabel": "Scan"
                        , "cancelLabel": "Cancel"
                        }
                    }
                }
            }
        }
    }
    """


{-| The renderer with its confirm dialog open: press the confirm-gated button and feed the
resulting message back through `Render.update`. Falls back to `init` if the press cannot be
simulated, which the suite asserts against so the test cannot pass vacuously.
-}
rendererWithOpenDialog : Render.Model
rendererWithOpenDialog =
    case JsonRender.decodeString confirmGatedScanManifest of
        Ok spec ->
            Render.view [] spec Encode.null Render.init
                |> Query.fromHtml
                |> Query.find [ Selector.tag "button" ]
                |> Event.simulate Event.click
                |> Event.toResult
                |> Result.map (\msg -> Render.update msg Render.init |> Tuple.first)
                |> Result.withDefault Render.init

        Err _ ->
            Render.init


rollbackScanRequestSuite : Test
rollbackScanRequestSuite =
    let
        -- The card as it stands with the confirm dialog open, before the user accepts.
        beforePress =
            { renderer = rendererWithOpenDialog
            , selection = Set.singleton "i-1"
            , selectAll = False
            , scanState = Dict.fromList [ ( "i-2", "done" ) ]
            , seq = 4
            , pending = Nothing
            , showDemoIframe = False
            , showManifestErrorDetail = False
            }

        -- Accepting the dialog closes it (renderer back to `init`) and, in the same step, runs
        -- `requestScan` — exactly what `Card.update`'s RendererMsg branch does.
        scanParams =
            Encode.object
                [ ( "kind", Encode.string "scan" )
                , ( "targetInstanceIds", Encode.list Encode.string [ "i-1" ] )
                ]

        afterPress =
            Card.dispatchVerb (Card.resolveAction "exoext.writeRequest" scanParams)
                scanParams
                { beforePress | renderer = Render.init }
                |> Tuple.first

        rolledBack =
            Card.rollbackScanRequest beforePress afterPress
    in
    describe "CloudShield rollbackScanRequest (host refused the §7.1 request)"
        [ test "the simulated press really opens the dialog (guards the fixture)" <|
            \_ ->
                Expect.notEqual Render.init rendererWithOpenDialog
        , test "the press does mutate the scan-tracking fields" <|
            \_ ->
                Expect.equal ( 5, Just "i-1", Just "queued" )
                    ( afterPress.seq
                    , afterPress.pending |> Maybe.map .subject
                    , Dict.get "i-1" afterPress.scanState
                    )
        , test "rolling back restores seq, pending and scanState" <|
            \_ ->
                Expect.equal ( beforePress.seq, beforePress.pending, beforePress.scanState )
                    ( rolledBack.seq, rolledBack.pending, rolledBack.scanState )
        , test "rolling back keeps the CLOSED dialog — the press must not re-open it" <|
            \_ ->
                -- The bug this guards: restoring the whole pre-press card would put back
                -- `pendingConfirm`, so Confirm would appear to do nothing and the dialog would
                -- stay open on exactly the path the §7.1 guard exists to protect.
                Expect.equal Render.init rolledBack.renderer
        , test "rolling back keeps the rest of the card untouched" <|
            \_ ->
                Expect.equal ( afterPress.selection, afterPress.selectAll, afterPress.showDemoIframe )
                    ( rolledBack.selection, rolledBack.selectAll, rolledBack.showDemoIframe )
        ]
