module Tests.CloudShield.Manifest exposing (shippedManifestSuite)

{-| The shipped CloudShield card manifest, decoded against THIS build's catalog.

The manifest is the extension's, not the host's, and the renderer is fail-closed — a manifest that
uses catalog surface this build does not have renders as an error notice rather than as a card. That
is the correct behavior and a terrible thing to discover on a live appliance, because the two halves
ship separately: the manifest rides a bridge release, the catalog rides a fork build. So the copy
the bridge serves is pinned here and decoded, which turns "these two releases do not fit together"
into a failing test.

Keep this in step with `~/dev/cloudshield-bridge-attach/cloudshield_bridge/card.json`.

-}

import Expect
import JsonRender
import Test exposing (Test, describe, test)


shippedManifestSuite : Test
shippedManifestSuite =
    describe "the shipped CloudShield card manifest"
        [ test "decodes against this build's catalog" <|
            \_ ->
                case JsonRender.decodeString shippedManifest of
                    Ok _ ->
                        Expect.pass

                    Err message ->
                        Expect.fail ("the shipped manifest was refused: " ++ message)
        ]


shippedManifest : String
shippedManifest =
    """
{
  "root": "card",
  "elements": {
    "card": {
      "type": "Card",
      "props": {},
      "children": [
        "columns",
        "results-header",
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
        "label": "Scan selected",
        "disabled": {
          "$state": "/scanBusy"
        }
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
        "row-scan-btn",
        "row-cancel-btn"
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
          "$cond": {
            "$item": "scanState",
            "eq": "stopping"
          },
          "$then": "stopping…",
          "$else": {
            "$item": "scanState"
          }
        }
      },
      "children": []
    },
    "row-scan-btn": {
      "type": "Button",
      "props": {
        "label": "Scan",
        "disabled": {
          "$state": "/scanBusy"
        }
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
    "row-cancel-btn": {
      "type": "Button",
      "props": {
        "label": {
          "$cond": {
            "$item": "cancellable"
          },
          "$then": "Stop",
          "$else": ""
        }
      },
      "on": {
        "press": {
          "action": "exoext.cancelRequest",
          "params": {
            "requestId": {
              "$template": "${cancelRequestId}"
            },
            "targetId": {
              "$template": "${cancelTargetId}"
            }
          },
          "confirm": {
            "title": "Stop this scan?",
            "message": "A running scan stops and its clone is cleaned up. A queued target is removed.",
            "variant": "danger"
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
        "history-view-btn",
        "history-remove-btn"
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
        "history-subline"
      ],
      "on": {
        "press": {
          "action": "exoext.openSession",
          "params": {
            "resultId": {
              "$cond": {
                "$item": "state",
                "eq": "failed"
              },
              "$then": "",
              "$else": {
                "$template": "${resultId}"
              }
            },
            "batchId": {
              "$cond": {
                "$item": "state",
                "eq": "failed"
              },
              "$then": "",
              "$else": {
                "$template": "${batchId}"
              }
            }
          }
        }
      }
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
            "eq": "removing"
          },
          "$then": "Removing…",
          "$else": {
            "$cond": {
              "$item": "state",
              "eq": "removeError"
            },
            "$then": {
              "$template": "Couldn't remove · ${removeErrorMessage}"
            },
            "$else": {
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
            }
          }
        },
        "variant": {
          "$item": "state"
        }
      },
      "children": [],
      "on": {
        "press": {
          "action": "exoext.showDetail",
          "params": {
            "title": "Scan failed",
            "text": {
              "$cond": {
                "$item": "state",
                "eq": "failed"
              },
              "$then": {
                "$cond": {
                  "$item": "errorDetail"
                },
                "$then": {
                  "$item": "errorDetail"
                },
                "$else": "This scan failed. The extension did not record a reason for it."
              },
              "$else": ""
            }
          }
        }
      }
    },
    "history-view-btn": {
      "type": "Button",
      "props": {
        "label": {
          "$cond": {
            "$or": [
              {
                "$item": "state",
                "eq": "removing"
              },
              {
                "$item": "state",
                "eq": "removeError"
              }
            ]
          },
          "$then": "",
          "$else": {
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
    "results-header": {
      "type": "Stack",
      "props": {
        "direction": "row",
        "gap": 2
      },
      "children": [
        "results-close-btn"
      ]
    },
    "results-close-btn": {
      "type": "Button",
      "props": {
        "label": {
          "$cond": {
            "$state": "/sessionOpen"
          },
          "$then": "Close results",
          "$else": ""
        }
      },
      "on": {
        "press": {
          "action": "exoext.dismissSession",
          "params": {}
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
    },
    "history-subline": {
      "type": "Stack",
      "props": {
        "direction": "row",
        "gap": 0
      },
      "children": [
        "history-target",
        "history-batch"
      ]
    },
    "history-target": {
      "type": "Text",
      "props": {
        "value": {
          "$item": "targetName"
        }
      },
      "on": {
        "press": {
          "action": "exoext.navigate",
          "params": {
            "instanceId": {
              "$template": "${targetId}"
            }
          }
        }
      },
      "children": []
    },
    "history-batch": {
      "type": "Text",
      "props": {
        "value": {
          "$item": "batchLabel"
        }
      },
      "children": []
    },
    "history-remove-btn": {
      "type": "Button",
      "props": {
        "label": {
          "$cond": {
            "$item": "state",
            "eq": "removing"
          },
          "$then": "",
          "$else": {
            "$cond": {
              "$item": "state",
              "eq": "removeError"
            },
            "$then": "Retry",
            "$else": "Remove"
          }
        },
        "disabled": {
          "$state": "/requestBusy"
        }
      },
      "on": {
        "press": {
          "action": "exoext.deleteResult",
          "params": {
            "resultId": {
              "$template": "${resultId}"
            },
            "batchId": {
              "$template": "${batchId}"
            }
          },
          "confirm": {
            "title": "Remove this scan?",
            "message": "Remove this scan from the history? Its results are deleted from the mailbox.",
            "variant": "danger"
          }
        }
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
    "scanBusy": false,
    "sessionOpen": false,
    "targetsLabel": "Scan targets",
    "historyLabel": "Scan history"
  }
}
"""
