module Tests.JsonRender.CountPillsTest exposing (suite)

{-| Focused coverage for `CountPills`: the `emptyLabel` prop, and the host/manifest vocabulary.

The `emptyLabel` rule: with no groups to show, an absent `emptyLabel` keeps the renderer's
provisional "No <plural> yet"; a resolved label replaces it; a resolved EMPTY string renders no
empty-state node at all. A table that does have rows is untouched either way.

Why it exists: a provisional default is right for a table with nothing bound and wrong for a
completed, genuinely empty one. Only the publisher knows which, so it supplies the string.

The vocabulary rule: the renderer counts and orders, but every WORD comes from the manifest when it
names one and from the host's `CountPillDefaults` when it does not. The wire-compatibility tests
below pin that an already-published `FindingsTable` manifest — old element name, no vocabulary keys
— still renders exactly what it rendered before the element was generalized, purely from the
defaults its adapter supplies.

Presence of the empty-state NODE is counted by `<div>`s rather than matched by class:
`Test.Html.Selector`'s class selectors do not see `Attr.class` under this runner.

-}

import Dict
import Expect
import Json.Encode as Encode
import JsonRender
import JsonRender.Render as Render
import JsonRender.Spec exposing (Spec)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


specOf : String -> Spec
specOf raw =
    case JsonRender.decodeString raw of
        Ok spec ->
            spec

        Err _ ->
            { root = "missing", elements = Dict.empty, state = Encode.null }


render : String -> Encode.Value -> Query.Single Render.Msg
render raw state =
    Render.view Render.defaultOptions (specOf raw) state Render.init
        |> Query.fromHtml


{-| The vocabulary the card adapter supplies, mirrored here so the wire-compatibility tests
below are about the mechanism and not about importing the adapter.
-}
scannerOptions : Render.Options
scannerOptions =
    { allowedIframeOrigins = []
    , countPills =
        { groupBy = "severity"
        , groupOrder = [ "critical", "high", "medium", "low", "info" ]
        , itemNoun = "finding"
        , itemNounPlural = "findings"
        }
    }


renderWith : Render.Options -> String -> Encode.Value -> Query.Single Render.Msg
renderWith options raw state =
    Render.view options (specOf raw) state Render.init
        |> Query.fromHtml


{-| A `CountPills` bound to `/results`, with whatever `emptyLabel` fragment is spliced in.
-}
tableWith : String -> String
tableWith emptyLabelProp =
    """
    { "root": "t"
    , "elements":
        { "t":
            { "type": "CountPills"
            , "props": { "bind": { "$state": "/results" }, "groupBy": "severity"@@ }
            , "children": []
            }
        }
    }
    """
        |> String.replace "@@" emptyLabelProp


{-| The element exactly as the deployed `card.json` v5 publishes it: the pre-rename type name and
no vocabulary keys at all.
-}
legacyTable : String
legacyTable =
    """
    { "root": "t"
    , "elements":
        { "t":
            { "type": "FindingsTable"
            , "props": { "bind": { "$state": "/results" }, "groupBy": "severity" }
            , "children": []
            }
        }
    }
    """


noRows : Encode.Value
noRows =
    Encode.object [ ( "results", Encode.list identity [] ) ]


someRows : Encode.Value
someRows =
    Encode.object
        [ ( "results"
          , Encode.list identity
                [ Encode.object [ ( "severity", Encode.string "high" ) ] ]
          )
        ]


{-| Two lows and one critical: enough that alphabetical order, descending-count order, and the
publisher's own order all disagree.
-}
mixedRows : Encode.Value
mixedRows =
    Encode.object
        [ ( "results"
          , Encode.list identity
                [ Encode.object [ ( "severity", Encode.string "low" ) ]
                , Encode.object [ ( "severity", Encode.string "critical" ) ]
                , Encode.object [ ( "severity", Encode.string "low" ) ]
                ]
          )
        ]


{-| How many `<div>`s the render emitted below the renderer root: 1 when an empty-state node was
drawn, 0 when none was.
-}
divCount : Query.Single Render.Msg -> Query.Multiple Render.Msg
divCount =
    Query.findAll [ Selector.tag "div" ]


conditionalEmptyLabel : String
conditionalEmptyLabel =
    """, "emptyLabel": { "$cond": { "$state": "/results" }, "$then": "No vulnerabilities found", "$else": "" } """


suite : Test
suite =
    describe "JsonRender.Render CountPills"
        [ describe "emptyLabel"
            [ test "an absent emptyLabel keeps the renderer's provisional default" <|
                \_ ->
                    render (tableWith "") noRows
                        |> Query.has [ Selector.text "No items yet" ]
            , test "a supplied emptyLabel is what an empty table says instead" <|
                \_ ->
                    render (tableWith """, "emptyLabel": "No vulnerabilities found" """) noRows
                        |> Query.has [ Selector.text "No vulnerabilities found" ]
            , test "an emptyLabel resolving to the empty string renders no empty-state node at all" <|
                \_ ->
                    render (tableWith """, "emptyLabel": "" """) noRows
                        |> divCount
                        |> Query.count (Expect.equal 0)
            , test "a supplied label does render a node (guards the count above)" <|
                \_ ->
                    render (tableWith """, "emptyLabel": "No vulnerabilities found" """) noRows
                        |> divCount
                        |> Query.count (Expect.equal 1)
            , test "an expression-valued emptyLabel resolves against state" <|
                \_ ->
                    render (tableWith conditionalEmptyLabel) noRows
                        |> Query.has [ Selector.text "No vulnerabilities found" ]
            , test "the same expression stays silent when nothing is bound (null results)" <|
                \_ ->
                    render (tableWith conditionalEmptyLabel) (Encode.object [ ( "results", Encode.null ) ])
                        |> divCount
                        |> Query.count (Expect.equal 0)
            , test "a table WITH rows is unaffected by emptyLabel" <|
                \_ ->
                    render (tableWith """, "emptyLabel": "No vulnerabilities found" """) someRows
                        |> Query.has [ Selector.text "1 item", Selector.text "high" ]
            ]
        , describe "vocabulary"
            [ test "the manifest's own nouns win over the host's" <|
                \_ ->
                    render
                        ("""
                        { "root": "t"
                        , "elements":
                            { "t":
                                { "type": "CountPills"
                                , "props": { "bind": { "$state": "/results" }, "groupBy": "severity", "itemNoun": "alert", "itemNounPlural": "alerts" }
                                , "children": []
                                }
                            }
                        }
                        """
                            |> String.trim
                        )
                        someRows
                        |> Query.has [ Selector.text "1 alert" ]
            , test "with no groupOrder anywhere, the biggest group leads" <|
                \_ ->
                    render (tableWith "") mixedRows
                        |> Query.findAll [ Selector.tag "span" ]
                        |> Query.index 4
                        |> Query.has [ Selector.text "low" ]
            ]
        , describe "wire compatibility with the published FindingsTable manifest"
            [ test "the old element name still decodes" <|
                \_ ->
                    renderWith scannerOptions legacyTable someRows
                        |> Query.has [ Selector.text "high" ]
            , test "the host's nouns are what an unchanged manifest counts in" <|
                \_ ->
                    renderWith scannerOptions legacyTable someRows
                        |> Query.has [ Selector.text "1 finding" ]
            , test "the host's empty label reads exactly as it did before the rename" <|
                \_ ->
                    renderWith scannerOptions legacyTable noRows
                        |> Query.has [ Selector.text "No findings yet" ]
            , test "the host's groupOrder puts critical ahead of the larger low group" <|
                \_ ->
                    renderWith scannerOptions legacyTable mixedRows
                        |> Query.findAll [ Selector.tag "span" ]
                        |> Query.index 4
                        |> Query.has [ Selector.text "critical" ]
            ]
        ]
