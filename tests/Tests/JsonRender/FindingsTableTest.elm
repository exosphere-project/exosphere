module Tests.JsonRender.FindingsTableTest exposing (suite)

{-| Focused coverage for the FindingsTable `emptyLabel` prop.

The rule: with no groups to show, an absent `emptyLabel` keeps the renderer's provisional
"No findings yet"; a resolved label replaces it; a resolved EMPTY string renders no empty-state
node at all. A table that does have findings is untouched either way.

Why it exists: "No findings yet" is right for a table with nothing bound and wrong for a
completed, genuinely clean scan. Only the publisher knows which, so it supplies the string.

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
    Render.view [] (specOf raw) state Render.init
        |> Query.fromHtml


{-| A FindingsTable bound to `/results`, with whatever `emptyLabel` fragment is spliced in.
-}
tableWith : String -> String
tableWith emptyLabelProp =
    """
    { "root": "t"
    , "elements":
        { "t":
            { "type": "FindingsTable"
            , "props": { "bind": { "$state": "/results" }, "groupBy": "severity"@@ }
            , "children": []
            }
        }
    }
    """
        |> String.replace "@@" emptyLabelProp


noFindings : Encode.Value
noFindings =
    Encode.object [ ( "results", Encode.list identity [] ) ]


someFindings : Encode.Value
someFindings =
    Encode.object
        [ ( "results"
          , Encode.list identity
                [ Encode.object [ ( "severity", Encode.string "high" ) ] ]
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
    describe "JsonRender.Render FindingsTable emptyLabel"
        [ test "an absent emptyLabel keeps the renderer's provisional default" <|
            \_ ->
                render (tableWith "") noFindings
                    |> Query.has [ Selector.text "No findings yet" ]
        , test "a supplied emptyLabel is what an empty table says instead" <|
            \_ ->
                render (tableWith """, "emptyLabel": "No vulnerabilities found" """) noFindings
                    |> Query.has [ Selector.text "No vulnerabilities found" ]
        , test "an emptyLabel resolving to the empty string renders no empty-state node at all" <|
            \_ ->
                render (tableWith """, "emptyLabel": "" """) noFindings
                    |> divCount
                    |> Query.count (Expect.equal 0)
        , test "a supplied label does render a node (guards the count above)" <|
            \_ ->
                render (tableWith """, "emptyLabel": "No vulnerabilities found" """) noFindings
                    |> divCount
                    |> Query.count (Expect.equal 1)
        , test "an expression-valued emptyLabel resolves against state" <|
            \_ ->
                render (tableWith conditionalEmptyLabel) noFindings
                    |> Query.has [ Selector.text "No vulnerabilities found" ]
        , test "the same expression stays silent when nothing is bound (null results)" <|
            \_ ->
                render (tableWith conditionalEmptyLabel) (Encode.object [ ( "results", Encode.null ) ])
                    |> divCount
                    |> Query.count (Expect.equal 0)
        , test "a table WITH findings is unaffected by emptyLabel" <|
            \_ ->
                render (tableWith """, "emptyLabel": "No vulnerabilities found" """) someFindings
                    |> Query.has [ Selector.text "1 finding", Selector.text "high" ]
        ]
