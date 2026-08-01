module Tests.JsonRender.BadgeTest exposing (badgeToneSuite, suite)

{-| Focused coverage for the Badge `variant` prop (vendored port of the elm-json-render
`feat/expr-map` addition).

The rule: an optional `variant` expression, when present, resolves to the badge's `data-state`
attribute while the visible text and tone class stay keyed on `value`; when absent, `data-state`
falls back to the `value` text (old manifests unaffected). A malformed `variant` fails the decode
(fail-closed).

-}

import Dict
import Expect
import Html.Attributes
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


withVariant : String
withVariant =
    """
    { "root": "b"
    , "elements":
        { "b": { "type": "Badge", "props": { "value": { "$state": "/label" }, "variant": { "$state": "/token" } }, "children": [] } }
    }
    """


withoutVariant : String
withoutVariant =
    """
    { "root": "b"
    , "elements":
        { "b": { "type": "Badge", "props": { "value": { "$state": "/label" } }, "children": [] } }
    }
    """


malformedVariant : String
malformedVariant =
    """
    { "root": "b"
    , "elements":
        { "b": { "type": "Badge", "props": { "value": "x", "variant": { "$computed": "evil" } }, "children": [] } }
    }
    """


suite : Test
suite =
    describe "JsonRender.Render Badge variant"
        [ test "a present variant becomes data-state while value stays the visible text" <|
            \_ ->
                render withVariant
                    (Encode.object
                        [ ( "label", Encode.string "Now viewing" )
                        , ( "token", Encode.string "viewing" )
                        ]
                    )
                    |> Query.has
                        [ Selector.attribute (Html.Attributes.attribute "data-state" "viewing")
                        , Selector.text "Now viewing"
                        ]
        , test "an absent variant falls back to the value text as data-state (unchanged)" <|
            \_ ->
                render withoutVariant
                    (Encode.object [ ( "label", Encode.string "Now viewing" ) ])
                    |> Query.has
                        [ Selector.attribute (Html.Attributes.attribute "data-state" "Now viewing")
                        , Selector.text "Now viewing"
                        ]
        , test "a malformed variant expression fails the decode (fail-closed)" <|
            \_ ->
                case JsonRender.decodeString malformedVariant of
                    Ok _ ->
                        Expect.fail "a Badge with a $computed variant must be rejected"

                    Err _ ->
                        Expect.pass
        ]


{-| `badgeTone`, the value → tone-class mapping the host stylesheet keys its badge colors on.

Asserted on the function rather than on the rendered class attribute because
`Test.Html.Selector.class` matches the `className` PROPERTY, which elm-test-rs does not reproduce —
every class-based selector fails here even against markup that plainly carries the class (verified).
`data-state` is a real attribute and is asserted above; the tone class is not reachable that way.

-}
badgeToneSuite : Test
badgeToneSuite =
    describe "JsonRender.Render badgeTone"
        [ test "the in-flight states all tone as info, ellipsis or not" <|
            \_ ->
                -- `stopping` is in flight like queued/running/scanning, and the neutral tone it used
                -- to fall through to read as already-over — the one thing that state must not say,
                -- since the stop control has withdrawn and the badge is the only feedback left. The
                -- ellipsis is punctuation on the display word, so the tone must not depend on the
                -- publisher's typography.
                Expect.equal [ "info", "info", "info", "info", "info" ]
                    ([ "queued", "running", "scanning · 0:15", "stopping", "stopping…" ]
                        |> List.map Render.badgeTone
                    )
        , test "the settled states keep their own tones, so the ellipsis rule changed nothing else" <|
            \_ ->
                Expect.equal [ "neutral", "success", "danger", "neutral" ]
                    ([ "idle", "done", "error", "cancelled" ] |> List.map Render.badgeTone)
        ]
