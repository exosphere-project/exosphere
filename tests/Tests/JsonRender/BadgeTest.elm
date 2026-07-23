module Tests.JsonRender.BadgeTest exposing (suite)

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
