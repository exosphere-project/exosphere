module Tests.JsonRender.ErrorKindTest exposing (suite)

{-| Coverage for `JsonRender.Spec.errorKind`, the classifier a host uses to decide WHICH
plain-language sentence a rejected manifest gets.

Every case here decodes a real manifest rather than asserting against a hand-written error string:
the point of keeping the classifier next to the decoder is that the two move together, and a test
fed its own copy of the message would not notice if they stopped.

-}

import Expect
import JsonRender
import JsonRender.Spec as Spec
import Test exposing (Test, describe, test)


{-| The `ErrorKind` of a manifest that must NOT decode. A manifest that decodes is itself a test
failure, reported as such rather than silently defaulting to one of the kinds.
-}
kindOf : String -> Result String Spec.ErrorKind
kindOf manifest =
    case JsonRender.decodeString manifest of
        Ok _ ->
            Err "expected this manifest to be rejected, but it decoded"

        Err message ->
            Ok (Spec.errorKind message)


suite : Test
suite =
    describe "JsonRender.Spec.errorKind"
        [ test "an off-catalog component type reads as version skew" <|
            \_ ->
                kindOf """{ "root": "x", "elements": { "x": { "type": "ScriptInjector", "props": {}, "children": [] } } }"""
                    |> Expect.equal (Ok Spec.UnknownCatalogSurface)
        , test "an unknown PROP key reads as version skew" <|
            \_ ->
                -- The manifest wants a Button prop this catalog has not grown yet.
                kindOf """{ "root": "b", "elements": { "b": { "type": "Button", "props": { "label": "Go", "tooltip": "Go now" }, "children": [] } } }"""
                    |> Expect.equal (Ok Spec.UnknownCatalogSurface)
        , test "an unknown icon NAME reads as version skew too" <|
            \_ ->
                -- The icon set is closed but grows, so a shape this build cannot draw is the same
                -- situation as a component type it cannot render: a newer manifest, not a broken one.
                kindOf """{ "root": "b", "elements": { "b": { "type": "Button", "props": { "label": "Go", "icon": "play" }, "children": [] } } }"""
                    |> Expect.equal (Ok Spec.UnknownCatalogSurface)
        , test "an unknown ELEMENT key reads as version skew" <|
            \_ ->
                -- `visible` is json-render surface this renderer deliberately does not implement.
                kindOf """{ "root": "t", "elements": { "t": { "type": "Text", "props": { "value": "hi" }, "visible": true, "children": [] } } }"""
                    |> Expect.equal (Ok Spec.UnknownCatalogSurface)
        , test "a missing required field reads as malformed" <|
            \_ ->
                -- Text without `value`: no newer catalog makes this renderable.
                kindOf """{ "root": "t", "elements": { "t": { "type": "Text", "props": {}, "children": [] } } }"""
                    |> Expect.equal (Ok Spec.Malformed)
        , test "a dangling child reference reads as malformed" <|
            \_ ->
                kindOf """{ "root": "c", "elements": { "c": { "type": "Card", "props": {}, "children": [ "ghost" ] } } }"""
                    |> Expect.equal (Ok Spec.Malformed)
        , test "a body that is not JSON at all reads as malformed" <|
            \_ ->
                kindOf "not json at all"
                    |> Expect.equal (Ok Spec.Malformed)
        , test "a wrong-typed prop reads as malformed, not as skew" <|
            \_ ->
                -- The KEY is known; its value is nonsense. Updating Exosphere would not help, so
                -- this must not be reported as version skew.
                kindOf """{ "root": "s", "elements": { "s": { "type": "Stack", "props": { "direction": "sideways" }, "children": [] } } }"""
                    |> Expect.equal (Ok Spec.Malformed)
        ]
