module JsonRender exposing
    ( decodeValue, decodeString
    , errorStub
    )

{-| A native, fail-closed Elm renderer for [json-render](https://github.com/vercel-labs/json-render)
manifests, pinned to `@json-render/core` v0.19.0.

This top module is the convenience entry point: validate a manifest fail-closed, and get
an error stub for the failure path. The renderer itself (a small TEA component —
`Model` / `Msg` / `Effect` / `update` / `view`) lives in
[`JsonRender.Render`](JsonRender-Render); the typed spec model in
[`JsonRender.Spec`](JsonRender-Spec); the expression dialect in
[`JsonRender.Expr`](JsonRender-Expr).

Typical host wiring:

    case JsonRender.decodeValue manifestJson of
        Ok spec ->
            -- mount the renderer, feeding host-owned state
            Html.map RendererMsg (JsonRender.Render.view spec hostState rendererModel)

        Err message ->
            JsonRender.errorStub message

Decoding is the security gate: an off-catalog component type, a malformed prop, a
dangling child key, or a missing root all produce `Err` — never a partial tree.


# Validate a manifest

@docs decodeValue, decodeString


# Failure path

@docs errorStub

-}

import Html exposing (Html)
import Html.Attributes as Attr
import Json.Decode as Decode exposing (Value)
import JsonRender.Spec as Spec exposing (Spec)


{-| Validate an already-parsed JSON `Value` into a [`Spec`](JsonRender-Spec#Spec),
fail-closed. `Err` carries a human-readable diagnostic.
-}
decodeValue : Value -> Result String Spec
decodeValue value =
    Decode.decodeValue Spec.decoder value
        |> Result.mapError Decode.errorToString


{-| Validate a raw JSON string into a [`Spec`](JsonRender-Spec#Spec), fail-closed.
-}
decodeString : String -> Result String Spec
decodeString raw =
    Decode.decodeString Spec.decoder raw
        |> Result.mapError Decode.errorToString


{-| The fail-closed failure view: a self-contained error stub the host renders instead of
a manifest that did not validate. Never a partial tree.
-}
errorStub : String -> Html msg
errorStub message =
    Html.div [ Attr.class "jr-error-stub" ]
        [ Html.strong [ Attr.class "jr-error-stub__title" ]
            [ Html.text "Could not render manifest" ]
        , Html.p [ Attr.class "jr-error-stub__detail" ] [ Html.text message ]
        ]
