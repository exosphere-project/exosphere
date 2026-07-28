module Tests.JsonRender.ButtonTest exposing (suite)

{-| Focused coverage for the Button `disabled` prop.

The rule: an optional `disabled` expression, when it resolves true, renders the button inert —
the native `disabled` attribute, a `jr-button--disabled` class, and NO press handler; when absent
or false the button is exactly as before. Fail-closed decoding is unchanged for every other
unknown prop.

The class is left to the eye: `Test.Html.Selector`'s class selectors do not see `Attr.class` under
this runner (they match nothing, which is why nothing in this repo uses them), so what is asserted
here is the part that MATTERS anyway — a disabled button emits no message.

-}

import Dict
import Expect
import Html.Attributes
import Json.Encode as Encode
import JsonRender
import JsonRender.Render as Render
import JsonRender.Spec exposing (Spec)
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
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


withDisabled : String
withDisabled =
    """
    { "root": "b"
    , "elements":
        { "b":
            { "type": "Button"
            , "props": { "label": "View", "disabled": { "$state": "/requestBusy" } }
            , "on": { "press": { "action": "exoext.openSession", "params": {} } }
            , "children": []
            }
        }
    }
    """


withoutDisabled : String
withoutDisabled =
    """
    { "root": "b"
    , "elements":
        { "b":
            { "type": "Button"
            , "props": { "label": "View" }
            , "on": { "press": { "action": "exoext.openSession", "params": {} } }
            , "children": []
            }
        }
    }
    """


unknownProp : String
unknownProp =
    """
    { "root": "b"
    , "elements":
        { "b": { "type": "Button", "props": { "label": "View", "visible": true }, "children": [] } }
    }
    """


busy : Bool -> Encode.Value
busy value =
    Encode.object [ ( "requestBusy", Encode.bool value ) ]


suite : Test
suite =
    describe "JsonRender.Render Button disabled"
        [ test "a truthy disabled renders the native disabled attribute, label intact" <|
            \_ ->
                render withDisabled (busy True)
                    |> Query.has
                        [ Selector.attribute (Html.Attributes.disabled True)
                        , Selector.text "View"
                        ]
        , test "a false disabled leaves the button enabled" <|
            \_ ->
                render withDisabled (busy False)
                    |> Query.hasNot [ Selector.attribute (Html.Attributes.disabled True) ]
        , test "a disabled button emits NO press message (the guard is real, not just styling)" <|
            \_ ->
                render withDisabled (busy True)
                    |> Query.find [ Selector.tag "button" ]
                    |> Event.simulate Event.click
                    |> Event.toResult
                    |> Expect.err
        , test "the same button emits its press once it is no longer disabled" <|
            \_ ->
                render withDisabled (busy False)
                    |> Query.find [ Selector.tag "button" ]
                    |> Event.simulate Event.click
                    |> Event.toResult
                    |> Expect.ok
        , test "an absent disabled keeps the historical enabled button" <|
            \_ ->
                render withoutDisabled Encode.null
                    |> Query.find [ Selector.tag "button" ]
                    |> Event.simulate Event.click
                    |> Event.toResult
                    |> Expect.ok
        , test "every OTHER unknown Button prop still fails the decode (fail-closed)" <|
            \_ ->
                case JsonRender.decodeString unknownProp of
                    Ok _ ->
                        Expect.fail "a Button carrying `visible` must still be rejected"

                    Err _ ->
                        Expect.pass
        ]
