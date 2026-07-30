module Tests.JsonRender.ButtonTest exposing (emptyLabelSuite, suite)

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


{-| An empty resolved label renders no button at all.

The catalog refuses an element-level `visible` prop, so collapsing the label to `""` is the only way
a manifest can say "this action does not apply to this row". Emitting a `<button>` anyway put an
invisible control with a live press handler on every row the action did not apply to — and the
empty-label row is precisely the one whose press carries no id. Not a hypothetical: the history
column's View action already resolves to `""` on a failed scan.

-}
emptyLabelSuite : Test
emptyLabelSuite =
    let
        -- A label that is a per-row `$cond`, the shape every "not applicable here" control takes.
        conditionalLabel =
            """
            { "root": "b"
            , "elements":
                { "b":
                    { "type": "Button"
                    , "props":
                        { "label": { "$cond": { "$state": "/cancellable" }, "$then": "Cancel", "$else": "" }
                        , "disabled": { "$state": "/requestBusy" }
                        }
                    , "on": { "press": { "action": "exoext.cancelRequest", "params": {} } }
                    , "children": []
                    }
                }
            }
            """

        state cancellable requestBusy =
            Encode.object
                [ ( "cancellable", Encode.bool cancellable )
                , ( "requestBusy", Encode.bool requestBusy )
                ]
    in
    describe "JsonRender.Render Button with an empty label"
        [ test "an empty resolved label renders no button element at all" <|
            \_ ->
                render conditionalLabel (state False False)
                    |> Query.hasNot [ Selector.tag "button" ]
        , test "and therefore emits no press — the phantom-clickable fix" <|
            \_ ->
                -- `Query.find` failing IS the assertion: there is no node to click.
                render conditionalLabel (state False False)
                    |> Query.findAll [ Selector.tag "button" ]
                    |> Query.count (Expect.equal 0)
        , test "a non-empty label still renders and still emits" <|
            \_ ->
                render conditionalLabel (state True False)
                    |> Query.find [ Selector.tag "button" ]
                    |> Event.simulate Event.click
                    |> Event.toResult
                    |> Expect.ok
        , test "a DISABLED non-empty button still renders — this rule does not swallow it" <|
            \_ ->
                -- Disabled means "here but unavailable" and must stay visible where the eye expects
                -- it; empty-label means "not applicable" and goes away. Two different states.
                render conditionalLabel (state True True)
                    |> Query.has
                        [ Selector.tag "button"
                        , Selector.attribute (Html.Attributes.disabled True)
                        , Selector.text "Cancel"
                        ]
        , test "a literal empty label is excluded too, not only a collapsed $cond" <|
            \_ ->
                let
                    literal =
                        """
                        { "root": "b"
                        , "elements":
                            { "b":
                                { "type": "Button"
                                , "props": { "label": "" }
                                , "on": { "press": { "action": "exoext.openSession", "params": {} } }
                                , "children": []
                                }
                            }
                        }
                        """
                in
                render literal Encode.null
                    |> Query.hasNot [ Selector.tag "button" ]
        ]
