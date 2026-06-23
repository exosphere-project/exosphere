module JsonRender.Spec exposing
    ( Spec
    , decoder
    , UIElement
    , ComponentType(..)
    , componentType
    , Props(..)
    , Direction(..)
    , CardProps
    , StackProps
    , TextProps
    , BadgeProps
    , ButtonProps
    , CheckboxProps
    , FindingsTableProps
    , ActionBinding
    , Confirm
    , Repeat
    )

{-| The json-render flat `Spec` model and its fail-closed decoder, scoped to the
CloudShield card's component set and pinned to `@json-render/core` v0.19.0.

A `Spec` is the canonical flat form: `{ root, elements, state }` where `elements` is a
map keyed by id and children are referenced by string key (never nested inline).

**Fail-closed by construction.** The decoder rejects, rather than silently dropping:

  - an **unknown / off-catalog** component `type` (json-render's renderer is fail-open
    here — we are not);
  - props that don't match the strict per-component shape;
  - a **dangling** child key, a **missing** root, or a `repeat` element with no children
    (the structural floor `validateSpec` enforces).

A rejected manifest never produces a partial tree — the host shows an error stub.


# Spec

@docs Spec
@docs decoder


# Elements

@docs UIElement
@docs ComponentType
@docs componentType


# Props

@docs Props
@docs Direction
@docs CardProps
@docs StackProps
@docs TextProps
@docs BadgeProps
@docs ButtonProps
@docs CheckboxProps
@docs FindingsTableProps


# Actions & iteration

@docs ActionBinding
@docs Confirm
@docs Repeat

-}

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import JsonRender.Expr as Expr exposing (Expr)


{-| A flat json-render spec: a root element key, a keyed map of elements, and the
host-owned initial state.
-}
type alias Spec =
    { root : String
    , elements : Dict String UIElement
    , state : Value
    }


{-| One element: its (validated) component type, its strictly-decoded props, its child
keys, its event bindings, and an optional `repeat`. Mirrors json-render's `UIElement`
shape (`type/props/children/on/repeat`), minus the `visible`/`watch` siblings the card
does not use.
-}
type alias UIElement =
    { componentType : ComponentType
    , props : Props
    , children : List String
    , on : Dict String (List ActionBinding)
    , repeat : Maybe Repeat
    }


{-| The allowlisted component types. Anything else fails the decode.
-}
type ComponentType
    = Card
    | Stack
    | Text
    | Badge
    | Button
    | Checkbox
    | FindingsTable


{-| Strictly-decoded props, one variant per component type. The variant always agrees
with the element's [`ComponentType`](#ComponentType) (both come from a single read of
the `type` field).
-}
type Props
    = CardP CardProps
    | StackP StackProps
    | TextP TextProps
    | BadgeP BadgeProps
    | ButtonP ButtonProps
    | CheckboxP CheckboxProps
    | FindingsTableP FindingsTableProps


{-| `Stack` layout direction.
-}
type Direction
    = Row
    | Col


{-| `Card` props. `title` is optional and may be any expression.
-}
type alias CardProps =
    { title : Maybe Expr }


{-| `Stack` props: a layout `direction` and a numeric `gap`.
-}
type alias StackProps =
    { direction : Direction
    , gap : Int
    }


{-| `Text` props: the displayed `value` expression.
-}
type alias TextProps =
    { value : Expr }


{-| `Badge` props: the `value` expression (a per-row `scanState` string in the card).
-}
type alias BadgeProps =
    { value : Expr }


{-| `Button` props: the `label` expression.
-}
type alias ButtonProps =
    { label : Expr }


{-| `Checkbox` props: an optional `label` and an optional `checked` binding (typically a
two-way `$bindState` / `$bindItem`).
-}
type alias CheckboxProps =
    { label : Maybe Expr
    , checked : Maybe Expr
    }


{-| `FindingsTable` props: the `bind` expression pointing at the findings payload and a
`groupBy` field name.
-}
type alias FindingsTableProps =
    { bind : Expr
    , groupBy : String
    }


{-| An event binding: a named verb, its (unresolved) params, and an optional confirm
dialog. The key is `action`/`params` per the pinned format — never a URL.
-}
type alias ActionBinding =
    { action : String
    , params : Value
    , confirm : Maybe Confirm
    }


{-| A confirm dialog shown by the renderer before an action emits. `title` and `message`
may be expressions (the per-row Scan message is a `$template`).
-}
type alias Confirm =
    { title : Expr
    , message : Expr
    , confirmLabel : Maybe String
    , cancelLabel : Maybe String
    , variant : String
    }


{-| Element-level iteration: `statePath` points at a state array, `key` is the optional
stable-list item field.
-}
type alias Repeat =
    { statePath : String
    , key : Maybe String
    }


{-| The component type's wire name, for diagnostics.
-}
componentType : ComponentType -> String
componentType ct =
    case ct of
        Card ->
            "Card"

        Stack ->
            "Stack"

        Text ->
            "Text"

        Badge ->
            "Badge"

        Button ->
            "Button"

        Checkbox ->
            "Checkbox"

        FindingsTable ->
            "FindingsTable"



-- DECODER


{-| Decode a flat json-render `Spec`, fail-closed. See the module doc for what is
rejected. On any structural or off-catalog error the decoder fails with a diagnostic
message; the host renders an error stub rather than a partial tree.
-}
decoder : Decoder Spec
decoder =
    Decode.map3 Spec
        (Decode.field "root" Decode.string)
        (Decode.field "elements" (Decode.dict elementDecoder))
        (optionalField "state" Decode.value (Encode.object []))
        |> Decode.andThen validateStructure


validateStructure : Spec -> Decoder Spec
validateStructure spec =
    case structuralErrors spec of
        [] ->
            Decode.succeed spec

        errors ->
            Decode.fail ("Invalid spec: " ++ String.join "; " errors)


structuralErrors : Spec -> List String
structuralErrors spec =
    let
        rootError =
            if Dict.member spec.root spec.elements then
                []

            else
                [ "root element `" ++ spec.root ++ "` is not present in `elements`" ]

        elementErrors =
            Dict.toList spec.elements
                |> List.concatMap (elementStructuralErrors spec.elements)
    in
    rootError ++ elementErrors


elementStructuralErrors : Dict String UIElement -> ( String, UIElement ) -> List String
elementStructuralErrors elements ( id, element ) =
    let
        danglingChildren =
            element.children
                |> List.filter (\child -> not (Dict.member child elements))
                |> List.map (\child -> "`" ++ id ++ "` references missing child `" ++ child ++ "`")

        repeatWithoutChildren =
            case element.repeat of
                Just _ ->
                    if List.isEmpty element.children then
                        [ "`" ++ id ++ "` has `repeat` but no children" ]

                    else
                        []

                Nothing ->
                    []
    in
    danglingChildren ++ repeatWithoutChildren


{-| The element-level keys this renderer handles. Any other sibling of `type` (notably
json-render's `visible` / `watch`) is **rejected**, not silently ignored — otherwise a
manifest relying on `visible` to hide a sensitive control would render it unconditionally
here, breaking the fail-closed boundary.
-}
allowedElementKeys : List String
allowedElementKeys =
    [ "type", "props", "children", "on", "repeat" ]


elementDecoder : Decoder UIElement
elementDecoder =
    rejectUnknownKeys "element" allowedElementKeys elementBodyDecoder


{-| Run `inner` only if `value` is a JSON object whose keys are all in `allowed`;
otherwise fail-closed. The strictness floor reused for elements, props, action bindings,
confirm, and repeat — Elm decoders ignore unknown keys (and silently default on
non-objects) by default, which would drop unsupported contract surface (`visible`,
`onSuccess`, a future `disabled` prop) or accept a malformed `"props": []`.
-}
rejectUnknownKeys : String -> List String -> Decoder a -> Decoder a
rejectUnknownKeys label allowed inner =
    Decode.value
        |> Decode.andThen
            (\value ->
                case Decode.decodeValue (Decode.keyValuePairs Decode.value) value of
                    Ok pairs ->
                        case List.filter (\( key, _ ) -> not (List.member key allowed)) pairs of
                            [] ->
                                decodeFromValue inner value

                            extra ->
                                Decode.fail
                                    ("Unsupported "
                                        ++ label
                                        ++ " key(s) (fail-closed; not implemented): "
                                        ++ String.join ", " (List.map Tuple.first extra)
                                    )

                    Err _ ->
                        Decode.fail ("`" ++ label ++ "` must be a JSON object")
            )


elementBodyDecoder : Decoder UIElement
elementBodyDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\name ->
                case parseComponentType name of
                    Just ct ->
                        Decode.map4 (UIElement ct)
                            (propsDecoder ct)
                            (optionalField "children" (Decode.list Decode.string) [])
                            (optionalField "on" (Decode.dict actionBindingsDecoder) Dict.empty)
                            (Decode.maybe (Decode.field "repeat" repeatDecoder))

                    Nothing ->
                        Decode.fail ("Unknown / off-catalog component type: `" ++ name ++ "`")
            )


parseComponentType : String -> Maybe ComponentType
parseComponentType name =
    case name of
        "Card" ->
            Just Card

        "Stack" ->
            Just Stack

        "Text" ->
            Just Text

        "Badge" ->
            Just Badge

        "Button" ->
            Just Button

        "Checkbox" ->
            Just Checkbox

        "FindingsTable" ->
            Just FindingsTable

        _ ->
            Nothing


{-| Decode the strict per-component props. `props` is decoded against an empty object
when absent, so the strict body decoder still runs (and still fails-closed when a
required field like `Text.value` is missing). Unknown prop keys are **rejected** per
component (a stray `disabled` on a Button must fail, not render an enabled button).
-}
propsDecoder : ComponentType -> Decoder Props
propsDecoder ct =
    Decode.maybe (Decode.field "props" Decode.value)
        |> Decode.andThen
            (\maybeProps ->
                case maybeProps of
                    Nothing ->
                        decodeFromValue (propsBodyDecoder ct) (Encode.object [])

                    Just _ ->
                        Decode.field "props"
                            (rejectUnknownKeys (componentType ct ++ " prop")
                                (allowedPropKeys ct)
                                (propsBodyDecoder ct)
                            )
            )


allowedPropKeys : ComponentType -> List String
allowedPropKeys ct =
    case ct of
        Card ->
            [ "title" ]

        Stack ->
            [ "direction", "gap" ]

        Text ->
            [ "value" ]

        Badge ->
            [ "value" ]

        Button ->
            [ "label" ]

        Checkbox ->
            [ "label", "checked" ]

        FindingsTable ->
            [ "bind", "groupBy" ]


decodeFromValue : Decoder a -> Value -> Decoder a
decodeFromValue dec value =
    case Decode.decodeValue dec value of
        Ok a ->
            Decode.succeed a

        Err err ->
            Decode.fail (Decode.errorToString err)


propsBodyDecoder : ComponentType -> Decoder Props
propsBodyDecoder ct =
    case ct of
        Card ->
            Decode.map (CardP << CardProps)
                (Decode.maybe (Decode.field "title" Expr.decoder))

        Stack ->
            Decode.map2 (\d g -> StackP (StackProps d g))
                (optionalField "direction" directionDecoder Col)
                (optionalField "gap" Decode.int 0)

        Text ->
            Decode.map (TextP << TextProps)
                (Decode.field "value" Expr.decoder)

        Badge ->
            Decode.map (BadgeP << BadgeProps)
                (Decode.field "value" Expr.decoder)

        Button ->
            Decode.map (ButtonP << ButtonProps)
                (Decode.field "label" Expr.decoder)

        Checkbox ->
            Decode.map2 (\l c -> CheckboxP (CheckboxProps l c))
                (Decode.maybe (Decode.field "label" Expr.decoder))
                (Decode.maybe (Decode.field "checked" Expr.decoder))

        FindingsTable ->
            Decode.map2 (\b g -> FindingsTableP (FindingsTableProps b g))
                (Decode.field "bind" Expr.decoder)
                (optionalField "groupBy" Decode.string "severity")


directionDecoder : Decoder Direction
directionDecoder =
    Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "row" ->
                        Decode.succeed Row

                    "col" ->
                        Decode.succeed Col

                    other ->
                        Decode.fail ("Unknown Stack direction: `" ++ other ++ "`")
            )


repeatDecoder : Decoder Repeat
repeatDecoder =
    rejectUnknownKeys "repeat"
        [ "statePath", "key" ]
        (Decode.map2 Repeat
            (Decode.field "statePath" Decode.string)
            (Decode.maybe (Decode.field "key" Decode.string))
        )


{-| An event's bindings: a single `ActionBinding` object, or an array of **exactly one**.
json-render allows `ActionBinding[]`, but multi-action dispatch is not yet implemented,
so an array of length ≠ 1 is **rejected** rather than silently truncated to the first.
-}
actionBindingsDecoder : Decoder (List ActionBinding)
actionBindingsDecoder =
    Decode.oneOf
        [ Decode.list actionBindingDecoder |> Decode.andThen requireSingleBinding
        , Decode.map List.singleton actionBindingDecoder
        ]


requireSingleBinding : List ActionBinding -> Decoder (List ActionBinding)
requireSingleBinding bindings =
    case bindings of
        [ single ] ->
            Decode.succeed [ single ]

        _ ->
            Decode.fail
                ("multiple action bindings per event are not yet supported (got "
                    ++ String.fromInt (List.length bindings)
                    ++ "); split them or use a single binding"
                )


actionBindingDecoder : Decoder ActionBinding
actionBindingDecoder =
    rejectUnknownKeys "action binding"
        [ "action", "params", "confirm" ]
        (Decode.map3 ActionBinding
            (Decode.field "action" Decode.string)
            (optionalField "params" Expr.validatedParams (Encode.object []))
            (Decode.maybe (Decode.field "confirm" confirmDecoder))
        )


confirmDecoder : Decoder Confirm
confirmDecoder =
    rejectUnknownKeys "confirm"
        [ "title", "message", "confirmLabel", "cancelLabel", "variant" ]
        (Decode.map5 Confirm
            (Decode.field "title" Expr.decoder)
            (Decode.field "message" Expr.decoder)
            (Decode.maybe (Decode.field "confirmLabel" Decode.string))
            (Decode.maybe (Decode.field "cancelLabel" Decode.string))
            (optionalField "variant" Decode.string "default")
        )



-- HELPERS


{-| Decode `field` strictly if present (failing on a wrong-typed value), else use the
default. Distinct from `oneOf [ field, succeed default ]`, which would mask a present
but malformed value.
-}
optionalField : String -> Decoder a -> a -> Decoder a
optionalField field dec default =
    Decode.maybe (Decode.field field Decode.value)
        |> Decode.andThen
            (\present ->
                case present of
                    Just _ ->
                        Decode.field field dec

                    Nothing ->
                        Decode.succeed default
            )
