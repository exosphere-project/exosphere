module JsonRender.Expr exposing
    ( Expr(..)
    , Condition(..)
    , SingleCondition
    , CondSource(..)
    , Comparison(..)
    , Comparand(..)
    , decoder
    , Context
    , rootContext
    , childContext
    , getByPath
    , resolve
    , resolveDisplay
    , resolveBool
    , writeBackPath
    , resolveParams
    , validatedParams
    )

{-| The json-render expression / binding dialect, scoped to the CloudShield card's
needs and pinned to `@json-render/core` v0.19.0 (see `contract/pinned-format-reference.md`).

Every dynamic value in a manifest is a plain JSON object carrying a single
`$`-prefixed discriminant key. This module decodes the supported forms into a typed
[`Expr`](#Expr) and resolves them against a host-owned state `Value` plus an optional
`repeat` scope, using RFC 6901 JSON Pointers throughout.

**Fail-closed:** an object carrying an unsupported `$`-directive (e.g. `$computed`)
**fails the decode**. json-render's own runtime is fail-open here; we are not. The
supported set is exactly what the card uses (which now includes `$cond`).


# Expressions

@docs Expr
@docs Condition
@docs SingleCondition
@docs CondSource
@docs Comparison
@docs Comparand
@docs decoder


# Resolution scope

@docs Context
@docs rootContext
@docs childContext


# JSON Pointers

@docs getByPath


# Resolving

@docs resolve
@docs resolveDisplay
@docs resolveBool
@docs writeBackPath
@docs resolveParams
@docs validatedParams

-}

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode


{-| A decoded json-render expression. The discriminant maps 1:1 to the pinned
`$`-forms:

  - `ELiteral v` — any value with no `$`-directive (scalar, array, or plain object).
  - `EState ptr` — `{ "$state": "/ptr" }`, read global state.
  - `EItem field` — `{ "$item": "field" }`, read the current repeat item (field `""` = whole item).
  - `EIndex` — `{ "$index": true }`, the current repeat index.
  - `EBindState ptr` — `{ "$bindState": "/ptr" }`, two-way bind to global state.
  - `EBindItem field` — `{ "$bindItem": "field" }`, two-way bind to a repeat-item field.
  - `ETemplate tmpl` — `{ "$template": "…${/ptr}…${bare}…" }`, string interpolation.
  - `ECond cond t e` — `{ "$cond": <condition>, "$then": <expr>, "$else": <expr> }`,
    picks `t` when `cond` holds, else `e`. Both branches are themselves expressions
    (recursive). A two-way `$bindState` / `$bindItem` inside the selected branch keeps its
    write-back: [`writeBackPath`](#writeBackPath) targets the branch chosen at render time.
    See [`Condition`](#Condition).

-}
type Expr
    = ELiteral Value
    | EState String
    | EItem String
    | EIndex
    | EBindState String
    | EBindItem String
    | ETemplate String
    | ECond Condition Expr Expr


{-| A json-render **condition** — the predicate of a `$cond`, and the same grammar as
core's `visible` / `hidden` (pinned to `@json-render/core` v0.19.0). Evaluated to a
`Bool` by [`resolve`](#resolve) when picking a `$cond` branch:

  - `CBool b`: a literal `true` / `false`.
  - `CSingle c`: one `$state` / `$item` / `$index` comparison.
  - `CEvery cs`: a bare JSON **array** of single conditions, an implicit AND (all true).
  - `CAnd cs` / `COr cs`: explicit `{ "$and": [ … ] }` / `{ "$or": [ … ] }`, recursive.

Malformed conditions (unknown keys, mixed sources, a non-`true` `not`, a non-numeric
operand for `gt`/`gte`/`lt`/`lte`) **fail the decode**, mirroring the rest of this module.

-}
type Condition
    = CBool Bool
    | CSingle SingleCondition
    | CEvery (List SingleCondition)
    | CAnd (List Condition)
    | COr (List Condition)


{-| One comparison against a single source value. `negate` is core's `"not": true`
modifier, which inverts whatever the operator (or the bare truthiness test) yields.
-}
type alias SingleCondition =
    { source : CondSource
    , comparison : Comparison
    , negate : Bool
    }


{-| The left-hand side of a single condition: `$state` pointer, `$item` field, or the
repeat `$index`.
-}
type CondSource
    = SrcState String
    | SrcItem String
    | SrcIndex


{-| The comparison operator. `CmpTruthy` is the no-operator case (JS `Boolean(value)`).
`eq` / `neq` compare against any value by JSON scalar equality; `gt` / `gte` / `lt` /
`lte` compare numbers only (a non-numeric operand on either side yields `False`, per core).

A **missing** path (an absent `$state` / `$item` / `$index` source or `{$state}` operand) is
JS `undefined`, kept distinct from JSON `null`: it is falsy, `eq`-equal only to another
missing path (`undefined === undefined`; `undefined !== null`), and never orders.

-}
type Comparison
    = CmpTruthy
    | CmpEq Comparand
    | CmpNeq Comparand
    | CmpGt Comparand
    | CmpGte Comparand
    | CmpLt Comparand
    | CmpLte Comparand


{-| The right-hand side of a comparison: either a literal JSON value or a
`{ "$state": "/ptr" }` reference resolved against global state at eval time (core's
`resolveComparisonValue`).
-}
type Comparand
    = CLiteral Value
    | CStateRef String


{-| Decode a prop value into an [`Expr`](#Expr).

A non-object (string / number / bool / array / null) decodes to `ELiteral`. An object
with no `$`-prefixed key decodes to `ELiteral`. An object with exactly one supported
`$`-directive decodes to that form. An object with an **unsupported** `$`-directive, or
more than one `$`-directive, **fails** — this is the fail-closed boundary.

-}
decoder : Decoder Expr
decoder =
    Decode.value |> Decode.andThen classify


classify : Value -> Decoder Expr
classify value =
    case Decode.decodeValue (Decode.keyValuePairs Decode.value) value of
        Ok pairs ->
            if List.any (Tuple.first >> (==) "$cond") pairs then
                -- `$cond` is the one multi-`$`-key form (`$cond`/`$then`/`$else`); it is
                -- routed before the single/multiple split, which would otherwise reject it.
                condExprDecoder value

            else
                case List.filter (Tuple.first >> String.startsWith "$") pairs of
                    [] ->
                        Decode.succeed (ELiteral value)

                    [ ( key, _ ) ] ->
                        if List.length pairs == 1 then
                            dispatch key value

                        else
                            -- A directive object must be the directive ALONE. Extra siblings
                            -- (e.g. `{ "$item": "id", "kind": "x" }`) would be silently dropped
                            -- at resolution, corrupting the emitted payload — fail-closed.
                            Decode.fail
                                ("directive `"
                                    ++ key
                                    ++ "` must be the only key, but the object also carries: "
                                    ++ String.join ", " (siblingKeys key pairs)
                                )

                    multiple ->
                        Decode.fail
                            ("json-render value carries multiple $-directives: "
                                ++ String.join ", " (List.map Tuple.first multiple)
                            )

        Err _ ->
            -- Not an object: scalar / array / null are all literals.
            Decode.succeed (ELiteral value)


siblingKeys : String -> List ( String, Value ) -> List String
siblingKeys directive pairs =
    pairs |> List.map Tuple.first |> List.filter (\k -> k /= directive)


dispatch : String -> Value -> Decoder Expr
dispatch key value =
    case key of
        "$state" ->
            stringField "$state" value EState

        "$bindState" ->
            stringField "$bindState" value EBindState

        "$item" ->
            stringField "$item" value EItem

        "$bindItem" ->
            stringField "$bindItem" value EBindItem

        "$template" ->
            stringField "$template" value ETemplate

        "$index" ->
            case Decode.decodeValue (Decode.field "$index" Decode.bool) value of
                Ok True ->
                    Decode.succeed EIndex

                Ok False ->
                    Decode.fail "$index must be the literal `true` sentinel"

                Err err ->
                    Decode.fail (Decode.errorToString err)

        other ->
            Decode.fail
                ("Unsupported json-render directive `"
                    ++ other
                    ++ "` (fail-closed: only $state/$item/$index/$bindState/$bindItem/$template/$cond are supported)"
                )


stringField : String -> Value -> (String -> Expr) -> Decoder Expr
stringField field value toExpr =
    case Decode.decodeValue (Decode.field field Decode.string) value of
        Ok s ->
            Decode.succeed (toExpr s)

        Err err ->
            Decode.fail (Decode.errorToString err)



-- CONDITIONS ($cond)


{-| Decode a `{ "$cond": …, "$then": …, "$else": … }` object into an [`ECond`](#Expr).
Fail-closed: the object must carry **exactly** those three keys (extra keys, or a missing
branch, fail). Both branches decode through the top-level [`decoder`](#decoder), so they may
be any supported expression, including a nested `$cond`.
-}
condExprDecoder : Value -> Decoder Expr
condExprDecoder value =
    case Decode.decodeValue (Decode.keyValuePairs Decode.value) value of
        Ok pairs ->
            if List.sort (List.map Tuple.first pairs) == [ "$cond", "$else", "$then" ] then
                Decode.map3 ECond
                    (Decode.field "$cond" conditionDecoder)
                    (Decode.field "$then" decoder)
                    (Decode.field "$else" decoder)

            else
                Decode.fail
                    ("`$cond` must carry exactly `$cond`, `$then`, and `$else`, but the object has: "
                        ++ String.join ", " (List.sort (List.map Tuple.first pairs))
                    )

        Err err ->
            Decode.fail (Decode.errorToString err)


{-| Decode a [`Condition`](#Condition). The order of the `oneOf` matters: a boolean, then
the composite `$and` / `$or` objects, then a bare array (implicit AND), then a single
condition. Recursion (`$and` / `$or` nesting) is guarded with `Decode.lazy`.
-}
conditionDecoder : Decoder Condition
conditionDecoder =
    Decode.oneOf
        [ Decode.bool |> Decode.map CBool
        , compositeDecoder "$and" CAnd
        , compositeDecoder "$or" COr
        , Decode.list singleConditionDecoder |> Decode.map CEvery
        , singleConditionDecoder |> Decode.map CSingle
        ]


{-| Decode a `{ "$and": [ … ] }` / `{ "$or": [ … ] }` composite. Fail-closed: the object
must have that key as its **only** key.
-}
compositeDecoder : String -> (List Condition -> Condition) -> Decoder Condition
compositeDecoder key toCond =
    Decode.keyValuePairs Decode.value
        |> Decode.andThen
            (\pairs ->
                case List.map Tuple.first pairs of
                    [ only ] ->
                        if only == key then
                            Decode.field key
                                (Decode.list (Decode.lazy (\_ -> conditionDecoder)))
                                |> Decode.map toCond

                        else
                            Decode.fail ("not a `" ++ key ++ "` condition")

                    _ ->
                        Decode.fail ("`" ++ key ++ "` must be the only key of its condition")
            )


{-| Allowed keys of a single condition object: exactly one source key plus at most one
comparison operator plus an optional `not`.
-}
condKeys : List String
condKeys =
    [ "$state", "$item", "$index", "eq", "neq", "gt", "gte", "lt", "lte", "not" ]


{-| Decode one `$state` / `$item` / `$index` comparison. Strict: unknown keys, more than
one source, more than one operator, or a non-`true` `not` all fail.
-}
singleConditionDecoder : Decoder SingleCondition
singleConditionDecoder =
    Decode.keyValuePairs Decode.value
        |> Decode.andThen
            (\pairs ->
                case buildSingle pairs of
                    Ok single ->
                        Decode.succeed single

                    Err message ->
                        Decode.fail message
            )


buildSingle : List ( String, Value ) -> Result String SingleCondition
buildSingle pairs =
    let
        keys =
            List.map Tuple.first pairs

        unknown =
            List.filter (\k -> not (List.member k condKeys)) keys
    in
    if not (List.isEmpty unknown) then
        Err ("unknown condition key(s): " ++ String.join ", " unknown)

    else
        Result.map3 SingleCondition
            (parseSource pairs)
            (parseComparison pairs)
            (parseNegate pairs)


getKey : String -> List ( String, Value ) -> Maybe Value
getKey key pairs =
    pairs |> List.filter (Tuple.first >> (==) key) |> List.head |> Maybe.map Tuple.second


parseSource : List ( String, Value ) -> Result String CondSource
parseSource pairs =
    case List.filter (\( k, _ ) -> List.member k [ "$state", "$item", "$index" ]) pairs of
        [ ( "$state", v ) ] ->
            decodeStringValue "$state" v |> Result.map SrcState

        [ ( "$item", v ) ] ->
            decodeStringValue "$item" v |> Result.map SrcItem

        [ ( "$index", v ) ] ->
            case Decode.decodeValue Decode.bool v of
                Ok True ->
                    Ok SrcIndex

                _ ->
                    Err "condition `$index` must be the literal `true`"

        [] ->
            Err "condition needs a `$state`, `$item`, or `$index` source"

        _ ->
            Err "condition must have exactly one of `$state` / `$item` / `$index`"


decodeStringValue : String -> Value -> Result String String
decodeStringValue key v =
    case Decode.decodeValue Decode.string v of
        Ok s ->
            Ok s

        Err _ ->
            Err ("condition `" ++ key ++ "` must be a string")


parseComparison : List ( String, Value ) -> Result String Comparison
parseComparison pairs =
    case List.filter (\( k, _ ) -> List.member k [ "eq", "neq", "gt", "gte", "lt", "lte" ]) pairs of
        [] ->
            Ok CmpTruthy

        [ ( "eq", v ) ] ->
            comparand False v |> Result.map CmpEq

        [ ( "neq", v ) ] ->
            comparand False v |> Result.map CmpNeq

        [ ( "gt", v ) ] ->
            comparand True v |> Result.map CmpGt

        [ ( "gte", v ) ] ->
            comparand True v |> Result.map CmpGte

        [ ( "lt", v ) ] ->
            comparand True v |> Result.map CmpLt

        [ ( "lte", v ) ] ->
            comparand True v |> Result.map CmpLte

        _ ->
            Err "condition must have at most one comparison operator"


parseNegate : List ( String, Value ) -> Result String Bool
parseNegate pairs =
    case getKey "not" pairs of
        Nothing ->
            Ok False

        Just v ->
            case Decode.decodeValue Decode.bool v of
                Ok True ->
                    Ok True

                _ ->
                    Err "condition `not` must be the literal `true`"


{-| Build a [`Comparand`](#Comparand). A `{ "$state": "/ptr" }` object is a state
reference for any operator (core resolves it at eval time). When `numericOnly` (for
`gt` / `gte` / `lt` / `lte`), a literal operand must be a number, else the decode fails.

Fail-closed on malformed references: any JSON **object** that carries a `$state` key must
be exactly the one-key form `{ "$state": "<string>" }` (so `{ "$state": 123 }` or
`{ "$state": "/x", "junk": true }` fail decode rather than degrading to a literal). A plain
object / array literal **without** a `$state` key stays a legal literal (`eq` against it is
then always `False` by the object-inequality rule).

-}
comparand : Bool -> Value -> Result String Comparand
comparand numericOnly v =
    case Decode.decodeValue (Decode.keyValuePairs Decode.value) v of
        Ok pairs ->
            if List.any (Tuple.first >> (==) "$state") pairs then
                case pairs of
                    [ ( "$state", ptrValue ) ] ->
                        case Decode.decodeValue Decode.string ptrValue of
                            Ok ptr ->
                                Ok (CStateRef ptr)

                            Err _ ->
                                Err "comparison `{ \"$state\": … }` reference must be a string pointer"

                    _ ->
                        Err "comparison reference must be exactly `{ \"$state\": \"<ptr>\" }`"

            else
                literalComparand numericOnly v

        Err _ ->
            -- Not an object (scalar / array / null): a plain literal operand.
            literalComparand numericOnly v


literalComparand : Bool -> Value -> Result String Comparand
literalComparand numericOnly v =
    if numericOnly then
        case Decode.decodeValue Decode.float v of
            Ok _ ->
                Ok (CLiteral v)

            Err _ ->
                Err "`gt` / `gte` / `lt` / `lte` operand must be a number or `{ \"$state\": … }`"

    else
        Ok (CLiteral v)



-- CONTEXT


{-| The resolution scope handed to every expression.

  - `state` — the host-owned global state `Value` (the single source of truth).
  - `item` — the current `repeat` item, if inside a repeat scope.
  - `index` — the current `repeat` index, if inside a repeat scope.
  - `basePath` — the repeat item's absolute JSON Pointer (`statePath ++ "/" ++ index`),
    used to compute `$bindItem` / top-level `$item` write-back paths.
  - `allowedOrigins` — the host-provided iframe origin allowlist. The `Iframe` element only
    renders when a resolved `src`'s origin is an exact member of this list (fail-closed).

-}
type alias Context =
    { state : Value
    , item : Maybe Value
    , index : Maybe Int
    , basePath : Maybe String
    , allowedOrigins : List String
    }


{-| The top-level scope: global state only, no repeat item. `allowedOrigins` is the
host-provided iframe origin allowlist, threaded to every expression scope.
-}
rootContext : List String -> Value -> Context
rootContext allowedOrigins state =
    { state = state
    , item = Nothing
    , index = Nothing
    , basePath = Nothing
    , allowedOrigins = allowedOrigins
    }


{-| Derive a per-item scope for a `repeat` element. `statePath` is the pointer to the
state array; `index` and `item` identify the current row. `basePath` becomes
`statePath ++ "/" ++ index` per the pinned reference.
-}
childContext : String -> Int -> Value -> Context -> Context
childContext statePath index item parent =
    { state = parent.state
    , item = Just item
    , index = Just index
    , basePath = Just (statePath ++ "/" ++ String.fromInt index)
    , allowedOrigins = parent.allowedOrigins
    }



-- JSON POINTERS (RFC 6901)


{-| Resolve an RFC 6901 JSON Pointer against a `Value`. Returns `Nothing` if any token
is missing. The empty pointer `""` returns the whole value. Tokens are unescaped
(`~1` → `/`, `~0` → `~`). A leading `/` is honored; a bare token (no leading `/`, as
produced by `$item` field lookups) is tolerated as a single step.
-}
getByPath : String -> Value -> Maybe Value
getByPath pointer value =
    walk (parsePointer pointer) value


parsePointer : String -> List String
parsePointer pointer =
    if pointer == "" then
        []

    else
        let
            tokens =
                String.split "/" pointer
        in
        (case tokens of
            "" :: rest ->
                rest

            _ ->
                tokens
        )
            |> List.map unescape


unescape : String -> String
unescape =
    String.replace "~1" "/" >> String.replace "~0" "~"


walk : List String -> Value -> Maybe Value
walk tokens value =
    case tokens of
        [] ->
            Just value

        token :: rest ->
            case step token value of
                Just next ->
                    walk rest next

                Nothing ->
                    Nothing


step : String -> Value -> Maybe Value
step token value =
    case Decode.decodeValue (Decode.field token Decode.value) value of
        Ok found ->
            Just found

        Err _ ->
            case String.toInt token of
                Just i ->
                    Decode.decodeValue (Decode.index i Decode.value) value
                        |> Result.toMaybe

                Nothing ->
                    Nothing



-- RESOLVING


{-| Resolve an expression to a JSON `Value`. A missing pointer / item field yields
`null`. Two-way `$bindState` / `$bindItem` read exactly like their read-only siblings;
their write-back path is exposed separately via [`writeBackPath`](#writeBackPath).
-}
resolve : Context -> Expr -> Value
resolve ctx expr =
    case expr of
        ELiteral value ->
            value

        EState ptr ->
            getByPath ptr ctx.state |> Maybe.withDefault Encode.null

        EBindState ptr ->
            getByPath ptr ctx.state |> Maybe.withDefault Encode.null

        EItem field ->
            resolveItem ctx field

        EBindItem field ->
            resolveItem ctx field

        EIndex ->
            ctx.index |> Maybe.map Encode.int |> Maybe.withDefault Encode.null

        ETemplate tmpl ->
            Encode.string (interpolate ctx tmpl)

        ECond condition ifThen ifElse ->
            if evalCondition ctx condition then
                resolve ctx ifThen

            else
                resolve ctx ifElse


resolveItem : Context -> String -> Value
resolveItem ctx field =
    case ctx.item of
        Just item ->
            getByPath field item |> Maybe.withDefault Encode.null

        Nothing ->
            Encode.null



-- CONDITION EVALUATION


{-| Evaluate a [`Condition`](#Condition) to a `Bool`, mirroring core's
`evaluateVisibility`: an array / `$and` is a conjunction, `$or` a disjunction.
-}
evalCondition : Context -> Condition -> Bool
evalCondition ctx condition =
    case condition of
        CBool b ->
            b

        CSingle single ->
            evalSingle ctx single

        CEvery singles ->
            List.all (evalSingle ctx) singles

        CAnd conditions ->
            List.all (evalCondition ctx) conditions

        COr conditions ->
            List.any (evalCondition ctx) conditions


evalSingle : Context -> SingleCondition -> Bool
evalSingle ctx { source, comparison, negate } =
    let
        lhs =
            resolveSource ctx source

        result =
            case comparison of
                CmpTruthy ->
                    truthyMaybe lhs

                CmpEq c ->
                    maybeEq lhs (resolveComparand ctx c)

                CmpNeq c ->
                    not (maybeEq lhs (resolveComparand ctx c))

                CmpGt c ->
                    numCompare (>) lhs (resolveComparand ctx c)

                CmpGte c ->
                    numCompare (>=) lhs (resolveComparand ctx c)

                CmpLt c ->
                    numCompare (<) lhs (resolveComparand ctx c)

                CmpLte c ->
                    numCompare (<=) lhs (resolveComparand ctx c)
    in
    if negate then
        not result

    else
        result


{-| Resolve a condition's left-hand side. `Nothing` marks a **missing** path (an absent
`$state` pointer, an `$item` field or item that is not present, or `$index` outside a repeat
scope) — JS `undefined`, kept distinct from a JSON `null` value (`Just null`).
-}
resolveSource : Context -> CondSource -> Maybe Value
resolveSource ctx source =
    case source of
        SrcState ptr ->
            getByPath ptr ctx.state

        SrcItem field ->
            ctx.item |> Maybe.andThen (getByPath field)

        SrcIndex ->
            ctx.index |> Maybe.map Encode.int


{-| Resolve a comparison operand. A `{ "$state": … }` reference is `Nothing` when its path
is missing (same `undefined` semantics as the left-hand side); a literal is always `Just`.
-}
resolveComparand : Context -> Comparand -> Maybe Value
resolveComparand ctx c =
    case c of
        CLiteral v ->
            Just v

        CStateRef ptr ->
            getByPath ptr ctx.state


{-| JS `Boolean(value)` over a possibly-missing value: a missing path is falsy; `null` /
`false` / `0` / `""` are falsy; every other scalar and any object / array is truthy.
-}
truthyMaybe : Maybe Value -> Bool
truthyMaybe maybeValue =
    case maybeValue of
        Nothing ->
            False

        Just value ->
            Decode.decodeValue truthyDecoder value |> Result.withDefault True


truthyDecoder : Decoder Bool
truthyDecoder =
    Decode.oneOf
        [ Decode.null False
        , Decode.bool
        , Decode.float |> Decode.map (\n -> n /= 0)
        , Decode.string |> Decode.map (\s -> s /= "")
        ]


{-| JS `===` over possibly-missing values. Two missing paths are equal (`undefined ===
undefined`); a missing path equals nothing else, not even JSON `null`. Two present values
compare by [`jsonEq`](#Expr).
-}
maybeEq : Maybe Value -> Maybe Value -> Bool
maybeEq a b =
    case ( a, b ) of
        ( Nothing, Nothing ) ->
            True

        ( Just va, Just vb ) ->
            jsonEq va vb

        _ ->
            False


{-| JS `===` over two present values: equal only when both are the **same** JSON scalar
(same type and value). Objects and arrays are never equal here. This is an intentional
divergence from JS, whose `===` returns `true` when both sides resolve to the same object
reference (e.g. the same `$state` pointer on both sides); we do not emulate reference
identity.
-}
jsonEq : Value -> Value -> Bool
jsonEq a b =
    case ( toScalar a, toScalar b ) of
        ( Just sa, Just sb ) ->
            scalarEq sa sb

        _ ->
            False


type Scalar
    = SNull
    | SBool Bool
    | SNum Float
    | SStr String


{-| Scalar equality by same-type-same-value. (The vendored copy extracts each constructor arg
explicitly rather than deriving `==` on `Scalar`, to satisfy Exosphere's stricter
`NoUnused.CustomTypeConstructorArgs` review rule — behavior is identical to the upstream `sa == sb`.)
-}
scalarEq : Scalar -> Scalar -> Bool
scalarEq a b =
    case ( a, b ) of
        ( SNull, SNull ) ->
            True

        ( SBool x, SBool y ) ->
            x == y

        ( SNum x, SNum y ) ->
            x == y

        ( SStr x, SStr y ) ->
            x == y

        _ ->
            False


toScalar : Value -> Maybe Scalar
toScalar value =
    Decode.decodeValue scalarDecoder value |> Result.toMaybe


scalarDecoder : Decoder Scalar
scalarDecoder =
    Decode.oneOf
        [ Decode.null SNull
        , Decode.bool |> Decode.map SBool
        , Decode.float |> Decode.map SNum
        , Decode.string |> Decode.map SStr
        ]


{-| A numeric comparison (`gt` / `gte` / `lt` / `lte`) is `False` unless both operands are
present and both are numbers, mirroring core. A missing path on either side is `False`.
-}
numCompare : (Float -> Float -> Bool) -> Maybe Value -> Maybe Value -> Bool
numCompare op a b =
    case ( Maybe.andThen toNum a, Maybe.andThen toNum b ) of
        ( Just x, Just y ) ->
            op x y

        _ ->
            False


toNum : Value -> Maybe Float
toNum value =
    Decode.decodeValue Decode.float value |> Result.toMaybe


{-| Resolve an expression to its display string (for `Text` / `Badge`). Scalars render
as themselves; `null` / missing renders as `""`; objects / arrays render as `""`.
-}
resolveDisplay : Context -> Expr -> String
resolveDisplay ctx expr =
    valueToString (resolve ctx expr)


{-| Resolve an expression to a boolean (for `Checkbox` `checked`). Only a JSON `true`
is `True`; everything else (including missing) is `False`.
-}
resolveBool : Context -> Expr -> Bool
resolveBool ctx expr =
    case Decode.decodeValue Decode.bool (resolve ctx expr) of
        Ok b ->
            b

        Err _ ->
            False


{-| The absolute JSON Pointer a two-way binding writes back to, or `Nothing` for
read-only expressions. `$bindState` writes its own pointer; `$bindItem` writes
`basePath ++ "/" ++ field`. A `$cond` passes through to the branch it selects at render
time (see [`Expr`](#Expr)), so a `$bindState` / `$bindItem` inside the chosen branch keeps
its write-back handler.
-}
writeBackPath : Context -> Expr -> Maybe String
writeBackPath ctx expr =
    case expr of
        EBindState ptr ->
            Just ptr

        EBindItem field ->
            ctx.basePath
                |> Maybe.map
                    (\base ->
                        if field == "" then
                            base

                        else
                            base ++ "/" ++ field
                    )

        ECond condition ifThen ifElse ->
            if evalCondition ctx condition then
                writeBackPath ctx ifThen

            else
                writeBackPath ctx ifElse

        _ ->
            Nothing


valueToString : Value -> String
valueToString value =
    case Decode.decodeValue displayDecoder value of
        Ok s ->
            s

        Err _ ->
            ""


displayDecoder : Decoder String
displayDecoder =
    Decode.oneOf
        [ Decode.string
        , Decode.int |> Decode.map String.fromInt
        , Decode.float |> Decode.map String.fromFloat
        , Decode.bool |> Decode.map boolToString
        , Decode.null ""
        ]


boolToString : Bool -> String
boolToString b =
    if b then
        "true"

    else
        "false"



-- TEMPLATES


interpolate : Context -> String -> String
interpolate ctx template =
    case String.split "${" template of
        [] ->
            ""

        first :: segments ->
            first ++ String.concat (List.map (segment ctx) segments)


segment : Context -> String -> String
segment ctx seg =
    case String.split "}" seg of
        [] ->
            ""

        placeholder :: after ->
            placeholderValue ctx placeholder ++ String.join "}" after


placeholderValue : Context -> String -> String
placeholderValue ctx inner =
    let
        found =
            if String.startsWith "/" inner then
                getByPath inner ctx.state

            else
                case ctx.item |> Maybe.andThen (getByPath inner) of
                    Just value ->
                        Just value

                    Nothing ->
                        getByPath ("/" ++ inner) ctx.state
    in
    found |> Maybe.map valueToString |> Maybe.withDefault ""



-- ACTION PARAMS


{-| A `Decoder` for an action's `params` that **validates fail-closed at decode time**:
it returns the params `Value` unchanged, but fails if any nested object carrying a
`$`-directive is not a supported, well-formed [`Expr`](#Expr) (e.g. an unsupported
`$computed`, or a malformed `{ "$item": 123 }`). This closes the gap where params are stored
raw and only resolved at dispatch — a bad directive is rejected with the rest of the
manifest, never emitted to the host.
-}
validatedParams : Decoder Value
validatedParams =
    Decode.value
        |> Decode.andThen
            (\value ->
                case checkParam value of
                    Ok () ->
                        Decode.succeed value

                    Err message ->
                        Decode.fail message
            )


checkParam : Value -> Result String ()
checkParam value =
    case Decode.decodeValue (Decode.keyValuePairs Decode.value) value of
        Ok pairs ->
            if List.any (Tuple.first >> String.startsWith "$") pairs then
                case Decode.decodeValue decoder value of
                    Ok _ ->
                        Ok ()

                    Err err ->
                        Err ("invalid directive in action params: " ++ Decode.errorToString err)

            else
                checkAll (List.map Tuple.second pairs)

        Err _ ->
            case Decode.decodeValue (Decode.list Decode.value) value of
                Ok items ->
                    checkAll items

                Err _ ->
                    -- A scalar (string / number / bool / null) is always a valid param.
                    Ok ()


checkAll : List Value -> Result String ()
checkAll values =
    case values of
        [] ->
            Ok ()

        first :: rest ->
            checkParam first |> Result.andThen (\() -> checkAll rest)


{-| Resolve an action's `params` object per the pinned `resolveActionParam` semantics
(`pinned-format-reference.md` §5.1):

  - A **top-level** `{ "$item": "field" }` resolves to the absolute state **path**
    (`basePath ++ "/" ++ field`) so the host can target the row.
  - A **top-level** `{ "$index": true }` resolves to the index **number**.
  - Everything else (including `$item` **nested in an array**, as the per-row Scan
    button uses) resolves to the **value** by deep recursion.

So `{ "targetInstanceIds": [ { "$item": "id" } ] }` yields `{ "targetInstanceIds":
["<id>"] }` — the literal id, value-clean by construction.

-}
resolveParams : Context -> Value -> Value
resolveParams ctx params =
    case Decode.decodeValue (Decode.keyValuePairs Decode.value) params of
        Ok pairs ->
            Encode.object (List.map (\( k, v ) -> ( k, resolveTopLevelParam ctx v )) pairs)

        Err _ ->
            params


resolveTopLevelParam : Context -> Value -> Value
resolveTopLevelParam ctx value =
    case Decode.decodeValue (Decode.field "$item" Decode.string) value of
        Ok field ->
            Encode.string (itemPath ctx field)

        Err _ ->
            case Decode.decodeValue (Decode.field "$index" Decode.bool) value of
                Ok True ->
                    ctx.index |> Maybe.map Encode.int |> Maybe.withDefault Encode.null

                _ ->
                    resolveDeep ctx value


itemPath : Context -> String -> String
itemPath ctx field =
    let
        base =
            Maybe.withDefault "" ctx.basePath
    in
    if field == "" then
        base

    else
        base ++ "/" ++ field


resolveDeep : Context -> Value -> Value
resolveDeep ctx value =
    case Decode.decodeValue (Decode.keyValuePairs Decode.value) value of
        Ok pairs ->
            if List.any (Tuple.first >> String.startsWith "$") pairs then
                -- A directive object nested below the top level: resolve to its VALUE.
                case Decode.decodeValue decoder value of
                    Ok expr ->
                        resolve ctx expr

                    Err _ ->
                        -- Unreachable for validated params (see validatedParams); emit
                        -- null rather than leak a raw directive object to the host.
                        Encode.null

            else
                Encode.object (List.map (\( k, v ) -> ( k, resolveDeep ctx v )) pairs)

        Err _ ->
            case Decode.decodeValue (Decode.list Decode.value) value of
                Ok items ->
                    Encode.list identity (List.map (resolveDeep ctx) items)

                Err _ ->
                    value
