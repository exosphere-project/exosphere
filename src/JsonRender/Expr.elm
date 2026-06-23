module JsonRender.Expr exposing
    ( Expr(..)
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

**Fail-closed:** an object carrying an unsupported `$`-directive (e.g. `$cond`,
`$computed`) **fails the decode**. json-render's own runtime is fail-open here; we are
not. The supported set is exactly what the card uses.


# Expressions

@docs Expr
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

-}
type Expr
    = ELiteral Value
    | EState String
    | EItem String
    | EIndex
    | EBindState String
    | EBindItem String
    | ETemplate String


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
                    ++ "` (fail-closed: only $state/$item/$index/$bindState/$bindItem/$template are supported)"
                )


stringField : String -> Value -> (String -> Expr) -> Decoder Expr
stringField field value toExpr =
    case Decode.decodeValue (Decode.field field Decode.string) value of
        Ok s ->
            Decode.succeed (toExpr s)

        Err err ->
            Decode.fail (Decode.errorToString err)



-- CONTEXT


{-| The resolution scope handed to every expression.

  - `state` — the host-owned global state `Value` (the single source of truth).
  - `item` — the current `repeat` item, if inside a repeat scope.
  - `index` — the current `repeat` index, if inside a repeat scope.
  - `basePath` — the repeat item's absolute JSON Pointer (`statePath ++ "/" ++ index`),
    used to compute `$bindItem` / top-level `$item` write-back paths.

-}
type alias Context =
    { state : Value
    , item : Maybe Value
    , index : Maybe Int
    , basePath : Maybe String
    }


{-| The top-level scope: global state only, no repeat item.
-}
rootContext : Value -> Context
rootContext state =
    { state = state, item = Nothing, index = Nothing, basePath = Nothing }


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


resolveItem : Context -> String -> Value
resolveItem ctx field =
    case ctx.item of
        Just item ->
            getByPath field item |> Maybe.withDefault Encode.null

        Nothing ->
            Encode.null


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
`basePath ++ "/" ++ field`.
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
`$cond`, or a malformed `{ "$item": 123 }`). This closes the gap where params are stored
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
