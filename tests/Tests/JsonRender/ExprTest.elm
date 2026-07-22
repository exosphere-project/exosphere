module Tests.JsonRender.ExprTest exposing (suite)

{-| Focused coverage for the `$cond` conditional expression (`{ $cond, $then, $else }`),
ported from the `elm-json-render` package's ExprTest (`feat/expr-map`, commits 9bc8aa4
and 89817849). Covers branch selection, the full VisibilityCondition grammar (operators,
`not`, `$and` / `$or` / bare-array composition, `{$state}` refs, `$item` scope), nested
`$cond`, missing-path (`undefined`) semantics, write-back pass-through, and the fail-closed
decode boundary.
-}

import Expect
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import JsonRender.Expr as Expr
import Test exposing (Test, describe, test)


{-| A small host-state fixture mirroring the renderer-facing projection.
-}
stateJson : String
stateJson =
    """
    { "selectAll": false
    , "instances":
        [ { "id": "i-0a1b2c3d", "name": "web-frontend-01", "selected": false, "scanState": "idle" }
        , { "id": "i-1b2c3d4e", "name": "api-backend-02",  "selected": true,  "scanState": "running" }
        , { "id": "i-2c3d4e5f", "name": "postgres-primary","selected": false, "scanState": "done" }
        ]
    , "results": null
    }
    """


state : Value
state =
    decode stateJson


{-| A fixture with one field per JS `Boolean()` edge case, for the truthiness table.
-}
truthyState : Value
truthyState =
    decode
        """
        { "emptyStr": "", "zeroStr": "0", "zero": 0, "negZero": -0
        , "nul": null, "arr": [], "obj": {}
        }
        """


decode : String -> Value
decode raw =
    case Decode.decodeString Decode.value raw of
        Ok v ->
            v

        Err _ ->
            Encode.null


{-| Build a repeat scope for a given instance index.
-}
rowCtx : Int -> Expr.Context
rowCtx index =
    let
        item =
            Expr.getByPath ("/instances/" ++ String.fromInt index) state
                |> Maybe.withDefault Encode.null
    in
    Expr.childContext "/instances" index item (Expr.rootContext [] state)


encode : Value -> String
encode =
    Encode.encode 0


{-| Decode a `$cond` (or any expression) and resolve it to its display string in `ctx`.
Returns `"DECODE_ERR"` if the manifest fails to decode, so happy-path tests read cleanly.
-}
cond : Expr.Context -> String -> String
cond ctx raw =
    case Decode.decodeString Expr.decoder raw of
        Ok expr ->
            Expr.resolveDisplay ctx expr

        Err _ ->
            "DECODE_ERR"


isErr : Result e a -> Bool
isErr result =
    case result of
        Err _ ->
            True

        Ok _ ->
            False


suite : Test
suite =
    describe "JsonRender.Expr $cond ($cond/$then/$else)"
        [ test "a truthy $state condition picks $then" <|
            \_ ->
                cond (Expr.rootContext [] state)
                    "{\"$cond\":{\"$state\":\"/instances/1/selected\"},\"$then\":\"YES\",\"$else\":\"NO\"}"
                    |> Expect.equal "YES"
        , test "a falsy $state condition picks $else" <|
            \_ ->
                cond (Expr.rootContext [] state)
                    "{\"$cond\":{\"$state\":\"/selectAll\"},\"$then\":\"ALL\",\"$else\":\"SOME\"}"
                    |> Expect.equal "SOME"
        , test "eq compares a string state value" <|
            \_ ->
                cond (Expr.rootContext [] state)
                    "{\"$cond\":{\"$state\":\"/instances/1/scanState\",\"eq\":\"running\"},\"$then\":\"go\",\"$else\":\"stop\"}"
                    |> Expect.equal "go"
        , test "eq is type-sensitive on a number ($index === 2)" <|
            \_ ->
                cond (rowCtx 2)
                    "{\"$cond\":{\"$index\":true,\"eq\":2},\"$then\":\"two\",\"$else\":\"other\"}"
                    |> Expect.equal "two"
        , test "gt compares numbers ($index > 1)" <|
            \_ ->
                ( cond (rowCtx 2) "{\"$cond\":{\"$index\":true,\"gt\":1},\"$then\":\"late\",\"$else\":\"early\"}"
                , cond (rowCtx 0) "{\"$cond\":{\"$index\":true,\"gt\":1},\"$then\":\"late\",\"$else\":\"early\"}"
                )
                    |> Expect.equal ( "late", "early" )
        , test "not inverts the truthiness test" <|
            \_ ->
                cond (Expr.rootContext [] state)
                    "{\"$cond\":{\"$state\":\"/selectAll\",\"not\":true},\"$then\":\"a\",\"$else\":\"b\"}"
                    |> Expect.equal "a"
        , test "$or is a disjunction" <|
            \_ ->
                cond (Expr.rootContext [] state)
                    "{\"$cond\":{\"$or\":[{\"$state\":\"/selectAll\"},{\"$state\":\"/instances/1/selected\"}]},\"$then\":\"y\",\"$else\":\"n\"}"
                    |> Expect.equal "y"
        , test "$and is a conjunction" <|
            \_ ->
                cond (Expr.rootContext [] state)
                    "{\"$cond\":{\"$and\":[{\"$state\":\"/instances/1/selected\"},{\"$state\":\"/instances/0/selected\"}]},\"$then\":\"y\",\"$else\":\"n\"}"
                    |> Expect.equal "n"
        , test "a bare array is an implicit AND" <|
            \_ ->
                cond (Expr.rootContext [] state)
                    "{\"$cond\":[{\"$state\":\"/instances/1/selected\"},{\"$state\":\"/instances/0/selected\",\"not\":true}],\"$then\":\"y\",\"$else\":\"n\"}"
                    |> Expect.equal "y"
        , test "a comparison RHS may be a {$state} reference" <|
            \_ ->
                cond (Expr.rootContext [] state)
                    "{\"$cond\":{\"$state\":\"/instances/1/scanState\",\"eq\":{\"$state\":\"/instances/1/scanState\"}},\"$then\":\"same\",\"$else\":\"diff\"}"
                    |> Expect.equal "same"
        , test "$item sources the current repeat item" <|
            \_ ->
                ( cond (rowCtx 2) "{\"$cond\":{\"$item\":\"scanState\",\"eq\":\"done\"},\"$then\":\"done\",\"$else\":\"pending\"}"
                , cond (rowCtx 0) "{\"$cond\":{\"$item\":\"scanState\",\"eq\":\"done\"},\"$then\":\"done\",\"$else\":\"pending\"}"
                )
                    |> Expect.equal ( "done", "pending" )
        , test "a branch may itself be a $cond (recursive)" <|
            \_ ->
                cond (Expr.rootContext [] state)
                    "{\"$cond\":{\"$state\":\"/selectAll\"},\"$then\":\"all\",\"$else\":{\"$cond\":{\"$state\":\"/instances/1/selected\"},\"$then\":\"some\",\"$else\":\"none\"}}"
                    |> Expect.equal "some"
        , test "a non-scalar source is never eq to a scalar → $else" <|
            \_ ->
                cond (Expr.rootContext [] state)
                    "{\"$cond\":{\"$state\":\"/instances\",\"eq\":\"x\"},\"$then\":\"t\",\"$else\":\"e\"}"
                    |> Expect.equal "e"
        , test "$cond resolves inside action params" <|
            \_ ->
                Expr.resolveParams (Expr.rootContext [] state)
                    (decode "{\"label\":{\"$cond\":{\"$state\":\"/selectAll\"},\"$then\":\"All\",\"$else\":\"Some\"}}")
                    |> encode
                    |> Expect.equal "{\"label\":\"Some\"}"
        , test "a missing $else branch fails the decode (fail-closed)" <|
            \_ ->
                Decode.decodeString Expr.decoder "{\"$cond\":true,\"$then\":\"a\"}"
                    |> isErr
                    |> Expect.equal True
        , test "an extra key beyond the triple fails the decode" <|
            \_ ->
                Decode.decodeString Expr.decoder "{\"$cond\":true,\"$then\":\"a\",\"$else\":\"b\",\"x\":1}"
                    |> isErr
                    |> Expect.equal True
        , test "an unknown condition operator fails the decode" <|
            \_ ->
                Decode.decodeString Expr.decoder "{\"$cond\":{\"$state\":\"/x\",\"approx\":1},\"$then\":\"a\",\"$else\":\"b\"}"
                    |> isErr
                    |> Expect.equal True
        , test "mixing two condition sources fails the decode" <|
            \_ ->
                Decode.decodeString Expr.decoder "{\"$cond\":{\"$state\":\"/x\",\"$item\":\"y\"},\"$then\":\"a\",\"$else\":\"b\"}"
                    |> isErr
                    |> Expect.equal True
        , test "a non-true `not` fails the decode" <|
            \_ ->
                Decode.decodeString Expr.decoder "{\"$cond\":{\"$state\":\"/x\",\"not\":false},\"$then\":\"a\",\"$else\":\"b\"}"
                    |> isErr
                    |> Expect.equal True
        , test "a non-numeric gt operand fails the decode" <|
            \_ ->
                Decode.decodeString Expr.decoder "{\"$cond\":{\"$state\":\"/x\",\"gt\":\"5\"},\"$then\":\"a\",\"$else\":\"b\"}"
                    |> isErr
                    |> Expect.equal True
        , test "two comparison operators fail the decode" <|
            \_ ->
                Decode.decodeString Expr.decoder "{\"$cond\":{\"$state\":\"/x\",\"eq\":1,\"neq\":2},\"$then\":\"a\",\"$else\":\"b\"}"
                    |> isErr
                    |> Expect.equal True

        -- Missing-path (JS `undefined`) semantics: distinct from JSON null.
        , test "eq: null does NOT match a missing path (undefined !== null)" <|
            \_ ->
                cond (Expr.rootContext [] state)
                    "{\"$cond\":{\"$state\":\"/missing\",\"eq\":null},\"$then\":\"T\",\"$else\":\"F\"}"
                    |> Expect.equal "F"
        , test "eq: null DOES match a present null value (null === null)" <|
            \_ ->
                cond (Expr.rootContext [] state)
                    "{\"$cond\":{\"$state\":\"/results\",\"eq\":null},\"$then\":\"T\",\"$else\":\"F\"}"
                    |> Expect.equal "T"
        , test "a missing path IS eq to another missing path (via {$state} ref RHS)" <|
            \_ ->
                cond (Expr.rootContext [] state)
                    "{\"$cond\":{\"$state\":\"/gone\",\"eq\":{\"$state\":\"/absent\"}},\"$then\":\"T\",\"$else\":\"F\"}"
                    |> Expect.equal "T"
        , test "neq is the negation: a missing path IS neq to null" <|
            \_ ->
                cond (Expr.rootContext [] state)
                    "{\"$cond\":{\"$state\":\"/missing\",\"neq\":null},\"$then\":\"T\",\"$else\":\"F\"}"
                    |> Expect.equal "T"
        , test "an ordering comparison with a missing side is False" <|
            \_ ->
                cond (Expr.rootContext [] state)
                    "{\"$cond\":{\"$state\":\"/missing\",\"gt\":0},\"$then\":\"T\",\"$else\":\"F\"}"
                    |> Expect.equal "F"

        -- Write-back passes through the branch a $cond selects at render time.
        , test "$cond → $bindState exposes the THEN branch write-back when the condition holds" <|
            \_ ->
                Decode.decodeString Expr.decoder
                    "{\"$cond\":{\"$state\":\"/instances/1/selected\"},\"$then\":{\"$bindState\":\"/a\"},\"$else\":{\"$bindState\":\"/b\"}}"
                    |> Result.map (Expr.writeBackPath (Expr.rootContext [] state))
                    |> Expect.equal (Ok (Just "/a"))
        , test "$cond → $bindState follows the condition to the ELSE branch" <|
            \_ ->
                Decode.decodeString Expr.decoder
                    "{\"$cond\":{\"$state\":\"/selectAll\"},\"$then\":{\"$bindState\":\"/a\"},\"$else\":{\"$bindState\":\"/b\"}}"
                    |> Result.map (Expr.writeBackPath (Expr.rootContext [] state))
                    |> Expect.equal (Ok (Just "/b"))
        , test "$cond → $bindItem write-back uses the row basePath" <|
            \_ ->
                Decode.decodeString Expr.decoder
                    "{\"$cond\":{\"$item\":\"selected\"},\"$then\":{\"$bindItem\":\"scanState\"},\"$else\":{\"$bindItem\":\"name\"}}"
                    |> Result.map (Expr.writeBackPath (rowCtx 2))
                    |> Expect.equal (Ok (Just "/instances/2/name"))

        -- Malformed {$state} comparison references fail decode; plain literals do not.
        , test "a {$state} ref with a non-string pointer fails the decode" <|
            \_ ->
                Decode.decodeString Expr.decoder "{\"$cond\":{\"$state\":\"/x\",\"eq\":{\"$state\":123}},\"$then\":\"a\",\"$else\":\"b\"}"
                    |> isErr
                    |> Expect.equal True
        , test "a {$state} ref with an extra key fails the decode" <|
            \_ ->
                Decode.decodeString Expr.decoder "{\"$cond\":{\"$state\":\"/x\",\"eq\":{\"$state\":\"/y\",\"junk\":true}},\"$then\":\"a\",\"$else\":\"b\"}"
                    |> isErr
                    |> Expect.equal True
        , test "a plain object literal (no $state) is a legal eq operand (always False)" <|
            \_ ->
                cond (Expr.rootContext [] state)
                    "{\"$cond\":{\"$state\":\"/instances/1/scanState\",\"eq\":{\"foo\":1}},\"$then\":\"t\",\"$else\":\"e\"}"
                    |> Expect.equal "e"

        -- JS Boolean() truthiness table (no-operator condition).
        , test "truthiness table: '' 0 -0 null missing → False; '0' [] {} → True" <|
            \_ ->
                let
                    t path =
                        cond (Expr.rootContext [] truthyState)
                            ("{\"$cond\":{\"$state\":\"" ++ path ++ "\"},\"$then\":\"T\",\"$else\":\"F\"}")
                in
                [ t "/emptyStr", t "/zeroStr", t "/zero", t "/negZero", t "/nul", t "/arr", t "/obj", t "/missing" ]
                    |> Expect.equal [ "F", "T", "F", "F", "F", "T", "T", "F" ]
        ]
