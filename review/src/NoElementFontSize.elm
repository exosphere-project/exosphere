module NoElementFontSize exposing (rule)

import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


type Context
    = NoImport
    | Import { aliasing : Maybe ModuleName, exposed : Bool }


rule : Rule
rule =
    Rule.newModuleRuleSchema "NoElementFontSize" NoImport
        |> Rule.withImportVisitor importVisitor
        |> Rule.withExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


importVisitor : Node Import -> Context -> ( List (Error {}), Context )
importVisitor node context =
    case Node.value node |> .moduleName |> Node.value of
        [ "Element", "Font" ] ->
            let
                exposed =
                    Node.value node
                        |> .exposingList
                        |> Maybe.map (Node.value >> Exposing.exposesFunction "size")
                        |> Maybe.withDefault False

                aliasing =
                    Node.value node
                        |> .moduleAlias
                        |> Maybe.map Node.value
            in
            ( []
            , Import { aliasing = aliasing, exposed = exposed }
            )

        _ ->
            ( [], context )


expressionVisitor : Node Expression -> Rule.Direction -> Context -> ( List (Error {}), Context )
expressionVisitor expression direction context =
    case ( direction, context, Node.value expression ) of
        ( Rule.OnEnter, Import { aliasing, exposed }, Expression.Application [ function, _ ] ) ->
            case ( Node.value function, exposed ) of
                ( Expression.FunctionOrValue [] "size", True ) ->
                    ( [ Rule.error error (Node.range expression) ], context )

                ( Expression.FunctionOrValue moduleName "size", _ ) ->
                    if moduleName == (aliasing |> Maybe.withDefault [ "Font" ]) then
                        ( [ Rule.error error (Node.range expression) ], context )

                    else
                        ( [], context )

                _ ->
                    ( [], context )

        _ ->
            ( [], context )


error : { message : String, details : List String }
error =
    { message = "Do not use Font.size"
    , details =
        [ "Instead of Font.size, you should use Style.Widgets.Text.fontSize"
        , "see https://gitlab.com/exosphere/exosphere/-/issues/878 for details"
        ]
    }
