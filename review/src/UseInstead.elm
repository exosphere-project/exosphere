module UseInstead exposing (rule)

import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


type alias Reference =
    { moduleName : ModuleName
    , name : String
    }


type alias ImportAs =
    { aliasing : Maybe ModuleName
    , exposed : Bool
    }


type alias Context =
    { insteadOf : Reference
    , useThis : Reference
    , reason : String
    , imported : Maybe ImportAs
    }


reference : ( ModuleName, String ) -> Reference
reference ( moduleName, name ) =
    Reference moduleName name


refToString : Reference -> String
refToString ref =
    String.join "." ref.moduleName ++ "." ++ ref.name


rule : ( ModuleName, String ) -> ( ModuleName, String ) -> String -> Rule
rule insteadOf useThis reason =
    Rule.newModuleRuleSchema "UseInstead"
        { insteadOf = reference insteadOf
        , useThis = reference useThis
        , reason = reason
        , imported = Nothing
        }
        |> Rule.withImportVisitor importVisitor
        |> Rule.withExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


importVisitor : Node Import -> Context -> ( List (Error {}), Context )
importVisitor node context =
    let
        importedModule =
            Node.value node |> .moduleName |> Node.value
    in
    if context.insteadOf.moduleName == importedModule then
        let
            exposed =
                Node.value node
                    |> .exposingList
                    |> Maybe.map Node.value
                    |> Maybe.map (Exposing.exposesFunction context.insteadOf.name)
                    |> Maybe.withDefault False

            aliasing =
                Node.value node
                    |> .moduleAlias
                    |> Maybe.map Node.value
        in
        ( []
        , { context
            | imported = Just { aliasing = aliasing, exposed = exposed }
          }
        )

    else
        ( [], context )


expressionVisitor : Node Expression -> Rule.Direction -> Context -> ( List (Error {}), Context )
expressionVisitor expression direction context =
    case ( context.imported, direction, Node.value expression ) of
        ( Just { aliasing, exposed }, Rule.OnEnter, Expression.FunctionOrValue moduleName funcName ) ->
            if
                ((Maybe.withDefault context.insteadOf.moduleName aliasing == moduleName)
                    || (exposed && [] == moduleName)
                )
                    && (funcName == context.insteadOf.name)
            then
                ( [ Rule.error (error context) (Node.range expression) ], context )

            else
                ( [], context )

        _ ->
            ( [], context )


error : Context -> { message : String, details : List String }
error context =
    { message =
        String.concat
            [ "Do not use "
            , refToString context.insteadOf
            ]
    , details =
        [ "Instead of " ++ refToString context.insteadOf ++ ", you should use " ++ refToString context.useThis
        , context.reason
        ]
    }
