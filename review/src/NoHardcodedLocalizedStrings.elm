module NoHardcodedLocalizedStrings exposing (LocalizedStrings, exosphereLocalizedStrings, rule)

import Char
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.Rule as Rule exposing (Error, Rule)


type alias Context =
    { tags : List (Node String) }


type alias LocalizedString =
    ( String, String )


type alias LocalizedStrings =
    List LocalizedString


exosphereLocalizedStrings : LocalizedStrings
exosphereLocalizedStrings =
    [ ( "openstackWithOwnKeystone", "cloud" )
    , ( "openstackSharingKeystoneWithAnother", "region" )
    , ( "unitOfTenancy", "project" )
    , ( "maxResourcesPerProject", "resource limit" )
    , ( "pkiPublicKeyForSsh", "SSH public key" )
    , ( "virtualComputer", "instance" )
    , ( "cloudInitData", "boot script" )
    , ( "commandDrivenTextInterface", "terminal" )
    , ( "staticRepresentationOfBlockDeviceContents", "image" )
    , ( "blockDevice", "volume" )
    , ( "share", "share" )
    , ( "exportLocation", "export location" )
    , ( "nonFloatingIpAddress", "internal IP address" )
    , ( "floatingIpAddress", "floating IP address" )
    , ( "publiclyRoutableIpAddress", "public IP address" )
    , ( "graphicalDesktopEnvironment", "graphical desktop" )
    , ( "hostname", "hostname" )

    --- Disabled, because `size` is too general a term
    -- , ( "virtualComputerHardwareConfig", "size" )
    ]


rule : LocalizedStrings -> Rule
rule localizedStrings =
    Rule.newModuleRuleSchema "NoHardcodedLocalizedStrings" { tags = [] }
        |> Rule.withCommentsVisitor commentVisitor
        |> Rule.withExpressionEnterVisitor (expressionVisitor localizedStrings)
        |> Rule.fromModuleRuleSchema


commentVisitor : List (Node String) -> Context -> ( List (Error {}), Context )
commentVisitor nodes context =
    ( []
    , { context
        | tags =
            context.tags
                ++ List.filter (\node -> Node.value node |> String.contains "@nonlocalized") nodes
      }
    )


searchStringForToken : Range -> String -> String -> String -> List (Error {})
searchStringForToken range varName localStr word =
    let
        isMatch =
            xor
                (localStr == String.toLower word)
                (pluralize localStr == String.toLower word)

        isPluralized =
            pluralize localStr == String.toLower word

        ( isTitleCase, isUpperCase ) =
            case String.uncons word of
                Just ( left, right ) ->
                    let
                        isFirstCharCapitalized : Bool
                        isFirstCharCapitalized =
                            Char.isUpper left

                        isRestCapitalized : Bool
                        isRestCapitalized =
                            List.all Char.isUpper (String.toList right)

                        isRestLowercase =
                            List.all Char.isLower (String.toList right)
                    in
                    ( isFirstCharCapitalized && isRestLowercase
                    , isFirstCharCapitalized && isRestCapitalized
                    )

                Nothing ->
                    ( False, False )

        filters : List String
        filters =
            List.concat
                [ if isPluralized then
                    [ "Helpers.String.pluralized" ]

                  else
                    []
                , if isTitleCase then
                    [ "Helpers.String.toTitleCase" ]

                  else
                    []
                , if isUpperCase then
                    [ "String.toUpper" ]

                  else
                    []
                ]

        expression =
            (if List.length filters > 0 then
                "("

             else
                ""
            )
                ++ (if List.length filters > 1 then
                        String.join " |> " (("context.localization." ++ varName) :: filters)

                    else
                        String.concat filters ++ " context.localization." ++ varName
                   )
                ++ (if List.length filters > 0 then
                        ")"

                    else
                        ""
                   )
    in
    if isMatch then
        [ Rule.error
            { message =
                "Replace `"
                    ++ word
                    ++ "` with "
                    ++ expression
            , details =
                [ word ++ " is a localized string, and should not be hardcoded"
                , "If this is intentional, tag the string with a {- @nonlocalized -} comment on the preceding line"
                ]
            }
            range
        ]

    else
        []


expressionVisitor : LocalizedStrings -> Node Expression -> Context -> ( List (Error {}), Context )
expressionVisitor localizedStrings node context =
    case Node.value node of
        Expression.Literal str ->
            let
                range =
                    Node.range node

                tagged =
                    List.any
                        (\tag ->
                            let
                                tagRange =
                                    Node.range tag
                            in
                            (-- Check if any tag is on the line preceding this node
                             (range.start.row - 1) == tagRange.start.row
                            )
                                || (-- Check if any tag is on the same line
                                    (range.start.row == tagRange.start.row)
                                        -- And within 5 characters of
                                        && (range.start.column - tagRange.end.column < 5)
                                   )
                        )
                        context.tags
            in
            if not tagged then
                ( List.concatMap
                    (\( varName, localStr ) ->
                        List.concatMap
                            (searchStringForToken range varName localStr)
                            (stringToCleanWords str)
                    )
                    localizedStrings
                , context
                )

            else
                ( [], context )

        _ ->
            ( [], context )


stringToCleanWords : String -> List String
stringToCleanWords str =
    str
        |> String.replace "'" " "
        |> String.replace "\"" " "
        |> String.words
        |> List.map String.trim
        |> List.map (String.filter Char.isAlpha)


pluralize : String -> String
pluralize word =
    String.concat
        [ word
        , if String.right 1 word == "s" then
            "es"

          else
            "s"
        ]
