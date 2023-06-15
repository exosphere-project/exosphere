module NoHardcodedLocalizedStrings exposing (LocalizedStrings, exosphereLocalizedStrings, rule)

import Char
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


type alias LocalizedStrings =
    List ( String, String )


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
    Rule.newModuleRuleSchema "NoHardcodedLocalizedStrings" ()
        |> Rule.withSimpleExpressionVisitor (expressionVisitor localizedStrings)
        |> Rule.fromModuleRuleSchema


expressionVisitor : List ( String, String ) -> Node Expression -> List (Error {})
expressionVisitor localizedStrings node =
    List.concatMap
        (\( varName, localStr ) ->
            case Node.value node of
                Expression.Literal str ->
                    List.concatMap
                        (\tok ->
                            let
                                isMatch =
                                    xor
                                        (localStr == String.toLower tok)
                                        (pluralize localStr == String.toLower tok)

                                isPluralized =
                                    pluralize localStr == String.toLower tok

                                ( isTitleCase, isUpperCase ) =
                                    case String.uncons tok of
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
                                            ++ tok
                                            ++ "` with "
                                            ++ expression
                                    , details = [ "This is a localized string, and should not be hardcoded" ]
                                    }
                                    (Node.range node)
                                ]

                            else
                                []
                        )
                        (stringToWords str)

                _ ->
                    []
        )
        localizedStrings


stringToWords : String -> List String
stringToWords str =
    String.replace "'" " " str
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
