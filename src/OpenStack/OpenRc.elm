module OpenStack.OpenRc exposing (processOpenRc)

import OpenStack.Types as OSTypes
import Parser exposing ((|.), (|=))
import Set


processOpenRc : OSTypes.OpenstackLogin -> String -> OSTypes.OpenstackLogin
processOpenRc existingCreds openRc =
    let
        parseVar : String -> Maybe String
        parseVar varName =
            let
                parseVariable =
                    Parser.succeed identity
                        |= Parser.variable
                            -- This discards any bash variables defined with other bash variables, e.g. $OS_PASSWORD_INPUT
                            { start = \c -> c /= '$'
                            , inner = \c -> not (List.member c [ '\n', '"', '\'' ])
                            , reserved = Set.empty
                            }

                varParser : Parser.Parser String
                varParser =
                    -- Why does this need to be Parser.succeed identity instead of Parser.succeed ()?
                    Parser.succeed identity
                        |. Parser.spaces
                        |. Parser.oneOf [ Parser.keyword "export", Parser.succeed () ]
                        |. Parser.spaces
                        |. Parser.keyword varName
                        |. Parser.symbol "="
                        |= Parser.oneOf
                            [ Parser.succeed identity
                                |. Parser.symbol "\""
                                |= parseVariable
                                |. Parser.symbol "\""
                            , Parser.succeed identity
                                |. Parser.symbol "'"
                                |= parseVariable
                                |. Parser.symbol "'"
                            , Parser.succeed identity
                                |= parseVariable
                            ]
                        |. Parser.oneOf [ Parser.symbol "\n", Parser.end ]
            in
            openRc
                |> String.split "\n"
                |> List.map (\line -> Parser.run varParser line)
                |> List.filterMap Result.toMaybe
                |> List.head
    in
    OSTypes.OpenstackLogin
        (parseVar "OS_AUTH_URL" |> Maybe.withDefault existingCreds.authUrl)
        (parseVar "OS_USER_DOMAIN_NAME"
            |> Maybe.withDefault
                (parseVar "OS_USER_DOMAIN_ID"
                    |> Maybe.withDefault existingCreds.userDomain
                )
        )
        (parseVar "OS_USERNAME" |> Maybe.withDefault existingCreds.username)
        (parseVar "OS_PASSWORD" |> Maybe.withDefault existingCreds.password)
