module OpenStack.OpenRc exposing (openRcUsesAppCredentialAuth, parseOpenRcAppCredential, processOpenRc)

import Helpers.String exposing (normalizeLineEndings)
import OpenStack.Types as OSTypes


processOpenRc : OSTypes.OpenstackLogin -> String -> OSTypes.OpenstackLogin
processOpenRc existingCreds openRc =
    OSTypes.OpenstackLogin
        (parseVar openRc "OS_AUTH_URL" |> Maybe.withDefault existingCreds.authUrl)
        (parseVar openRc "OS_USER_DOMAIN_NAME"
            |> Maybe.withDefault
                (parseVar openRc "OS_USER_DOMAIN_ID"
                    |> Maybe.withDefault existingCreds.userDomain
                )
        )
        (parseVar openRc "OS_USERNAME" |> Maybe.withDefault existingCreds.username)
        (parseVar openRc "OS_PASSWORD" |> Maybe.withDefault existingCreds.password)


parseOpenRcAppCredential : String -> Maybe OSTypes.ApplicationCredential
parseOpenRcAppCredential openRc =
    case ( parseVar openRc "OS_APPLICATION_CREDENTIAL_ID", parseVar openRc "OS_APPLICATION_CREDENTIAL_SECRET" ) of
        ( Just uuid, Just secret ) ->
            Just (OSTypes.ApplicationCredential uuid secret)

        _ ->
            Nothing


openRcUsesAppCredentialAuth : String -> Bool
openRcUsesAppCredentialAuth openRc =
    parseVar openRc "OS_AUTH_TYPE"
        |> Maybe.map String.toLower
        |> Maybe.map ((==) "v3applicationcredential")
        |> Maybe.withDefault False


parseVar : String -> String -> Maybe String
parseVar openRc varName =
    openRc
        |> normalizeLineEndings
        |> String.lines
        |> List.filterMap (parseLine varName)
        |> List.head


parseLine : String -> String -> Maybe String
parseLine varName line =
    let
        trimmedLine =
            line |> String.trim

        lineWithoutExport =
            if String.startsWith "export " trimmedLine then
                String.dropLeft 7 trimmedLine |> String.trimLeft

            else
                trimmedLine

        keyPrefix =
            varName ++ "="
    in
    if String.startsWith keyPrefix lineWithoutExport then
        lineWithoutExport
            |> String.dropLeft (String.length keyPrefix)
            |> parseValue

    else
        Nothing


parseValue : String -> Maybe String
parseValue rawValue =
    let
        value =
            rawValue |> String.trim

        startsWith : String -> Bool
        startsWith prefix =
            String.startsWith prefix value

        unwrapQuotedValue : String -> Maybe String
        unwrapQuotedValue quote =
            if startsWith quote && String.endsWith quote value then
                Just
                    (value
                        |> String.dropLeft 1
                        |> String.dropRight 1
                    )

            else
                Nothing

        discardBashVariable : String -> Maybe String
        discardBashVariable unwrapped =
            if String.startsWith "$" unwrapped then
                -- Discard values that reference other bash variables, e.g. $OS_PASSWORD_INPUT
                Nothing

            else
                Just unwrapped
    in
    if startsWith "\"" then
        -- Bash expands variables inside double quotes, so "$VAR" is discarded like a bare $VAR
        unwrapQuotedValue "\"" |> Maybe.andThen discardBashVariable

    else if startsWith "'" then
        -- Single quotes are literal in bash, so '$VAR' is kept as-is
        unwrapQuotedValue "'"

    else
        discardBashVariable value
