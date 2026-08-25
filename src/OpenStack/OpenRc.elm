module OpenStack.OpenRc exposing (looksLikeOpenRc, openRcUsesAppCredentialAuth, parseOpenRcAppCredential, parseOpenRcAuthUrl, parseOpenRcProjectName, processOpenRc)

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


{-| The cloud an OpenRC file authenticates against, as the file itself gives it. Unlike
`processOpenRc` this falls back on nothing, so a caller with no form for the user to correct can
tell an absent auth URL apart from one the user typed somewhere else.
-}
parseOpenRcAuthUrl : String -> Maybe OSTypes.KeystoneUrl
parseOpenRcAuthUrl openRc =
    parseVar openRc "OS_AUTH_URL"


{-| The project an OpenRC file is scoped to, when it says. Used to name what the user is about
to log in to, so it is fine for this to be absent.
-}
parseOpenRcProjectName : String -> Maybe String
parseOpenRcProjectName openRc =
    parseVar openRc "OS_PROJECT_NAME"


{-| True when the text assigns at least one `OS_` variable, which is what makes an OpenRC file
an OpenRC file. Used to tell OpenRC apart from other pasted or uploaded text.
-}
looksLikeOpenRc : String -> Bool
looksLikeOpenRc openRc =
    openRc
        |> normalizeLineEndings
        |> String.lines
        |> List.any lineAssignsOpenStackVar


lineAssignsOpenStackVar : String -> Bool
lineAssignsOpenStackVar line =
    let
        assignment : String
        assignment =
            line |> String.trim |> dropExport
    in
    String.startsWith "OS_" assignment && String.contains "=" assignment


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
            dropExport trimmedLine

        keyPrefix =
            varName ++ "="
    in
    if String.startsWith keyPrefix lineWithoutExport then
        lineWithoutExport
            |> String.dropLeft (String.length keyPrefix)
            |> parseValue

    else
        Nothing


dropExport : String -> String
dropExport line =
    if String.startsWith "export " line || String.startsWith "export\t" line then
        String.dropLeft 7 line |> String.trimLeft

    else
        line


parseValue : String -> Maybe String
parseValue rawValue =
    let
        value : String
        value =
            rawValue |> String.trim

        discardBashVariable : String -> Maybe String
        discardBashVariable unwrapped =
            if String.startsWith "$" unwrapped then
                -- Discard values that reference other bash variables, e.g. $OS_PASSWORD_INPUT
                Nothing

            else
                Just unwrapped
    in
    if String.startsWith "\"" value then
        -- Bash expands variables inside double quotes, so "$VAR" is discarded like a bare $VAR
        unwrapQuotedValue "\"" value |> Maybe.andThen discardBashVariable

    else if String.startsWith "'" value then
        -- Single quotes are literal in bash, so '$VAR' is kept as-is
        unwrapQuotedValue "'" value

    else
        let
            unquoted : String
            unquoted =
                dropTrailingComment value
        in
        if String.endsWith "\"" unquoted || String.endsWith "'" unquoted then
            -- A value that closes a quote it never opened is an unterminated string in bash too,
            -- so it is refused rather than stored with the stray quote still on it. This is the
            -- mirror of an opening quote that is never closed, refused below.
            Nothing

        else
            discardBashVariable unquoted


{-| The text between an opening quote and its match, given a value that starts with one. What
follows the closing quote may only be a comment: anything else is a construct this parser does not
understand, and guessing at it would be worse than refusing the line. A quote that is never closed
is refused for the same reason bash refuses it.
-}
unwrapQuotedValue : String -> String -> Maybe String
unwrapQuotedValue quote value =
    let
        afterOpeningQuote : String
        afterOpeningQuote =
            String.dropLeft 1 value
    in
    case String.indexes quote afterOpeningQuote of
        closingQuoteIndex :: _ ->
            if isBlankOrComment (String.dropLeft (closingQuoteIndex + 1) afterOpeningQuote) then
                Just (String.left closingQuoteIndex afterOpeningQuote)

            else
                Nothing

        [] ->
            Nothing


{-| Everything an unquoted value has before a trailing comment. A `#` only opens a comment in bash
when it starts a word, so `pass#word` keeps its hash while `password # note` does not.
-}
dropTrailingComment : String -> String
dropTrailingComment value =
    case List.minimum (String.indexes " #" value ++ String.indexes "\t#" value) of
        Just commentIndex ->
            value |> String.left commentIndex |> String.trimRight

        Nothing ->
            value


isBlankOrComment : String -> Bool
isBlankOrComment text =
    let
        trimmed : String
        trimmed =
            String.trimLeft text
    in
    String.isEmpty trimmed || String.startsWith "#" trimmed
