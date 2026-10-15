module OpenStack.CredentialFile exposing (Kind(..), detect, hasSecrets, redactSecrets)

{-| Tells apart the two credential file formats a user can hand to Exosphere, by looking at the
contents rather than the file name, since browsers do not reliably report a file name we could
trust and users rename these files freely.
-}

import List.Extra
import OpenStack.CloudsYaml
import OpenStack.OpenRc


type Kind
    = OpenRcFile
    | CloudsYamlFile
    | UnrecognizedFile


{-| A file that decodes as a clouds.yaml with a credential in it is a clouds.yaml, whatever else
it may contain. Nothing weaker settles the question: an `OS_` assignment can appear inside a
clouds.yaml, in a comment or a block of text, and a `clouds:` line can appear inside a shell
script, in a here-document that writes one out. Only once the strong answer comes back empty do
the two heuristics run, and then only to choose which of the two files the user was probably
trying to give us, so that the message names the right one.
-}
detect : String -> Kind
detect contents =
    if cloudsYamlIsUsable contents then
        CloudsYamlFile

    else if OpenStack.OpenRc.looksLikeOpenRc contents then
        OpenRcFile

    else if OpenStack.CloudsYaml.looksLikeCloudsYaml contents then
        CloudsYamlFile

    else
        UnrecognizedFile


cloudsYamlIsUsable : String -> Bool
cloudsYamlIsUsable contents =
    case OpenStack.CloudsYaml.parse contents of
        Ok _ ->
            True

        Err _ ->
            False


{-| The text as it can safely be shown, with the value of every line that names a password, secret
or token replaced by eight bullets, so that neither the secret nor its length shows. The word has
to come before the `=` or `:`, which is why `$env:OS_PASSWORD = "..."` hides the value and not
everything after `$env:`. A YAML value written below its key, after a blank value or a `|` or `>`,
is hidden on every line indented deeper than the key. Hiding a comment that only mentions one of
the words is fine; showing a secret is not.
-}
redactSecrets : String -> String
redactSecrets text =
    text
        |> String.split "\n"
        |> List.foldl redactLine ( Nothing, [] )
        |> Tuple.second
        |> List.reverse
        |> String.join "\n"


hasSecrets : String -> Bool
hasSecrets text =
    redactSecrets text /= text


{-| Carries the indentation of a secret's key while its value is being read from the lines below.
-}
redactLine : String -> ( Maybe Int, List String ) -> ( Maybe Int, List String )
redactLine line ( maybeKeyIndent, redacted ) =
    let
        indent =
            String.length line - String.length (String.trimLeft line)

        bullets =
            String.repeat 8 "•"
    in
    case maybeKeyIndent of
        Just keyIndent ->
            if String.isEmpty (String.trim line) then
                ( maybeKeyIndent, line :: redacted )

            else if indent > keyIndent then
                ( maybeKeyIndent, (String.left indent line ++ bullets) :: redacted )

            else
                redactLine line ( Nothing, redacted )

        Nothing ->
            case splitAtSecretSeparator line of
                Just ( key, value ) ->
                    if String.isEmpty (String.trim value) || isYamlBlockIndicator (String.trim value) then
                        ( Just indent, line :: redacted )

                    else
                        ( Nothing, (key ++ String.left (String.length value - String.length (String.trimLeft value)) value ++ bullets) :: redacted )

                Nothing ->
                    ( Nothing, line :: redacted )


{-| A line that names a secret, split just after the `=` or `:` that follows the name.
-}
splitAtSecretSeparator : String -> Maybe ( String, String )
splitAtSecretSeparator line =
    let
        -- None of the words can span a separator, so the first separator with a whole word
        -- before it is the first separator after the first word.
        namesASecret : String -> Bool
        namesASecret beforeSeparator =
            List.any (\word -> String.contains word (String.toLower beforeSeparator)) [ "password", "secret", "token" ]
    in
    (String.indexes "=" line ++ String.indexes ":" line)
        |> List.sort
        |> List.Extra.find (\index -> namesASecret (String.left index line))
        |> Maybe.map (\index -> ( String.left (index + 1) line, String.dropLeft (index + 1) line ))


{-| `|` or `>`, with any chomping or indentation indicator after it, such as `|-`, `>+` or `|2`.
-}
isYamlBlockIndicator : String -> Bool
isYamlBlockIndicator value =
    case String.uncons value of
        Just ( first, rest ) ->
            (first == '|' || first == '>') && String.all (\char -> char == '-' || char == '+' || Char.isDigit char) rest

        Nothing ->
            False
