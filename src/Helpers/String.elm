module Helpers.String exposing
    ( capitalizeWord
    , formatStringTemplate
    , hyphenate
    , indefiniteArticle
    , itemsListToString
    , normalizeLineEndings
    , pluralize
    , pluralizeCount
    , removeEmptiness
    , toTitleCase
    , utf8ByteLength
    )

import List.Extra
import Regex


formatStringTemplate : String -> List ( String, String ) -> String
formatStringTemplate =
    List.foldl (\t -> String.replace (Tuple.first t) (Tuple.second t))


indefiniteArticle : String -> String
indefiniteArticle phrase =
    case
        -- Look at whatever comes before the first space, hyphen, or period
        phrase
            |> String.split " "
            |> List.concatMap (String.split "-")
            |> List.concatMap (String.split ".")
            |> List.head
    of
        Nothing ->
            -- String is empty, so we have "a" nothing
            "a"

        Just firstWord ->
            let
                acronymRegex =
                    Regex.fromString "[A-Z]{2}"
                        |> Maybe.withDefault Regex.never

                firstWordIsPronouncedLikeAcronym =
                    Regex.contains acronymRegex firstWord

                firstLetterLowercase =
                    phrase
                        |> String.left 1
                        |> String.toLower
            in
            if firstWordIsPronouncedLikeAcronym || String.length firstWord == 1 then
                let
                    lettersThatSoundVowely =
                        [ "a", "e", "f", "h", "i", "l", "m", "n", "o", "r", "s", "x" ]
                in
                if List.member firstLetterLowercase lettersThatSoundVowely then
                    "an"

                else
                    "a"

            else
                let
                    vowels =
                        [ "a", "e", "i", "o", "u" ]
                in
                if List.member firstLetterLowercase vowels then
                    "an"

                else
                    "a"


pluralize : String -> String
pluralize word =
    String.concat
        [ word
        , if String.right 1 word == "s" then
            "es"

          else
            "s"
        ]


pluralizeCount : Int -> String -> String
pluralizeCount count word =
    if count == 1 then
        word

    else
        pluralize word


capitalizeWord : String -> String
capitalizeWord word =
    String.concat
        [ String.left 1 word
            |> String.toUpper
        , String.dropLeft 1 word
        ]


toTitleCase : String -> String
toTitleCase s =
    String.words s
        |> List.map capitalizeWord
        |> String.join " "


hyphenate : List String -> String
hyphenate strings =
    strings |> List.intersperse "-" |> String.concat


itemsListToString : List String -> String
itemsListToString items =
    case items of
        [] ->
            ""

        firstItem :: items_ ->
            case List.Extra.unconsLast items_ of
                Nothing ->
                    firstItem

                Just ( lastItem, intermediateItems ) ->
                    if List.isEmpty intermediateItems then
                        firstItem ++ " and " ++ lastItem

                    else
                        String.join ", " (firstItem :: intermediateItems)
                            ++ ", and "
                            ++ lastItem


normalizeLineEndings : String -> String
normalizeLineEndings =
    String.replace "\u{000D}\n" "\n"
        >> String.replace "\u{000D}" "\n"


{-| The number of bytes a string occupies when encoded as UTF-8.

Swift limits names by encoded bytes, not characters, so `String.length` (UTF-16 code units) is the
wrong measure. `String.foldl` folds whole code points, so astral characters count as 4 bytes.

-}
utf8ByteLength : String -> Int
utf8ByteLength string =
    let
        byteWidth : Char -> Int
        byteWidth char =
            let
                code =
                    Char.toCode char
            in
            if code < 0x80 then
                1

            else if code < 0x0800 then
                2

            else if code < 0x00010000 then
                3

            else
                4
    in
    String.foldl (\char acc -> acc + byteWidth char) 0 string


{-| Remove extraneous whitespace & prefer `Nothing` to `Just ""`.
-}
removeEmptiness : Maybe String -> Maybe String
removeEmptiness description =
    description
        |> Maybe.map String.trim
        |> Maybe.andThen
            (\d ->
                if String.isEmpty d then
                    Nothing

                else
                    Just d
            )
