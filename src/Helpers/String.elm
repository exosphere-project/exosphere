module Helpers.String exposing
    ( indefiniteArticle
    , pluralizeWord
    , stringToTitleCase
    )

import Regex


indefiniteArticle : String -> String
indefiniteArticle phrase =
    case
        -- Look at whatever comes before the first space, hyphen, or period
        phrase
            |> String.split " "
            |> List.map (String.split "-")
            |> List.concat
            |> List.map (String.split ".")
            |> List.concat
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


pluralizeWord : String -> String
pluralizeWord word =
    String.concat
        [ word
        , if String.right 1 word == "s" then
            ""

          else
            "s"
        ]


capitalizeWord : String -> String
capitalizeWord word =
    String.concat
        [ String.left 1 word
            |> String.toUpper
        , String.dropLeft 1 word
        ]


stringToTitleCase : String -> String
stringToTitleCase s =
    String.words s
        |> List.map capitalizeWord
        |> String.join " "
