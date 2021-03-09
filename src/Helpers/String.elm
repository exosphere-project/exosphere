module Helpers.String exposing
    ( capitalizeString
    , indefiniteArticle
    , pluralizeWord
    , stringToTitleCase
    )


indefiniteArticle : String -> String
indefiniteArticle word =
    let
        firstLetterLower =
            word
                |> String.left 1
                |> String.toLower
    in
    if List.member firstLetterLower [ "a", "e", "i", "o", "u" ] then
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


capitalizeString : String -> String
capitalizeString word =
    String.concat
        [ String.left 1 word
            |> String.toUpper
        , String.dropLeft 1 word
        ]


stringToTitleCase : String -> String
stringToTitleCase s =
    String.words s
        |> List.map capitalizeString
        |> String.join " "
