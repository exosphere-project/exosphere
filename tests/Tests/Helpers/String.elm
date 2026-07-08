module Tests.Helpers.String exposing (indefiniteArticlesSuite, normalizeLineEndingsSuite, utf8ByteLengthSuite)

import Expect
import Helpers.String
import Test exposing (Test, describe, test)


utf8ByteLengthSuite : Test
utf8ByteLengthSuite =
    describe "utf8ByteLength counts encoded UTF-8 bytes, not UTF-16 chars"
        [ test "ASCII maps 1:1" <|
            \_ ->
                Expect.equal 5 (Helpers.String.utf8ByteLength "hello")
        , test "empty string is zero bytes" <|
            \_ ->
                Expect.equal 0 (Helpers.String.utf8ByteLength "")
        , test "a 2-byte character (é, U+00E9) counts as 2" <|
            \_ ->
                Expect.equal 2 (Helpers.String.utf8ByteLength "é")
        , test "a 3-byte character (中, U+4E2D CJK) counts as 3" <|
            \_ ->
                Expect.equal 3 (Helpers.String.utf8ByteLength "中")
        , test "a 4-byte / astral character (😀, U+1F600) is counted correctly-or-conservatively-over (4 to 6)" <|
            \_ ->
                let
                    byteCount =
                        Helpers.String.utf8ByteLength "😀"
                in
                Expect.equal True (byteCount >= 4 && byteCount <= 6)
        , test "mixed ASCII + multibyte sums per-character" <|
            \_ ->
                -- "aé中" = 1 + 2 + 3 = 6
                Expect.equal 6 (Helpers.String.utf8ByteLength "aé中")
        ]


indefiniteArticlesSuite : Test
indefiniteArticlesSuite =
    let
        testData =
            [ { expectedArticle = "an"
              , phrase = "orange"
              , description = "simple word starting with vowel"
              }
            , { expectedArticle = "a"
              , phrase = "fruit"
              , description = "simple word starting with consonant"
              }
            , { expectedArticle = "a"
              , phrase = "TV antenna"
              , description = "acronym starting with consonant sound"
              }
            , { expectedArticle = "an"
              , phrase = "HIV patient"
              , description = "acronym starting with vowel sound"
              }
            , { expectedArticle = "an"
              , phrase = "mRNA vaccine"
              , description = "acronym beginning with a lower-case letter"
              }
            , { expectedArticle = "an"
              , phrase = "R value"
              , description = "first word is just a letter that starts with vowel sound"
              }
            , { expectedArticle = "a"
              , phrase = "U boat"
              , description = "first word is just a letter that starts with consonant sound"
              }
            , { expectedArticle = "a"
              , phrase = "U-boat"
              , description = "hyphenated prefix to first word with consonanty sound"
              }
            , { expectedArticle = "an"
              , phrase = "I-beam"
              , description = "hyphenated prefix to first word with vowely sound"
              }
            , { expectedArticle = "an"
              , phrase = "E.B. White"
              , description = "vowely-sounding initials"
              }
            , { expectedArticle = "a"
              , phrase = "C. Mart"
              , description = "consonanty-sounding initials"
              }
            ]
    in
    describe "Correct indefinite article for various phrases"
        (List.map
            (\item ->
                test item.description <|
                    \_ ->
                        Expect.equal
                            item.expectedArticle
                            (Helpers.String.indefiniteArticle item.phrase)
            )
            testData
        )


normalizeLineEndingsSuite : Test
normalizeLineEndingsSuite =
    describe "normalizeLineEndings"
        [ test "normalizes Windows CRLF to LF" <|
            \_ ->
                "first\u{000D}\nsecond\u{000D}\nthird"
                    |> Helpers.String.normalizeLineEndings
                    |> Expect.equal "first\nsecond\nthird"
        , test "normalizes classic CR to LF" <|
            \_ ->
                "first\u{000D}second\u{000D}third"
                    |> Helpers.String.normalizeLineEndings
                    |> Expect.equal "first\nsecond\nthird"
        ]
