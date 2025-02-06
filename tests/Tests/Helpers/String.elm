module Tests.Helpers.String exposing (indefiniteArticlesSuite)

-- Test related Modules
-- Exosphere Modules Under Test

import Expect
import Helpers.String
import Test exposing (Test, describe, test)


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
