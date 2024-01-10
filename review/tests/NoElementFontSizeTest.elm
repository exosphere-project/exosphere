module NoElementFontSizeTest exposing (all)

import NoElementFontSize exposing (rule)
import Review.Rule exposing (Rule)
import Review.Test
import Test exposing (Test, describe, test)


type alias TestDatum =
    { name : String
    , code : String
    , under : String
    }


testData : List TestDatum
testData =
    [ { name = "Should report an error when Element.Font.size is used"
      , code = """module A exposing (..)

import Element.Font

lg = Element.Font.size 16
"""
      , under = "Element.Font.size 16"
      }
    , { name = "Should report an error when imported with an alias"
      , code = """module A exposing (..)

import Element.Font as Font

lg = Font.size 16
"""
      , under = "Font.size 16"
      }
    , { name = "Should report an error when imported with a disguised alias"
      , code = """module A exposing (..)

import Element.Font as TotallyNotFont

lg = TotallyNotFont.size 16
"""
      , under = "TotallyNotFont.size 16"
      }
    , { name = "Should report an error when import is exposed"
      , code = """module A exposing (..)

import Element.Font exposing (size)

lg = size 16
"""
      , under = "size 16"
      }
    ]


toTest : TestDatum -> Test
toTest datum =
    test datum.name <|
        \() ->
            datum.code
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = "Do not use Font.size"
                        , details =
                            [ "Instead of Font.size, you should use Style.Widgets.Text.fontSize"
                            , "see https://gitlab.com/exosphere/exosphere/-/issues/878 for details"
                            ]
                        , under = datum.under
                        }
                    ]


all : Test
all =
    describe "NoElementFontSize"
        (List.map toTest testData)
