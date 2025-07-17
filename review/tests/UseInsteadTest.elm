module UseInsteadTest exposing (all)

import Review.Rule exposing (Rule)
import Review.Test
import Test exposing (Test, describe, test)
import UseInstead


type alias TestDatum =
    { name : String
    , code : String
    , under : String
    }


rule : Rule
rule =
    UseInstead.rule ( [ "Element", "Font" ], "size" ) ( [ "Style", "Widgets", "Text" ], "fontSize" ) "see https://gitlab.com/exosphere/exosphere/-/issues/878 for details"


all : Test
all =
    describe "NoElementFontSize"
        [ test "Should report an error when Element.Font.size is used" <|
            \() ->
                """module A exposing (..)
import Element.Font
lg = Element.Font.size 16
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Do not use Element.Font.size"
                            , details =
                                [ "Instead of Element.Font.size, you should use Style.Widgets.Text.fontSize"
                                , "see https://gitlab.com/exosphere/exosphere/-/issues/878 for details"
                                ]
                            , under = "Element.Font.size"
                            }
                        ]
        , test "Should report an error when Element.Font.size is aliased" <|
            \() ->
                """module A exposing (..)
import Element.Font
mySize = Element.Font.size
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Do not use Element.Font.size"
                            , details =
                                [ "Instead of Element.Font.size, you should use Style.Widgets.Text.fontSize"
                                , "see https://gitlab.com/exosphere/exosphere/-/issues/878 for details"
                                ]
                            , under = "Element.Font.size"
                            }
                        ]
        , test
            "Should report an error when imported with an alias"
          <|
            \() ->
                """module A exposing (..)
import Element.Font as Font
lg = Font.size 16
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Do not use Element.Font.size"
                            , details =
                                [ "Instead of Element.Font.size, you should use Style.Widgets.Text.fontSize"
                                , "see https://gitlab.com/exosphere/exosphere/-/issues/878 for details"
                                ]
                            , under = "Font.size"
                            }
                        ]
        , test "Should report an error when imported with a disguised alias" <|
            \() ->
                """module A exposing (..)
import Element.Font as TotallyNotFont
lg = TotallyNotFont.size 16
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Do not use Element.Font.size"
                            , details =
                                [ "Instead of Element.Font.size, you should use Style.Widgets.Text.fontSize"
                                , "see https://gitlab.com/exosphere/exosphere/-/issues/878 for details"
                                ]
                            , under = "TotallyNotFont.size"
                            }
                        ]
        , test "Should report an error when the import is exposed" <|
            \() ->
                """module A exposing (..)
import Element.Font exposing (size)
lg = size 16
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Do not use Element.Font.size"
                            , details =
                                [ "Instead of Element.Font.size, you should use Style.Widgets.Text.fontSize"
                                , "see https://gitlab.com/exosphere/exosphere/-/issues/878 for details"
                                ]
                            , under = "size"
                            }
                            |> Review.Test.atExactly { start = { row = 3, column = 6 }, end = { row = 3, column = 10 } }
                        ]
        ]
