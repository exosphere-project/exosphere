module NoElementFontSizeTest exposing (all)

import NoElementFontSize exposing (rule)
import Review.Rule exposing (Rule)
import Review.Test
import Test exposing (Test, describe, test)


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
                            { message = "Do not use Font.size"
                            , details =
                                [ "Instead of Font.size, you should use Style.Widgets.Text.fontSize"
                                , "see https://gitlab.com/exosphere/exosphere/-/issues/878 for details"
                                ]
                            , under = "Element.Font.size 16"
                            }
                        ]
        , test "Should report an error when imported with an alias" <|
            \() ->
                """module A exposing (..)

import Element.Font as Font

lg = Font.size 16
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Do not use Font.size"
                            , details =
                                [ "Instead of Font.size, you should use Style.Widgets.Text.fontSize"
                                , "see https://gitlab.com/exosphere/exosphere/-/issues/878 for details"
                                ]
                            , under = "Font.size 16"
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
                            { message = "Do not use Font.size"
                            , details =
                                [ "Instead of Font.size, you should use Style.Widgets.Text.fontSize"
                                , "see https://gitlab.com/exosphere/exosphere/-/issues/878 for details"
                                ]
                            , under = "TotallyNotFont.size 16"
                            }
                        ]
        , test "Should report an error when import is exposed" <|
            \() ->
                """module A exposing (..)

import Element.Font exposing (size)

lg = size 16
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Do not use Font.size"
                            , details =
                                [ "Instead of Font.size, you should use Style.Widgets.Text.fontSize"
                                , "see https://gitlab.com/exosphere/exosphere/-/issues/878 for details"
                                ]
                            , under = "size 16"
                            }
                        ]
        ]
