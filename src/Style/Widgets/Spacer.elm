module Style.Widgets.Spacer exposing (Spacer, spacer)


type alias Spacer =
    { px4 : Int
    , px8 : Int
    , px12 : Int
    , px16 : Int
    , px24 : Int
    , px32 : Int
    , px48 : Int
    , px64 : Int
    }


{-| Fixed amount of space between elements that should be used to maintain consistency.

In elm-ui Element.padding and Element.spacing fields of this record should be used instead of hardcoded numbers.
The sizes chosen in this record are based on <https://www.refactoringui.com/> book's page number 73 (establishing spacing and sizing system).

-}
spacer : Spacer
spacer =
    { px4 = 4
    , px8 = 8
    , px12 = 12
    , px16 = 16
    , px24 = 24
    , px32 = 32
    , px48 = 48
    , px64 = 64
    }
