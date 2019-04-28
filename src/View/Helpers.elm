module View.Helpers exposing
    ( compactKVRow
    , compactKVSubRow
    , edges
    , exoColumnAttributes
    , exoElementAttributes
    , exoPaddingSpacingAttributes
    , exoRowAttributes
    , heading2
    , heading3
    )

import Element
import Element.Font as Font
import Element.Region as Region
import Types.Types exposing (Msg)



{- Elm UI Doodads -}


exoRowAttributes : List (Element.Attribute Msg)
exoRowAttributes =
    exoElementAttributes


exoColumnAttributes : List (Element.Attribute Msg)
exoColumnAttributes =
    exoElementAttributes


exoElementAttributes : List (Element.Attribute Msg)
exoElementAttributes =
    exoPaddingSpacingAttributes


exoPaddingSpacingAttributes : List (Element.Attribute Msg)
exoPaddingSpacingAttributes =
    [ Element.padding 10
    , Element.spacing 10
    ]


heading2 : List (Element.Attribute Msg)
heading2 =
    [ Region.heading 2
    , Font.bold
    , Font.size 24
    ]


heading3 : List (Element.Attribute Msg)
heading3 =
    [ Region.heading 3
    , Font.bold
    , Font.size 20
    ]


compactKVRow : String -> Element.Element Msg -> Element.Element Msg
compactKVRow key value =
    Element.row
        (exoRowAttributes ++ [ Element.padding 0, Element.spacing 10 ])
        [ Element.paragraph [ Element.alignTop, Element.width (Element.px 200), Font.bold ] [ Element.text key ]
        , Element.el [] value
        ]


compactKVSubRow : String -> Element.Element Msg -> Element.Element Msg
compactKVSubRow key value =
    Element.row
        (exoRowAttributes ++ [ Element.padding 0, Element.spacing 10, Font.size 14 ])
        [ Element.paragraph [ Element.width (Element.px 175), Font.bold ] [ Element.text key ]
        , Element.el [] value
        ]


edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }
