module Style.Widgets.Text exposing (TextVariant(..), text)

import Element
import Element.Font as Font



--- model


type FontWeight
    = Bold
    | Regular


type alias Typography =
    { size : Int
    , weight : FontWeight
    }


type TextVariant
    = H1
    | H2
    | H3
    | H4
    | Body



--- component


typography : TextVariant -> Typography
typography variant =
    case variant of
        H1 ->
            { size = 26
            , weight = Bold
            }

        H2 ->
            { size = 24
            , weight = Bold
            }

        H3 ->
            { size = 20
            , weight = Bold
            }

        H4 ->
            { size = 16
            , weight = Bold
            }

        _ ->
            { size = 17
            , weight = Regular
            }


fontWeightAttr : FontWeight -> Element.Attribute msg
fontWeightAttr weight =
    case weight of
        Bold ->
            Font.bold

        _ ->
            Font.regular


defaultTypeface : Element.Attribute msg
defaultTypeface =
    Font.family
        [ Font.typeface "Open Sans"
        , Font.sansSerif
        ]


p : List (Element.Attribute msg) -> String -> Element.Element msg
p options label =
    Element.paragraph
        options
        [ Element.text label ]


text : TextVariant -> List (Element.Attribute msg) -> String -> Element.Element msg
text variant options label =
    let
        typo =
            typography variant
    in
    p
        ([ defaultTypeface
         , Font.size typo.size
         , fontWeightAttr typo.weight
         ]
            ++ options
        )
        label
