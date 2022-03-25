module Style.Widgets.Text exposing (TextVariant(..), body, bold, button, p, text)

import Element
import Element.Font as Font



--- model


type FontWeight
    = Bold
    | Semibold
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
    | Button
    | Strong



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

        Strong ->
            { size = 17
            , weight = Bold
            }

        Button ->
            { size = 14
            , weight = Semibold
            }

        Body ->
            { size = 17
            , weight = Regular
            }


fontWeightAttr : FontWeight -> Element.Attribute msg
fontWeightAttr weight =
    case weight of
        Bold ->
            Font.bold

        Semibold ->
            Font.semiBold

        Regular ->
            Font.regular


defaultTypeface : Element.Attribute msg
defaultTypeface =
    Font.family
        [ Font.typeface "Open Sans"
        , Font.sansSerif
        ]


p : List (Element.Attribute msg) -> List (Element.Element msg) -> Element.Element msg
p options lines =
    Element.paragraph
        options
        lines


el : List (Element.Attribute msg) -> String -> Element.Element msg
el options label =
    Element.el
        options
        (Element.text label)


text : TextVariant -> List (Element.Attribute msg) -> String -> Element.Element msg
text variant options label =
    let
        typo =
            typography variant
    in
    el
        ([ defaultTypeface
         , Font.size typo.size
         , fontWeightAttr typo.weight
         ]
            ++ options
        )
        label



--- helpers


body : String -> Element.Element msg
body label =
    text Body [] label


bold : String -> Element.Element msg
bold label =
    text Strong [] label


button : String -> Element.Element msg
button label =
    text Button
        [ Font.letterSpacing 1.25 -- ref. Widget.Style.Material buttonFont consistency.
        ]
        label
