module Style.Widgets.Text exposing (TextVariant(..), body, bold, heading, p, text)

import Element
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Style.Helpers as SH
import Style.Types exposing (ExoPalette)



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


row : List (Element.Attribute msg) -> List (Element.Element msg) -> Element.Element msg
row options elements =
    Element.row
        options
        elements


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


heading : ExoPalette -> List (Element.Attribute msg) -> Element.Element msg -> String -> Element.Element msg
heading palette options icon label =
    row
        ([ Region.heading 2
         , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
         , Border.color (palette.muted |> SH.toElementColor)
         , Element.width Element.fill
         , Element.paddingEach { bottom = 8, left = 0, right = 0, top = 0 }
         , Element.spacing 12
         ]
            ++ options
        )
        [ icon
        , text H2 [] label
        ]
