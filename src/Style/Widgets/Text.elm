module Style.Widgets.Text exposing (TextVariant(..), body, bold, fontWeightAttr, heading, headingStyleAttrs, p, subheading, subheadingStyleAttrs, text, typography, typographyAttrs, underline)

import Element
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Style.Helpers as SH
import Style.Types exposing (ExoPalette)



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
            { size = 17
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

        Regular ->
            Font.regular


defaultTypeface : Element.Attribute msg
defaultTypeface =
    Font.family
        [ Font.typeface "Open Sans"
        , Font.sansSerif
        ]


typographyAttrs : TextVariant -> List (Element.Attribute msg)
typographyAttrs variant =
    let
        typo =
            typography variant
    in
    [ defaultTypeface
    , Font.size typo.size
    , fontWeightAttr typo.weight
    ]


headingStyleAttrs : ExoPalette -> List (Element.Attribute msg)
headingStyleAttrs palette =
    [ Region.heading 2
    , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
    , Border.color (palette.muted |> SH.toElementColor)
    , Element.width Element.fill
    , Element.paddingEach { bottom = 8, left = 0, right = 0, top = 0 }
    , Element.spacing 12
    ]


subheadingStyleAttrs : ExoPalette -> List (Element.Attribute msg)
subheadingStyleAttrs palette =
    [ Region.heading 3
    , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
    , Border.color (palette.muted |> SH.toElementColor)
    , Element.width Element.fill
    , Element.paddingEach { bottom = 8, left = 0, right = 0, top = 0 }
    , Element.spacing 12
    ]


p : List (Element.Attribute msg) -> List (Element.Element msg) -> Element.Element msg
p styleAttrs lines =
    Element.paragraph
        ([ defaultTypeface
         , Element.spacing 8
         ]
            ++ styleAttrs
        )
        lines


text : TextVariant -> List (Element.Attribute msg) -> String -> Element.Element msg
text variant styleAttrs label =
    Element.el
        (typographyAttrs variant
            ++ styleAttrs
        )
        (Element.text label)



--- helpers


body : String -> Element.Element msg
body label =
    text Body [] label


bold : String -> Element.Element msg
bold label =
    text Strong [] label


underline : String -> Element.Element msg
underline label =
    text Body
        [ Border.widthEach
            { bottom = 1
            , left = 0
            , top = 0
            , right = 0
            }
        ]
        label


heading : ExoPalette -> List (Element.Attribute msg) -> Element.Element msg -> String -> Element.Element msg
heading palette styleAttrs icon label =
    Element.row
        (headingStyleAttrs palette
            ++ styleAttrs
        )
        [ icon
        , text H2 [] label
        ]


subheading : ExoPalette -> List (Element.Attribute msg) -> Element.Element msg -> String -> Element.Element msg
subheading palette styleAttrs icon label =
    Element.row
        (subheadingStyleAttrs palette
            ++ styleAttrs
        )
        [ icon
        , text H3 [] label
        ]
