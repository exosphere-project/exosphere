module Style.Widgets.Card exposing (badge, exoCard)

import Color
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Framework.Card as Card
import Framework.Color


exoCard : String -> String -> Element msg -> Element msg
exoCard title subTitle content =
    Card.normal
        { title = title
        , subTitle = subTitle
        , content = content
        , colorBackground = Framework.Color.white
        , colorFont = Framework.Color.black
        , colorFontSecondary = Framework.Color.grey
        , colorBorder = Framework.Color.grey_light
        , colorBorderSecondary = Framework.Color.grey_light
        , colorShadow = Color.rgba 0 0 0 0.05
        , extraAttributes = []
        }


badge : String -> Element msg
badge title =
    Element.el
        [ Border.shadow
            { blur = 10
            , color = Color.toElementColor <| Color.rgba 0 0 0 0.05
            , offset = ( 0, 2 )
            , size = 1
            }
        , Border.width 1
        , Border.color <| Color.toElementColor Framework.Color.grey_light
        , Background.gradient
            { angle = pi
            , steps =
                [ Color.toElementColor <| Color.hexToColor "A0A0A0"
                , Color.toElementColor <| Color.hexToColor "8F8F8F"
                ]
            }
        , Font.color <| Color.toElementColor Framework.Color.white
        , Font.size 11
        , Font.shadow
            { offset = ( 0, 2 )
            , blur = 10
            , color = Color.toElementColor Framework.Color.grey_dark
            }
        , Border.rounded 4
        , Element.paddingEach
            { top = 4
            , right = 6
            , bottom = 5
            , left = 6
            }
        , Element.width Element.shrink
        , Element.height Element.shrink
        ]
    <|
        Element.text title
