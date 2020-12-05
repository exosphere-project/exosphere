module Style.Widgets.Card exposing (badge, exoCard)

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Style.Theme
import Widget
import Widget.Style.Material as Material


exoCard : Material.Palette -> String -> String -> Element msg -> Element msg
exoCard palette title subTitle content =
    Widget.column
        (Style.Theme.materialStyle palette).cardColumn
        [ Element.row
            [ Element.width Element.fill, Element.spacing 15 ]
            [ Element.el [ Font.bold, Font.size 16 ] (Element.text title)
            , Element.el [] (Element.text subTitle)
            ]
        , content
        ]


badge : String -> Element msg
badge title =
    Element.el
        [ Border.shadow
            { blur = 10
            , color = Element.rgba255 0 0 0 0.05
            , offset = ( 0, 2 )
            , size = 1
            }
        , Border.width 1
        , Border.color <| Element.rgb255 181 181 181
        , Background.gradient
            { angle = pi
            , steps =
                [ Element.rgb255 160 160 160
                , Element.rgb255 143 143 143
                ]
            }
        , Font.color <| Element.rgb255 255 255 255
        , Font.size 11
        , Font.shadow
            { offset = ( 0, 2 )
            , blur = 10
            , color = Element.rgb255 74 74 74
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
