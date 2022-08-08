module Style.Widgets.Card exposing
    ( badge
    , clickableCardFixedSize
    , exoCard
    )

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Style.Helpers as SH
import Style.Types
import Widget


exoCard : Style.Types.ExoPalette -> Element msg -> Element msg
exoCard palette content =
    -- Disabling mouseover styles because entire card is not clickable
    let
        baseAttribs =
            (SH.materialStyle palette).cardColumn

        attribs =
            { baseAttribs
                | containerColumn =
                    List.append baseAttribs.containerColumn
                        [ Element.mouseOver [] ]
            }
    in
    Widget.column
        attribs
        [ content ]


clickableCardFixedSize : Style.Types.ExoPalette -> Int -> Int -> List (Element msg) -> Element msg
clickableCardFixedSize palette width height content =
    let
        baseAttribs =
            (SH.materialStyle palette).cardColumn

        widthHeightAttribs =
            [ Element.width (Element.px width)
            , Element.height (Element.px height)
            ]

        attribs =
            { baseAttribs
                | containerColumn =
                    List.append baseAttribs.containerColumn
                        widthHeightAttribs
                , element =
                    List.append baseAttribs.element
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        ]
            }
    in
    Widget.column
        attribs
        content


badge : String -> Element msg
badge title =
    -- TODO a bunch of hard-coded colors here that don't cleanly fit in the palette. Look into functions to lighten/darken palette colors
    Element.el
        [ Border.shadow SH.shadowDefaults
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
