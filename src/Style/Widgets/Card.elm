module Style.Widgets.Card exposing
    ( badge
    , exoCard
    , expandoCard
    )

-- import Element.Events as Events

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Style.Helpers as SH
import Style.Types
import Widget


exoCard : Style.Types.ExoPalette -> Element msg -> Element msg -> Element msg -> Element msg
exoCard palette title subTitle content =
    Widget.column
        (SH.materialStyle palette).cardColumn
        [ Element.row
            [ Element.width Element.fill, Element.spacing 15 ]
            [ Element.el [ Font.bold, Font.size 16 ] title
            , Element.el [ Element.alignRight ] subTitle
            ]
        , content
        ]


expandoCard : Style.Types.ExoPalette -> Bool -> (Bool -> msg) -> String -> Element msg -> Element msg -> Element msg
expandoCard palette expanded expandToggleMsg title subTitle content =
    let
        expandButton : Element.Element msg
        expandButton =
            let
                iconFunction checked =
                    let
                        featherIcon =
                            if checked then
                                FeatherIcons.chevronDown

                            else
                                FeatherIcons.chevronRight
                    in
                    featherIcon |> FeatherIcons.toHtml [] |> Element.html

                checkboxLabel =
                    ""
            in
            Element.el
                [ Element.alignLeft
                , Element.centerY
                , Element.width Element.shrink
                ]
                (Input.checkbox [ Element.paddingXY 5 5 ]
                    { checked = expanded
                    , onChange = expandToggleMsg
                    , icon = iconFunction
                    , label = Input.labelRight [] (Element.text checkboxLabel)
                    }
                )

        firstRow =
            Element.row
                [ Element.width Element.fill, Element.spacing 15 ]
                [ expandButton
                , Element.el [ Font.bold, Font.size 20 ] (Element.text title)
                , Element.el [ Element.alignRight, Element.paddingXY 10 0 ] subTitle
                ]
    in
    Widget.column
        ((SH.materialStyle palette).cardColumn
            |> (\x ->
                    { x
                        | containerColumn =
                            (SH.materialStyle palette).cardColumn.containerColumn
                                ++ [ Element.padding 0

                                   -- TODO make this work with buttons on the card
                                   -- , Events.onClick (expanded |> not |> expandToggleMsg)
                                   ]
                        , element =
                            (SH.materialStyle palette).cardColumn.element
                                ++ [ Element.padding 3
                                   ]
                    }
               )
        )
        (if expanded then
            [ firstRow
            , content
            ]

         else
            [ firstRow ]
        )


badge : String -> Element msg
badge title =
    -- TODO a bunch of hard-coded colors here that don't cleanly fit in the palette. Look into functions to lighten/darken palette colors
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
