module Style.Helpers exposing
    ( materialStyle
    , popoverAttribs
    , popoverStyleDefaults
    , shadowDefaults
    , toCssColor
    , toElementColor
    , toElementColorWithOpacity
    , toExoPalette
    , toMaterialPalette
    )

import Color
import Css
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes
import Style.Types as ST exposing (ElmUiWidgetStyle, ExoPalette, PopoverPosition(..), StyleMode, Theme(..))
import Widget.Style.Material as Material


toExoPalette : ST.DeployerColorThemes -> StyleMode -> ExoPalette
toExoPalette deployerColors { theme, systemPreference } =
    let
        themeChoice =
            case theme of
                ST.Override choice ->
                    choice

                ST.System ->
                    systemPreference |> Maybe.withDefault ST.Light
    in
    case themeChoice of
        Light ->
            { primary = deployerColors.light.primary

            -- I (cmart) don't believe secondary gets used right now, but at some point we'll want to pick a secondary color?
            , secondary = deployerColors.light.secondary
            , background = Color.rgb255 255 255 255
            , surface = Color.rgb255 242 242 242
            , error = Color.rgb255 204 0 0
            , on =
                { primary = Color.rgb255 255 255 255
                , secondary = Color.rgb255 0 0 0
                , background = Color.rgb255 0 0 0
                , surface = Color.rgb255 0 0 0
                , error = Color.rgb255 255 255 255
                , warn = Color.rgb255 0 0 0
                , readyGood = Color.rgb255 0 0 0
                , muted = Color.rgb255 255 255 255
                }
            , warn = Color.rgb255 252 175 62
            , readyGood = Color.rgb255 35 209 96
            , muted = Color.rgb255 122 122 122
            , menu =
                { secondary = Color.rgb255 29 29 29
                , background = Color.rgb255 36 36 36
                , surface = Color.rgb255 51 51 51
                , on =
                    { background = Color.rgb255 181 181 181
                    , surface = Color.rgb255 255 255 255
                    }
                }
            }

        Dark ->
            { primary = deployerColors.dark.primary
            , secondary = deployerColors.dark.primary
            , background = Color.rgb255 36 36 36
            , surface = Color.rgb255 51 51 51
            , error = Color.rgb255 240 84 84
            , on =
                { primary = Color.rgb255 221 221 221
                , secondary = Color.rgb255 0 0 0
                , background = Color.rgb255 205 205 205
                , surface = Color.rgb255 255 255 255
                , error = Color.rgb255 255 255 255
                , warn = Color.rgb255 0 0 0
                , readyGood = Color.rgb255 0 0 0
                , muted = Color.rgb255 255 255 255
                }
            , warn = Color.rgb255 252 175 62
            , readyGood = Color.rgb255 23 183 148
            , muted = Color.rgb255 122 122 122
            , menu =
                { secondary = Color.rgb255 29 29 29
                , background = Color.rgb255 36 36 36
                , surface = Color.rgb255 51 51 51
                , on =
                    { background = Color.rgb255 181 181 181
                    , surface = Color.rgb255 255 255 255
                    }
                }
            }


materialStyle : ExoPalette -> ElmUiWidgetStyle {} msg
materialStyle exoPalette =
    let
        regularPalette =
            toMaterialPalette exoPalette

        warningPalette =
            { regularPalette | primary = exoPalette.warn }

        dangerPalette =
            { regularPalette | primary = exoPalette.error }

        exoButtonAttributes =
            [ Element.htmlAttribute <| Html.Attributes.style "text-transform" "none"
            , Font.bold
            ]

        outlinedButton palette_ =
            let
                ob =
                    Material.outlinedButton palette_
            in
            { ob
                | container =
                    ob.container
                        ++ exoButtonAttributes
            }

        containedButton palette_ =
            let
                cb =
                    Material.containedButton palette_
            in
            { cb
                | container =
                    cb.container
                        ++ exoButtonAttributes
            }

        textButton palette_ =
            let
                tb =
                    Material.textButton palette_
            in
            { tb
                | container =
                    tb.container
                        ++ exoButtonAttributes
            }

        iconButton palette_ =
            let
                tb =
                    Material.iconButton palette_
            in
            { tb
                | container =
                    tb.container
                        ++ [ Font.color (toElementColor palette_.primary) ]
            }

        tab =
            let
                defaultTab =
                    Material.tab regularPalette

                defaultTB =
                    defaultTab.button

                exoTBAttributes =
                    exoButtonAttributes
                        ++ [ Element.fill
                                |> Element.maximum 500
                                |> Element.width
                           ]

                exoTB =
                    { defaultTB
                        | container = defaultTB.container ++ exoTBAttributes
                    }
            in
            { defaultTab
                | button = exoTB
                , containerColumn = defaultTab.containerColumn ++ [ Element.spacing 15 ]
            }
    in
    { textInput = Material.textInput regularPalette
    , column = Material.column
    , cardColumn =
        let
            style =
                Material.cardColumn regularPalette
        in
        { style
            | element = style.element ++ [ Element.paddingXY 8 6 ]
        }
    , primaryButton = containedButton regularPalette
    , button = outlinedButton regularPalette
    , warningButton = containedButton warningPalette
    , dangerButton = containedButton dangerPalette
    , dangerButtonSecondary = outlinedButton dangerPalette
    , chipButton = Material.chip regularPalette
    , iconButton = iconButton regularPalette
    , textButton = textButton regularPalette
    , row = Material.row
    , progressIndicator = Material.progressIndicator regularPalette
    , tab = tab
    }


toMaterialPalette : ExoPalette -> Material.Palette
toMaterialPalette exoPalette =
    { primary = exoPalette.primary
    , secondary = exoPalette.secondary
    , background = exoPalette.background
    , surface = exoPalette.surface
    , error = exoPalette.error
    , on =
        { primary = exoPalette.on.primary
        , secondary = exoPalette.on.secondary
        , background = exoPalette.on.background
        , surface = exoPalette.on.surface
        , error = exoPalette.on.error
        }
    }


toElementColor : Color.Color -> Element.Color
toElementColor color =
    -- https://github.com/mdgriffith/elm-ui/issues/28#issuecomment-566337247
    let
        { red, green, blue, alpha } =
            Color.toRgba color
    in
    Element.rgba red green blue alpha


toElementColorWithOpacity : Color.Color -> Float -> Element.Color
toElementColorWithOpacity color alpha =
    -- https://github.com/mdgriffith/elm-ui/issues/28#issuecomment-566337247
    let
        { red, green, blue } =
            Color.toRgba color
    in
    Element.rgba red green blue alpha


toCssColor : Color.Color -> Css.Color
toCssColor color =
    let
        { red, green, blue } =
            Color.toRgba color

        to255Int float =
            float
                * 255
                |> round
    in
    Css.rgb (to255Int red) (to255Int green) (to255Int blue)


shadowDefaults : { offset : ( Float, Float ), blur : Float, size : Float, color : Element.Color }
shadowDefaults =
    { offset = ( 0, 4 )
    , blur = 8
    , size = 0
    , color = Element.rgba255 3 3 3 0.18
    }


popoverStyleDefaults : ExoPalette -> List (Element.Attribute msg)
popoverStyleDefaults palette =
    [ Element.padding 10
    , Background.color <| toElementColor palette.background
    , Border.width 1
    , Border.color <| toElementColorWithOpacity palette.on.background 0.16
    , Border.shadow shadowDefaults
    ]


popoverAttribs :
    Element.Element msg
    -> PopoverPosition
    -> Maybe Int
    -> List (Element.Attribute msg)
popoverAttribs popover position distanceToTarget =
    let
        padding =
            Maybe.withDefault 6 distanceToTarget

        alignOnYAttribs percentStr =
            -- alignment on Y axis of a nearby element doesn't work without this
            [ Element.htmlAttribute <| Html.Attributes.style "top" percentStr
            , Element.htmlAttribute <| Html.Attributes.style "transform" ("translateY(-" ++ percentStr ++ ")")
            ]

        attribs :
            { nearbyElement : Element.Element msg -> Element.Attribute msg
            , alignment : Element.Attribute msg
            , onLeftOrRight : Bool
            , additional : List (Element.Attribute msg)
            }
        attribs =
            case position of
                PositionTopLeft ->
                    { nearbyElement = Element.above
                    , alignment = Element.alignLeft
                    , onLeftOrRight = False
                    , additional = []
                    }

                PositionTop ->
                    { nearbyElement = Element.above
                    , alignment = Element.centerX
                    , onLeftOrRight = False
                    , additional = []
                    }

                PositionTopRight ->
                    { nearbyElement = Element.above
                    , alignment = Element.alignRight
                    , onLeftOrRight = False
                    , additional = []
                    }

                PositionRightTop ->
                    { nearbyElement = Element.onRight
                    , alignment = Element.alignTop
                    , onLeftOrRight = True
                    , additional = alignOnYAttribs "0%"
                    }

                PositionRight ->
                    { nearbyElement = Element.onRight
                    , alignment = Element.centerY
                    , onLeftOrRight = True
                    , additional = alignOnYAttribs "50%"
                    }

                PositionRightBottom ->
                    { nearbyElement = Element.onRight
                    , alignment = Element.alignBottom
                    , onLeftOrRight = True
                    , additional = alignOnYAttribs "100%"
                    }

                PositionBottomRight ->
                    { nearbyElement = Element.below
                    , alignment = Element.alignRight
                    , onLeftOrRight = False
                    , additional = []
                    }

                PositionBottom ->
                    { nearbyElement = Element.below
                    , alignment = Element.centerX
                    , onLeftOrRight = False
                    , additional = []
                    }

                PositionBottomLeft ->
                    { nearbyElement = Element.below
                    , alignment = Element.alignLeft
                    , onLeftOrRight = False
                    , additional = []
                    }

                PositionLeftBottom ->
                    { nearbyElement = Element.onLeft
                    , alignment = Element.alignBottom
                    , onLeftOrRight = True
                    , additional = alignOnYAttribs "100%"
                    }

                PositionLeft ->
                    { nearbyElement = Element.onLeft
                    , alignment = Element.centerY
                    , onLeftOrRight = True
                    , additional = alignOnYAttribs "50%"
                    }

                PositionLeftTop ->
                    { nearbyElement = Element.onLeft
                    , alignment = Element.alignTop
                    , onLeftOrRight = True
                    , additional = alignOnYAttribs "0%"
                    }
    in
    [ attribs.nearbyElement <|
        Element.el
            ([ attribs.alignment
             , if attribs.onLeftOrRight then
                Element.paddingXY padding 0

               else
                Element.paddingXY 0 padding
             ]
                ++ attribs.additional
            )
            popover
    ]
