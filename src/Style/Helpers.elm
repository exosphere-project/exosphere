module Style.Helpers exposing
    ( materialStyle
    , toElementColor
    , toElementColorWithOpacity
    , toExoPalette
    , toMaterialPalette
    )

import Color
import Element
import Element.Font as Font
import Html.Attributes
import Style.Types exposing (ElmUiWidgetStyle, ExoPalette, StyleMode(..))
import Widget.Style.Material as Material


toExoPalette : Color.Color -> Color.Color -> StyleMode -> ExoPalette
toExoPalette primaryColor secondaryColor styleMode =
    case styleMode of
        LightMode ->
            { primary = primaryColor

            -- I (cmart) don't believe secondary gets used right now, but at some point we'll want to pick a secondary color?
            , secondary = secondaryColor
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

        DarkMode ->
            { primary = primaryColor
            , secondary = secondaryColor
            , background = Color.rgb255 36 36 36
            , surface = Color.rgb255 51 51 51
            , error = Color.rgb255 204 0 0
            , on =
                { primary = Color.rgb255 255 255 255
                , secondary = Color.rgb255 0 0 0
                , background = Color.rgb255 205 205 205
                , surface = Color.rgb255 255 255 255
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
    , chipButton = Material.chip regularPalette
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
