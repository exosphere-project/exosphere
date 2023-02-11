module Style.Helpers exposing
    ( allColorsPalette
    , materialStyle
    , shadowDefaults
    , toElementColor
    , toElementColorWithOpacity
    , toExoPalette
    , toMaterialPalette
    )

import Color
import Color.Convert exposing (hexToColor)
import Element
import Element.Font as Font
import Html.Attributes
import Style.Types as ST exposing (ElmUiWidgetStyle, ExoPalette, StyleMode, Theme(..))
import Style.Widgets.Spacer exposing (spacer)
import Widget.Style.Material as Material


{-| Palette of the shades of all major colors, from which ExoPalette is derived.
-}
allColorsPalette : ST.AllColorsPalette
allColorsPalette =
    -- Colors are taken from https://tailwindcss.com/docs/customizing-colors#default-color-palette
    -- as {lightest=100, lighter=200, light=300, semiLight=400, base=500,
    -- semiDark=600, dark=700, darker=800, darkest=900}
    { gray =
        -- "Zinc" along with white=0 & black=1000, and 2 intemediatory shades
        -- at both light and dark end: semiWhite=50, semiLightest=150,
        -- semiDarkest=850, semiBlack=950. Their hex code is derived by
        -- interpolation, using color blending tool at https://colorkit.io/
        { white = hexToColor "#ffffff" |> Result.withDefault Color.gray
        , semiWhite = hexToColor "#fafafa" |> Result.withDefault Color.gray
        , lightest = hexToColor "#f4f4f5" |> Result.withDefault Color.gray
        , semiLightest = hexToColor "#ececee" |> Result.withDefault Color.gray
        , lighter = hexToColor "#e4e4e7" |> Result.withDefault Color.gray
        , light = hexToColor "#d4d4d8" |> Result.withDefault Color.gray
        , semiLight = hexToColor "#a1a1aa" |> Result.withDefault Color.gray
        , base = hexToColor "#71717a" |> Result.withDefault Color.gray
        , semiDark = hexToColor "#52525b" |> Result.withDefault Color.gray
        , dark = hexToColor "#3f3f46" |> Result.withDefault Color.gray
        , darker = hexToColor "#27272a" |> Result.withDefault Color.gray
        , semiDarkest = hexToColor "#202023" |> Result.withDefault Color.gray
        , darkest = hexToColor "#18181b" |> Result.withDefault Color.gray
        , semiBlack = hexToColor "#0c0c0e" |> Result.withDefault Color.gray
        , black = hexToColor "#000000" |> Result.withDefault Color.gray
        }
    , blue =
        -- "Sky"
        { lightest = hexToColor "#e0f2fe" |> Result.withDefault Color.blue
        , lighter = hexToColor "#bae6fd" |> Result.withDefault Color.blue
        , light = hexToColor "#7dd3fc" |> Result.withDefault Color.blue
        , semiLight = hexToColor "#38bdf8" |> Result.withDefault Color.blue
        , base = hexToColor "#0ea5e9" |> Result.withDefault Color.blue
        , semiDark = hexToColor "#0284c7" |> Result.withDefault Color.blue
        , dark = hexToColor "#0369a1" |> Result.withDefault Color.blue
        , darker = hexToColor "#075985" |> Result.withDefault Color.blue
        , darkest = hexToColor "#0c4a6e" |> Result.withDefault Color.blue
        }
    , green =
        -- "Lime"
        { lightest = hexToColor "#ecfccb" |> Result.withDefault Color.green
        , lighter = hexToColor "#d9f99d" |> Result.withDefault Color.green
        , light = hexToColor "#bef264" |> Result.withDefault Color.green
        , semiLight = hexToColor "#a3e635" |> Result.withDefault Color.green
        , base = hexToColor "#84cc16" |> Result.withDefault Color.green
        , semiDark = hexToColor "#65a30d" |> Result.withDefault Color.green
        , dark = hexToColor "#4d7c0f" |> Result.withDefault Color.green
        , darker = hexToColor "#3f6212" |> Result.withDefault Color.green
        , darkest = hexToColor "#365314" |> Result.withDefault Color.green
        }
    , yellow =
        -- "Yellow"
        { lightest = hexToColor "#fef9c3" |> Result.withDefault Color.yellow
        , lighter = hexToColor "#fef08a" |> Result.withDefault Color.yellow
        , light = hexToColor "#fde047" |> Result.withDefault Color.yellow
        , semiLight = hexToColor "#facc15" |> Result.withDefault Color.yellow
        , base = hexToColor "#eab308" |> Result.withDefault Color.yellow
        , semiDark = hexToColor "#ca8a04" |> Result.withDefault Color.yellow
        , dark = hexToColor "#a16207" |> Result.withDefault Color.yellow
        , darker = hexToColor "#854d0e" |> Result.withDefault Color.yellow
        , darkest = hexToColor "#713f12" |> Result.withDefault Color.yellow
        }
    , red =
        -- "Red"
        { lightest = hexToColor "#fee2e2" |> Result.withDefault Color.red
        , lighter = hexToColor "#fecaca" |> Result.withDefault Color.red
        , light = hexToColor "#fca5a5" |> Result.withDefault Color.red
        , semiLight = hexToColor "#f87171" |> Result.withDefault Color.red
        , base = hexToColor "#ef4444" |> Result.withDefault Color.red
        , semiDark = hexToColor "#dc2626" |> Result.withDefault Color.red
        , dark = hexToColor "#b91c1c" |> Result.withDefault Color.red
        , darker = hexToColor "#991b1b" |> Result.withDefault Color.red
        , darkest = hexToColor "#7f1d1d" |> Result.withDefault Color.red
        }
    }


{-| Prepares a palette of the specific colors used throughout the app
by picking them from the all colors palette `allColorsPalette`
based on the deployer colors and the theme chosen
-}
toExoPalette : ST.DeployerColorThemes -> StyleMode -> ExoPalette
toExoPalette deployerColors { theme, systemPreference } =
    let
        themeChoice =
            case theme of
                ST.Override choice ->
                    choice

                ST.System ->
                    systemPreference |> Maybe.withDefault ST.Light

        menuColors =
            -- remain same in Light and Dark theme
            { background = allColorsPalette.gray.semiDarkest
            , textOrIcon = allColorsPalette.gray.lightest
            }
    in
    case themeChoice of
        Light ->
            { primary = deployerColors.light.primary
            , secondary = deployerColors.light.secondary
            , neutral =
                { background =
                    { backLayer = allColorsPalette.gray.lightest
                    , frontLayer = allColorsPalette.gray.white
                    }
                , border = allColorsPalette.gray.light
                , icon = allColorsPalette.gray.semiDark
                , text =
                    { default = allColorsPalette.gray.darkest
                    , subdued = allColorsPalette.gray.semiDark
                    }
                }
            , info =
                { default = allColorsPalette.blue.base
                , background = allColorsPalette.blue.lightest
                , border = allColorsPalette.blue.light
                , textOnNeutralBG = allColorsPalette.blue.semiDark
                , textOnColoredBG = allColorsPalette.blue.darkest
                }
            , success =
                { default = allColorsPalette.green.base
                , background = allColorsPalette.green.lightest
                , border = allColorsPalette.green.light
                , textOnNeutralBG = allColorsPalette.green.dark -- `semiDark` has poor contrast since it's a bright color, hence one shade darker
                , textOnColoredBG = allColorsPalette.green.darkest
                }
            , warning =
                { default = allColorsPalette.yellow.base
                , background = allColorsPalette.yellow.lightest
                , border = allColorsPalette.yellow.semiLight -- `light` has relatively poor contrast w/ `lightest` background, hence a shade darker
                , textOnNeutralBG = allColorsPalette.yellow.dark -- `semiDark` has poor contrast since it's a bright color, hence one shade darker
                , textOnColoredBG = allColorsPalette.yellow.darkest
                }
            , danger =
                { default = allColorsPalette.red.base
                , background = allColorsPalette.red.lightest
                , border = allColorsPalette.red.light
                , textOnNeutralBG = allColorsPalette.red.semiDark
                , textOnColoredBG = allColorsPalette.red.darkest
                }
            , muted =
                { default = allColorsPalette.gray.base
                , background = allColorsPalette.gray.lighter -- `lightest` will blend with `neutral.background`, hence a shade darker
                , border = allColorsPalette.gray.semiLight
                , textOnNeutralBG = allColorsPalette.gray.semiDark
                , textOnColoredBG = allColorsPalette.gray.darkest
                }
            , menu = menuColors
            , activeTheme = themeChoice
            }

        Dark ->
            { primary = deployerColors.dark.primary
            , secondary = deployerColors.dark.secondary
            , neutral =
                { background =
                    { backLayer = allColorsPalette.gray.darkest
                    , frontLayer = allColorsPalette.gray.darker
                    }
                , border = allColorsPalette.gray.dark
                , icon = allColorsPalette.gray.semiLight
                , text =
                    { default = allColorsPalette.gray.lightest
                    , subdued = allColorsPalette.gray.semiLight
                    }
                }
            , info =
                { default = allColorsPalette.blue.base
                , background = allColorsPalette.blue.darkest
                , border = allColorsPalette.blue.semiDark
                , textOnNeutralBG = allColorsPalette.blue.semiLight
                , textOnColoredBG = allColorsPalette.blue.lightest
                }
            , success =
                { default = allColorsPalette.green.base
                , background = allColorsPalette.green.darkest
                , border = allColorsPalette.green.semiDark
                , textOnNeutralBG = allColorsPalette.green.semiLight
                , textOnColoredBG = allColorsPalette.green.lightest
                }
            , warning =
                { default = allColorsPalette.yellow.base
                , background = allColorsPalette.yellow.darkest
                , border = allColorsPalette.yellow.semiDark
                , textOnNeutralBG = allColorsPalette.yellow.semiLight
                , textOnColoredBG = allColorsPalette.yellow.lightest
                }
            , danger =
                { default = allColorsPalette.red.base
                , background = allColorsPalette.red.darkest
                , border = allColorsPalette.red.semiDark
                , textOnNeutralBG = allColorsPalette.red.semiLight
                , textOnColoredBG = allColorsPalette.red.lightest
                }
            , muted =
                { default = allColorsPalette.gray.base
                , background = allColorsPalette.gray.dark -- `darkest` & `darker` will blend with `neutral.background`, hence `dark`
                , border = allColorsPalette.gray.base
                , textOnNeutralBG = allColorsPalette.gray.semiLight
                , textOnColoredBG = allColorsPalette.gray.lightest
                }
            , menu = menuColors
            , activeTheme = themeChoice
            }


materialStyle : ExoPalette -> ElmUiWidgetStyle {} msg
materialStyle exoPalette =
    let
        regularPalette =
            toMaterialPalette exoPalette

        warningPalette =
            { regularPalette | primary = exoPalette.warning.default }

        dangerPalette =
            { regularPalette
              -- in light theme, white text has relatively poor contrast on red.base (=danger.default)
              -- hence we need a shade darker i.e. semiDark stored as textOnNeutralBG
                | primary = exoPalette.danger.textOnNeutralBG
            }

        exoButtonAttributes =
            [ Element.htmlAttribute <| Html.Attributes.style "text-transform" "none"
            , Font.semiBold
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
                        ++ [ Font.color (toElementColor palette_.primary)
                           , Element.height Element.shrink -- remove fixed px height
                           ]
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
                , containerColumn = defaultTab.containerColumn ++ [ Element.spacing spacer.px24 ]
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
            | element = style.element ++ [ Element.padding spacer.px8 ]
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
    , background = exoPalette.neutral.background.backLayer
    , surface = exoPalette.neutral.background.frontLayer
    , error = exoPalette.danger.background
    , on =
        { -- on.primary is only being used by Material.containedButton for mixing <1 opacity of it with primary color to obtain hover/focussed background color.
          -- Using neutral.text.default we get sufficiently darker background color in light theme and lighter background color in dark theme.
          primary = exoPalette.neutral.text.default
        , secondary = exoPalette.neutral.text.default -- no evident use
        , background = exoPalette.neutral.text.default -- no evident use
        , surface = exoPalette.neutral.text.default -- border of Material.outlinedButton, Material.cardColum with 0.14 opacity
        , error = exoPalette.danger.textOnColoredBG -- no evident use
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


shadowDefaults : { offset : ( Float, Float ), blur : Float, size : Float, color : Element.Color }
shadowDefaults =
    { offset = ( 0, 4 )
    , blur = 8
    , size = 0
    , color = Element.rgba255 3 3 3 0.18
    }
