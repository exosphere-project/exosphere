module DesignSystem.Stories.ColorPalette exposing (stories)

import Color
import Color.Convert exposing (colorToHex)
import DesignSystem.Helpers exposing (Plugins, Renderer, ThemeModel, palettize)
import Element exposing (rgba)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Style.Helpers as SH
import Style.Types as ST
import Style.Widgets.Text as Text
import UIExplorer exposing (storiesOf)
import UIExplorer.ColorMode exposing (ColorMode(..))


{-| Creates stores for UIExplorer.

    renderer – An elm-ui to html converter
    palette  – Takes a UIExplorer.Model and produces an ExoPalette
    plugins  – UIExplorer plugins (can be empty {})

-}
stories : Renderer msg -> UIExplorer.UI (ThemeModel model) msg Plugins
stories renderer =
    storiesOf
        "Color Palette"
        [ ( "Exosphere Colors"
          , \m ->
                renderer (palettize m) <|
                    Element.text "TODO: ExoPalette"
          , { note = """
## Palette

ExoPalette has 5 fields for 5 UI states: `info`, `success`, `warning`, `danger`, `muted`. Each of them has following subfields that are meant to be used as follows:

- `default`: For coloring indicators/shapes/lines. As the name suggests, you can use it when other options don't make sense. E.g. server state's indicators on ServerList & ServerDetails page, grey border of icon buttons, etc.
- `background`, `border`, `textOnColoredBG`: These 3 are usually used together for coloring alert/badge type component that is essentially a container with a background, border, and some text in it. E.g. status badge widget, alert widget, etc.
- `textOnNeutralBG`: For coloring text (and icons, in some cases) on a neutral (aka plain white/black/grey) background. E.g. text input's invalid message text; muted text on Home, ProjectOverview, ServerDetails pages; different colored messages on MessageLog page; etc.

## Readability

Use WCAG content blocks with foreground colours on background colours to test readability.

WCAG are Web Content Accessibility Guidelines.
(Check out the [official quick reference](https://www.w3.org/WAI/WCAG21/quickref/) or
read a [summary on Wikipedia](https://en.wikipedia.org/wiki/Web_Content_Accessibility_Guidelines).)

In particular, this visual test supports:

> **Guideline 1.4 – Distinguishable**
>
> "Make it easier for users to see and hear content including separating foreground from background."
          """ }
          )
        , ( "All Colors"
          , \m ->
                renderer (palettize m) <|
                    collection
                        [ swatch "blue" <|
                            List.map
                                (\colorShade ->
                                    namedBlock
                                        (Tuple.first colorShade)
                                        (Tuple.second colorShade <| SH.allColorsPalette.blue)
                                )
                                colorShades9
                        , swatch "green" <|
                            List.map
                                (\colorShade ->
                                    namedBlock
                                        (Tuple.first colorShade)
                                        (Tuple.second colorShade <| SH.allColorsPalette.green)
                                )
                                colorShades9
                        , swatch "yellow" <|
                            List.map
                                (\colorShade ->
                                    namedBlock
                                        (Tuple.first colorShade)
                                        (Tuple.second colorShade <| SH.allColorsPalette.yellow)
                                )
                                colorShades9
                        , swatch "red" <|
                            List.map
                                (\colorShade ->
                                    namedBlock
                                        (Tuple.first colorShade)
                                        (Tuple.second colorShade <| SH.allColorsPalette.red)
                                )
                                colorShades9
                        , swatch "gray" <|
                            List.map
                                (\colorShade ->
                                    namedBlock
                                        (Tuple.first colorShade)
                                        (Tuple.second colorShade <| SH.allColorsPalette.gray)
                                )
                                grayShades15
                        ]
          , { note = "" }
          )
        ]


{-| The size of the square blocks in the view.
-}
blockSize : number
blockSize =
    72


{-| A border color to create a clear block boundary on pure black or white background.
-}
blockBorderColor : Element.Color
blockBorderColor =
    rgba 0 0 0 0.1


{-| The common attributes of color blocks such as size & border.
-}
blockStyleAttributes : List (Element.Attribute msg)
blockStyleAttributes =
    [ Element.width (Element.px blockSize)
    , Element.height (Element.px blockSize)
    , Border.width 1
    , Border.color blockBorderColor
    ]


{-| A square block of a solid color.
-}
block : Color.Color -> Element.Element msg
block color =
    Element.el
        ((Background.color <| SH.toElementColor <| color)
            :: blockStyleAttributes
        )
        Element.none


{-| A labelled block with its hex colour code.
-}
namedBlock : String -> Color.Color -> Element.Element msg
namedBlock label color =
    Element.column
        [ Element.spacing 6
        , Element.width <| Element.px blockSize
        , Element.alignTop
        , Font.size 12
        ]
        [ block color
        , Element.el [ Font.family [ Font.monospace ] ] <|
            Element.text (colorToHex color)
        , Element.paragraph [] [ Element.text label ]
        ]


{-| A WCAG content block which places foreground & background palette colours together to test readability.
-}
wcagBlock : String -> Color.Color -> Color.Color -> Element.Element msg
wcagBlock label foreground background =
    Element.row
        ((Background.color <| SH.toElementColor <| background)
            :: blockStyleAttributes
        )
        [ Text.text Text.Body [ Font.color <| SH.toElementColor <| foreground, Element.centerX ] label ]


{-| A row of colored blocks, like a color swatch.
-}
swatch : String -> List (Element.Element msg) -> Element.Element msg
swatch name blocks =
    Element.row
        [ Element.spacing 12 ]
        ((Element.el
            [ Element.width <| Element.minimum 60 Element.fill
            , Font.semiBold
            ]
          <|
            Element.text name
         )
            :: blocks
        )


{-| A row of colored blocks, like a color swatch.
-}
collection : List (Element.Element msg) -> Element.Element msg
collection swatches =
    Element.column
        [ Element.spacing 30 ]
        swatches


colorShades9 : List ( String, ST.ColorShades9 -> Color.Color )
colorShades9 =
    [ ( "lightest", .lightest )
    , ( "lighter", .lighter )
    , ( "light", .light )
    , ( "semiLight", .semiLight )
    , ( "base", .base )
    , ( "semiDark", .semiDark )
    , ( "dark", .dark )
    , ( "darker", .darker )
    , ( "darkest", .darkest )
    ]


grayShades15 : List ( String, ST.GrayShades15 -> Color.Color )
grayShades15 =
    [ ( "white", .white )
    , ( "semiWhite", .semiWhite )
    , ( "lightest", .lightest )
    , ( "semiLightest", .semiLightest )
    , ( "lighter", .lighter )
    , ( "light", .light )
    , ( "semiLight", .semiLight )
    , ( "base", .base )
    , ( "semiDark", .semiDark )
    , ( "dark", .dark )
    , ( "darker", .darker )
    , ( "semiDarkest", .semiDarkest )
    , ( "darkest", .darkest )
    , ( "semiBlack", .semiBlack )
    , ( "black", .black )
    ]
