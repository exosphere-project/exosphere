module DesignSystem.Stories.ColorPalette exposing (stories)

import Color
import Color.Convert exposing (colorToHex)
import DesignSystem.Helpers exposing (Renderer, ThemeModel, palettize)
import Element exposing (rgba)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Style.Helpers as SH
import Style.Widgets.Text as Text
import UIExplorer exposing (storiesOf)
import UIExplorer.ColorMode exposing (ColorMode(..))


{-| Creates stores for UIExplorer.

    renderer – An elm-ui to html converter
    palette  – Takes a UIExplorer.Model and produces an ExoPalette
    plugins  – UIExplorer plugins (can be empty {})

-}
stories : Renderer msg -> plugins -> UIExplorer.UI (ThemeModel model) msg plugins
stories renderer plugins =
    storiesOf
        "Color Palette"
        [ ( "brand"
          , \m ->
                renderer (palettize m) <|
                    collection
                        -- TODO: redesign swatches in a way that `textOnNeutralBG`, `default`, `border` of UI states (danger, warning, etc.) can also be shown
                        [ swatch
                            [ namedBlock "primary" <| (palettize m).primary
                            , namedBlock "secondary" <| (palettize m).secondary
                            , namedBlock "background" <| (palettize m).background
                            , namedBlock "surface" <| (palettize m).surface
                            , namedBlock "danger.background" <| (palettize m).danger.background
                            , namedBlock "warning.background" <| (palettize m).warning.background
                            , namedBlock "success.background" <| (palettize m).success.background
                            , namedBlock "info.background" <| (palettize m).info.background
                            , namedBlock "muted.background" <| (palettize m).muted.background
                            ]
                        , swatch
                            [ namedBlock "on.primary" <| (palettize m).on.primary
                            , namedBlock "on.secondary" <| (palettize m).on.secondary
                            , namedBlock "on.background" <| (palettize m).on.background
                            , namedBlock "on.surface" <| (palettize m).on.surface
                            , namedBlock "danger.textOnColoredBG" <| (palettize m).danger.textOnColoredBG
                            , namedBlock "warning.textOnColoredBG" <| (palettize m).warning.textOnColoredBG
                            , namedBlock "success.textOnColoredBG" <| (palettize m).success.textOnColoredBG
                            , namedBlock "info.textOnColoredBG" <| (palettize m).info.textOnColoredBG
                            , namedBlock "muted.textOnColoredBG" <| (palettize m).muted.textOnColoredBG
                            ]
                        , swatch
                            [ wcagBlock "primary" (palettize m).on.primary (palettize m).primary
                            , wcagBlock "secondary" (palettize m).on.secondary (palettize m).secondary
                            , wcagBlock "background" (palettize m).on.background (palettize m).background
                            , wcagBlock "surface" (palettize m).on.surface (palettize m).surface
                            , wcagBlock "danger" (palettize m).danger.textOnColoredBG (palettize m).danger.background
                            , wcagBlock "warning" (palettize m).warning.textOnColoredBG (palettize m).warning.background
                            , wcagBlock "success" (palettize m).success.textOnColoredBG (palettize m).success.background
                            , wcagBlock "info" (palettize m).info.textOnColoredBG (palettize m).info.background
                            , wcagBlock "muted" (palettize m).muted.textOnColoredBG (palettize m).muted.background
                            ]
                        ]
          , plugins
          )
        , ( "menu"
          , \m ->
                renderer (palettize m) <|
                    collection
                        [ swatch
                            [ namedBlock "background" <| (palettize m).menu.background
                            , namedBlock "surface" <| (palettize m).menu.surface
                            , namedBlock "secondary" <| (palettize m).menu.secondary
                            ]
                        , swatch
                            [ namedBlock "on.background" <| (palettize m).menu.on.background
                            , namedBlock "on.surface" <| (palettize m).menu.on.surface
                            ]
                        , swatch
                            [ wcagBlock "background" (palettize m).menu.on.background (palettize m).menu.background
                            , wcagBlock "surface" (palettize m).menu.on.surface (palettize m).menu.surface
                            ]
                        ]
          , plugins
          )

        --TODO: material palette
        ]


{-| The size of the square blocks in the view.
-}
blockSize : number
blockSize =
    108


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
    , Border.widthEach { bottom = 1, left = 1, right = 1, top = 1 }
    , Border.color blockBorderColor
    ]


{-| A square block of a solid color.
-}
block : Color.Color -> Element.Element msg
block color =
    Element.row
        ((Background.color <| SH.toElementColor <| color)
            :: blockStyleAttributes
        )
        []


{-| A labelled block with its hex colour code.
-}
namedBlock : String -> Color.Color -> Element.Element msg
namedBlock label color =
    Element.column
        [ Element.spacing 6, Element.width <| Element.px blockSize, Element.alignTop ]
        [ block color
        , Text.mono <| colorToHex color
        , Element.paragraph [ Font.size 14, Font.semiBold ] [ Element.text label ]
        ]


{-| This WCAG content block uses foreground & background palette colours to test readability.

---

WCAG are Web Content Accessibility Guidelines.
(Check out the [official quick reference](https://www.w3.org/WAI/WCAG21/quickref/) or
read a [summary on Wikipedia](https://en.wikipedia.org/wiki/Web_Content_Accessibility_Guidelines).)

In particular, this visual test supports:

**Guideline 1.4 – Distinguishable**
"Make it easier for users to see and hear content including separating foreground from background."

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
swatch : List (Element.Element msg) -> Element.Element msg
swatch blocks =
    Element.row
        [ Element.spacing 16 ]
        blocks


{-| A row of colored blocks, like a color swatch.
-}
collection : List (Element.Element msg) -> Element.Element msg
collection swatches =
    Element.column
        [ Element.spacing 30 ]
        swatches
