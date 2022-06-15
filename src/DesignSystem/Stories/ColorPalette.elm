module DesignSystem.Stories.ColorPalette exposing (stories)

import Color
import Color.Convert exposing (colorToHex)
import Element exposing (rgba)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html
import Style.Helpers as SH
import Style.Types
import Style.Widgets.Text as Text
import UIExplorer exposing (storiesOf)
import UIExplorer.ColorMode exposing (ColorMode(..))


{-| Creates stores for UIExplorer.

    renderer – An elm-ui to html converter
    palette  – Takes a UIExplorer.Model and produces an ExoPalette
    plugins  – UIExplorer plugins (can be empty {})

-}
stories : (Style.Types.ExoPalette -> Element.Element msg -> Html.Html msg) -> (UIExplorer.Model { model | deployerColors : Style.Types.DeployerColorThemes } msg plugins -> Style.Types.ExoPalette) -> plugins -> UIExplorer.UI { model | deployerColors : Style.Types.DeployerColorThemes } msg plugins
stories renderer palette plugins =
    storiesOf
        "Color Palette"
        [ ( "brand"
          , \m ->
                renderer (palette m) <|
                    collection
                        -- TODO: redesign swatches in a way that `textOnNeutralBG`, `default`, `border` of UI states (danger, warning, etc.) can also be shown
                        [ swatch
                            [ namedBlock "primary" <| (palette m).primary
                            , namedBlock "secondary" <| (palette m).secondary
                            , namedBlock "background" <| (palette m).background
                            , namedBlock "surface" <| (palette m).surface
                            , namedBlock "danger.background" <| (palette m).danger.background
                            , namedBlock "warning.background" <| (palette m).warning.background
                            , namedBlock "success.background" <| (palette m).success.background
                            , namedBlock "info.background" <| (palette m).info.background
                            , namedBlock "muted.background" <| (palette m).muted.background
                            ]
                        , swatch
                            [ namedBlock "on.primary" <| (palette m).on.primary
                            , namedBlock "on.secondary" <| (palette m).on.secondary
                            , namedBlock "on.background" <| (palette m).on.background
                            , namedBlock "on.surface" <| (palette m).on.surface
                            , namedBlock "danger.textOnColoredBG" <| (palette m).danger.textOnColoredBG
                            , namedBlock "warning.textOnColoredBG" <| (palette m).warning.textOnColoredBG
                            , namedBlock "success.textOnColoredBG" <| (palette m).success.textOnColoredBG
                            , namedBlock "info.textOnColoredBG" <| (palette m).info.textOnColoredBG
                            , namedBlock "muted.textOnColoredBG" <| (palette m).muted.textOnColoredBG
                            ]
                        , swatch
                            [ wcagBlock "primary" (palette m).on.primary (palette m).primary
                            , wcagBlock "secondary" (palette m).on.secondary (palette m).secondary
                            , wcagBlock "background" (palette m).on.background (palette m).background
                            , wcagBlock "surface" (palette m).on.surface (palette m).surface
                            , wcagBlock "danger" (palette m).danger.textOnColoredBG (palette m).danger.background
                            , wcagBlock "warning" (palette m).warning.textOnColoredBG (palette m).warning.background
                            , wcagBlock "success" (palette m).success.textOnColoredBG (palette m).success.background
                            , wcagBlock "info" (palette m).info.textOnColoredBG (palette m).info.background
                            , wcagBlock "muted" (palette m).muted.textOnColoredBG (palette m).muted.background
                            ]
                        ]
          , plugins
          )
        , ( "menu"
          , \m ->
                renderer (palette m) <|
                    collection
                        [ swatch
                            [ namedBlock "background" <| (palette m).menu.background
                            , namedBlock "surface" <| (palette m).menu.surface
                            , namedBlock "secondary" <| (palette m).menu.secondary
                            ]
                        , swatch
                            [ namedBlock "on.background" <| (palette m).menu.on.background
                            , namedBlock "on.surface" <| (palette m).menu.on.surface
                            ]
                        , swatch
                            [ wcagBlock "background" (palette m).menu.on.background (palette m).menu.background
                            , wcagBlock "surface" (palette m).menu.on.surface (palette m).menu.surface
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
        , Element.paragraph [ Font.size 14, Text.strong ] [ Element.text label ]
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
