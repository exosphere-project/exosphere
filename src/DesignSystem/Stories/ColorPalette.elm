module DesignSystem.Stories.ColorPalette exposing (stories)

import Color
import Color.Convert exposing (colorToHex)
import DesignSystem.Helpers exposing (Plugins, Renderer, ThemeModel, palettize)
import Element exposing (rgba)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import FeatherIcons
import Style.Helpers as SH
import Style.Types as ST
import UIExplorer exposing (storiesOf)
import UIExplorer.ColorMode exposing (ColorMode(..))


notes : String
notes =
    """
## Usage

### Exosphere Colors Palette (ExoPalette)

This is a palette of the specific colors used throughout the Exosphere app, picked from the [All Colors Palette](#Atoms/Color%20Palette/All%20Colors) based on the active theme (light or dark). 

It can be accessed as `palette` field of the `context` record that is passed to almost all `view` functions.

ExoPalette has the following fields that are named *meaningfully* to make color choices intuitive:

- `primary`, `secondary` - the brand colors provided by the deployer. They are used in action buttons, meters, etc.

- `neutral` - plain white/black/gray colors used throughout the UI:

    - `background` - at least two [background layers](https://spectrum.adobe.com/page/using-color/#Background-layers) are required to create depth.

        - `backLayer` - for the background of the outermost container of the app.

        - `frontLayer` - for the background of the elements within the container, such as a card, data list or popover.

    - `border` - for border coloring.

    - `icon` - for iconography; including icons, shapes, illustrations, etc. For example, a slider's track or ticks & axes on a graph.

    - `text` - for text content:

        - `default` - for default text.

        - `subdued` - for text that less important. It is often paired with default colored text to contrast the two. E.g. In the data list on ServerList & VolumeList.

    > Note: When icons are used along with default colored text, use `neutral.text.default` rather than `neutral.icon` for them.

- `info`, `success`, `warning`, `danger`, `muted` - 5 fields to communicate 5 different *UI states* to the user. Each has the following subfields:

    - `default` - for icons, shapes, lines, etc. As the name suggests, use this when other options don't make sense. (e.g. Accent lines on valid/invalid input.)

    - `background`, `border`, `textOnColoredBG` - used together for alert/badge components that have a container with a background, a border, and some text. (e.g. Status badge widgets, alert widgets, etc.)

    - `textOnNeutralBG`- for text (& sometimes icons) on a neutral background. (e.g. A text input's invalid message text.)

- `menu` - for the menu or navigation bar:
    
    - `background` - for the menu background.

    - `textOrIcon` - for text or icons in the menu.

#### Readability

The demos to the right of each color swatch are readability tests based on WCAG - Web Content Accessibility Guidelines.

(Check out the [official quick reference](https://www.w3.org/WAI/WCAG21/quickref/) or read a [summary on Wikipedia](https://en.wikipedia.org/wiki/Web_Content_Accessibility_Guidelines).)

In particular, this visual test supports:

> **Guideline 1.4 – Distinguishable**
>
> "Make it easier for users to see and hear content including separating foreground from background."


### All Colors Palette

This is a palette of the shades of all major colors, from which [ExoPalette](#Atoms/Color%20Palette/Exosphere%20Colors) is derived.

> Note: This must be used only when ExoPalette can't cater to your needs. Be aware that the color shade will remain the same in both light and dark theme. In most cases, add a new field in ExoPalette that can adapt to both the light and dark theme.

As illustrated above, the All Colors Palette has:

- 9 shades of each color (except `gray`), ranging from light on one end to dark on another.

- 15 shades of `gray` color – the 6 extra shades are due to `white` and `black` added on the ends, along with two intermediatory shades. Finer gradation is required near the ends of the gray palette because multiple layers in the app require different shades, and the shades closer to the mid or "base" shade look dirty as background colors.

"""


stories : Renderer msg -> UIExplorer.UI (ThemeModel model) msg Plugins
stories renderer =
    storiesOf
        "Color Palette"
        [ ( "Exosphere Colors"
          , \m ->
                renderer (palettize m) <|
                    Element.column [ Element.spacing 36 ] <|
                        List.concat
                            [ [ swatch "brand"
                                    [ namedBlock "primary" (palettize m).primary
                                    , namedBlock "secondary" (palettize m).secondary
                                    ]
                              , Element.row [ Element.spacing 24 ]
                                    [ swatch "neutral"
                                        [ namedBlock "background. backLayer" (palettize m).neutral.background.backLayer
                                        , namedBlock "background. frontLayer" (palettize m).neutral.background.frontLayer
                                        , namedBlock "border" (palettize m).neutral.border
                                        , namedBlock "icon" (palettize m).neutral.icon
                                        , namedBlock "text.default" (palettize m).neutral.text.default
                                        , namedBlock "text. subdued" (palettize m).neutral.text.subdued
                                        ]
                                    , demoSeperator (palettize m)
                                    , exoNeutralDemo (palettize m)
                                    ]
                              ]
                            , List.map2
                                (\stateName toStateColor ->
                                    Element.row [ Element.spacing 24 ]
                                        [ swatch stateName
                                            [ namedBlock "default" (palettize m |> toStateColor).default
                                            , namedBlock "background" (palettize m |> toStateColor).background
                                            , namedBlock "border" (palettize m |> toStateColor).border
                                            , namedBlock "textOnColoredBG" (palettize m |> toStateColor).textOnColoredBG
                                            , namedBlock "textOnNeutralBG" (palettize m |> toStateColor).textOnNeutralBG
                                            , Element.el [ Element.width <| Element.px blockSize ] Element.none -- to fill space of 6th block in neutral
                                            ]
                                        , demoSeperator (palettize m)
                                        , exoUIStateDemo (palettize m) stateName toStateColor
                                        ]
                                )
                                [ "info", "success", "warning", "danger", "muted" ]
                                [ .info, .success, .warning, .danger, .muted ]
                            , [ swatch "menu"
                                    [ namedBlock "background" (palettize m).menu.background
                                    , namedBlock "textOrIcon" (palettize m).menu.textOrIcon
                                    ]
                              ]
                            ]
          , { note = notes }
          )
        , ( "All Colors"
          , \m ->
                renderer (palettize m) <|
                    Element.column [ Element.spacing 36 ]
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
          , { note = notes }
          )
        ]


{-| The size of the square blocks in the view.
-}
blockSize : Int
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


{-| A row of colored blocks, like a color swatch.
-}
swatch : String -> List (Element.Element msg) -> Element.Element msg
swatch name blocks =
    Element.row
        [ Element.spacing 12 ]
        ((Element.el
            [ Element.width <| Element.minimum 72 Element.fill
            , Font.semiBold
            , Element.alignTop
            , Element.paddingXY 0 8
            ]
          <|
            Element.text name
         )
            :: blocks
        )


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


demoColumnAttrs : ST.ExoPalette -> List (Element.Attribute msg)
demoColumnAttrs palette =
    [ Element.padding 20
    , Element.spacing 20
    , Background.color <| SH.toElementColor palette.neutral.background.backLayer
    , Border.width 1
    , Border.color <| SH.toElementColor palette.neutral.border
    , Font.size 14
    ]


demoSeperator : ST.ExoPalette -> Element.Element msg
demoSeperator palette =
    Element.el
        [ Element.height Element.fill
        , Element.width <| Element.px 1
        , Background.color <| SH.toElementColor palette.neutral.border
        ]
        Element.none


exoNeutralDemo : ST.ExoPalette -> Element.Element msg
exoNeutralDemo palette =
    let
        textDemo layerName =
            Element.row
                []
                [ Element.text "Default text with "
                , Element.el
                    [ Font.color <| SH.toElementColor palette.neutral.text.subdued ]
                    (Element.text "subdued text")
                , Element.text <| " on " ++ layerName ++ " layer."
                ]

        iconDemo =
            FeatherIcons.type_
                |> FeatherIcons.toHtml []
                |> Element.html
                |> Element.el [ Font.color <| SH.toElementColor palette.neutral.icon ]

        textAndIconDemo layerName =
            Element.row [ Element.spacing 8 ] [ iconDemo, textDemo layerName ]
    in
    Element.column
        (demoColumnAttrs palette
            ++ [ Font.color <| SH.toElementColor palette.neutral.text.default ]
        )
        [ Element.el
            [ Element.padding 16
            , Background.color <| SH.toElementColor palette.neutral.background.frontLayer
            , Border.width 1
            , Border.color <| SH.toElementColor palette.neutral.border
            ]
            (textAndIconDemo "front")
        , textAndIconDemo "back"
        ]


exoUIStateDemo :
    ST.ExoPalette
    -> String
    -> (ST.ExoPalette -> ST.UIStateColors)
    -> Element.Element msg
exoUIStateDemo palette stateName toStateColor =
    let
        iconDemo =
            FeatherIcons.eye
                |> FeatherIcons.toHtml []
                |> Element.html
                |> Element.el [ Font.color <| SH.toElementColor (toStateColor palette).default ]
    in
    Element.column
        (demoColumnAttrs palette)
        [ Element.el
            [ Element.padding 16
            , Background.color <| SH.toElementColor (toStateColor palette).background
            , Border.width 1
            , Border.color <| SH.toElementColor (toStateColor palette).border
            , Font.color <| SH.toElementColor (toStateColor palette).textOnColoredBG
            ]
            (Element.text <| stateName ++ " text on colored background.")
        , Element.row [ Element.spacing 8 ]
            [ iconDemo
            , Element.el
                [ Font.color <| SH.toElementColor (toStateColor palette).textOnNeutralBG ]
                (Element.text <| stateName ++ " text on neutral background.")
            ]
        ]
