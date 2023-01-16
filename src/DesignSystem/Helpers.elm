module DesignSystem.Helpers exposing (Plugins, Renderer, ThemeModel, palettize, toHtml)

import Element
import Element.Font as Font
import Html
import Style.Helpers as SH
import Style.Types
import Style.Widgets.Text as Text exposing (FontFamily(..), TextVariant(..))
import UIExplorer
import UIExplorer.ColorMode exposing (ColorMode(..))



--- elm-ui helpers


type alias ThemeModel model =
    { model | deployerColors : Style.Types.DeployerColorThemes }


type alias Plugins =
    { note : String }


{-| Creates an ExoPalette based on the light/dark color mode.
-}
palettize : UIExplorer.Model (ThemeModel model) msg plugins -> Style.Types.ExoPalette
palettize m =
    let
        colorTheme =
            m.customModel.deployerColors

        maybeColorMode =
            m.colorMode

        theme =
            case maybeColorMode of
                Just colorMode ->
                    case colorMode of
                        Light ->
                            Style.Types.Light

                        Dark ->
                            Style.Types.Dark

                Nothing ->
                    Style.Types.Light
    in
    SH.toExoPalette
        colorTheme
        { theme = Style.Types.Override theme, systemPreference = Nothing }


{-| A component that uses a palette to transform a Elm-UI Element to HTML.
-}
type alias Renderer msg =
    Style.Types.ExoPalette -> Element.Element msg -> Html.Html msg


{-| Converts an Elm-UI Element to HTML, applying global styling attributes.
-}
toHtml : Renderer msg
toHtml palette a =
    Element.layout
        [ Text.fontSize Body
        , Text.fontFamily Default
        , Font.color <| SH.toElementColor <| palette.neutral.text.default
        ]
        a
