module DesignSystem.Stories.ColorPalette exposing (stories)

import Color
import Color.Convert exposing (colorToHex)
import Element
import Element.Background as Background
import Html
import Style.Helpers as SH
import Style.Types
import Style.Widgets.Text as Text
import UIExplorer exposing (storiesOf)
import UIExplorer.ColorMode exposing (ColorMode(..))


stories : (Style.Types.ExoPalette -> Element.Element msg -> Html.Html msg) -> (Maybe ColorMode -> Style.Types.ExoPalette) -> plugins -> UIExplorer.UI model msg plugins
stories renderer palette plugins =
    storiesOf
        "Color Palette"
        [ ( "brand"
          , \m ->
                renderer (palette m.colorMode) <|
                    collection
                        [ swatch
                            [ namedBlock "primary" <| (palette m.colorMode).primary
                            , namedBlock "secondary" <| (palette m.colorMode).secondary
                            , namedBlock "background" <| (palette m.colorMode).background
                            , namedBlock "surface" <| (palette m.colorMode).surface
                            , namedBlock "error" <| (palette m.colorMode).error
                            , namedBlock "warn" <| (palette m.colorMode).warn
                            , namedBlock "readyGood" <| (palette m.colorMode).readyGood
                            , namedBlock "muted" <| (palette m.colorMode).muted
                            ]
                        , swatch
                            [ namedBlock "on.primary" <| (palette m.colorMode).on.primary
                            , namedBlock "on.secondary" <| (palette m.colorMode).on.secondary
                            , namedBlock "on.background" <| (palette m.colorMode).on.background
                            , namedBlock "on.surface" <| (palette m.colorMode).on.surface
                            , namedBlock "on.error" <| (palette m.colorMode).on.error
                            , namedBlock "on.warn" <| (palette m.colorMode).on.warn
                            , namedBlock "on.readyGood" <| (palette m.colorMode).on.readyGood
                            , namedBlock "on.muted" <| (palette m.colorMode).on.muted
                            ]
                        ]
          , plugins
          )
        , ( "menu"
          , \m ->
                renderer (palette m.colorMode) <|
                    collection
                        [ swatch
                            [ namedBlock "background" <| (palette m.colorMode).menu.background
                            , namedBlock "surface" <| (palette m.colorMode).menu.surface
                            , namedBlock "secondary" <| (palette m.colorMode).menu.secondary
                            ]
                        , swatch
                            [ namedBlock "on.background" <| (palette m.colorMode).menu.on.background
                            , namedBlock "on.surface" <| (palette m.colorMode).menu.on.surface
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
    120


{-| A square block of a solid color.
-}
block : Color.Color -> Element.Element msg
block color =
    Element.row
        [ Background.color <| SH.toElementColor <| color
        , Element.width (Element.px blockSize)
        , Element.height (Element.px blockSize)
        ]
        []


{-| A labelled block with its hex colour code.
-}
namedBlock : String -> Color.Color -> Element.Element msg
namedBlock label color =
    Element.column
        [ Element.spacing 4 ]
        [ block color, Text.bold label, Text.mono <| colorToHex color ]


{-| A row of colored blocks, like a color swatch.
-}
swatch : List (Element.Element msg) -> Element.Element msg
swatch blocks =
    Element.row
        [ Element.spacing 10 ]
        blocks


{-| A row of colored blocks, like a color swatch.
-}
collection : List (Element.Element msg) -> Element.Element msg
collection swatches =
    Element.column
        [ Element.spacing 30 ]
        swatches
