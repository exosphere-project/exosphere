module DesignSystem.Stories.IconButton exposing (stories)

import DesignSystem.Helpers exposing (Plugins, Renderer, ThemeModel, palettize)
import Element.Background
import Element.Font as Font
import Style.Helpers exposing (toElementColor)
import Style.Widgets.Icon as Icon
import Style.Widgets.IconButton exposing (FlowOrder(..), iconButton, notes)
import UIExplorer exposing (storiesOf)


stories :
    Renderer msg
    -> UIExplorer.UI (ThemeModel model) msg Plugins
stories renderer =
    storiesOf "Icon Button"
        [ ( "Icon Before Label"
          , \m ->
                let
                    palette =
                        palettize m
                in
                renderer palette <|
                    iconButton palette
                        [ Font.color (toElementColor palette.menu.textOrIcon)
                        , Element.Background.color <| toElementColor palette.menu.background
                        ]
                        { icon = Icon.HelpCircle
                        , iconPlacement = Before
                        , label = "Help Me"
                        , onClick = Nothing
                        }
          , { note = notes }
          )
        , ( "Icon After Label"
          , \m ->
                let
                    palette =
                        palettize m
                in
                renderer palette <|
                    iconButton palette
                        [ Font.color (toElementColor palette.menu.textOrIcon)
                        , Element.Background.color <| toElementColor palette.menu.background
                        ]
                        { icon = Icon.HelpCircle
                        , iconPlacement = After
                        , label = "Help Me"
                        , onClick = Nothing
                        }
          , { note = notes }
          )

        -- , ( "Icon After"
        --   , \m -> Element.none
        --   , { note = notes }
        --   )
        ]
