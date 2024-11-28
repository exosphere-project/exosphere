module DesignSystem.Stories.Grid exposing (stories)

import DesignSystem.Helpers exposing (Plugins, Renderer, ThemeModel, palettize)
import Element
import Element.Background as Background
import Element.Border as Border
import Style.Helpers as SH
import Style.Widgets.Grid as Grid exposing (GridCell(..), GridRow(..))
import Style.Widgets.Text as Text
import UIExplorer
    exposing
        ( storiesOf
        )


stories : Renderer msg -> UIExplorer.UI (ThemeModel model) msg Plugins
stories renderer =
    storiesOf
        "Grid"
        [ ( "example"
          , \m ->
                renderer (palettize m) <|
                    Grid.grid
                        [ Border.width 1
                        , Border.color <| SH.toElementColor (palettize m).neutral.border
                        , Border.rounded 4
                        ]
                        [ GridRow [ Background.color <| Element.rgba 80 0 0 0.1 ]
                            [ GridCell [ Background.color <| Element.rgba 80 0 0 0.1 ] (Text.body "Cell 1")
                            , GridCell [ Background.color <| Element.rgba 0 80 0 0.1 ] (Text.body "Cell 2")
                            ]
                        , GridRow [ Background.color <| Element.rgba 0 80 0 0.1 ]
                            [ GridCell [ Background.color <| Element.rgba 80 0 0 0.1 ] (Text.body "Cell 3")
                            , GridCell [ Background.color <| Element.rgba 0 80 0 0.1 ] (Text.body "Cell 4")
                            , Grid.GridCell [ Background.color <| Element.rgba 0 0 80 0.1 ] (Text.body "Cell 5")
                            ]
                        , GridRow [ Background.color <| Element.rgba 0 0 80 0.1 ]
                            [ GridCell [ Background.color <| Element.rgba 80 0 0 0.1, Element.width <| Element.fillPortion 2 ] (Text.body "Cell 6")
                            , GridCell [ Background.color <| Element.rgba 0 80 0 0.1 ] (Text.body "Cell 7")
                            , GridCell [ Background.color <| Element.rgba 0 0 80 0.1 ] (Text.body "Cell 8")
                            ]
                        ]
          , { note = Grid.notes }
          )
        ]
