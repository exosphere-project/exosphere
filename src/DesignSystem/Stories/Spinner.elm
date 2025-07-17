module DesignSystem.Stories.Spinner exposing (stories)

import DesignSystem.Helpers exposing (Plugins, Renderer, ThemeModel, palettize)
import Style.Widgets.Spinner as Spinner
import UIExplorer
    exposing
        ( storiesOf
        )


stories : Renderer msg -> UIExplorer.UI (ThemeModel model) msg Plugins
stories renderer =
    storiesOf
        "Spinner"
        [ ( "small"
          , \m ->
                renderer (palettize m) <|
                    Spinner.small <|
                        palettize m
          , { note = Spinner.notes }
          )
        , ( "medium"
          , \m ->
                renderer (palettize m) <|
                    Spinner.medium <|
                        palettize m
          , { note = Spinner.notes }
          )
        ]
