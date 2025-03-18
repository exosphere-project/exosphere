module DesignSystem.Stories.Uuid exposing (stories)

import DesignSystem.Helpers exposing (Plugins, Renderer, ThemeModel, palettize)
import Style.Widgets.Uuid exposing (copyableUuid, notes, uuidLabel)
import UIExplorer
    exposing
        ( storiesOf
        )


stories :
    Renderer msg
    -> UIExplorer.UI (ThemeModel model) msg Plugins
stories renderer =
    storiesOf
        "UUID"
        [ ( "uuid label"
          , \m ->
                renderer (palettize m) <|
                    uuidLabel (palettize m) "632033bd-9121-49fd-a064-f1d5eedb024f"
          , { note = notes }
          )
        , ( "copyable uuid"
          , \m ->
                renderer (palettize m) <|
                    copyableUuid (palettize m) "632033bd-9121-49fd-a064-f1d5eedb024f"
          , { note = notes }
          )
        ]
