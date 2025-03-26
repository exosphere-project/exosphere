module DesignSystem.Stories.Select exposing (stories)

import DesignSystem.Helpers exposing (Plugins, Renderer, ThemeModel, palettize)
import Element
import Style.Widgets.Select as Select
import Style.Widgets.Spacer exposing (spacer)
import UIExplorer
    exposing
        ( storiesOf
        )


stories : Renderer msg -> (Maybe Select.Value -> msg) -> UIExplorer.UI (ThemeModel model) msg Plugins
stories renderer onChange =
    storiesOf
        "Select"
        [ ( "select"
          , \m ->
                renderer (palettize m) <|
                    Element.column [ Element.spacing spacer.px12 ]
                        [ Select.select
                            []
                            (palettize m)
                            { onChange = onChange
                            , options = [ ( "server", "An instance" ), ( "security-group", "A firewall ruleset" ) ]
                            , selected = Nothing
                            , label = "What do you need help with?"
                            }
                        ]
          , { note = Select.notes }
          )
        ]
