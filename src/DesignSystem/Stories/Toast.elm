module DesignSystem.Stories.Toast exposing (stories)

import DesignSystem.Helpers exposing (Plugins, Renderer, ThemeModel, palettize)
import Element
import Style.Widgets.Button as Button
import UIExplorer
    exposing
        ( storiesOf
        )


{-| Creates stories for UIExplorer.

    renderer – An elm-ui to html converter
    palette  – Takes a UIExplorer.Model and produces an ExoPalette
    plugins  – UIExplorer plugins (can be empty {})

-}
stories :
    Renderer msg
    ->
        { toast
            | onPress : Maybe msg
        }
    -> UIExplorer.UI (ThemeModel model) msg Plugins
stories renderer { onPress } =
    storiesOf
        "Toast"
        (List.map
            (\message ->
                ( message.name
                , \m ->
                    let
                        button press =
                            Button.primary (palettize m)
                                { text = "Show toast"
                                , onPress = press
                                }
                    in
                    renderer (palettize m) <|
                        Element.el [ Element.paddingXY 400 100 ]
                            (button onPress)
                , { note = note }
                )
            )
            [ { name = "warning" }
            ]
        )


note : String
note =
    """
## Usage
    """
