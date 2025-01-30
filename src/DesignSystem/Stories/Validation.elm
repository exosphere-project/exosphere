module DesignSystem.Stories.Validation exposing (stories)

import DesignSystem.Helpers exposing (Plugins, Renderer, ThemeModel, palettize)
import Element
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Validation exposing (invalidMessage, notes, validMessage, warningAlreadyExists, warningMessage)
import UIExplorer exposing (storiesOf)


stories :
    Renderer msg
    -> (String -> msg)
    -> UIExplorer.UI (ThemeModel model) msg Plugins
stories renderer onSuggestionPressed =
    storiesOf
        "Validation"
        [ ( "valid", \m -> renderer (palettize m) <| validMessage (palettize m) "The record is up to date.", { note = notes } )
        , ( "invalid", \m -> renderer (palettize m) <| invalidMessage (palettize m) "Name cannot start with a space", { note = notes } )
        , ( "warning", \m -> renderer (palettize m) <| warningMessage (palettize m) "This volume name already exists for this project.", { note = notes } )
        , ( "warning already exists"
          , \m ->
                renderer (palettize m) <|
                    Element.column [ Element.spacing spacer.px24 ]
                        (warningAlreadyExists
                            { palette = palettize m }
                            { alreadyExists = True
                            , message = "That name is already exists. Please choose another."
                            , suggestions = [ "Timothy", "Clara", "Nightmare Moon" ]
                            , onSuggestionPressed = onSuggestionPressed
                            }
                        )
          , { note = notes }
          )
        ]
