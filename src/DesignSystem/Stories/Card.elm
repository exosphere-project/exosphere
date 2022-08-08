module DesignSystem.Stories.Card exposing (stories)

import DesignSystem.Helpers exposing (Plugins, Renderer, ThemeModel, palettize)
import Element
import Style.Widgets.Card exposing (clickableCardFixedSize, exoCard)
import Style.Widgets.CopyableText exposing (copyableText)
import Style.Widgets.Text as Text
import UIExplorer
    exposing
        ( storiesOf
        )
import View.Helpers as VH


stories :
    Renderer msg
    -> UIExplorer.UI (ThemeModel model) msg Plugins
stories renderer =
    storiesOf
        "Card"
        [ ( "exoCard"
          , \m ->
                renderer (palettize m) <|
                    Element.column []
                        [ exoCard (palettize m)
                            (Element.column
                                VH.contentContainer
                                [ Text.subheading (palettize m)
                                    []
                                    Element.none
                                    "Status"
                                , Element.row []
                                    [ Element.el [] (Element.text <| "Available") ]
                                , VH.compactKVRow "UUID:" <| copyableText (palettize m) [] "3050e6a0-10e2-4ecf-97c5-d197995ac621"
                                , VH.compactKVRow
                                    (String.concat
                                        [ "Created from "
                                        , "image"
                                        , ":"
                                        ]
                                    )
                                    (Element.text "JS-API-Featured-Ubuntu16-MATLAB-Latest")
                                ]
                            )
                        ]
          , { note = note }
          )
        , ( "clickableCardFixedSize"
          , \m ->
                renderer (palettize m) <|
                    Element.link []
                        { url = "/#Organisms/Card"
                        , label = clickableCardFixedSize (palettize m) 300 300 [ Text.body "Navigate to first story." ]
                        }
          , { note = note }
          )
        ]


note : String
note =
    """
## Usage

Cards separate logical units of associated information. Their content is very flexible. 

### Variants

#### exoCard

Used for displaying related information & creates a border around content. It is not interactive; while its content may be clickable, the card itself does not link to a detail view.

#### clickableCardFixedSize

Has a hover effect with the intention that it is wrapped in a link element.

It typically navigates users to a detail page for the represented item e.g. the project, the volume, etc.
"""
