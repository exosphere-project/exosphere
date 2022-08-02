module DesignSystem.Stories.Card exposing (ExpandoCardState, stories)

import DesignSystem.Helpers exposing (Plugins, Renderer, ThemeModel, palettize)
import Element
import Style.Widgets.Button as Button
import Style.Widgets.Card exposing (clickableCardFixedSize, exoCard, exoCardWithTitleAndSubtitle, expandoCard)
import Style.Widgets.CopyableText exposing (copyableText)
import Style.Widgets.Text as Text
import UIExplorer
    exposing
        ( storiesOf
        )
import View.Helpers as VH


{-| Is the Expandable Card expanded or collapsed?
-}
type alias ExpandoCardState =
    { expanded : Bool }


{-| Creates stores for UIExplorer.

    renderer – An elm-ui to html converter
    palette  – Takes a UIExplorer.Model and produces an ExoPalette
    plugins  – UIExplorer plugins (can be empty {})

-}
stories :
    Renderer msg
    ->
        { card
            | onPress : Maybe msg
            , onExpand : Bool -> msg
        }
    -> UIExplorer.UI (ThemeModel { model | expandoCard : ExpandoCardState }) msg Plugins
stories renderer { onPress, onExpand } =
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
        , ( "clickable card with fixed size"
          , \m ->
                renderer (palettize m) <|
                    Element.link []
                        { url = "/#Organisms/Card/exoCard"
                        , label =
                            clickableCardFixedSize (palettize m)
                                200
                                200
                                [ Text.p [ Element.padding 10 ]
                                    [ Text.strong "Clickable card" ]
                                , Text.p [ Element.padding 10, Element.spacing 12 ]
                                    [ Text.body "Click on me anywhere to navigate to exoCard story" ]
                                ]
                        }
          , { note = note }
          )
        , ( "exoCard with title & subtitle"
          , \m ->
                renderer (palettize m) <|
                    Element.column []
                        [ exoCardWithTitleAndSubtitle (palettize m)
                            (copyableText
                                (palettize m)
                                [ Text.monoFontFamily ]
                                "192.168.1.1"
                            )
                            (Button.default
                                (palettize m)
                                { text = "Unassign"
                                , onPress = onPress
                                }
                            )
                            (Text.body "Assigned to a resource that Exosphere cannot represent")
                        ]
          , { note = note }
          )
        , ( "expandoCard"
          , \m ->
                renderer (palettize m) <|
                    Element.column []
                        [ expandoCard (palettize m)
                            m.customModel.expandoCard.expanded
                            onExpand
                            (Text.body "Backup SSD")
                            (Text.body "25 GB")
                            (Element.column
                                VH.contentContainer
                                [ VH.compactKVRow "Name:" <| Text.body "Backup SSD"
                                , VH.compactKVRow "Status:" <| Text.body "Ready"
                                , VH.compactKVRow "Description:" <|
                                    Text.p [ Element.width Element.fill ] <|
                                        [ Element.text "Solid State Drive" ]
                                , VH.compactKVRow "UUID:" <| copyableText (palettize m) [] "6205e1a8-9a5d-4325-bb0d-219f09a4d988"
                                ]
                            )
                        ]
          , { note = note }
          )
        ]


note : String
note =
    """
## Usage

Cards separate logical units of associated information. Their content is very flexible. 

You can use following functions from `Style.Widgets.Card`:

### exoCard

The `exoCard` is used for displaying related information, & creates a border around content. It is not interactive; while it's content may be clickable, the card itself does not link to a detail view.

### clickableCardFixedSize

A `clickableCardFixedSize` has a hover effect with the intention that it is wrapped in a link element:

    Element.link []
        { url = "/#Organisms/Card"
        , label = clickableCardFixedSize (palettize m) 300 300 [ Text.body "Lorem ipsum dolor sit amet." ]
        }

This navigates users to a detail page for the represented item e.g. the project, the volume, etc.

### exoCardWithTitleAndSubtitle

An `exoCardWithTitleAndSubtitle` has a hover effect similar to `clickableCardFixedSize` but also separates content into a title & subtitle. The main content of the card should be contained in the subtitle.

_This widget is currently unused._

### expandoCard

The `expandoCard` has a hover effect & can be expanded to reveal additional content using a toggle button.

_This widget is currently unused._
    """
