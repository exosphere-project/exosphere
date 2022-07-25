module DesignSystem.Stories.Card exposing (ExpandoCardState, stories)

import DesignSystem.Helpers exposing (Plugins, Renderer, ThemeModel, palettize)
import Element
import Style.Widgets.Button as Button
import Style.Widgets.Card exposing (clickableCardFixedSize, exoCard, exoCardWithTitleAndSubtitle, expandoCard)
import Style.Widgets.CopyableText exposing (copyableText)
import Style.Widgets.Link as Link
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
                    exoCard (palettize m)
                        (Element.column VH.exoColumnAttributes
                            [ Text.mono "192.168.1.1" ]
                        )
          , { note = note }
          )
        , -- TODO: Render a more complete version of this based on Page.Home.
          ( "clickable card with fixed size", \m -> renderer (palettize m) <| clickableCardFixedSize (palettize m) 300 300 [ Text.body "Lorem ipsum dolor sit amet." ], { note = note } )
        , ( "exoCard with title & subtitle"
          , \m ->
                renderer (palettize m) <|
                    exoCardWithTitleAndSubtitle (palettize m)
                        (Style.Widgets.CopyableText.copyableText
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
          , { note = note }
          )
        , ( "expandoCard"
          , \m ->
                renderer (palettize m) <|
                    expandoCard (palettize m)
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
          , { note = note }
          )
        ]


note : String
note =
    """
## Usage
    """
