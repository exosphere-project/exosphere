module DesignSystem.Stories.Space exposing (stories)

import DesignSystem.Helpers exposing (Plugins, Renderer, ThemeModel, palettize)
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import FeatherIcons
import Style.Helpers as SH exposing (spacer)
import Style.Types as ST
import Style.Widgets.Text as Text
import UIExplorer exposing (storiesOf)


notes : String
notes =
    """
## Usage

Space is set by elm-ui's [padding and spacing](https://package.elm-lang.org/packages/mdgriffith/elm-ui/1.1.8/Element#padding-and-spacing) attributes. 

For specifying space size (in pixels) in both padding and spacing, numbers from the `spacer` record must be used. This helps in preventing visual imbalance and maintains consistency. Hardcoded numbers should only be used if it's a special case with a valid reason.

### Important Guidelines
- When laying out different elements, spacing should be almost always used to set the space between the elements. Using padding for this purpose, will create visual inconsistencies as demonstrated above. You can notice:
    - relatively extra space between the 1st element with padding and element above it.
    - broken vertical alignment, as padding wasn't constrained to only the Y axis (with spacing, you don't need to remember that)

- When creating a widget (or view helper), if its topmost container is borderless, then it must not have padding. E.g. if the "meter" widget (with column as its topmost container) had padding, the resulting space would have appeared externally when placing it on a page, creating problems as explained in the last point. On the other hand, "status badge" widget can rightfully have padding because the resulting space appears bounded by a border (and background color).
"""


stories : Renderer msg -> UIExplorer.UI (ThemeModel model) msg Plugins
stories renderer =
    storiesOf
        "Space"
        [ ( "spacer"
          , \m ->
                renderer (palettize m) <|
                    Element.column [ Element.spacing spacer.px16 ] <|
                        List.map spacerBlock spacerFields
          , { note = notes }
          )
        , ( "prefer spacing over padding"
          , \m ->
                renderer (palettize m) <|
                    Element.column [ Element.spacing spacer.px24 ]
                        [ Text.p [] [ Text.body "Suppose, we want to add a group (column) of elements in a page while maintaining uniformity (i.e. all of them vertically align and are 24px apart from each other)." ]
                        , Element.row [ Element.spacing spacer.px32 ]
                            [ Element.column [ Element.spacing spacer.px24 ]
                                [ demoPage (palettize m) <|
                                    Element.column []
                                        [ Text.p [ Element.padding spacer.px12 ] [ Text.body "In the column: Demo text with 12px padding" ]
                                        , Text.p [ Element.padding spacer.px12 ] [ Text.body "In the column: Demo text with 12px padding" ]
                                        , Text.p [ Element.padding spacer.px12 ] [ Text.body "In the column: Demo text with 12px padding" ]
                                        ]
                                , methodStatus (palettize m) False "Using padding on each element of the column to set space between them"
                                ]
                            , Element.column
                                [ Element.spacing spacer.px24
                                , Element.alignTop
                                ]
                                [ demoPage (palettize m) <|
                                    Element.column [ Element.spacing spacer.px24 ]
                                        [ Text.p [] [ Text.body "In the column with 24px spacing: Demo text" ]
                                        , Text.p [] [ Text.body "In the column with 24px spacing: Demo text" ]
                                        , Text.p [] [ Text.body "In the column with 24px spacing: Demo text" ]
                                        ]
                                , methodStatus (palettize m) True "Using spacing on the column to set space between its elements"
                                ]
                            ]
                        ]
          , { note = notes }
          )
        ]


spacerFields : List (ST.Spacer -> Int)
spacerFields =
    [ .px4
    , .px8
    , .px12
    , .px16
    , .px24
    , .px32
    , .px48
    , .px64
    ]


spacerBlock : (ST.Spacer -> Int) -> Element.Element msg
spacerBlock spacerField =
    let
        spacerValue =
            spacerField spacer
    in
    Element.row [ Element.spacing spacer.px16 ]
        [ Element.paragraph
            [ Font.size 14
            , Font.family [ Font.monospace ]
            , Element.width (Element.px 100)
            ]
            [ Element.text "spacer.px"
            , Element.el [ Font.semiBold ] <| Element.text (String.fromInt spacerValue)
            ]
        , Element.el
            [ Element.width (Element.px spacerValue)
            , Element.height (Element.px spacer.px24)
            , Background.color <| SH.toElementColor SH.allColorsPalette.blue.base
            ]
            Element.none
        ]


demoPage : ST.ExoPalette -> Element.Element msg -> Element.Element msg
demoPage palette elementToAdd =
    Element.column [ Element.spacing spacer.px24 ]
        [ Element.column
            [ Element.padding spacer.px16
            , Element.spacing spacer.px24
            , Border.color <| SH.toElementColor palette.neutral.border
            , Border.width 1
            , Background.color <| SH.toElementColor palette.neutral.background.backLayer
            , Element.width (Element.px 350)
            ]
          <|
            [ Text.p [] [ Text.body "In the page: demo text for comparing white space (and hence alignment) of the following column of text elements." ]
            , elementToAdd
            ]
        ]


methodStatus : ST.ExoPalette -> Bool -> String -> Element.Element msg
methodStatus palette isCorrect text =
    let
        ( icon, iconColor ) =
            if isCorrect then
                ( FeatherIcons.check, palette.success.default )

            else
                ( FeatherIcons.x, palette.danger.default )
    in
    Element.row [ Element.spacing spacer.px12 ]
        [ icon
            |> FeatherIcons.withSize 32
            |> FeatherIcons.toHtml []
            |> Element.html
            |> Element.el [ Font.color <| SH.toElementColor iconColor ]
        , Text.p [ Font.size 14 ] [ Element.text text ]
        ]
