module DesignSystem.Stories.Text exposing (stories)

import DesignSystem.Helpers exposing (Plugins, Renderer, ThemeModel, palettize)
import Element
import Element.Font as Font
import Element.Region
import FeatherIcons
import Style.Widgets.Link as Link
import Style.Widgets.Text as Text
import UIExplorer
    exposing
        ( storiesOf
        )


{-| Creates stores for UIExplorer.

    renderer – An elm-ui to html converter
    palette  – Takes a UIExplorer.Model and produces an ExoPalette
    plugins  – UIExplorer plugins (can be empty {})

-}
stories : Renderer msg -> UIExplorer.UI (ThemeModel model) msg Plugins
stories renderer =
    storiesOf
        "Text"
        [ ( "unstyled", \m -> renderer (palettize m) <| Element.text "This is text rendered using `Element.text` and no styling. It will inherit attributes from the document layout.", { note = note } )
        , ( "p"
          , \m ->
                renderer (palettize m) <|
                    Text.p [ Font.justify ]
                        [ Text.body veryLongCopy
                        , Text.body "[ref. "
                        , Link.externalLink (palettize m) "https://www.lipsum.com/" "www.lipsum.com"
                        , Text.body "]"
                        ]
          , { note = note }
          )
        , ( "bold", \m -> renderer (palettize m) <| Text.p [] [ Text.body "Logged in as ", Text.strong "@Jimmy:3421", Text.body "." ], { note = note } )
        , ( "mono", \m -> renderer (palettize m) <| Text.p [] [ Text.body "Your IP address is ", Text.mono "192.168.1.1", Text.body "." ], { note = note } )
        , ( "underline"
          , \m ->
                renderer (palettize m) <|
                    Text.p []
                        [ Text.body "Exosphere is a "
                        , Text.underline "user-friendly"
                        , Text.body ", extensible client for cloud computing."
                        ]
          , { note = note }
          )
        , ( "heading"
          , \m ->
                renderer (palettize m) <|
                    Text.heading (palettize m)
                        []
                        (FeatherIcons.helpCircle
                            |> FeatherIcons.toHtml []
                            |> Element.html
                            |> Element.el []
                        )
                        "Get Support"
          , { note = note }
          )
        , ( "subheading"
          , \m ->
                renderer (palettize m) <|
                    Text.subheading (palettize m)
                        []
                        (FeatherIcons.hardDrive
                            |> FeatherIcons.toHtml []
                            |> Element.html
                            |> Element.el []
                        )
                        "Volumes"
          , { note = note }
          )
        , ( "h1"
          , \m ->
                renderer (palettize m) <|
                    Text.text Text.H1
                        [ Element.Region.heading 1 ]
                        "App Config Info"
          , { note = note }
          )
        , ( "h2"
          , \m ->
                renderer (palettize m) <|
                    Text.text Text.H2
                        [ Element.Region.heading 2 ]
                        "App Config Info"
          , { note = note }
          )
        , ( "h3"
          , \m ->
                renderer (palettize m) <|
                    Text.text Text.H3
                        [ Element.Region.heading 3 ]
                        "App Config Info"
          , { note = note }
          )
        , ( "h4", \m -> renderer (palettize m) <| Text.text Text.H4 [ Element.Region.heading 4 ] "App Config Info", { note = note } )
        ]


note : String
note =
    """
## Usage

Text widgets use `elm-ui` under the hood, particularly [Element.text](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element#text).

Where possible, use or extend `Text` rather than resorting to `Element.text` or custom styling with `Font` as this helps to ensure:

- Consistent typography, font sizing, etc.
- Centralised, predictable refactoring of text styles.

## Typeface

Exosphere's default font is [Open Sans](https://gitlab.com/exosphere/exosphere/-/blob/master/src/Style/Widgets/Text.elm#L102).

It is [self-vendored from `/fonts`](https://gitlab.com/exosphere/exosphere/-/blob/master/fonts/open-sans-400-700.css) to:

- Protect end-user privacy, &
- Provide fast, predictable availability.
    """


veryLongCopy : String
veryLongCopy =
    """
    Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC,
    making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginia, looked up one of the more obscure Latin words,
    consectetur, from a Lorem Ipsum passage, and going through the cites of the word in classical literature, discovered the undoubtable source.
    Lorem Ipsum comes from sections 1.10.32 and 1.10.33 of "de Finibus Bonorum et Malorum" (The Extremes of Good and Evil) by Cicero, written in 45 BC.
    This book is a treatise on the theory of ethics, very popular during the Renaissance.
    """
