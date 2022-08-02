module DesignSystem.Stories.Link exposing (stories)

import DesignSystem.Helpers exposing (Plugins, Renderer, ThemeModel, palettize)
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
        "Link"
        [ ( "internal"
          , \m ->
                renderer (palettize m) <|
                    Text.p []
                        [ Text.body "Follow this link to the "
                        , Link.link
                            (palettize m)
                            "/#Atoms/Text/underline"
                            "underlined text"
                        , Text.body " widget."
                        ]
          , { note = note }
          )
        , ( "external"
          , \m ->
                renderer (palettize m) <|
                    Text.p []
                        [ Text.body "Exosphere is a user-friendly, extensible client for cloud computing. Check out our "
                        , Link.externalLink
                            (palettize m)
                            "https://gitlab.com/exosphere/exosphere/blob/master/README.md"
                            "README on GitLab"
                        , Text.body "."
                        ]
          , { note = note }
          )
        ]


note : String
note =
    """
## Usage

### Internal Link

Use `Style.Widgets.Link.link` by default when navigating within the app. It opens links in the current window.

### External Link

Use `Style.Widgets.Link.externalLink` for navigating outside of the app, or to deliberately open a new browser tab.

    """
