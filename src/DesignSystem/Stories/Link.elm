module DesignSystem.Stories.Link exposing (stories)

import DesignSystem.Helpers exposing (Plugins, Renderer, ThemeModel, palettize)
import Style.Widgets.Link as Link
import Style.Widgets.Text as Text
import UIExplorer
    exposing
        ( storiesOf
        )


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
          , { note = Link.notes }
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
          , { note = Link.notes }
          )
        ]
