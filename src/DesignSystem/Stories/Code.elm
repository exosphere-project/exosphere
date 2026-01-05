module DesignSystem.Stories.Code exposing (stories)

import DesignSystem.Helpers exposing (Plugins, Renderer, ThemeModel, palettize)
import Style.Widgets.Code exposing (codeBlock, codeSpan, copyableCodeSpan, notes)
import UIExplorer
    exposing
        ( storiesOf
        )


stories :
    Renderer msg
    -> UIExplorer.UI (ThemeModel model) msg Plugins
stories renderer =
    storiesOf
        "Code"
        [ ( "code span"
          , \m ->
                renderer (palettize m) <|
                    codeSpan
                        (palettize m)
                        "/media/share/my-drive"
          , { note = notes }
          )
        , ( "copyable code span"
          , \m ->
                renderer (palettize m) <|
                    copyableCodeSpan
                        (palettize m)
                        "openstack --os-cloud=ABC123123"
          , { note = notes }
          )
        , ( "code block"
          , \m ->
                renderer (palettize m) <|
                    codeBlock (palettize m) """Element.column [ Element.spacing spacer.px16, Element.width Element.fill ] <|
    VH.renderMarkdown
        palette
        markdown
"""
          , { note = notes }
          )
        ]
