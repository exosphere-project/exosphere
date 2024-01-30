module DesignSystem.Stories.Markdown exposing (stories)

import DesignSystem.Helpers exposing (Plugins, Renderer, ThemeModel, palettize)
import Element
import Style.Widgets.Spacer exposing (spacer)
import UIExplorer
    exposing
        ( storiesOf
        )
import View.Helpers as VH


markdown : String
markdown =
    """
## Usage

Takes a _Github-flavored_ markdown string & renders it using the app's **widgets & theme**.

It uses [elm-markdown](https://package.elm-lang.org/packages/dillonkearns/elm-markdown/latest/) under the hood.

This example is written in markdown so you can see how it renders.

### Code Spans

This is a UUID: `632033bd-9121-49fd-a064-f1d5eedb024f`.

### Code Blocks

```elm
Element.column [ Element.spacing spacer.px16, Element.width Element.fill ] <|
    VH.renderMarkdown
        palette
        markdown
```

### Block Quote

> Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer posuere erat a ante.

### Lists

#### Unordered Lists

- Item 1
- Item 2
- Item 3

#### Ordered Lists

1. Number 1
2. Number 2
3. Number 3

### Tables

| Header 1 | Header 2 | Header 3 |
| -------- | -------- | -------- |
| Cell 1   | Cell 2   | Cell 3   |
| Cell 4   | Cell 5   | Cell 6   |

### Images

![Exosphere Logo](./assets/img/logo-alt.svg)

"""


stories :
    Renderer msg
    -> UIExplorer.UI (ThemeModel model) msg Plugins
stories renderer =
    storiesOf
        "Markdown"
        [ ( "example"
          , \m ->
                renderer (palettize m) <|
                    Element.column [ Element.spacing spacer.px16, Element.width Element.fill ] <|
                        VH.renderMarkdown
                            (palettize m)
                            markdown
          , { note = markdown }
          )
        ]
