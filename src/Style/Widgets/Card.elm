module Style.Widgets.Card exposing
    ( clickableCardFixedSize
    , exoCard
    , notes
    )

import Element exposing (Element)
import Style.Helpers as SH
import Style.Types
import Widget


notes : String
notes =
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


exoCard : Style.Types.ExoPalette -> Element msg -> Element msg
exoCard palette content =
    -- Disabling mouseover styles because entire card is not clickable
    let
        baseAttribs =
            (SH.materialStyle palette).cardColumn

        attribs =
            { baseAttribs
                | containerColumn =
                    List.append baseAttribs.containerColumn
                        [ Element.mouseOver [] ]
            }
    in
    Widget.column
        attribs
        [ content ]


clickableCardFixedSize : Style.Types.ExoPalette -> Int -> Int -> List (Element msg) -> Element msg
clickableCardFixedSize palette width height content =
    let
        baseAttribs =
            (SH.materialStyle palette).cardColumn

        widthHeightAttribs =
            [ Element.width (Element.px width)
            , Element.height (Element.px height)
            ]

        attribs =
            { baseAttribs
                | containerColumn =
                    List.append baseAttribs.containerColumn
                        widthHeightAttribs
                , element =
                    List.append baseAttribs.element
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        ]
            }
    in
    Widget.column
        attribs
        content
