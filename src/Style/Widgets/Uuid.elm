module Style.Widgets.Uuid exposing (copyableUuid, notes, uuidLabel)

import Element
import Element.Font as Font
import Style.Helpers as SH
import Style.Types exposing (ExoPalette)
import Style.Widgets.CopyableText exposing (copyableText)
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text


notes : String
notes =
    """
## Usage

For consistent display of UUIDs:

- Use `uuidLabel` for non-interactive informational purposes (e.g. on list item rows) &
- Use `copyableUuid` for larger copyable UUIDs in contexts like detail pages.
"""


copyableUuid : ExoPalette -> String -> Element.Element msg
copyableUuid palette uuid =
    Element.el
        [ Text.fontSize Text.Small
        , Font.color (SH.toElementColor palette.neutral.text.subdued)
        , Element.paddingXY spacer.px12 0
        , Text.fontFamily Text.Mono
        ]
        (copyableText palette
            [ Element.width (Element.shrink |> Element.minimum 305), Element.alignBottom ]
            uuid
        )


uuidLabel : ExoPalette -> String -> Element.Element msg
uuidLabel palette uuid =
    Text.text Text.Tiny
        [ Font.color (SH.toElementColor palette.neutral.text.subdued)
        , Text.fontFamily Text.Mono
        ]
        uuid
