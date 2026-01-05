module Style.Widgets.Code exposing (codeAttrs, codeBlock, codeSpan, copyableCodeSpan, notes)

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Style.Helpers as SH
import Style.Types exposing (ExoPalette)
import Style.Widgets.CopyableText exposing (copyableTextAccessory)
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text


notes : String
notes =
    """
## Usage

### Code Span

A **code span** is a small piece of code that is used inline with text.

### Copyable Code Span

A **copyable code span** is a code span with a copy button.

(Uses [clipboard.js](https://clipboardjs.com/) under the hood & relies on `Ports.instantiateClipboardJs` for initialization.)

### Code Block

A **code block** contains 1 or more lines of read-only code, & can be copied to the clipboard.

(Uses [clipboard.js](https://clipboardjs.com/) under the hood & relies on `Ports.instantiateClipboardJs` for initialization.)

### Alternatives

Use [Copyable Text](#Molecules/Copyable%20Text/default) for regular copyable text or labels.

Use [Copyable Script](#Molecules/Copyable%20Text/copyable%20scripts) when you need an uneditable text area with a copy button.
"""


codeAttrs : ExoPalette -> List (Element.Attribute msg)
codeAttrs palette =
    [ Text.fontFamily Text.Mono
    , Border.rounded spacer.px4
    , Border.color (SH.toElementColor palette.muted.border)
    , Background.color (SH.toElementColor palette.muted.background)
    , Font.color (SH.toElementColor palette.muted.textOnColoredBG)
    ]


codeSpanWithAttrs : ExoPalette -> List (Element.Attribute msg) -> String -> Element.Element msg
codeSpanWithAttrs palette attrs =
    Text.text Text.Body
        (Element.paddingXY spacer.px4 spacer.px4
            :: codeAttrs palette
            ++ attrs
        )


codeSpan : ExoPalette -> String -> Element.Element msg
codeSpan palette =
    codeSpanWithAttrs palette []


copyableCodeSpan : ExoPalette -> String -> Element.Element msg
copyableCodeSpan palette text =
    let
        copyable =
            copyableTextAccessory palette text
    in
    Element.row
        [ Element.spacing spacer.px8, Element.width Element.fill ]
        [ codeSpanWithAttrs palette [ copyable.id ] text
        , copyable.accessory
        ]


codeBlock : ExoPalette -> String -> Element.Element msg
codeBlock palette body =
    let
        copyableAccessory =
            copyableTextAccessory palette body
    in
    Element.row
        (codeAttrs palette
            ++ [ Element.paddingXY spacer.px8 spacer.px8
               , Element.width Element.fill

               -- Copy button
               , Element.inFront <|
                    Element.el
                        [ Element.alignRight
                        , Element.moveLeft <| toFloat spacer.px4
                        , Element.moveDown <| toFloat spacer.px4
                        ]
                        copyableAccessory.accessory
               , copyableAccessory.id
               ]
        )
        [ codeSpan palette body ]
