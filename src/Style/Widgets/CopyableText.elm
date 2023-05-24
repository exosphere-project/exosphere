module Style.Widgets.CopyableText exposing (copyableText, copyableTextAccessory, notes)

import Element exposing (Element)
import Element.Input as Input
import Html.Attributes
import Murmur3
import Style.Helpers as SH
import Style.Types exposing (ExoPalette)
import Style.Widgets.Icon exposing (copyToClipboard)
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text


notes : String
notes =
    """
## Usage

Shows stylable text with an accessory button for copying the text content to the user's clipboard.

It uses [clipboard.js](https://clipboardjs.com/) under the hood & relies on a port (`Ports.instantiateClipboardJs`) for initialization.
"""


{-| Display text with a button to copy to the clipboard. Requires you to do a `Ports.instantiateClipboardJs`.

    copyableText (Element.text "foobar")

-}
copyableText : ExoPalette -> List (Element.Attribute msg) -> String -> Element msg
copyableText palette textAttributes text =
    let
        copyable =
            copyableTextAccessory palette text
    in
    Element.row
        [ Element.spacing spacer.px8, Element.width Element.fill ]
        [ Element.paragraph (textAttributes ++ [ copyable.id ])
            [ Element.text text ]
        , Element.el [] Element.none -- To preserve spacing
        , copyable.accessory
        ]


{-| Create a copy text accessory & an html attribute id to identify the copyable text.
When text appears inside a custom element, it can be useful to display the copy text accessory button separately.
Clipboard.js requires a unique id to be present on visible text for the copy action to work. Add this id to the widget containing your text.

    let
        copyable =
            copyableTextAccessory palette text
    in
    Element.row
        [ Element.spacing spacer.px8 ]
        [ Input.multiline
            [ copyable.id ]
            { onChange = \_ -> NoOp
            , text = text
            , placeholder = Nothing
            , label = Input.labelHidden "Greeting"
            , spellcheck = False
            }
        , copyable.accessory
        ]

-}
copyableTextAccessory : ExoPalette -> String -> { id : Element.Attribute msg, accessory : Element msg }
copyableTextAccessory palette text =
    { id = Html.Attributes.id ("copy-me-" ++ hash text) |> Element.htmlAttribute, accessory = copyTextButton palette text }


copyTextButton : ExoPalette -> String -> Element msg
copyTextButton palette text =
    Input.button
        [ Element.htmlAttribute <| Html.Attributes.class "copy-button"
        , Element.htmlAttribute <| Html.Attributes.attribute "data-clipboard-target" ("#copy-me-" ++ hash text)
        , Element.alignTop
        , Element.inFront <|
            Element.row
                [ Element.transparent True
                , Element.mouseOver [ Element.transparent False ]
                , Element.spacing spacer.px8
                ]
            <|
                [ copyToClipboard (SH.toElementColor palette.primary) 18
                , Text.text Text.Small [] "Copy"
                ]
        ]
        { onPress = Nothing
        , label =
            Element.el
                [ Element.mouseOver [ Element.transparent True ] ]
                (copyToClipboard (SH.toElementColor palette.neutral.icon) 18)
        }


hash : String -> String
hash str =
    Murmur3.hashString 1234 str
        |> String.fromInt
