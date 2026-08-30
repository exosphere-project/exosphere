module Style.Widgets.CopyableText exposing
    ( copyButton
    , copyTextAttributes
    , copyableScript
    , copyableScriptMasked
    , copyableText
    , copyableTextAccessory
    , notes
    )

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Html
import Html.Attributes
import Murmur3
import Style.Helpers as SH
import Style.Types exposing (ExoPalette)
import Style.Widgets.Icon exposing (featherIcon)
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text


notes : String
notes =
    """
## Usage

Shows stylable text with an accessory button for copying the text content to the user's clipboard.

It uses [clipboard.js](https://clipboardjs.com/) under the hood & relies on a port (`Ports.instantiateClipboardJs`) for initialization.

`copyButton` is the compact variant for row-action clusters, where a labelled icon has to sit next to other icons.
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


{-| Create a copyable script block

    copyableScript context.palette "some \\\n  preformatted \\\n  script"

-}
copyableScript : ExoPalette -> String -> Element.Element msg
copyableScript palette script =
    copyableScriptMasked palette { display = script, clipboard = script }


{-| A copyable script block whose displayed text can differ from what the copy button puts on the
clipboard, so a secret can be masked on screen while a client still gets a working snippet.

This is the implementation behind `copyableScript`, which is the same block with nothing masked.

-}
copyableScriptMasked : ExoPalette -> { display : String, clipboard : String } -> Element.Element msg
copyableScriptMasked palette { display, clipboard } =
    Element.el
        ([ Element.inFront <|
            Element.el
                [ Element.alignRight
                , Element.moveLeft <| toFloat spacer.px4
                , Element.moveDown <| toFloat spacer.px4
                ]
                (copyAccessory palette (copyTextAttributes clipboard))
         , Element.width Element.fill
         , Border.solid
         , Border.width 1
         , Border.rounded 3
         , Element.padding spacer.px8
         , Text.fontFamily Text.Mono
         , Background.color <| SH.toElementColor palette.neutral.background.frontLayer
         , Border.color <| SH.toElementColor palette.neutral.border
         ]
            ++ Text.typographyAttrs Text.Small
        )
    <|
        Element.html <|
            Html.pre
                [ Html.Attributes.style "margin" "0"
                , Html.Attributes.style "white-space" "pre-wrap"
                , Html.Attributes.style "word-wrap" "anywhere"
                ]
                [ Html.text display ]


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
    { id = Html.Attributes.id ("copy-me-" ++ hash text) |> Element.htmlAttribute
    , accessory = copyAccessory palette (copyTargetAttributes text)
    }


{-| A compact copy button: a labelled icon that puts a literal string on the clipboard when clicked.

Use it where the accessory above is too heavy, for example in a row of action icons, or beside a
value that already reads as one line. The caller picks the icon so a link copy and a text copy can
look different in the same cluster.

    copyButton context.palette
        { icon = FeatherIcons.clipboard
        , accessibilityLabel = "Copy name"
        , textToCopy = container.name
        }

-}
copyButton : ExoPalette -> { icon : FeatherIcons.Icon, accessibilityLabel : String, textToCopy : String } -> Element msg
copyButton palette { icon, accessibilityLabel, textToCopy } =
    Input.button
        (copyTextAttributes textToCopy ++ [ Element.pointer ])
        { onPress = Nothing
        , label =
            featherIcon
                [ Element.paddingXY spacer.px4 0
                , Font.color (SH.toElementColor palette.neutral.icon)
                , Element.mouseOver [ Font.color (SH.toElementColor palette.primary) ]
                , Element.htmlAttribute (Html.Attributes.attribute "aria-label" accessibilityLabel)
                , Element.htmlAttribute (Html.Attributes.attribute "title" accessibilityLabel)
                , Element.htmlAttribute (Html.Attributes.attribute "role" "button")
                ]
                (icon |> FeatherIcons.withSize 18)
        }


{-| The clipboard.js wiring for copying a literal string, as element attributes.

Put these on anything that should copy `textToCopy` when clicked, for example a text button in a
dropdown menu, where neither the accessory nor `copyButton` fits. Requires a
`Ports.instantiateClipboardJs`.

-}
copyTextAttributes : String -> List (Element.Attribute msg)
copyTextAttributes textToCopy =
    [ Element.htmlAttribute <| Html.Attributes.class "copy-button"
    , Element.htmlAttribute <| Html.Attributes.attribute "data-clipboard-text" textToCopy
    ]


{-| The clipboard.js wiring for copying whatever text is inside the element carrying the matching
`copyableTextAccessory` id.
-}
copyTargetAttributes : String -> List (Element.Attribute msg)
copyTargetAttributes text =
    [ Element.htmlAttribute <| Html.Attributes.class "copy-button"
    , Element.htmlAttribute <| Html.Attributes.attribute "data-clipboard-target" ("#copy-me-" ++ hash text)
    ]


{-| The copy affordance itself: a clipboard icon that reveals a "Copy" label on hover. The caller
supplies the clipboard.js attributes, which is the only thing that differs between copying a literal
string and copying the contents of another element.
-}
copyAccessory : ExoPalette -> List (Element.Attribute msg) -> Element msg
copyAccessory palette clipboardAttributes =
    Input.button
        (clipboardAttributes
            ++ [ Element.alignTop
               , Element.pointer
               , Element.inFront <|
                    Element.row
                        [ Element.transparent True
                        , Element.mouseOver [ Element.transparent False ]
                        , Element.spacing spacer.px8
                        ]
                        [ clipboardIcon (SH.toElementColor palette.primary)
                        , Text.text Text.Small [] "Copy"
                        ]
               ]
        )
        { onPress = Nothing
        , label =
            Element.el
                [ Element.mouseOver [ Element.transparent True ] ]
                (clipboardIcon (SH.toElementColor palette.neutral.icon))
        }


clipboardIcon : Element.Color -> Element msg
clipboardIcon color =
    featherIcon [ Font.color color ] (FeatherIcons.clipboard |> FeatherIcons.withSize 18)


hash : String -> String
hash str =
    Murmur3.hashString 1234 str
        |> String.fromInt
