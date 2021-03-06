module Style.Widgets.CopyableText exposing (copyableText)

import Element exposing (Element)
import Element.Input as Input
import Html
import Html.Attributes
import Murmur3
import Style.Helpers as SH
import Style.Types exposing (ExoPalette)
import Style.Widgets.Icon exposing (copyToClipboard)


{-| Display text with a button to copy to the clipboard. Requires you to do a Ports.InstantiateClipboardJS

    copyableText (Element.text "foobar")

-}
copyableText : ExoPalette -> List (Element.Attribute msg) -> String -> Element msg
copyableText palette textAttributes text =
    Element.row
        [ Element.spacing 8, Element.width Element.fill ]
        [ Element.paragraph textAttributes <|
            [ Element.html <|
                Html.div [ Html.Attributes.id ("copy-me-" ++ hash text) ] <|
                    [ Html.text text ]
            ]
        , Element.el [] Element.none -- To preserve spacing
        , Input.button
            [ Element.htmlAttribute <| Html.Attributes.class "copy-button"
            , Element.htmlAttribute <| Html.Attributes.attribute "data-clipboard-target" ("#copy-me-" ++ hash text)
            , Element.inFront <|
                Element.row
                    [ Element.transparent True
                    , Element.mouseOver [ Element.transparent False ]
                    , Element.spacing 10
                    ]
                <|
                    [ copyToClipboard (SH.toElementColor palette.primary) 18
                    , Element.text "Copy"
                    ]
            ]
            { onPress = Nothing
            , label =
                Element.el
                    [ Element.mouseOver [ Element.transparent True ] ]
                    (copyToClipboard (SH.toElementColor palette.on.background) 18)
            }
        ]



-- TODO copyableTextarea


hash : String -> String
hash str =
    Murmur3.hashString 1234 str
        |> String.fromInt
