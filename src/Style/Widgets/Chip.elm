module Style.Widgets.Chip exposing (chip)

import Element
import Element.Border as Border
import Element.Font as Font
import FeatherIcons
import Style.Helpers as SH
import Style.Types as ST
import View.Helpers exposing (edges)
import Widget


chip : ST.ExoPalette -> List (Element.Attribute msg) -> Element.Element msg -> Maybe msg -> Element.Element msg
chip palette styleAttrs chipContent onClose =
    Element.row
        ([ Border.width 1
         , Border.color <|
            -- opacity is used to match it with containedButton's border color i.e. determined by elm-ui-widget and non-customizable
            SH.toElementColorWithOpacity palette.neutral.border 0.8
         , Border.rounded 4
         , Element.paddingEach { edges | left = 6 }
         , Font.size 14
         ]
            -- to let consumer add/override the chip style
            ++ styleAttrs
        )
        [ chipContent
        , Widget.iconButton (SH.materialStyle palette).iconButton
            { text = "Close"
            , icon =
                Element.el []
                    (FeatherIcons.x
                        |> FeatherIcons.withSize 16
                        |> FeatherIcons.toHtml []
                        |> Element.html
                    )
            , onPress = onClose
            }
        ]
