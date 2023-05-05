module Style.Widgets.Chip exposing (chip)

import Element
import Element.Border as Border
import FeatherIcons as Icons
import Style.Helpers as SH
import Style.Types as ST
import Style.Widgets.Icon exposing (sizedFeatherIcon)
import Style.Widgets.Text as Text
import View.Helpers exposing (edges)
import Widget


chip : ST.ExoPalette -> List (Element.Attribute msg) -> Element.Element msg -> Maybe msg -> Element.Element msg
chip palette styleAttrs chipContent onClose =
    let
        chipPadding =
            -- spacer.px4 is too small and spacer.px8 is too big
            6

        defaultIconBtnStyle =
            (SH.materialStyle palette).iconButton

        iconBtnStyle =
            { defaultIconBtnStyle | container = defaultIconBtnStyle.container ++ [ Element.padding chipPadding ] }
    in
    Element.row
        ([ Border.width 1
         , Border.color <|
            -- opacity is used to match it with containedButton's border color i.e. determined by elm-ui-widget and non-customizable
            SH.toElementColorWithOpacity palette.neutral.border 0.8
         , Border.rounded 4
         , Element.paddingEach { edges | left = chipPadding }
         , Text.fontSize Text.Small
         ]
            -- to let consumer add/override the chip style
            ++ styleAttrs
        )
        [ chipContent
        , Widget.iconButton iconBtnStyle
            { text = "Close"
            , icon = sizedFeatherIcon 16 Icons.x
            , onPress = onClose
            }
        ]
