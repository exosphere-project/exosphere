module Style.Widgets.IconButton exposing (FlowOrder(..), goToButton, iconButton, notes)

import Element exposing (Element)
import Element.Border as Border
import Element.Input as Input
import FeatherIcons as Icons
import Style.Helpers as SH
import Style.Types exposing (ExoPalette)
import Style.Widgets.Icon exposing (Icon, iconEl, sizedFeatherIcon)
import Style.Widgets.Spacer exposing (spacer)


notes : String
notes =
    {- @nonlocalized -}
    """
## Usage

Icon Buttons are commonly used to show hints with actions, such as a warning icon when deleting an instance

At the moment, the icon uses `palette.menu.textOrIcon` for color, if needed that can be moved into the configuration
"""


type FlowOrder
    = Before
    | After


goToButton : ExoPalette -> Maybe msg -> Element msg
goToButton palette onPress =
    Input.button
        [ Border.width 1
        , Border.rounded 6
        , Border.color (SH.toElementColor palette.neutral.border)
        , Element.padding spacer.px4
        ]
        { onPress = onPress
        , label = sizedFeatherIcon 14 Icons.chevronRight
        }


iconButton : ExoPalette -> List (Element.Attribute msg) -> { icon : Icon, iconPlacement : FlowOrder, label : String, onClick : Maybe msg } -> Element.Element msg
iconButton palette attributes { icon, iconPlacement, label, onClick } =
    let
        labelUI =
            Element.text label

        iconUI =
            iconEl [] icon 20 palette.menu.textOrIcon
    in
    Input.button attributes
        { onPress = onClick
        , label =
            Element.row
                [ Element.padding spacer.px8
                , Element.spacing spacer.px8
                ]
                (case iconPlacement of
                    Before ->
                        [ iconUI, labelUI ]

                    After ->
                        [ labelUI, iconUI ]
                )
        }
