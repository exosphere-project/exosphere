module Style.Widgets.IconButton exposing (FlowOrder(..), clickableIcon, goToButton, navIconButton, notes)

import Element exposing (Element)
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import FeatherIcons as Icons exposing (withSize)
import Html.Attributes exposing (attribute, style)
import Style.Helpers as SH
import Style.Types exposing (ExoPalette)
import Style.Widgets.Icon exposing (Icon, featherIcon, iconEl, sizedFeatherIcon)
import Style.Widgets.Spacer exposing (spacer)


notes : String
notes =
    """
## Usage

Icon buttons are are clickable elements which communicate their function through an icon.

### Icon Button

An elm-ui `Widget.iconButton` with an icon & label using `SH.materialStyle palette` button styles.

### Go To Button

A button with a chevron that indicates a navigate to detail action.

### Clickable Icon

A clickable [FeatherIcons](https://package.elm-lang.org/packages/1602/elm-feather/latest/FeatherIcons) icon with an accessibility label, & hover & disabled states.

### Nav Icon Button

Used from the header bar for top-level navigation.

At the moment, the icon uses `palette.menu.textOrIcon` for color, if needed that can be moved into the configuration.
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


clickableIcon : List (Element.Attribute msg) -> { icon : Icons.Icon, accessibilityLabel : String, onClick : Maybe msg, color : Element.Color, hoverColor : Element.Color } -> Element.Element msg
clickableIcon attributes { icon, accessibilityLabel, onClick, color, hoverColor } =
    featherIcon
        ([ Element.paddingXY spacer.px4 0
         , Font.color color
         , Element.mouseOver
            [ -- darken the icon color
              Font.color hoverColor
            ]
         , Element.htmlAttribute (attribute "aria-label" accessibilityLabel)
         , Element.htmlAttribute (attribute "role" "button")
         ]
            ++ (case onClick of
                    Just msg ->
                        [ Element.pointer
                        , Events.onClick msg
                        ]

                    Nothing ->
                        [ Element.htmlAttribute (style "cursor" "not-allowed")
                        , Element.alpha 0.6
                        ]
               )
            ++ attributes
        )
        (icon |> withSize 20)


navIconButton : ExoPalette -> List (Element.Attribute msg) -> { icon : Icon, iconPlacement : FlowOrder, label : String, onClick : Maybe msg } -> Element.Element msg
navIconButton palette attributes { icon, iconPlacement, label, onClick } =
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
