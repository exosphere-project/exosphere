module Style.Widgets.IconButton exposing (FlowOrder(..), clickableIcon, navIconButton, notes)

import Element
import Element.Font as Font
import Element.Input as Input
import FeatherIcons as Icons exposing (withSize)
import Html.Attributes exposing (attribute, style)
import Style.Types exposing (ExoPalette)
import Style.Widgets.Icon exposing (Icon, featherIcon, iconEl)
import Style.Widgets.Spacer exposing (spacer)


notes : String
notes =
    """
## Usage

Icon buttons are are clickable elements which communicate their function through an icon.

### Icon Button

An elm-ui `Widget.iconButton` with an icon & label using `SH.materialStyle palette` button styles.

### Clickable Icon

A clickable [FeatherIcons](https://package.elm-lang.org/packages/1602/elm-feather/latest/FeatherIcons) icon with an accessibility label, & hover & disabled states.

### Nav Icon Button

Used from the header bar for top-level navigation.

At the moment, the icon uses `palette.menu.textOrIcon` for color, if needed that can be moved into the configuration.
"""


type FlowOrder
    = Before
    | After


clickableIcon : List (Element.Attribute msg) -> { icon : Icons.Icon, accessibilityLabel : String, onClick : Maybe msg, color : Element.Color, hoverColor : Element.Color } -> Element.Element msg
clickableIcon attributes { icon, accessibilityLabel, onClick, color, hoverColor } =
    Input.button []
        { onPress = onClick
        , label =
            featherIcon
                ([ Element.paddingXY spacer.px4 0
                 , Font.color color
                 , Element.mouseOver
                    [ -- darken the icon color
                      Font.color hoverColor
                    ]
                 , Element.htmlAttribute (attribute "aria-label" accessibilityLabel)
                 , Element.htmlAttribute (attribute "title" accessibilityLabel)
                 , Element.htmlAttribute (attribute "role" "button")
                 ]
                    ++ (if onClick == Nothing then
                            [ Element.htmlAttribute (style "cursor" "not-allowed")
                            , Element.htmlAttribute (attribute "aria-disabled" "true")
                            , Element.alpha 0.6
                            ]

                        else
                            []
                       )
                    ++ attributes
                )
                (icon |> withSize 22)
        }


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
