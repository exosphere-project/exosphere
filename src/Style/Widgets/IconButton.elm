module Style.Widgets.IconButton exposing (chip, iconButton)

import Color
import Element exposing (Element)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Framework.Button
import Framework.Color
import Framework.Modifier exposing (Modifier(..))
import Style.Widgets.Icon exposing (timesCircle)


{-| Generate an Input.button element with an icon and no text label

    iconButton [ Medium, Success, Outlined ] Nothing (edit black 32)

-}
iconButton : List Modifier -> Maybe msg -> Element msg -> Element msg
iconButton modifiers onPress icon =
    Input.button
        (Framework.Button.buttonAttr modifiers)
        { onPress = onPress
        , label = icon
        }


chip : Maybe msg -> Element msg -> Element msg
chip onPress label =
    Element.row
        [ Element.spacing 8
        , Element.padding 8
        , Border.width 1
        , Font.size 12
        , Border.color <| Color.toElementColor Framework.Color.grey_darker
        , Border.rounded 3
        ]
        [ label
        , Input.button
            [ Element.mouseOver
                [ Border.color <| Color.toElementColor Color.black
                ]
            ]
            { onPress = onPress
            , label = timesCircle Color.white 12
            }
        ]
