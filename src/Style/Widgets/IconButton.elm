module Style.Widgets.IconButton exposing (chip, iconButton)

import Element exposing (Element)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Framework.Button
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
        , Border.color <| Element.rgb255 54 54 54
        , Border.rounded 3
        ]
        [ label
        , Input.button
            [ Element.mouseOver
                [ Border.color <| Element.rgb255 10 10 10
                ]
            ]
            { onPress = onPress
            , label = timesCircle (Element.rgb255 255 255 255) 12
            }
        ]
