module Style.Widgets.IconButton exposing (iconButton)

import Element exposing (Element, text)
import Element.Input as Input
import Framework.Button
import Framework.Color
import Framework.Modifier exposing (Modifier(..))
import Style.Widgets.Icon as Icon


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
