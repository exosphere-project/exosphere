module Style.Widgets.Button exposing (ThemedButton, Variant(..), button, default, primary)

import Element
import Html.Attributes exposing (attribute)
import Style.Helpers as SH
import Style.Types
import Widget exposing (textButton)



--- model


type Variant
    = Primary
    | Secondary
    | Text
    | Danger
    | DangerSecondary
    | Warning


type alias ThemedButton textButton msg =
    Style.Types.ExoPalette
    ->
        { textButton
            | onPress : Maybe msg
            , text : String
        }
    -> Element.Element msg



--- component


primary : ThemedButton textButton msg
primary =
    button Primary


default : ThemedButton textButton msg
default =
    button Secondary


button : Variant -> ThemedButton textButton msg
button variant palette params =
    let
        style =
            case variant of
                Primary ->
                    (SH.materialStyle palette).primaryButton

                Secondary ->
                    (SH.materialStyle palette).button

                Text ->
                    (SH.materialStyle palette).textButton

                Danger ->
                    (SH.materialStyle palette).dangerButton

                DangerSecondary ->
                    (SH.materialStyle palette).dangerButtonSecondary

                Warning ->
                    (SH.materialStyle palette).warningButton

        patchedStyle =
            -- The `Input.button` from elm-ui does not have a valid, accessible disabled state.
            { style | ifDisabled = style.ifDisabled ++ [ Element.htmlAttribute (attribute "aria-disabled" "true") ] }
    in
    textButton
        patchedStyle
        params
