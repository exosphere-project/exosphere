module Style.Widgets.Button exposing (Variant(..), button)

import Element
import Style.Helpers as SH
import Style.Types
import Widget exposing (textButton)



--- model


type Variant
    = Primary
    | Secondary
    | Danger
    | DangerSecondary
    | Warning



--- component


button : Variant -> Style.Types.ExoPalette -> { textButton | onPress : Maybe msg, text : String } -> Element.Element msg
button variant palette params =
    let
        style =
            case variant of
                Primary ->
                    (SH.materialStyle palette).primaryButton

                Danger ->
                    (SH.materialStyle palette).dangerButton

                DangerSecondary ->
                    (SH.materialStyle palette).dangerButtonSecondary

                Warning ->
                    (SH.materialStyle palette).warningButton

                _ ->
                    (SH.materialStyle palette).button
    in
    textButton
        style
        params
