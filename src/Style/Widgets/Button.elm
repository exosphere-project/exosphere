module Style.Widgets.Button exposing (dangerButton, warningButton)

import Color exposing (Color)
import Widget.Style exposing (ButtonStyle)
import Widget.Style.Material exposing (Palette, containedButton)


warningButton : Palette -> Color -> ButtonStyle msg
warningButton palette warningColor =
    -- A slight modification of the containedButton from `src/Widget/Style/Material.elm`, swapping in a "warning" color instead of using the primary color.
    let
        warningPalette =
            { palette | primary = warningColor }
    in
    containedButton warningPalette


dangerButton : Palette -> ButtonStyle msg
dangerButton palette =
    -- A slight modification of the containedButton from `src/Widget/Style/Material.elm`, swapping in the palette's danger color instead of using the primary color.
    let
        dangerPalette =
            { palette | primary = palette.error }
    in
    containedButton dangerPalette
