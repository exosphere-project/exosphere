module Style.Widgets.Button exposing (dangerButton, warningButton)

import Style.Helpers as SH
import Style.Types
import Widget.Style exposing (ButtonStyle)
import Widget.Style.Material exposing (containedButton)


warningButton : Style.Types.ExoPalette -> ButtonStyle msg
warningButton palette =
    -- A slight modification of the containedButton from `src/Widget/Style/Material.elm`, swapping in a "warning" color instead of using the primary color.
    let
        materialPalette =
            SH.toMaterialPalette palette

        warningPalette =
            { materialPalette | primary = palette.warn }
    in
    containedButton warningPalette


dangerButton : Style.Types.ExoPalette -> ButtonStyle msg
dangerButton palette =
    -- A slight modification of the containedButton from `src/Widget/Style/Material.elm`, swapping in the palette's danger color instead of using the primary color.
    let
        materialPalette =
            SH.toMaterialPalette palette

        dangerPalette =
            { materialPalette | primary = palette.error }
    in
    containedButton dangerPalette
