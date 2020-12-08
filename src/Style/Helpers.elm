module Style.Helpers exposing
    ( materialStyle
    , toElementColor
    , toElementColorWithOpacity
    , toMaterialPalette
    )

import Color
import Element
import Style.Types exposing (ExoPalette, Style)
import Widget.Style.Material as Material


materialStyle : ExoPalette -> Style {} msg
materialStyle exoPalette =
    let
        palette =
            toMaterialPalette exoPalette
    in
    { textInput = Material.textInput palette
    , column = Material.column
    , cardColumn = Material.cardColumn palette
    , primaryButton = Material.containedButton palette
    , button = Material.outlinedButton palette
    , chipButton = Material.chip palette
    , row = Material.row
    , progressIndicator = Material.progressIndicator palette
    }


toMaterialPalette : ExoPalette -> Material.Palette
toMaterialPalette exoPalette =
    { primary = exoPalette.primary
    , secondary = exoPalette.secondary
    , background = exoPalette.background
    , surface = exoPalette.surface
    , error = exoPalette.error
    , on =
        { primary = exoPalette.on.primary
        , secondary = exoPalette.on.secondary
        , background = exoPalette.on.background
        , surface = exoPalette.on.surface
        , error = exoPalette.on.error
        }
    }


toElementColor : Color.Color -> Element.Color
toElementColor color =
    -- https://github.com/mdgriffith/elm-ui/issues/28#issuecomment-566337247
    let
        { red, green, blue, alpha } =
            Color.toRgba color
    in
    Element.rgba red green blue alpha


toElementColorWithOpacity : Color.Color -> Float -> Element.Color
toElementColorWithOpacity color alpha =
    -- https://github.com/mdgriffith/elm-ui/issues/28#issuecomment-566337247
    let
        { red, green, blue } =
            Color.toRgba color
    in
    Element.rgba red green blue alpha
