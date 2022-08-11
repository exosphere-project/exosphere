module Style.Toast exposing (toastConfig)

import Html
import Html.Attributes
import Toasty
import Toasty.Defaults


toastConfig : Toasty.Config msg
toastConfig =
    let
        containerAttrs : List (Html.Attribute msg)
        containerAttrs =
            -- copied from Toasty.Defaults.containerAttrs (because it isn't exposed)
            -- with "position" changed from "fixed" to "absolute"
            -- and "top" changed from 0 to -1em
            [ Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "top" "-1em"
            , Html.Attributes.style "right" "0"
            , Html.Attributes.style "width" "100%"
            , Html.Attributes.style "max-width" "300px"
            , Html.Attributes.style "list-style-type" "none"
            , Html.Attributes.style "padding" "0"
            , Html.Attributes.style "margin" "0"
            ]

        itemAttrs =
            -- copied from Toasty.Defaults.itemAttrs (because it isn't exposed)
            -- with margin-right decreased from 1em to 0.5em
            -- and "max-height" increased from 100px to 500 px
            -- (and its transition duration decreased to 0.3s)
            -- since content overflows in 100 px
            [ Html.Attributes.style "margin" "1em 0.5em 0 1em"
            , Html.Attributes.style "max-height" "500px"
            , Html.Attributes.style "transition" "max-height 0.3s, margin-top 0.6s"
            ]
    in
    -- Toasty.Defaults.config uses classes defined in assets/css/toasty.css
    Toasty.Defaults.config
        |> Toasty.delay 60000
        |> Toasty.containerAttrs containerAttrs
        |> Toasty.itemAttrs itemAttrs
