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
            [ Html.Attributes.style "position" "fixed"
            , Html.Attributes.style "top" "60"
            , Html.Attributes.style "right" "0"
            , Html.Attributes.style "width" "100%"
            , Html.Attributes.style "max-width" "300px"
            , Html.Attributes.style "list-style-type" "none"
            , Html.Attributes.style "padding" "0"
            , Html.Attributes.style "margin" "0"
            ]
    in
    Toasty.Defaults.config
        |> Toasty.delay 60000
        |> Toasty.containerAttrs containerAttrs
