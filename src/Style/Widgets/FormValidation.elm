module Style.Widgets.FormValidation exposing (renderValidationError)

import Element
import Element.Font as Font
import FeatherIcons
import Style.Helpers as SH
import View.Types


renderValidationError : View.Types.Context -> String -> Element.Element a
renderValidationError context msg =
    Element.row
        [ Element.spacing 5
        , Font.color <| SH.toElementColor context.palette.error
        ]
        [ Element.el
            []
            (FeatherIcons.alertCircle
                |> FeatherIcons.toHtml []
                |> Element.html
            )
        , Element.text msg
        ]
