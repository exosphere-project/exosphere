module Style.Widgets.Validation exposing (invalidMessage, warningMessage)

import Element
import Element.Font as Font
import FeatherIcons
import Style.Helpers as SH exposing (spacer)
import Style.Types exposing (ExoPalette)



--- components


invalidMessage : ExoPalette -> String -> Element.Element msg
invalidMessage palette helperText =
    Element.row [ Element.spacingXY spacer.px8 0 ]
        [ Element.el
            [ Font.color (palette.danger.textOnNeutralBG |> SH.toElementColor)
            ]
            (FeatherIcons.alertCircle
                |> FeatherIcons.toHtml []
                |> Element.html
            )
        , -- let text wrap if it exceeds container's width
          Element.paragraph
            [ Font.color (SH.toElementColor palette.danger.textOnNeutralBG)
            , Font.size 16
            ]
            [ Element.text helperText ]
        ]


warningMessage : ExoPalette -> String -> Element.Element msg
warningMessage palette helperText =
    Element.row [ Element.spacingXY spacer.px8 0 ]
        [ Element.el
            [ Font.color (palette.warning.textOnNeutralBG |> SH.toElementColor)
            ]
            (FeatherIcons.alertTriangle
                |> FeatherIcons.toHtml []
                |> Element.html
            )
        , Element.el
            [ Font.color (SH.toElementColor palette.warning.textOnNeutralBG)
            , Font.size 16
            ]
            (Element.text helperText)
        ]
