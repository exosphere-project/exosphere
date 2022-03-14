module Style.Widgets.DeleteButton exposing (deleteIconButton, deletePopconfirm)

import Element
import FeatherIcons
import Html.Attributes as HtmlA
import Style.Helpers as SH
import Style.Types exposing (ExoPalette)
import Style.Widgets.Button as Button
import Widget


deleteIconButton : ExoPalette -> Bool -> String -> Maybe msg -> Element.Element msg
deleteIconButton palette styleIsPrimary text onPress =
    let
        dangerBtnStyleDefaults =
            if styleIsPrimary then
                (SH.materialStyle palette).dangerButton

            else
                -- secondary style
                (SH.materialStyle palette).dangerButtonSecondary

        deleteBtnStyle =
            { dangerBtnStyleDefaults
                | container =
                    dangerBtnStyleDefaults.container
                        ++ [ Element.htmlAttribute <| HtmlA.title text
                           ]
                , labelRow =
                    dangerBtnStyleDefaults.labelRow
                        ++ [ Element.width Element.shrink
                           , Element.paddingXY 4 0
                           ]
            }
    in
    Widget.iconButton
        deleteBtnStyle
        { icon =
            FeatherIcons.trash2
                |> FeatherIcons.withSize 18
                |> FeatherIcons.toHtml []
                |> Element.html
        , text = text
        , onPress = onPress
        }


deletePopconfirm :
    ExoPalette
    ->
        { confirmationText : String
        , onConfirm : Maybe msg
        , onCancel : Maybe msg
        }
    -> Element.Element msg
deletePopconfirm palette { confirmationText, onConfirm, onCancel } =
    Element.el [ Element.paddingXY 0 6, Element.alignRight ] <|
        Element.column
            (SH.popoverStyleDefaults palette
                ++ [ Element.padding 16, Element.spacing 16 ]
            )
            [ Element.row [ Element.spacing 8 ]
                [ FeatherIcons.alertCircle
                    |> FeatherIcons.withSize 20
                    |> FeatherIcons.toHtml []
                    |> Element.html
                    |> Element.el []
                , Element.text confirmationText
                ]
            , Element.row [ Element.spacing 10, Element.alignRight ]
                [ Button.default
                    palette
                    { text = "Cancel"
                    , onPress = onCancel
                    }
                , Button.button Button.Danger
                    palette
                    { text = "Delete"
                    , onPress = onConfirm
                    }
                ]
            ]
