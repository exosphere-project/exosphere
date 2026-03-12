module Style.Widgets.DeleteButton exposing
    ( DeleteButtonState(..)
    , PopconfirmContent
    , deleteIconButton
    , deleteIconButtonWithDisabledHint
    )

import Element
import FeatherIcons as Icons
import Html.Attributes as HtmlA
import Style.Helpers as SH
import Style.Types exposing (ExoPalette)
import Style.Widgets.Button as Button
import Style.Widgets.Icon exposing (sizedFeatherIcon)
import Style.Widgets.Spacer exposing (spacer)
import Widget


type alias PopconfirmContent msg =
    { confirmation : Element.Element msg
    , buttonText : String
    , buttonVariant : Button.Variant
    , onConfirm : Maybe msg
    , onCancel : Maybe msg
    }


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
                           , Element.paddingXY spacer.px4 0
                           ]
            }
    in
    Widget.iconButton
        deleteBtnStyle
        { icon = sizedFeatherIcon 18 Icons.trash2
        , text = text
        , onPress = onPress
        }


type DeleteButtonState
    = Enabled String
    | Disabled String


deleteIconButtonWithDisabledHint : ExoPalette -> Bool -> DeleteButtonState -> Maybe msg -> Element.Element msg
deleteIconButtonWithDisabledHint palette styleIsPrimary enabledDisabled onPress =
    let
        ( hint, action ) =
            case enabledDisabled of
                Enabled text ->
                    ( text, onPress )

                Disabled text ->
                    ( text, Nothing )
    in
    deleteIconButton palette
        styleIsPrimary
        hint
        action
