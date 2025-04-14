module Style.Widgets.DeleteButton exposing
    ( DeleteButtonState(..)
    , PopconfirmContent
    , deleteIconButton
    , deleteIconButtonWithDisabledHint
    , deletePopconfirm
    , deletePopconfirmContent
    )

import Element
import FeatherIcons as Icons
import Html.Attributes as HtmlA
import Set
import Style.Helpers as SH
import Style.Types exposing (ExoPalette)
import Style.Widgets.Button as Button
import Style.Widgets.Icon exposing (sizedFeatherIcon)
import Style.Widgets.Popover.Popover exposing (popover)
import Style.Widgets.Popover.Types exposing (PopoverId)
import Style.Widgets.Spacer exposing (spacer)
import Widget


type alias PopconfirmContent msg =
    { confirmation : Element.Element msg
    , buttonText : Maybe String
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


deletePopconfirmContent : ExoPalette -> PopconfirmContent msg -> Element.Attribute msg -> Element.Element msg
deletePopconfirmContent palette { confirmation, buttonText, onConfirm, onCancel } closePopconfirm =
    Element.column
        [ Element.spacing spacer.px16, Element.padding spacer.px4, Element.width Element.fill ]
        [ Element.row [ Element.spacing spacer.px8 ]
            [ Element.el [ Element.alignTop ] <|
                sizedFeatherIcon 20 Icons.alertTriangle
            , confirmation
            ]
        , Element.row [ Element.spacing spacer.px12, Element.alignRight ]
            [ Element.el [ closePopconfirm ] <|
                Button.default
                    palette
                    { text = "Cancel"
                    , onPress = onCancel
                    }
            , Element.el [ closePopconfirm ] <|
                Button.button Button.Danger
                    palette
                    { text = Maybe.withDefault "Delete" buttonText
                    , onPress = onConfirm
                    }
            ]
        ]


deletePopconfirm :
    { viewContext | palette : ExoPalette, showPopovers : Set.Set PopoverId }
    -> (PopoverId -> msg)
    -> PopoverId
    -> PopconfirmContent msg
    -> Style.Types.PopoverPosition
    -> (msg -> Bool -> Element.Element msg)
    -> Element.Element msg
deletePopconfirm context msgMapper id content position target =
    popover context
        msgMapper
        { id = id
        , content = deletePopconfirmContent context.palette content
        , contentStyleAttrs = []
        , position = position
        , distanceToTarget = Nothing
        , target = target
        , targetStyleAttrs = []
        }
