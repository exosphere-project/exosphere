module Style.Widgets.RemoveButton exposing (removePopconfirm)

import Element
import FeatherIcons
import Set
import Style.Types
import Style.Widgets.Button
import Style.Widgets.Popover.Popover exposing (popover)
import Style.Widgets.Popover.Types exposing (PopoverId)
import Style.Widgets.Spacer exposing (spacer)


type alias PopconfirmContent msg =
    { confirmation : Element.Element msg
    , onConfirm : Maybe msg
    , onCancel : Maybe msg
    }


removePopconfirm :
    { viewContext | palette : Style.Types.ExoPalette, showPopovers : Set.Set PopoverId }
    -> (PopoverId -> msg)
    -> PopoverId
    -> PopconfirmContent msg
    -> Style.Types.PopoverPosition
    -> (msg -> Bool -> Element.Element msg)
    -> Element.Element msg
removePopconfirm context msgMapper id content position target =
    popover context
        msgMapper
        { id = id
        , content = removePopconfirmContent context.palette content
        , contentStyleAttrs = []
        , position = position
        , distanceToTarget = Nothing
        , target = target
        , targetStyleAttrs = []
        }


removePopconfirmContent : Style.Types.ExoPalette -> PopconfirmContent msg -> Element.Attribute msg -> Element.Element msg
removePopconfirmContent palette { confirmation, onConfirm, onCancel } closePopconfirm =
    Element.column
        [ Element.spacing spacer.px16, Element.padding spacer.px4, Element.width Element.fill ]
        [ Element.row [ Element.spacing spacer.px8 ]
            [ FeatherIcons.alertCircle
                |> FeatherIcons.withSize 20
                |> FeatherIcons.toHtml []
                |> Element.html
                |> Element.el []
            , confirmation
            ]
        , Element.row [ Element.spacing spacer.px12, Element.alignRight ]
            [ Element.el [ closePopconfirm ] <|
                Style.Widgets.Button.default
                    palette
                    { text = "Cancel"
                    , onPress = onCancel
                    }
            , Element.el [ closePopconfirm ] <|
                Style.Widgets.Button.button Style.Widgets.Button.Warning
                    palette
                    { text = "Remove"
                    , onPress = onConfirm
                    }
            ]
        ]
