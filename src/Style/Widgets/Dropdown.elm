module Style.Widgets.Dropdown exposing (dropdown)

import Element
import FeatherIcons as Icons
import Set
import Style.Helpers as SH
import Style.Types as ST exposing (ExoPalette)
import Style.Widgets.Icon exposing (sizedFeatherIcon)
import Style.Widgets.Popover.Popover exposing (popover)
import Style.Widgets.Popover.Types exposing (PopoverId)
import Style.Widgets.Spacer exposing (spacer)
import Widget


{-| A dropdown menu built on top of Popover.

Takes a target label, a unique popover ID, and content that receives a
close attribute. The target renders as a button with a chevron indicator.

    dropdown context
        msgMapper
        { id = "myDropdown"
        , label = "Actions"
        , content =
            \closeDropdown ->
                Element.column [ Element.spacing spacer.px8 ]
                    [ Element.el [ closeDropdown ] (Element.text "Item 1")
                    , Element.el [ closeDropdown ] (Element.text "Item 2")
                    ]
        }

-}
dropdown :
    { viewContext | palette : ExoPalette, showPopovers : Set.Set PopoverId }
    -> (PopoverId -> msg)
    ->
        { id : PopoverId
        , label : String
        , content : Element.Attribute msg -> Element.Element msg
        }
    -> Element.Element msg
dropdown context msgMapper { id, label, content } =
    let
        dropdownTarget toggleDropdownMsg dropdownIsShown =
            Widget.iconButton
                (SH.materialStyle context.palette).button
                { text = label
                , icon =
                    Element.row
                        [ Element.spacing spacer.px4 ]
                        [ Element.text label
                        , sizedFeatherIcon 18 <|
                            if dropdownIsShown then
                                Icons.chevronUp

                            else
                                Icons.chevronDown
                        ]
                , onPress = Just toggleDropdownMsg
                }
    in
    popover context
        msgMapper
        { id = id
        , content = content
        , contentStyleAttrs = [ Element.padding spacer.px12 ]
        , position = ST.PositionBottomRight
        , distanceToTarget = Nothing
        , target = dropdownTarget
        , targetStyleAttrs = []
        }
