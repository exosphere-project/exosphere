module Style.Widgets.Popover exposing (popover, popoverAttribs, popoverStyleDefaults)

import Element
import Element.Background as Background
import Element.Border as Border
import Html.Attributes
import Html.Events
import Set
import Style.Helpers as SH
import Style.Types as ST exposing (ExoPalette, PopoverPosition(..))
import Types.SharedMsg
import View.Types


popoverStyleDefaults : ExoPalette -> List (Element.Attribute msg)
popoverStyleDefaults palette =
    [ Element.padding 10
    , Background.color <| SH.toElementColor palette.background
    , Border.width 1
    , Border.color <| SH.toElementColorWithOpacity palette.on.background 0.16
    , Border.shadow SH.shadowDefaults
    ]


popoverAttribs :
    Element.Element msg
    -> PopoverPosition
    -> Maybe Int
    -> List (Element.Attribute msg)
popoverAttribs popoverContent position distanceToTarget =
    let
        padding =
            Maybe.withDefault 6 distanceToTarget

        alignOnYAttribs percentStr =
            -- alignment on Y axis of a nearby element doesn't work without this
            [ Element.htmlAttribute <| Html.Attributes.style "top" percentStr
            , Element.htmlAttribute <| Html.Attributes.style "transform" ("translateY(-" ++ percentStr ++ ")")
            ]

        attribs :
            { nearbyElement : Element.Element msg -> Element.Attribute msg
            , alignment : Element.Attribute msg
            , onLeftOrRight : Bool
            , additional : List (Element.Attribute msg)
            }
        attribs =
            case position of
                PositionTopLeft ->
                    { nearbyElement = Element.above
                    , alignment = Element.alignLeft
                    , onLeftOrRight = False
                    , additional = []
                    }

                PositionTop ->
                    { nearbyElement = Element.above
                    , alignment = Element.centerX
                    , onLeftOrRight = False
                    , additional = []
                    }

                PositionTopRight ->
                    { nearbyElement = Element.above
                    , alignment = Element.alignRight
                    , onLeftOrRight = False
                    , additional = []
                    }

                PositionRightTop ->
                    { nearbyElement = Element.onRight
                    , alignment = Element.alignTop
                    , onLeftOrRight = True
                    , additional = alignOnYAttribs "0%"
                    }

                PositionRight ->
                    { nearbyElement = Element.onRight
                    , alignment = Element.centerY
                    , onLeftOrRight = True
                    , additional = alignOnYAttribs "50%"
                    }

                PositionRightBottom ->
                    { nearbyElement = Element.onRight
                    , alignment = Element.alignBottom
                    , onLeftOrRight = True
                    , additional = alignOnYAttribs "100%"
                    }

                PositionBottomRight ->
                    { nearbyElement = Element.below
                    , alignment = Element.alignRight
                    , onLeftOrRight = False
                    , additional = []
                    }

                PositionBottom ->
                    { nearbyElement = Element.below
                    , alignment = Element.centerX
                    , onLeftOrRight = False
                    , additional = []
                    }

                PositionBottomLeft ->
                    { nearbyElement = Element.below
                    , alignment = Element.alignLeft
                    , onLeftOrRight = False
                    , additional = []
                    }

                PositionLeftBottom ->
                    { nearbyElement = Element.onLeft
                    , alignment = Element.alignBottom
                    , onLeftOrRight = True
                    , additional = alignOnYAttribs "100%"
                    }

                PositionLeft ->
                    { nearbyElement = Element.onLeft
                    , alignment = Element.centerY
                    , onLeftOrRight = True
                    , additional = alignOnYAttribs "50%"
                    }

                PositionLeftTop ->
                    { nearbyElement = Element.onLeft
                    , alignment = Element.alignTop
                    , onLeftOrRight = True
                    , additional = alignOnYAttribs "0%"
                    }
    in
    [ attribs.nearbyElement <|
        Element.el
            ([ attribs.alignment
             , if attribs.onLeftOrRight then
                Element.paddingXY padding 0

               else
                Element.paddingXY 0 padding
             ]
                ++ attribs.additional
            )
            popoverContent
    ]


{-| We need unique id for each popover because we need to check if a click target was descendant of a specific popover
TODO: remove `popoverId` as parameter - consumer shouldn't bother about it.
Generate a unique id in widget itself, something like CopyableText but there's no string to hash
-}
popover :
    View.Types.Context
    -> (Types.SharedMsg.SharedMsg -> msg)
    ->
        { id : View.Types.PopoverId
        , content : Element.Attribute msg -> Element.Element msg
        , contentStyleAttrs : List (Element.Attribute msg)
        , position : ST.PopoverPosition
        , distanceToTarget : Maybe Int
        , target : msg -> Bool -> Element.Element msg
        , targetStyleAttrs : List (Element.Attribute msg)
        }
    -> Element.Element msg
popover context sharedMsgMapper { id, content, contentStyleAttrs, position, distanceToTarget, target, targetStyleAttrs } =
    -- TODO: add doc to explain record fields
    let
        popoverIsShown =
            Set.member id context.showPopovers

        -- close popover when an action happens in the content
        closePopover =
            (Element.htmlAttribute <|
                Html.Events.onClick (Types.SharedMsg.TogglePopover id)
            )
                |> Element.mapAttribute sharedMsgMapper
    in
    Element.el
        ((Element.htmlAttribute <| Html.Attributes.id id)
            :: (if popoverIsShown then
                    popoverAttribs
                        (Element.el
                            (popoverStyleDefaults context.palette
                                -- Add or override default style with passed style attributes
                                ++ contentStyleAttrs
                            )
                            (content closePopover)
                        )
                        position
                        distanceToTarget

                else
                    []
               )
            ++ targetStyleAttrs
        )
        (target (sharedMsgMapper <| Types.SharedMsg.TogglePopover id)
            popoverIsShown
        )
