module Style.Widgets.Popover.Popover exposing (popover, popoverAttribs, popoverStyleDefaults)

import Element
import Element.Background as Background
import Element.Border as Border
import Html.Attributes
import Html.Events
import Set
import Style.Helpers as SH
import Style.Types as ST exposing (ExoPalette, PopoverPosition(..))
import Style.Widgets.Popover.Types exposing (PopoverId)


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


{-| Popover widget that can auto-close on clicking outside of it. Parameters:

  - `context` - State of the popover, intends to take `View.Types.Context` (but
    doesn't explicitly define so to keep it generic for use in style guide etc.)
  - `msgMapper` - Maps popoverId to a consumer's msg.
  - `id` - Unique id of the popover so that it can be checked if the element
    where click event happened in app, was descendant of specifically this
    popover (not others).
  - `content` - Content of the popover - takes close-popover event (attribute)
    and produces an element. If you want to close the popover after an action
    hapens within the content, trigger the closePopover event by adding it in
    attributes of the element that produces action.
  - `contentStyleAttrs` - Style to be used to override the default popover style.
  - `position` - Where popover should appear w.r.t. its target.
  - `distanceToTarget` - Distance of popover to its target in px.
  - `target` - Element that opens/closes popover - takes a toggle popover message
    and a boolean indicating if the popover is shown, to produce an element. The
    message should be emitted when an action happens on target (element) like
    button click, etc. The boolean can be used to created a distinguished look
    ofthe target when popover is opened and closed.
  - `targetStyleAttrs` - Style of target's wrapper (element containing the
    element passed in `target` parameter). You'll rarely need this, only in the
    cases where style don't work as expected due to target being wrapped in
    another container.

TODO: remove `popoverId` as parameter - consumer shouldn't bother about it.
Generate a unique id in widget itself, something like CopyableText but there's no string to hash

-}
popover :
    { viewContext | palette : ExoPalette, showPopovers : Set.Set PopoverId }
    -> (PopoverId -> msg)
    ->
        { id : PopoverId
        , content : Element.Attribute msg -> Element.Element msg
        , contentStyleAttrs : List (Element.Attribute msg)
        , position : ST.PopoverPosition
        , distanceToTarget : Maybe Int
        , target : msg -> Bool -> Element.Element msg
        , targetStyleAttrs : List (Element.Attribute msg)
        }
    -> Element.Element msg
popover context msgMapper { id, content, contentStyleAttrs, position, distanceToTarget, target, targetStyleAttrs } =
    let
        popoverIsShown =
            Set.member id context.showPopovers

        -- close popover when an action happens in the content
        closePopover =
            Element.htmlAttribute <|
                Html.Events.onClick (msgMapper id)
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
        (target (msgMapper id) popoverIsShown)
