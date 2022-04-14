module Style.Widgets.Popover exposing (popover)

import Element
import Html.Attributes
import Html.Events
import Set
import Style.Helpers as SH
import Style.Types as ST
import Types.SharedMsg
import View.Types



-- We need unique id for each popover because we need to check if a click target was descendant of a specific popover
-- TODO: remove `popoverId` as parameter - consumer shouldn't bother about it.
-- Generate a unique id in widget itself, something like CopyableText but there's no string to hash


popover :
    View.Types.Context
    -> (Types.SharedMsg.SharedMsg -> msg)
    ->
        { id : View.Types.PopoverId
        , styleAttrs : List (Element.Attribute msg)
        , content : Element.Attribute msg -> Element.Element msg
        , position : ST.PopoverPosition
        , distance : Maybe Int
        , target : msg -> Bool -> Element.Element msg
        }
    -> Element.Element msg
popover context sharedMsgMapper { id, styleAttrs, content, position, distance, target } =
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
                    SH.popoverAttribs
                        (Element.el
                            (SH.popoverStyleDefaults context.palette
                                -- Add or override default style with passed style attributes
                                ++ styleAttrs
                            )
                            (content closePopover)
                        )
                        position
                        distance

                else
                    []
               )
        )
        (target (sharedMsgMapper <| Types.SharedMsg.TogglePopover id)
            popoverIsShown
        )
