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
    -> View.Types.PopoverId
    -> (Types.SharedMsg.SharedMsg -> Bool -> Element.Element msg)
    -> { styleAttrs : List (Element.Attribute msg), contents : Element.Element msg }
    -> ST.PopoverPosition
    -> Maybe Int
    -> Element.Element msg
popover context sharedMsgMapper popoverId target panel position distance =
    let
        popoverIsShown =
            Set.member popoverId context.showPopovers

        -- close popover whenever an actionable component inside panel is clicked
        -- FIXME: a popover shouldn't ideally close when clicking inside it on a non-actionable component
        -- but this solution closes popover on evey click (preventing uses to copy text from it)
        closePopoverAttrib =
            (Element.htmlAttribute <|
                Html.Events.onClick <|
                    Types.SharedMsg.TogglePopover popoverId
            )
                |> Element.mapAttribute sharedMsgMapper
    in
    Element.el
        ((Element.htmlAttribute <| Html.Attributes.id popoverId)
            :: (if popoverIsShown then
                    SH.popoverAttribs
                        (Element.el
                            (closePopoverAttrib
                                :: SH.popoverStyleDefaults context.palette
                                -- Add or override default style with passed style attributes
                                ++ panel.styleAttrs
                            )
                            panel.contents
                        )
                        position
                        distance

                else
                    []
               )
        )
        (target (Types.SharedMsg.TogglePopover popoverId)
            popoverIsShown
        )
