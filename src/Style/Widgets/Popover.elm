module Style.Widgets.Popover exposing (popover)

import Element
import Html.Attributes
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
    -> View.Types.PopoverId
    -> (Types.SharedMsg.SharedMsg -> Bool -> Element.Element msg)
    -> { styleAttrs : List (Element.Attribute msg), contents : Element.Element msg }
    -> ST.PopoverPosition
    -> Maybe Int
    -> Element.Element msg
popover context popoverId target panel position distance =
    let
        popoverIsShown =
            Set.member popoverId context.showPopovers
    in
    Element.el
        ((Element.htmlAttribute <| Html.Attributes.id popoverId)
            :: (if popoverIsShown then
                    SH.popoverAttribs
                        (Element.el
                            (SH.popoverStyleDefaults context.palette
                                -- Add or override default style with passed style attributes
                                ++ panel.styleAttrs
                            )
                            -- TODO: find a way to close popover whenever an action happens from panel
                            -- how to combine panel's message with TogglePopover SharedMsg?
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
