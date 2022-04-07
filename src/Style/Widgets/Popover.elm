module Style.Widgets.Popover exposing (popover)

import Element
import Html.Attributes
import Style.Helpers as SH
import Style.Types as ST
import View.Types



-- create model in this widget so that each operates independent of others in single consumer
-- make use of shared msg and shared model
-- for unique id, do something like copyable text widget: "popover-uuid"
-- and each popover should subscribe when panel is shown so save id data in shared model
-- seaparte ids because we need to check if click target was descendant of a specific popover


popover :
    View.Types.Context
    -> (Bool -> Element.Element msg)
    -> { styleAttrs : List (Element.Attribute msg), contents : Element.Element msg }
    -> ST.PopoverPosition
    -> Maybe Int
    -> Element.Element msg
popover context target panel position distance =
    Element.el
        ((Element.htmlAttribute <| Html.Attributes.id "SA-popover")
            :: (if context.showServerActionsPopover then
                    SH.popoverAttribs
                        (Element.el
                            (SH.popoverStyleDefaults context.palette
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
        (target context.showServerActionsPopover)
