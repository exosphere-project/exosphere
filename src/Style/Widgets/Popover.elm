module Style.Widgets.Popover exposing (popover)

import Element
import Style.Helpers as SH
import Style.Types as ST



-- create model in this widget so that each operates independent of others in single consumer
-- make use of shared msg and shared model
-- for unique id, do something like copyable text widget: "popover-uuid"
-- and each popover should subscribe when panel is shown so save id data in shared model
-- seaparte ids because we need to check if click target was descendant of a specific popover


popover : Element.Element msg -> Element.Element msg -> ST.PopoverPosition -> Maybe Int -> Element.Element msg
popover target panel position distance =
    Element.el
        (if model.shown then
            SH.popoverAttribs (Element.el SH.popoverStyleDefaults panel) position distance

         else
            []
        )
        target
