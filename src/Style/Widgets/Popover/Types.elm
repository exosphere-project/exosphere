module Style.Widgets.Popover.Types exposing (NodePositionRelativeToPopover(..), PopoverId)

{-| This will be used as HTML id attribute so it must be unique.
Using name/purpose of popover is generally enough to avoid collisions.
-}


type alias PopoverId =
    String


type NodePositionRelativeToPopover
    = NodeInside
    | NodeOutside
