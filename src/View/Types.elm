module View.Types exposing
    ( BrowserLinkLabel(..)
    , ImageTag
    , SortTableModel
    )

import Element
import Types.Types exposing (Msg)


type BrowserLinkLabel
    = BrowserLinkTextLabel String
    | BrowserLinkFancyLabel (Element.Element Msg)


type alias ImageTag =
    { label : String
    , frequency : Int
    }


type alias SortTableModel =
    { title : String
    , asc : Bool
    }
