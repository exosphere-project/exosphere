module View.Types exposing
    ( BrowserLinkLabel(..)
    , ImageTag
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
