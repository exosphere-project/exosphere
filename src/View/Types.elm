module View.Types exposing (BrowserLinkLabel(..))

import Element
import Types.Types exposing (Msg)


type BrowserLinkLabel
    = BrowserLinkTextLabel String
    | BrowserLinkFancyLabel (Element.Element Msg)
