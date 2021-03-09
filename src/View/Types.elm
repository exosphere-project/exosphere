module View.Types exposing
    ( BrowserLinkLabel(..)
    , ImageTag
    , ViewContext
    )

import Element
import Style.Types exposing (ExoPalette)
import Types.Types exposing (Localization, Msg)


type alias ViewContext =
    { palette : ExoPalette
    , isElectron : Bool
    , localization : Localization
    }


type BrowserLinkLabel
    = BrowserLinkTextLabel String
    | BrowserLinkFancyLabel (Element.Element Msg)


type alias ImageTag =
    { label : String
    , frequency : Int
    }
