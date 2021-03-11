module View.Types exposing
    ( BrowserLinkLabel(..)
    , Context
    , ImageTag
    )

import Element
import Style.Types exposing (ExoPalette)
import Types.Types exposing (Localization, Msg)


type alias Context =
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
