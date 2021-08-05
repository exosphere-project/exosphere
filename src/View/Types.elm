module View.Types exposing
    ( BrowserLinkLabel(..)
    , Context
    , ImageTag
    )

import Dict
import Element
import Style.Types exposing (ExoPalette)
import Types.Types exposing (Localization, WindowSize)


type alias Context =
    { palette : ExoPalette
    , localization : Localization
    , cloudSpecificConfigs : Dict.Dict Types.Types.KeystoneHostname Types.Types.CloudSpecificConfig
    , windowSize : WindowSize
    }


type BrowserLinkLabel msg
    = BrowserLinkTextLabel String
    | BrowserLinkFancyLabel (Element.Element msg)


type alias ImageTag =
    { label : String
    , frequency : Int
    }
