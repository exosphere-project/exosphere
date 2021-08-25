module View.Types exposing
    ( BrowserLinkLabel(..)
    , Context
    , ImageTag
    )

import Dict
import Element
import Style.Types exposing (ExoPalette)
import Types.HelperTypes exposing (CloudSpecificConfig, KeystoneHostname, Localization, WindowSize)


type alias Context =
    { palette : ExoPalette
    , localization : Localization
    , cloudSpecificConfigs : Dict.Dict KeystoneHostname CloudSpecificConfig
    , windowSize : WindowSize
    , experimentalFeaturesEnabled : Bool
    }


type BrowserLinkLabel msg
    = BrowserLinkTextLabel String
    | BrowserLinkFancyLabel (Element.Element msg)


type alias ImageTag =
    { label : String
    , frequency : Int
    }
