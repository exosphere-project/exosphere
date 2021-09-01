module View.Types exposing
    ( BrowserLinkLabel(..)
    , Context
    , ImageTag
    )

import Browser.Navigation
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
    , urlPathPrefix : Maybe String
    , navigationKey : Browser.Navigation.Key
    }


type BrowserLinkLabel msg
    = BrowserLinkTextLabel String
    | BrowserLinkFancyLabel (Element.Element msg)


type alias ImageTag =
    { label : String
    , frequency : Int
    }
