module View.Types exposing
    ( Context
    , ImageTag
    , PortRangeBounds(..)
    , RemoteType(..)
    )

import Browser.Navigation
import Dict
import FormatNumber.Locales
import Set
import Style.Types exposing (ExoPalette)
import Style.Widgets.Popover.Types exposing (PopoverId)
import Types.HelperTypes exposing (CloudSpecificConfig, KeystoneHostname, Localization, WindowSize)
import Url


type alias Context =
    { palette : ExoPalette
    , locale : FormatNumber.Locales.Locale
    , localization : Localization
    , cloudSpecificConfigs : Dict.Dict KeystoneHostname CloudSpecificConfig
    , windowSize : WindowSize
    , experimentalFeaturesEnabled : Bool
    , baseUrl : Url.Url
    , urlPathPrefix : Maybe String
    , navigationKey : Browser.Navigation.Key
    , showPopovers : Set.Set PopoverId
    }


type alias ImageTag =
    { label : String
    , frequency : Int
    }


type RemoteType
    = Any
    | IpPrefix
    | GroupId


type PortRangeBounds
    = PortRangeAny
    | PortRangeSingle
    | PortRangeMinMax
