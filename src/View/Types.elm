module View.Types exposing
    ( Context
    , ImageTag
    , PortRangeBounds(..)
    , RemoteType(..)
    , SelectMod(..)
    , ServerActionOption
    )

import Browser.Navigation
import Dict
import FormatNumber.Locales
import OpenStack.ServerActions exposing (ServerActionName)
import OpenStack.Types as OSTypes
import Set
import Style.Types exposing (ExoPalette)
import Style.Widgets.Popover.Types exposing (PopoverId)
import Types.HelperTypes exposing (CloudSpecificConfig, KeystoneHostname, Localization, ProjectIdentifier, WindowSize)
import Types.Server exposing (Server)
import Types.SharedMsg exposing (SharedMsg)
import Url


type alias Context =
    { palette : ExoPalette
    , locale : FormatNumber.Locales.Locale
    , localization : Localization
    , cloudSpecificConfigs : Dict.Dict KeystoneHostname CloudSpecificConfig
    , windowSize : WindowSize
    , experimentalFeaturesEnabled : Bool
    , appVersionUpdateNotificationsEnabled : Bool
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
    | SecurityGroup


type PortRangeBounds
    = PortRangeAny
    | PortRangeSingle
    | PortRangeMinMax


type SelectMod
    = NoMod
    | Primary
    | Warning
    | Danger


type alias ServerActionOption =
    { name : ServerActionName
    , description : String
    , allowedStatuses : Maybe (List OSTypes.ServerStatus)
    , allowedLockStatus : Maybe OSTypes.ServerLockStatus
    , action : ProjectIdentifier -> Server -> Bool -> SharedMsg
    , selectMod : SelectMod
    , confirmable : Bool
    }
