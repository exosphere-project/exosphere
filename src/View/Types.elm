module View.Types exposing
    ( Context
    , ImageTag
    , PopoverId
    )

import Browser.Navigation
import Dict
import FormatNumber.Locales
import Set
import Style.Types exposing (ExoPalette)
import Types.HelperTypes exposing (CloudSpecificConfig, KeystoneHostname, Localization, WindowSize)


type alias Context =
    { palette : ExoPalette
    , locale : FormatNumber.Locales.Locale
    , localization : Localization
    , cloudSpecificConfigs : Dict.Dict KeystoneHostname CloudSpecificConfig
    , windowSize : WindowSize
    , experimentalFeaturesEnabled : Bool
    , urlPathPrefix : Maybe String
    , navigationKey : Browser.Navigation.Key
    , showPopovers : Set.Set PopoverId
    }


type alias ImageTag =
    { label : String
    , frequency : Int
    }


{-| This will be used as HTML id attribute so it must be unique.
Using name/purpose of popover is generally enough to avoid collisions.
-}
type alias PopoverId =
    String
