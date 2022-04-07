module View.Types exposing
    ( Context
    , ImageTag
    )

import Browser.Navigation
import Dict
import FormatNumber.Locales
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

    -- TODO: make it a set of ids of popovers that are open?
    , showServerActionsPopover : Bool
    }


type alias ImageTag =
    { label : String
    , frequency : Int
    }
