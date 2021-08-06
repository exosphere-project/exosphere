module Types.Types exposing (LogMessage, SharedModel, Style)

import Browser.Navigation
import Color
import Dict
import Style.Types
import Time
import Toasty
import Types.Error exposing (ErrorContext, Toast)
import Types.HelperTypes as HelperTypes
import Types.Project exposing (Project)
import UUID


type alias SharedModel =
    { logMessages : List LogMessage
    , urlPathPrefix : Maybe String
    , navigationKey : Browser.Navigation.Key

    -- Used to determine whether to pushUrl (change of view) or replaceUrl (just change of view parameters)
    , prevUrl : String
    , windowSize : HelperTypes.WindowSize
    , unscopedProviders : List HelperTypes.UnscopedProvider
    , projects : List Project
    , toasties : Toasty.Stack Toast
    , cloudCorsProxyUrl : Maybe CloudCorsProxyUrl
    , clientUuid : UUID.UUID
    , clientCurrentTime : Time.Posix
    , timeZone : Time.Zone
    , showDebugMsgs : Bool
    , style : Style
    , openIdConnectLoginConfig :
        Maybe HelperTypes.OpenIdConnectLoginConfig
    , cloudSpecificConfigs : Dict.Dict HelperTypes.KeystoneHostname HelperTypes.CloudSpecificConfig
    , instanceConfigMgtRepoUrl : HelperTypes.Url
    , instanceConfigMgtRepoCheckout : String
    }


type alias Style =
    { logo : HelperTypes.Url
    , primaryColor : Color.Color
    , secondaryColor : Color.Color
    , styleMode : Style.Types.StyleMode
    , appTitle : String
    , topBarShowAppTitle : Bool
    , defaultLoginView : Maybe HelperTypes.DefaultLoginView
    , aboutAppMarkdown : Maybe String
    , supportInfoMarkdown : Maybe String
    , userSupportEmail : String
    , localization : HelperTypes.Localization
    }


type alias CloudCorsProxyUrl =
    HelperTypes.Url


type alias LogMessage =
    { message : String
    , context : ErrorContext
    , timestamp : Time.Posix
    }
