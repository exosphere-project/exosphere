module Types.SharedModel exposing (LogMessage, SharedModel, Style)

import OpenStack.Types as OSTypes
import Style.Types
import Time
import Toasty
import Types.Error exposing (ErrorContext, Toast)
import Types.HelperTypes as HelperTypes
import Types.Project exposing (Project)
import UUID
import View.Types


type alias SharedModel =
    { logMessages : List LogMessage
    , unscopedProviders : List HelperTypes.UnscopedProvider
    , scopedAuthTokensWaitingRegionSelection : List OSTypes.ScopedAuthToken
    , projects : List Project
    , toasties : Toasty.Stack Toast
    , networkConnectivity : Maybe Bool -- assume online, Just False means received offline event
    , cloudCorsProxyUrl : Maybe CloudCorsProxyUrl
    , clientUuid : UUID.UUID
    , clientCurrentTime : Time.Posix
    , timeZone : Time.Zone
    , showDebugMsgs : Bool
    , style : Style
    , openIdConnectLoginConfig :
        Maybe HelperTypes.OpenIdConnectLoginConfig
    , instanceConfigMgtRepoUrl : HelperTypes.Url
    , instanceConfigMgtRepoCheckout : String
    , viewContext : View.Types.Context
    , sentryConfig : Maybe HelperTypes.SentryConfig
    }


type alias Style =
    { logo : HelperTypes.Url
    , deployerColors : Style.Types.DeployerColorThemes
    , styleMode : Style.Types.StyleMode
    , appTitle : String
    , topBarShowAppTitle : Bool
    , defaultLoginView : Maybe HelperTypes.DefaultLoginView
    , aboutAppMarkdown : Maybe String
    , supportInfoMarkdown : Maybe String
    , userSupportEmail : String
    }


type alias CloudCorsProxyUrl =
    HelperTypes.Url


type alias LogMessage =
    { message : String
    , context : ErrorContext
    , timestamp : Time.Posix
    }
