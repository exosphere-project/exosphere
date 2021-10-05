module Types.SharedModel exposing (LogMessage, SharedModel, Style)

import Color
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
    , instanceConfigMgtRepoUrl : HelperTypes.Url
    , instanceConfigMgtRepoCheckout : String
    , viewContext : View.Types.Context
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
    }


type alias CloudCorsProxyUrl =
    HelperTypes.Url


type alias LogMessage =
    { message : String
    , context : ErrorContext
    , timestamp : Time.Posix
    }
