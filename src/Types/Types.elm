module Types.Types exposing
    ( CloudSpecificConfig
    , ExcludeFilter
    , Flags
    , KeystoneHostname
    , Localization
    , LogMessage
    , Model
    , OpenIdConnectLoginConfig
    , Style
    , UserAppProxyHostname
    , WindowSize
    )

import Browser.Navigation
import Color
import Dict
import Json.Decode as Decode
import Style.Types
import Time
import Toasty
import Types.Error exposing (ErrorContext, Toast)
import Types.HelperTypes as HelperTypes
import Types.Project exposing (Project)
import Types.View as ViewTypes
import UUID



{- Top app-Level Types -}


type alias Flags =
    -- Flags intended to be configured by cloud operators
    { showDebugMsgs : Bool
    , cloudCorsProxyUrl : Maybe HelperTypes.Url
    , urlPathPrefix : Maybe String
    , appTitle : Maybe String
    , topBarShowAppTitle : Bool
    , palette :
        Maybe
            { primary :
                { r : Int
                , g : Int
                , b : Int
                }
            , secondary :
                { r : Int
                , g : Int
                , b : Int
                }
            }
    , logo : Maybe String
    , favicon : Maybe String
    , defaultLoginView : Maybe String
    , aboutAppMarkdown : Maybe String
    , supportInfoMarkdown : Maybe String
    , userSupportEmail : Maybe String
    , openIdConnectLoginConfig :
        Maybe OpenIdConnectLoginConfig
    , localization : Maybe Localization
    , clouds :
        List
            { keystoneHostname : KeystoneHostname
            , userAppProxy : Maybe UserAppProxyHostname
            , imageExcludeFilter : Maybe ExcludeFilter
            , featuredImageNamePrefix : Maybe String
            }
    , instanceConfigMgtRepoUrl : Maybe String
    , instanceConfigMgtRepoCheckout : Maybe String

    -- Flags that Exosphere sets dynamically
    , width : Int
    , height : Int
    , storedState : Maybe Decode.Value
    , randomSeed0 : Int
    , randomSeed1 : Int
    , randomSeed2 : Int
    , randomSeed3 : Int
    , epoch : Int
    , timeZone : Int
    }


type alias CloudSpecificConfig =
    { userAppProxy : Maybe UserAppProxyHostname
    , imageExcludeFilter : Maybe ExcludeFilter
    , featuredImageNamePrefix : Maybe String
    }


type alias WindowSize =
    { width : Int
    , height : Int
    }


type alias Model =
    { logMessages : List LogMessage
    , urlPathPrefix : Maybe String
    , navigationKey : Browser.Navigation.Key

    -- Used to determine whether to pushUrl (change of view) or replaceUrl (just change of view parameters)
    , prevUrl : String
    , viewState : ViewTypes.ViewState
    , windowSize : WindowSize
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
        Maybe OpenIdConnectLoginConfig
    , cloudSpecificConfigs : Dict.Dict KeystoneHostname CloudSpecificConfig
    , instanceConfigMgtRepoUrl : HelperTypes.Url
    , instanceConfigMgtRepoCheckout : String
    }


type alias ExcludeFilter =
    { filterKey : String
    , filterValue : String
    }


type alias Localization =
    { openstackWithOwnKeystone : String
    , openstackSharingKeystoneWithAnother : String
    , unitOfTenancy : String
    , maxResourcesPerProject : String
    , pkiPublicKeyForSsh : String
    , virtualComputer : String
    , virtualComputerHardwareConfig : String
    , cloudInitData : String
    , commandDrivenTextInterface : String
    , staticRepresentationOfBlockDeviceContents : String
    , blockDevice : String
    , nonFloatingIpAddress : String
    , floatingIpAddress : String
    , publiclyRoutableIpAddress : String
    , graphicalDesktopEnvironment : String
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
    , localization : Localization
    }


type alias OpenIdConnectLoginConfig =
    { keystoneAuthUrl : String
    , webssoKeystoneEndpoint : String
    , oidcLoginIcon : String
    , oidcLoginButtonLabel : String
    , oidcLoginButtonDescription : String
    }


type alias CloudCorsProxyUrl =
    HelperTypes.Url


type alias KeystoneHostname =
    HelperTypes.Hostname


type alias UserAppProxyHostname =
    HelperTypes.Hostname


type alias LogMessage =
    { message : String
    , context : ErrorContext
    , timestamp : Time.Posix
    }
