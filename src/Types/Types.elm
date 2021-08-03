module Types.Types exposing
    ( CloudSpecificConfig
    , Endpoints
    , ExcludeFilter
    , ExoServerProps
    , ExoServerVersion
    , ExoSetupStatus(..)
    , Flags
    , HttpRequestMethod(..)
    , KeystoneHostname
    , Localization
    , LogMessage
    , Model
    , Msg(..)
    , NewServerNetworkOptions(..)
    , OpenIdConnectLoginConfig
    , Project
    , ProjectName
    , ProjectSecret(..)
    , ProjectSpecificMsgConstructor(..)
    , ProjectTitle
    , ResourceUsageRDPP
    , Server
    , ServerFromExoProps
    , ServerOrigin(..)
    , ServerSpecificMsgConstructor(..)
    , ServerUiStatus(..)
    , Style
    , TickInterval
    , Toast
    , UserAppProxyHostname
    , WindowSize
    , currentExoServerVersion
    )

import Browser.Navigation
import Color
import Dict
import Helpers.RemoteDataPlusPlus as RDPP
import Http
import Json.Decode as Decode
import OpenStack.Types as OSTypes
import RemoteData exposing (WebData)
import Style.Types
import Time
import Toasty
import Types.Error exposing (ErrorContext, HttpErrorWithBody)
import Types.Guacamole as GuacTypes
import Types.HelperTypes as HelperTypes
import Types.ServerResourceUsage
import Types.View as ViewTypes
import UUID
import Url



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
    , defaultLoginView : Maybe ViewTypes.LoginView
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



{- Project types -}


type alias Project =
    { secret : ProjectSecret
    , auth : OSTypes.ScopedAuthToken
    , endpoints : Endpoints
    , images : List OSTypes.Image
    , servers : RDPP.RemoteDataPlusPlus HttpErrorWithBody (List Server)
    , flavors : List OSTypes.Flavor
    , keypairs : WebData (List OSTypes.Keypair)
    , volumes : WebData (List OSTypes.Volume)
    , networks : RDPP.RemoteDataPlusPlus HttpErrorWithBody (List OSTypes.Network)
    , autoAllocatedNetworkUuid : RDPP.RemoteDataPlusPlus HttpErrorWithBody OSTypes.NetworkUuid
    , floatingIps : RDPP.RemoteDataPlusPlus HttpErrorWithBody (List OSTypes.FloatingIp)
    , ports : RDPP.RemoteDataPlusPlus HttpErrorWithBody (List OSTypes.Port)
    , securityGroups : List OSTypes.SecurityGroup
    , computeQuota : WebData OSTypes.ComputeQuota
    , volumeQuota : WebData OSTypes.VolumeQuota
    , pendingCredentialedRequests : List (OSTypes.AuthTokenString -> Cmd Msg) -- Requests waiting for a valid auth token
    }


type ProjectSecret
    = ApplicationCredential OSTypes.ApplicationCredential
    | NoProjectSecret


type alias Endpoints =
    { cinder : HelperTypes.Url
    , glance : HelperTypes.Url
    , keystone : HelperTypes.Url
    , nova : HelperTypes.Url
    , neutron : HelperTypes.Url
    }



{- Msg types -}


type Msg
    = Tick TickInterval Time.Posix
    | DoOrchestration Time.Posix
    | SetNonProjectView ViewTypes.NonProjectViewConstructor
    | HandleApiErrorWithBody ErrorContext HttpErrorWithBody
    | RequestUnscopedToken OSTypes.OpenstackLogin
    | JetstreamLogin ViewTypes.JetstreamCreds
    | ReceiveScopedAuthToken ( Http.Metadata, String )
    | ReceiveUnscopedAuthToken OSTypes.KeystoneUrl ( Http.Metadata, String )
    | ReceiveUnscopedProjects OSTypes.KeystoneUrl (List HelperTypes.UnscopedProviderProject)
    | RequestProjectLoginFromProvider OSTypes.KeystoneUrl (List HelperTypes.UnscopedProviderProject)
    | ProjectMsg HelperTypes.ProjectIdentifier ProjectSpecificMsgConstructor
    | SubmitOpenRc OSTypes.OpenstackLogin String
    | OpenNewWindow String
    | NavigateToUrl String
    | ToastyMsg (Toasty.Msg Toast)
    | MsgChangeWindowSize Int Int
    | UrlChange Url.Url
    | SetStyle Style.Types.StyleMode
    | NoOp


type alias TickInterval =
    Int


type ProjectSpecificMsgConstructor
    = SetProjectView ViewTypes.ProjectViewConstructor
    | ReceiveAppCredential OSTypes.ApplicationCredential
    | PrepareCredentialedRequest (Maybe HelperTypes.Url -> OSTypes.AuthTokenString -> Cmd Msg) Time.Posix
    | ToggleCreatePopup
    | RemoveProject
    | ServerMsg OSTypes.ServerUuid ServerSpecificMsgConstructor
    | RequestServers
    | RequestCreateServer ViewTypes.CreateServerViewParams OSTypes.NetworkUuid
    | RequestDeleteServers (List OSTypes.ServerUuid)
    | RequestCreateVolume OSTypes.VolumeName OSTypes.VolumeSize
    | RequestDeleteVolume OSTypes.VolumeUuid
    | RequestDetachVolume OSTypes.VolumeUuid
    | RequestKeypairs
    | RequestCreateKeypair OSTypes.KeypairName OSTypes.PublicKey
    | RequestDeleteKeypair OSTypes.KeypairName
    | RequestDeleteFloatingIp OSTypes.IpAddressUuid
    | RequestAssignFloatingIp OSTypes.Port OSTypes.IpAddressUuid
    | RequestUnassignFloatingIp OSTypes.IpAddressUuid
    | ReceiveImages (List OSTypes.Image)
    | ReceiveServer OSTypes.ServerUuid ErrorContext (Result HttpErrorWithBody OSTypes.Server)
    | ReceiveServers ErrorContext (Result HttpErrorWithBody (List OSTypes.Server))
    | ReceiveCreateServer OSTypes.ServerUuid
    | ReceiveFlavors (List OSTypes.Flavor)
    | ReceiveKeypairs (List OSTypes.Keypair)
    | ReceiveCreateKeypair OSTypes.Keypair
    | ReceiveDeleteKeypair ErrorContext OSTypes.KeypairName (Result Http.Error ())
    | ReceiveNetworks ErrorContext (Result HttpErrorWithBody (List OSTypes.Network))
    | ReceiveAutoAllocatedNetwork ErrorContext (Result HttpErrorWithBody OSTypes.NetworkUuid)
    | ReceiveFloatingIps (List OSTypes.FloatingIp)
    | ReceivePorts ErrorContext (Result HttpErrorWithBody (List OSTypes.Port))
    | ReceiveDeleteFloatingIp OSTypes.IpAddressUuid
    | ReceiveAssignFloatingIp OSTypes.FloatingIp
    | ReceiveUnassignFloatingIp OSTypes.FloatingIp
    | ReceiveSecurityGroups (List OSTypes.SecurityGroup)
    | ReceiveCreateExoSecurityGroup OSTypes.SecurityGroup
    | ReceiveCreateVolume
    | ReceiveVolumes (List OSTypes.Volume)
    | ReceiveDeleteVolume
    | ReceiveUpdateVolumeName
    | ReceiveAttachVolume OSTypes.VolumeAttachment
    | ReceiveDetachVolume
    | ReceiveComputeQuota OSTypes.ComputeQuota
    | ReceiveVolumeQuota OSTypes.VolumeQuota


type ServerSpecificMsgConstructor
    = RequestServer
    | RequestDeleteServer Bool
    | RequestSetServerName String
    | RequestAttachVolume OSTypes.VolumeUuid
    | RequestCreateServerImage String
    | ReceiveServerEvents ErrorContext (Result HttpErrorWithBody (List OSTypes.ServerEvent))
    | ReceiveConsoleUrl (Result HttpErrorWithBody OSTypes.ConsoleUrl)
    | ReceiveDeleteServer
    | ReceiveCreateFloatingIp ErrorContext (Result HttpErrorWithBody OSTypes.FloatingIp)
    | ReceiveServerPassword OSTypes.ServerPassword
    | ReceiveSetServerName String ErrorContext (Result HttpErrorWithBody String)
    | ReceiveSetServerMetadata OSTypes.MetadataItem ErrorContext (Result HttpErrorWithBody (List OSTypes.MetadataItem))
    | ReceiveDeleteServerMetadata OSTypes.MetadataKey ErrorContext (Result HttpErrorWithBody String)
    | ReceiveGuacamoleAuthToken (Result Http.Error GuacTypes.GuacamoleAuthToken)
    | RequestServerAction (Project -> Server -> Cmd Msg) (Maybe (List OSTypes.ServerStatus))
    | ReceiveConsoleLog ErrorContext (Result HttpErrorWithBody String)



{- Resource-Level Types -}


type alias Server =
    { osProps : OSTypes.Server
    , exoProps : ExoServerProps
    , events : WebData (List OSTypes.ServerEvent)
    }


type alias ExoServerProps =
    { floatingIpCreationOption : HelperTypes.FloatingIpOption
    , deletionAttempted : Bool
    , targetOpenstackStatus : Maybe (List OSTypes.ServerStatus) -- Maybe we have performed an instance action and are waiting for server to reflect that
    , serverOrigin : ServerOrigin
    , receivedTime : Maybe Time.Posix -- Used only if this server was polled more recently than the other servers in the project
    , loadingSeparately : Bool -- Again, used only if server was polled more recently on its own.
    }


type ServerOrigin
    = ServerFromExo ServerFromExoProps
    | ServerNotFromExo


type alias ServerFromExoProps =
    { exoServerVersion : ExoServerVersion
    , exoSetupStatus : RDPP.RemoteDataPlusPlus HttpErrorWithBody ExoSetupStatus
    , resourceUsage : ResourceUsageRDPP
    , guacamoleStatus : GuacTypes.ServerGuacamoleStatus
    , exoCreatorUsername : Maybe String
    }


type alias ResourceUsageRDPP =
    RDPP.RemoteDataPlusPlus HttpErrorWithBody Types.ServerResourceUsage.History


type alias ExoServerVersion =
    Int


currentExoServerVersion : ExoServerVersion
currentExoServerVersion =
    4


type ServerUiStatus
    = ServerUiStatusUnknown
    | ServerUiStatusBuilding
    | ServerUiStatusRunningSetup
    | ServerUiStatusReady
    | ServerUiStatusPaused
    | ServerUiStatusUnpausing
    | ServerUiStatusRebooting
    | ServerUiStatusSuspending
    | ServerUiStatusSuspended
    | ServerUiStatusResuming
    | ServerUiStatusShutoff
    | ServerUiStatusStopped
    | ServerUiStatusStarting
    | ServerUiStatusDeleting
    | ServerUiStatusSoftDeleted
    | ServerUiStatusError
    | ServerUiStatusRescued
    | ServerUiStatusShelving
    | ServerUiStatusShelved
    | ServerUiStatusUnshelving
    | ServerUiStatusDeleted


type ExoSetupStatus
    = ExoSetupWaiting
    | ExoSetupRunning
    | ExoSetupComplete
    | ExoSetupError
    | ExoSetupTimeout
    | ExoSetupUnknown



{- More project-y types -}


type alias ProjectName =
    String


type alias ProjectTitle =
    String



{- ??? types -}


type NewServerNetworkOptions
    = NetworksLoading
    | AutoSelectedNetwork OSTypes.NetworkUuid
    | ManualNetworkSelection
    | NoneAvailable



-- REST Types


type HttpRequestMethod
    = Get
    | Post
    | Put
    | Delete


type alias Toast =
    { context : ErrorContext
    , error : String
    }
