module Types.Types exposing
    ( CockpitLoginStatus(..)
    , CreateServerRequest
    , DeleteConfirmation
    , DeleteVolumeConfirmation
    , Endpoints
    , ExoServerProps
    , Flags
    , FloatingIpState(..)
    , GlobalDefaults
    , HttpRequestMethod(..)
    , IPInfoLevel(..)
    , ImageListViewParams
    , JetstreamCreds
    , JetstreamProvider(..)
    , LogMessage
    , Model
    , Msg(..)
    , NewServerNetworkOptions(..)
    , NonProjectViewConstructor(..)
    , PasswordVisibility(..)
    , Project
    , ProjectIdentifier
    , ProjectName
    , ProjectSecret(..)
    , ProjectSpecificMsgConstructor(..)
    , ProjectTitle
    , ProjectViewConstructor(..)
    , ProjectViewParams
    , Server
    , ServerDetailViewParams
    , ServerFromExoProps
    , ServerListViewParams
    , ServerOrigin(..)
    , ServerSelection
    , ServerUiStatus(..)
    , SortTableParams
    , TickInterval
    , Toast
    , UnscopedProvider
    , UnscopedProviderProject
    , VerboseStatus
    , ViewState(..)
    , WindowSize
    , currentExoServerVersion
    )

import Helpers.Error exposing (ErrorContext, HttpErrorWithBody)
import Helpers.RemoteDataPlusPlus as RDPP
import Http
import Json.Decode as Decode
import OpenStack.Types as OSTypes
import RemoteData exposing (WebData)
import Set
import Time
import Toasty
import Types.HelperTypes as HelperTypes
import UUID



{- App-Level Types -}


type alias Flags =
    { width : Int
    , height : Int
    , storedState : Maybe Decode.Value
    , proxyUrl : Maybe HelperTypes.Url
    , isElectron : Bool
    , randomSeed0 : Int
    , randomSeed1 : Int
    , randomSeed2 : Int
    , randomSeed3 : Int
    , epoch : Int
    , showDebugMsgs : Bool
    }


type alias WindowSize =
    { width : Int
    , height : Int
    }


type alias Model =
    { logMessages : List LogMessage
    , viewState : ViewState
    , maybeWindowSize : Maybe WindowSize
    , unscopedProviders : List UnscopedProvider
    , projects : List Project
    , globalDefaults : GlobalDefaults
    , toasties : Toasty.Stack Toast
    , proxyUrl : Maybe HelperTypes.Url
    , isElectron : Bool
    , clientUuid : UUID.UUID
    , clientCurrentTime : Time.Posix
    , showDebugMsgs : Bool
    }


type alias LogMessage =
    { message : String
    , context : ErrorContext
    , timestamp : Time.Posix
    }


type alias GlobalDefaults =
    { shellUserData : String
    }


type alias UnscopedProvider =
    { authUrl : OSTypes.KeystoneUrl
    , keystonePassword : HelperTypes.Password
    , token : OSTypes.UnscopedAuthToken
    , projectsAvailable : WebData (List UnscopedProviderProject)
    }


type alias UnscopedProviderProject =
    { name : ProjectName
    , description : String
    , domainId : HelperTypes.Uuid
    , enabled : Bool
    }


type alias Project =
    { secret : ProjectSecret
    , auth : OSTypes.ScopedAuthToken
    , endpoints : Endpoints
    , images : List OSTypes.Image
    , servers : RDPP.RemoteDataPlusPlus HttpErrorWithBody (List Server)
    , flavors : List OSTypes.Flavor
    , keypairs : List OSTypes.Keypair
    , volumes : WebData (List OSTypes.Volume)
    , networks : RDPP.RemoteDataPlusPlus HttpErrorWithBody (List OSTypes.Network)
    , floatingIps : List OSTypes.IpAddress
    , ports : RDPP.RemoteDataPlusPlus HttpErrorWithBody (List OSTypes.Port)
    , securityGroups : List OSTypes.SecurityGroup
    , computeQuota : WebData OSTypes.ComputeQuota
    , volumeQuota : WebData OSTypes.VolumeQuota
    , pendingCredentialedRequests : List (OSTypes.AuthTokenString -> Cmd Msg) -- Requests waiting for a valid auth token
    }


type alias ProjectIdentifier =
    -- We use this when referencing a Project in a Msg (or otherwise passing through the runtime)
    { name : ProjectName
    , authUrl : HelperTypes.Url
    }


type ProjectSecret
    = OpenstackPassword HelperTypes.Password
    | ApplicationCredential OSTypes.ApplicationCredential


type alias Endpoints =
    { cinder : HelperTypes.Url
    , glance : HelperTypes.Url
    , keystone : HelperTypes.Url
    , nova : HelperTypes.Url
    , neutron : HelperTypes.Url
    }


type Msg
    = Tick TickInterval Time.Posix
    | DoOrchestration Time.Posix
    | SetNonProjectView NonProjectViewConstructor
    | HandleApiErrorWithBody ErrorContext HttpErrorWithBody
    | RequestUnscopedToken OSTypes.OpenstackLogin
    | RequestNewProjectToken OSTypes.OpenstackLogin
    | JetstreamLogin JetstreamCreds
    | ReceiveScopedAuthToken (Maybe HelperTypes.Password) ( Http.Metadata, String )
    | ReceiveUnscopedAuthToken OSTypes.KeystoneUrl HelperTypes.Password ( Http.Metadata, String )
    | ReceiveUnscopedProjects OSTypes.KeystoneUrl (List UnscopedProviderProject)
    | RequestProjectLoginFromProvider OSTypes.KeystoneUrl HelperTypes.Password (List UnscopedProviderProject)
    | ProjectMsg ProjectIdentifier ProjectSpecificMsgConstructor
    | InputOpenRc OSTypes.OpenstackLogin String
    | OpenInBrowser String
    | OpenNewWindow String
    | ToastyMsg (Toasty.Msg Toast)
    | NewLogMessage LogMessage
    | MsgChangeWindowSize Int Int
    | NoOp


type alias TickInterval =
    Int


type ProjectSpecificMsgConstructor
    = SetProjectView ProjectViewConstructor
    | ReceiveAppCredential OSTypes.ApplicationCredential
    | PrepareCredentialedRequest (Maybe HelperTypes.Url -> OSTypes.AuthTokenString -> Cmd Msg) Time.Posix
    | RequestAppCredential Time.Posix
    | ToggleCreatePopup
    | RemoveProject
    | RequestServers
    | RequestServer OSTypes.ServerUuid
    | RequestCreateServer CreateServerRequest
    | RequestDeleteServer OSTypes.ServerUuid
    | RequestDeleteServers (List OSTypes.ServerUuid)
    | RequestServerAction Server (Project -> Server -> Cmd Msg) (Maybe (List OSTypes.ServerStatus))
    | RequestCreateVolume OSTypes.VolumeName OSTypes.VolumeSize
    | RequestDeleteVolume OSTypes.VolumeUuid
    | RequestAttachVolume OSTypes.ServerUuid OSTypes.VolumeUuid
    | RequestDetachVolume OSTypes.VolumeUuid
    | RequestCreateServerImage OSTypes.ServerUuid String
    | ReceiveImages (List OSTypes.Image)
    | ReceiveServer OSTypes.ServerUuid ErrorContext (Result HttpErrorWithBody OSTypes.Server)
    | ReceiveServers ErrorContext (Result HttpErrorWithBody (List OSTypes.Server))
    | ReceiveConsoleUrl OSTypes.ServerUuid (Result HttpErrorWithBody OSTypes.ConsoleUrl)
    | ReceiveCreateServer OSTypes.ServerUuid
    | ReceiveDeleteServer OSTypes.ServerUuid (Maybe OSTypes.IpAddressValue)
    | ReceiveFlavors (List OSTypes.Flavor)
    | ReceiveKeypairs (List OSTypes.Keypair)
    | ReceiveNetworks ErrorContext (Result HttpErrorWithBody (List OSTypes.Network))
    | ReceiveFloatingIps (List OSTypes.IpAddress)
    | ReceivePorts ErrorContext (Result HttpErrorWithBody (List OSTypes.Port))
    | ReceiveCreateFloatingIp OSTypes.ServerUuid OSTypes.IpAddress
    | ReceiveDeleteFloatingIp OSTypes.IpAddressUuid
    | ReceiveSecurityGroups (List OSTypes.SecurityGroup)
    | ReceiveCreateExoSecurityGroup OSTypes.SecurityGroup
    | ReceiveCockpitLoginStatus OSTypes.ServerUuid (Result Http.Error String)
    | ReceiveCreateVolume
    | ReceiveVolumes (List OSTypes.Volume)
    | ReceiveDeleteVolume
    | ReceiveUpdateVolumeName
    | ReceiveAttachVolume OSTypes.VolumeAttachment
    | ReceiveDetachVolume
    | ReceiveComputeQuota OSTypes.ComputeQuota
    | ReceiveVolumeQuota OSTypes.VolumeQuota
    | ReceiveServerPassword OSTypes.ServerUuid OSTypes.ServerPassword
    | ReceiveConsoleLog OSTypes.ServerUuid String


type ViewState
    = NonProjectView NonProjectViewConstructor
    | ProjectView ProjectIdentifier ProjectViewParams ProjectViewConstructor


type NonProjectViewConstructor
    = LoginPicker
    | LoginOpenstack OSTypes.OpenstackLogin
    | LoginJetstream JetstreamCreds
    | SelectProjects OSTypes.KeystoneUrl (List UnscopedProviderProject)
    | MessageLog
    | HelpAbout


type alias ImageListViewParams =
    { searchText : String
    , tags : Set.Set String
    , onlyOwnImages : Bool
    , expandImageDetails : Set.Set OSTypes.ImageUuid
    }


type alias SortTableParams =
    { title : String
    , asc : Bool
    }


type alias ProjectViewParams =
    { createPopup : Bool
    }


type ProjectViewConstructor
    = ListImages ImageListViewParams SortTableParams
    | ListProjectServers ServerListViewParams
    | ListProjectVolumes (List DeleteVolumeConfirmation)
    | ServerDetail OSTypes.ServerUuid ServerDetailViewParams
    | CreateServerImage OSTypes.ServerUuid String
    | VolumeDetail OSTypes.VolumeUuid (List DeleteVolumeConfirmation)
    | CreateServer CreateServerRequest
    | CreateVolume OSTypes.VolumeName String
    | AttachVolumeModal (Maybe OSTypes.ServerUuid) (Maybe OSTypes.VolumeUuid)
    | MountVolInstructions OSTypes.VolumeAttachment


type alias ServerListViewParams =
    { onlyOwnServers : Bool
    , selectedServers : Set.Set ServerSelection
    , deleteConfirmations : List DeleteConfirmation
    }


type alias ServerDetailViewParams =
    { verboseStatus : VerboseStatus
    , passwordVisibility : PasswordVisibility
    , ipInfoLevel : IPInfoLevel
    , serverActionNamePendingConfirmation : Maybe String
    }


type alias ServerSelection =
    OSTypes.ServerUuid


type alias DeleteConfirmation =
    OSTypes.ServerUuid


type alias DeleteVolumeConfirmation =
    OSTypes.VolumeUuid


type IPInfoLevel
    = IPDetails
    | IPSummary


type alias VerboseStatus =
    Bool


type PasswordVisibility
    = PasswordShown
    | PasswordHidden


type alias JetstreamCreds =
    { jetstreamProviderChoice : JetstreamProvider
    , jetstreamProjectName : String
    , taccUsername : String
    , taccPassword : String
    }


type JetstreamProvider
    = IUCloud
    | TACCCloud
    | BothJetstreamClouds



-- Resource-Level Types


type alias Server =
    { osProps : OSTypes.Server
    , exoProps : ExoServerProps
    }


type alias ExoServerProps =
    { priorFloatingIpState : FloatingIpState
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
    , cockpitStatus : CockpitLoginStatus
    }


type alias ExoServerVersion =
    Int


currentExoServerVersion : ExoServerVersion
currentExoServerVersion =
    1


type FloatingIpState
    = Unknown
    | NotRequestable
    | Requestable
    | RequestedWaiting
    | Success
    | Failed


type CockpitLoginStatus
    = NotChecked
    | CheckedNotReady
    | Ready
    | ReadyButRecheck


type ServerUiStatus
    = ServerUiStatusUnknown
    | ServerUiStatusBuilding
    | ServerUiStatusPartiallyActive
    | ServerUiStatusReady
    | ServerUiStatusPaused
    | ServerUiStatusReboot
    | ServerUiStatusSuspended
    | ServerUiStatusShutoff
    | ServerUiStatusStopped
    | ServerUiStatusSoftDeleted
    | ServerUiStatusError
    | ServerUiStatusRescued
    | ServerUiStatusShelved
    | ServerUiStatusDeleted


type alias CreateServerRequest =
    { name : String
    , projectId : ProjectIdentifier
    , imageUuid : OSTypes.ImageUuid
    , imageName : String
    , count : Int
    , flavorUuid : OSTypes.FlavorUuid
    , volBackedSizeGb : Maybe Int
    , keypairName : Maybe String
    , userData : String
    , networkUuid : OSTypes.NetworkUuid
    , showAdvancedOptions : Bool
    }


type alias ProjectName =
    String


type alias ProjectTitle =
    String


type NewServerNetworkOptions
    = NoNetsAutoAllocate
    | OneNet OSTypes.Network
    | MultipleNetsWithGuess (List OSTypes.Network) OSTypes.Network GoodGuess


type alias GoodGuess =
    Bool



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
