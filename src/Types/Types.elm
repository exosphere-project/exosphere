module Types.Types exposing
    ( CockpitLoginStatus(..)
    , CreateServerField(..)
    , CreateServerRequest
    , Creds
    , Endpoints
    , ExoServerProps
    , Flags
    , FloatingIpState(..)
    , GlobalDefaults
    , HttpRequestMethod(..)
    , LoginField(..)
    , Model
    , Msg(..)
    , NewServerNetworkOptions(..)
    , NonProviderViewConstructor(..)
    , PasswordVisibility(..)
    , Provider
    , ProviderName
    , ProviderSpecificMsgConstructor(..)
    , ProviderTitle
    , ProviderViewConstructor(..)
    , Server
    , ServerUiStatus(..)
    , VerboseStatus
    , ViewState(..)
    , WindowSize
    )

import Http
import Json.Decode as Decode
import RemoteData exposing (WebData)
import Time
import Toasty
import Toasty.Defaults
import Types.HelperTypes as HelperTypes
import Types.OpenstackTypes as OSTypes



{- App-Level Types -}


type alias Flags =
    { width : Int
    , height : Int
    , storedState : Maybe Decode.Value
    }


type alias WindowSize =
    { width : Int, height : Int }


type alias Model =
    { messages : List String
    , viewState : ViewState
    , maybeWindowSize : Maybe WindowSize
    , providers : List Provider
    , creds : Creds
    , imageFilterTag : Maybe String
    , globalDefaults : GlobalDefaults
    , toasties : Toasty.Stack Toasty.Defaults.Toast
    }


type alias GlobalDefaults =
    { shellUserData : String
    }


type alias Provider =
    { name : ProviderName
    , creds : Creds
    , auth : OSTypes.AuthToken
    , endpoints : Endpoints
    , images : List OSTypes.Image
    , servers : WebData (List Server)
    , flavors : List OSTypes.Flavor
    , keypairs : List OSTypes.Keypair
    , networks : List OSTypes.Network
    , floatingIps : List OSTypes.IpAddress
    , ports : List OSTypes.Port
    , securityGroups : List OSTypes.SecurityGroup
    , pendingCredentialedRequests : List (OSTypes.AuthTokenString -> Cmd Msg) -- Requests waiting for a valid auth token
    }


type alias Endpoints =
    { glance : HelperTypes.Url
    , nova : HelperTypes.Url
    , neutron : HelperTypes.Url
    }


type Msg
    = Tick Time.Posix
    | SetNonProviderView NonProviderViewConstructor
    | RequestNewProviderToken
    | ReceiveAuthToken Creds (Result Http.Error (Http.Response String))
    | ProviderMsg ProviderName ProviderSpecificMsgConstructor
    | InputLoginField LoginField
    | InputCreateServerField CreateServerRequest CreateServerField
    | InputImageFilterTag String
    | OpenInBrowser String
    | OpenNewWindow String
    | RandomPassword Provider String
    | ToastyMsg (Toasty.Msg Toasty.Defaults.Toast)
    | MsgChangeWindowSize Int Int


type ProviderSpecificMsgConstructor
    = SetProviderView ProviderViewConstructor
    | ValidateTokenForCredentialedRequest (OSTypes.AuthTokenString -> Cmd Msg) Time.Posix
    | RemoveProvider
    | SelectServer Server Bool
    | SelectAllServers Bool
    | RequestServers
    | RequestServerDetail OSTypes.ServerUuid
    | RequestCreateServer CreateServerRequest
    | RequestDeleteServer Server
    | RequestDeleteServers (List Server)
    | ReceiveImages (Result Http.Error (List OSTypes.Image))
    | ReceiveServers (Result Http.Error (List OSTypes.Server))
    | ReceiveServerDetail OSTypes.ServerUuid (Result Http.Error OSTypes.ServerDetails)
    | ReceiveConsoleUrl OSTypes.ServerUuid (Result Http.Error OSTypes.ConsoleUrl)
    | ReceiveCreateServer (Result Http.Error OSTypes.Server)
    | ReceiveDeleteServer OSTypes.ServerUuid (Maybe OSTypes.IpAddressValue) (Result Http.Error String)
    | ReceiveFlavors (Result Http.Error (List OSTypes.Flavor))
    | ReceiveKeypairs (Result Http.Error (List OSTypes.Keypair))
    | ReceiveNetworks (Result Http.Error (List OSTypes.Network))
    | ReceiveFloatingIps (Result Http.Error (List OSTypes.IpAddress))
    | GetFloatingIpReceivePorts OSTypes.ServerUuid (Result Http.Error (List OSTypes.Port))
    | ReceiveCreateFloatingIp OSTypes.ServerUuid (Result Http.Error OSTypes.IpAddress)
    | ReceiveDeleteFloatingIp OSTypes.IpAddressUuid (Result Http.Error String)
    | ReceiveSecurityGroups (Result Http.Error (List OSTypes.SecurityGroup))
    | ReceiveCreateExoSecurityGroup (Result Http.Error OSTypes.SecurityGroup)
    | ReceiveCreateExoSecurityGroupRules (Result Http.Error String)
    | ReceiveCockpitLoginStatus OSTypes.ServerUuid (Result Http.Error String)


type ViewState
    = NonProviderView NonProviderViewConstructor
    | ProviderView ProviderName ProviderViewConstructor


type NonProviderViewConstructor
    = Login
    | MessageLog


type ProviderViewConstructor
    = ListImages
    | ListProviderServers
    | ServerDetail OSTypes.ServerUuid VerboseStatus PasswordVisibility
    | CreateServer CreateServerRequest


type alias VerboseStatus =
    Bool


type PasswordVisibility
    = PasswordShown
    | PasswordHidden


type LoginField
    = AuthUrl String
    | ProjectDomain String
    | ProjectName String
    | UserDomain String
    | Username String
    | Password String
    | OpenRc String


type CreateServerField
    = CreateServerName String
    | CreateServerCount String
    | CreateServerUserData String
    | CreateServerShowAdvancedOptions Bool
    | CreateServerSize String
    | CreateServerKeypairName String
    | CreateServerVolBacked Bool
    | CreateServerVolBackedSize String
    | CreateServerNetworkUuid OSTypes.NetworkUuid


type alias Creds =
    { authUrl : String
    , projectDomain : String
    , projectName : String
    , userDomain : String
    , username : String
    , password : String
    }



-- Resource-Level Types


type alias ExoServerProps =
    { floatingIpState : FloatingIpState
    , selected : Bool
    , cockpitStatus : CockpitLoginStatus
    , deletionAttempted : Bool
    }


type alias Server =
    { osProps : OSTypes.Server
    , exoProps : ExoServerProps
    }


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


type ServerUiStatus
    = ServerUiStatusUnknown
    | ServerUiStatusBuilding
    | ServerUiStatusStarting
    | ServerUiStatusReady
    | ServerUiStatusPaused
    | ServerUiStatusSuspended
    | ServerUiStatusShutoff
    | ServerUiStatusStopped
    | ServerUiStatusSoftDeleted
    | ServerUiStatusError
    | ServerUiStatusRescued
    | ServerUiStatusShelved


type alias CreateServerRequest =
    { name : String
    , providerName : ProviderName
    , imageUuid : OSTypes.ImageUuid
    , imageName : String
    , count : String
    , flavorUuid : OSTypes.FlavorUuid
    , volBacked : Bool
    , volBackedSizeGb : String
    , keypairName : Maybe String
    , userData : String
    , exouserPassword : String
    , networkUuid : OSTypes.NetworkUuid
    , showAdvancedOptions : Bool
    }


type alias ProviderName =
    String


type alias ProviderTitle =
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
    | Delete
