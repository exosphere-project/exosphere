module Types.Types exposing (AuthToken, CockpitLoginStatus(..), CreateServerField(..), CreateServerRequest, Creds, Endpoints, ExoServerProps, FloatingIpState(..), GlobalDefaults, LoginField(..), Model, Msg(..), NewServerNetworkOptions(..), NonProviderViewConstructor(..), Provider, ProviderName, ProviderSpecificMsgConstructor(..), ProviderTitle, ProviderViewConstructor(..), Server, ServerUiStatus(..), VerboseStatus, ViewState(..))

import Http
import Maybe
import RemoteData exposing (WebData)
import Time
import Types.HelperTypes as HelperTypes
import Types.OpenstackTypes as OSTypes



{- App-Level Types -}


type alias Model =
    { messages : List String
    , viewState : ViewState
    , providers : List Provider
    , creds : Creds
    , imageFilterTag : Maybe String
    , globalDefaults : GlobalDefaults
    }


type alias GlobalDefaults =
    { shellUserData : String
    }


type alias Provider =
    { name : ProviderName
    , authToken : AuthToken
    , projectUuid : OSTypes.ProjectUuid
    , projectName : OSTypes.ProjectName
    , userUuid : OSTypes.UserUuid
    , userName : OSTypes.UserName
    , endpoints : Endpoints
    , images : List OSTypes.Image
    , servers : WebData (List Server)
    , flavors : List OSTypes.Flavor
    , keypairs : List OSTypes.Keypair
    , networks : List OSTypes.Network
    , ports : List OSTypes.Port
    , securityGroups : List OSTypes.SecurityGroup
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
    | ReceiveAuthToken (Result Http.Error (Http.Response String))
    | ProviderMsg ProviderName ProviderSpecificMsgConstructor
    | InputLoginField LoginField
    | InputCreateServerField CreateServerRequest CreateServerField
    | InputImageFilterTag String
    | OpenInBrowser String
    | OpenNewWindow String
    | RandomPassword Provider String


type ProviderSpecificMsgConstructor
    = SetProviderView ProviderViewConstructor
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
    | ReceiveCreateServer (Result Http.Error OSTypes.Server)
    | ReceiveDeleteServer (Result Http.Error String)
    | ReceiveFlavors (Result Http.Error (List OSTypes.Flavor))
    | ReceiveKeypairs (Result Http.Error (List OSTypes.Keypair))
    | ReceiveNetworks (Result Http.Error (List OSTypes.Network))
    | GetFloatingIpReceivePorts OSTypes.ServerUuid (Result Http.Error (List OSTypes.Port))
    | ReceiveFloatingIp OSTypes.ServerUuid (Result Http.Error OSTypes.IpAddress)
    | ReceiveSecurityGroups (Result Http.Error (List OSTypes.SecurityGroup))
    | ReceiveCreateExoSecurityGroup (Result Http.Error OSTypes.SecurityGroup)
    | ReceiveCreateExoSecurityGroupRules (Result Http.Error String)
    | ReceiveCockpitLoginStatus OSTypes.ServerUuid (Result Http.Error String)


type ViewState
    = NonProviderView NonProviderViewConstructor
    | ProviderView ProviderName ProviderViewConstructor


type NonProviderViewConstructor
    = Login


type ProviderViewConstructor
    = ListImages
    | ListProviderServers
    | ServerDetail OSTypes.ServerUuid VerboseStatus
    | CreateServer CreateServerRequest


type alias VerboseStatus =
    Bool


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


type alias CreateServerRequest =
    { name : String
    , providerName : ProviderName
    , imageUuid : OSTypes.ImageUuid
    , imageName : String
    , count : String
    , flavorUuid : OSTypes.FlavorUuid
    , volBacked : Bool
    , volBackedSizeGb : String
    , keypairName : String
    , userData : String
    , exouserPassword : String
    , networkUuid : OSTypes.NetworkUuid
    }


type alias ProviderName =
    String


type alias ProviderTitle =
    String


type alias AuthToken =
    String


type NewServerNetworkOptions
    = NoNetsAutoAllocate
    | OneNet OSTypes.Network
    | MultipleNetsWithGuess (List OSTypes.Network) OSTypes.Network GoodGuess


type alias GoodGuess =
    Bool
