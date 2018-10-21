module Types.Types exposing (AuthToken, CockpitStatus(..), CreateServerField(..), CreateServerRequest, Creds, Endpoints, Flavor, FlavorUuid, FloatingIpState(..), GlobalDefaults, Image, ImageStatus(..), ImageUuid, IpAddress, IpAddressOpenstackType(..), Keypair, LoginField(..), Model, Msg(..), Network, NetworkUuid, NonProviderViewConstructor(..), Port, PortUuid, Provider, ProviderName, ProviderSpecificMsgConstructor(..), ProviderTitle, ProviderViewConstructor(..), SecurityGroup, SecurityGroupRule, SecurityGroupRuleDirection(..), SecurityGroupRuleEthertype(..), SecurityGroupRuleProtocol(..), SecurityGroupRuleUuid, SecurityGroupUuid, Server, ServerDetails, ServerOpenstackStatus(..), ServerPowerState(..), ServerUiStatus(..), ServerUuid, ViewState(..))

import Http
import Maybe
import RemoteData exposing (WebData)
import Time
import Types.HelperTypes as HelperTypes



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
    , endpoints : Endpoints
    , images : List Image
    , servers : WebData (List Server)
    , flavors : List Flavor
    , keypairs : List Keypair
    , networks : List Network
    , ports : List Port
    , securityGroups : List SecurityGroup
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


type ProviderSpecificMsgConstructor
    = SetProviderView ProviderViewConstructor
    | SelectServer Server Bool
    | SelectAllServers Bool
    | RequestServers
    | RequestServerDetail ServerUuid
    | RequestCreateServer CreateServerRequest
    | RequestDeleteServer Server
    | RequestDeleteServers (List Server)
    | ReceiveImages (Result Http.Error (List Image))
    | ReceiveServers (Result Http.Error (List Server))
    | ReceiveServerDetail ServerUuid (Result Http.Error ServerDetails)
    | ReceiveCreateServer (Result Http.Error Server)
    | ReceiveDeleteServer (Result Http.Error String)
    | ReceiveFlavors (Result Http.Error (List Flavor))
    | ReceiveKeypairs (Result Http.Error (List Keypair))
    | ReceiveNetworks (Result Http.Error (List Network))
    | GetFloatingIpReceivePorts ServerUuid (Result Http.Error (List Port))
    | ReceiveFloatingIp ServerUuid (Result Http.Error IpAddress)
    | ReceiveSecurityGroups (Result Http.Error (List SecurityGroup))
    | ReceiveCreateExoSecurityGroup (Result Http.Error SecurityGroup)
    | ReceiveCreateExoSecurityGroupRules (Result Http.Error String)
    | ReceiveCockpitStatus ServerUuid (Result Http.Error CockpitStatus)


type ViewState
    = NonProviderView NonProviderViewConstructor
    | ProviderView ProviderName ProviderViewConstructor


type NonProviderViewConstructor
    = Login


type ProviderViewConstructor
    = ListImages
    | ListProviderServers
    | ServerDetail ServerUuid
    | CreateServer CreateServerRequest


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


type alias Creds =
    { authUrl : String
    , projectDomain : String
    , projectName : String
    , userDomain : String
    , username : String
    , password : String
    }


type alias Endpoints =
    { glance : HelperTypes.Url
    , nova : HelperTypes.Url
    , neutron : HelperTypes.Url
    }



{- Resource-Level Types -}


type alias Image =
    { name : String
    , status : ImageStatus
    , uuid : ImageUuid
    , size : Maybe Int
    , checksum : Maybe String
    , diskFormat : Maybe String
    , containerFormat : Maybe String
    , tags : List String
    }


type alias ImageUuid =
    HelperTypes.Uuid


type ImageStatus
    = Queued
    | Saving
    | Active
    | Killed
    | Deleted
    | PendingDelete
    | Deactivated


type alias Server =
    { name : String
    , uuid : ServerUuid
    , details : Maybe ServerDetails
    , floatingIpState : FloatingIpState
    , selected : Bool
    , cockpitStatus : CockpitStatus
    , deletionAttempted : Bool
    }


type alias ServerUuid =
    HelperTypes.Uuid


type FloatingIpState
    = Unknown
    | NotRequestable
    | Requestable
    | RequestedWaiting
    | Success
    | Failed


type CockpitStatus
    = NotChecked
    | CheckedNotReady
    | Ready
    | Error


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



{- Todo add to ServerDetail:
   - Metadata
   - Volumes
   - Security Groups
   - Etc

   Also, make keypairName a key type, created a real date/time, etc
-}


type alias ServerDetails =
    { openstackStatus : ServerOpenstackStatus
    , created : String
    , powerState : ServerPowerState
    , imageUuid : ImageUuid
    , flavorUuid : FlavorUuid
    , keypairName : String
    , ipAddresses : List IpAddress
    }


type ServerOpenstackStatus
    = ServerOSStatusPaused
    | ServerOSStatusSuspended
    | ServerOSStatusActive
    | ServerOSStatusShutoff
    | ServerOSStatusStopped
    | ServerOSStatusSoftDeleted
    | ServerOSStatusError
    | ServerOSStatusBuilding
    | ServerOSStatusRescued


type ServerPowerState
    = NoState
    | Running
    | Paused
    | Shutdown
    | Crashed
    | Suspended


type alias CreateServerRequest =
    { name : String
    , providerName : ProviderName
    , imageUuid : ImageUuid
    , imageName : String
    , count : String
    , flavorUuid : FlavorUuid
    , volBacked : Bool
    , volBackedSizeGb : String
    , keypairName : String
    , userData : String
    }


type alias ProviderName =
    String


type alias ProviderTitle =
    String


type alias AuthToken =
    String


type alias Flavor =
    { uuid : FlavorUuid
    , name : String
    , vcpu : Int
    , ram_mb : Int
    , disk_root : Int
    , disk_ephemeral : Int
    }


type alias FlavorUuid =
    HelperTypes.Uuid


type alias Keypair =
    { name : String
    , publicKey : String
    , fingerprint : String
    }


type alias IpAddress =
    { address : String
    , openstackType : IpAddressOpenstackType
    }


type IpAddressOpenstackType
    = Fixed
    | Floating


type alias Network =
    { uuid : NetworkUuid
    , name : String
    , adminStateUp : Bool
    , status : String
    , isExternal : Bool
    }


type alias NetworkUuid =
    HelperTypes.Uuid


type alias Port =
    { uuid : PortUuid
    , deviceUuid : ServerUuid
    , adminStateUp : Bool
    , status : String
    }


type alias PortUuid =
    HelperTypes.Uuid


type alias SecurityGroup =
    { uuid : SecurityGroupUuid
    , name : String
    , description : Maybe String
    , rules : List SecurityGroupRule
    }


type alias SecurityGroupUuid =
    HelperTypes.Uuid


type alias SecurityGroupRule =
    { uuid : SecurityGroupRuleUuid
    , ethertype : SecurityGroupRuleEthertype
    , direction : SecurityGroupRuleDirection
    , protocol : Maybe SecurityGroupRuleProtocol
    , port_range_min : Maybe Int
    , port_range_max : Maybe Int
    , remoteGroupUuid : Maybe SecurityGroupRuleUuid
    }


type alias SecurityGroupRuleUuid =
    HelperTypes.Uuid


type SecurityGroupRuleDirection
    = Ingress
    | Egress


type SecurityGroupRuleEthertype
    = Ipv4
    | Ipv6


type SecurityGroupRuleProtocol
    = AnyProtocol
    | Icmp
    | Icmpv6
    | Tcp
    | Udp
