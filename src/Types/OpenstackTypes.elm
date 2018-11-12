module Types.OpenstackTypes exposing (Endpoint, EndpointInterface(..), Flavor, FlavorUuid, Image, ImageStatus(..), ImageUuid, IpAddress, IpAddressType(..), Keypair, MetadataItem, Network, NetworkUuid, Port, ProjectName, ProjectUuid, SecurityGroup, SecurityGroupRule, SecurityGroupRuleDirection(..), SecurityGroupRuleEthertype(..), SecurityGroupRuleProtocol(..), Server, ServerDetails, ServerPowerState(..), ServerStatus(..), ServerUuid, Service, ServiceCatalog, ServiceName(..), TokenDetails, UserName, UserUuid)

import Types.HelperTypes as HelperTypes



{-
   Types that match structure of data returned from OpenStack API, used for
   decoding JSON and representing state of OpenStack resources.
-}


type alias MetadataItem =
    { key : String
    , value : String
    }



-- Keystone


type alias TokenDetails =
    {- This is currently used as an intermediary type by Rest.createProvider, candidate for future refactor -}
    { catalog : ServiceCatalog
    , projectUuid : ProjectUuid
    , projectName : ProjectName
    , userUuid : UserUuid
    , userName : UserName
    }


type alias ProjectUuid =
    HelperTypes.Uuid


type alias ProjectName =
    String


type alias UserUuid =
    HelperTypes.Uuid


type alias UserName =
    String


type alias ServiceCatalog =
    List Service


type alias Service =
    { name : String
    , type_ : String
    , endpoints : List Endpoint
    }


type ServiceName
    = Glance
    | Nova
    | Neutron


type alias Endpoint =
    { interface : EndpointInterface
    , url : HelperTypes.Url
    }


type EndpointInterface
    = Public
    | Admin
    | Internal



-- Glance


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
    = ImageQueued
    | ImageSaving
    | ImageActive
    | ImageKilled
    | ImageDeleted
    | ImagePendingDelete
    | ImageDeactivated



-- Nova


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


type alias Server =
    { name : String
    , uuid : ServerUuid
    , details : Maybe ServerDetails
    }


type alias ServerUuid =
    HelperTypes.Uuid


type ServerStatus
    = ServerPaused
    | ServerSuspended
    | ServerActive
    | ServerShutoff
    | ServerStopped
    | ServerSoftDeleted
    | ServerError
    | ServerBuilding
    | ServerRescued
    | ServerShelved
    | ServerShelvedOffloaded


type ServerPowerState
    = PowerNoState
    | PowerRunning
    | PowerPaused
    | PowerShutdown
    | PowerCrashed
    | PowerSuspended



{- Todo add to ServerDetail:
   - Metadata
   - Volumes
   - Security Groups
   - Etc

   Also, make keypairName a key type, created a real date/time, etc
-}


type alias ServerDetails =
    { openstackStatus : ServerStatus
    , created : String
    , powerState : ServerPowerState
    , imageUuid : ImageUuid
    , flavorUuid : FlavorUuid
    , keypairName : Maybe String
    , ipAddresses : List IpAddress
    , metadata : List MetadataItem
    }



-- Neutron


type alias IpAddress =
    { address : String
    , openstackType : IpAddressType
    }


type IpAddressType
    = IpAddressFixed
    | IpAddressFloating


type alias Keypair =
    { name : String
    , publicKey : String
    , fingerprint : String
    }


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
