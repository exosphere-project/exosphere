module Types.Types exposing (..)

import Http
import Time
import Types.HelperTypes as HelperTypes


{- App-Level Types -}


type alias Model =
    { messages : List String
    , viewState : ViewState
    , selectedProvider : Provider
    , otherProviders : List Provider
    , creds : Creds
    }


type alias Provider =
    { name : ProviderName
    , authToken : AuthToken
    , endpoints : Endpoints
    , images : List Image
    , servers : List Server
    , flavors : List Flavor
    , keypairs : List Keypair
    , networks : List Network
    , ports : List Port
    }


type Msg
    = Tick Time.Time
    | ChangeViewState ViewState
    | RequestNewProviderToken
    | SelectProvider ProviderName
    | SelectServer Server Bool
    | SelectAllServers Bool
    | RequestCreateServer CreateServerRequest
    | RequestDeleteServer Server
    | RequestDeleteServers (List Server)
    | ReceiveAuthToken (Result Http.Error (Http.Response String))
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
    | InputAuthURL String
    | InputProjectDomain String
    | InputProjectName String
    | InputUserDomain String
    | InputUsername String
    | InputPassword String
    | InputCreateServerName CreateServerRequest String
    | InputCreateServerCount CreateServerRequest String
    | InputCreateServerUserData CreateServerRequest String
    | InputCreateServerSize CreateServerRequest String
    | InputCreateServerKeypairName CreateServerRequest String


type ViewState
    = Login
    | Home
    | ListImages
    | ListUserServers
    | ServerDetail ServerUuid
    | CreateServer CreateServerRequest


type alias Creds =
    { authURL : String
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
    , diskFormat : String
    , containerFormat : String
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



{- Todo add to ServerDetail:
   - Metadata
   - Volumes
   - Security Groups
   - Etc

   Also, make keypairName a key type, created a real date/time, etc
-}


type alias ServerDetails =
    { status : String
    , created : String
    , powerState : ServerPowerState
    , imageUuid : ImageUuid
    , flavorUuid : FlavorUuid
    , keypairName : String
    , ipAddresses : List IpAddress
    }


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
    , keypairName : String
    , userData : String
    }


type alias ProviderName =
    String


type alias AuthToken =
    String


type alias Flavor =
    { uuid : FlavorUuid
    , name : String
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
