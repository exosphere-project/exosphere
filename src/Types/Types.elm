module Types.Types exposing (..)

import Http
import Time
import Types.HelperTypes as HelperTypes


{- App-Level Types -}


type alias Model =
    { messages : List String
    , viewState : ViewState
    , providers : List Provider
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
    | SelectServer ProviderName Server Bool
    | SelectAllServers ProviderName Bool
    | RequestCreateServer ProviderName CreateServerRequest
    | RequestDeleteServer ProviderName Server
    | RequestDeleteServers ProviderName (List Server)
    | ReceiveAuthToken (Result Http.Error (Http.Response String))
    | ReceiveImages ProviderName (Result Http.Error (List Image))
    | ReceiveServers ProviderName (Result Http.Error (List Server))
    | ReceiveServerDetail ProviderName ServerUuid (Result Http.Error ServerDetails)
    | ReceiveCreateServer ProviderName (Result Http.Error Server)
    | ReceiveDeleteServer ProviderName (Result Http.Error String)
    | ReceiveFlavors ProviderName (Result Http.Error (List Flavor))
    | ReceiveKeypairs ProviderName (Result Http.Error (List Keypair))
    | ReceiveNetworks ProviderName (Result Http.Error (List Network))
    | GetFloatingIpReceivePorts ProviderName ServerUuid (Result Http.Error (List Port))
    | ReceiveFloatingIp ProviderName ServerUuid (Result Http.Error IpAddress)
    | InputAuthURL String
    | InputProjectDomain String
    | InputProjectName String
    | InputUserDomain String
    | InputUsername String
    | InputPassword String
    | InputCreateServerName ProviderName CreateServerRequest String
    | InputCreateServerCount ProviderName CreateServerRequest String
    | InputCreateServerUserData ProviderName CreateServerRequest String
    | InputCreateServerSize ProviderName CreateServerRequest String
    | InputCreateServerKeypairName ProviderName CreateServerRequest String


type ViewState
    = Login
    | Home
    | ListImages ProviderName
    | ListUserServers ProviderName
    | ServerDetail ProviderName ServerUuid
    | CreateServer ProviderName CreateServerRequest


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
   - Flavor
   - Image
   - Metadata
   - Volumes
   - Security Groups
   - Etc

   Also, make status and powerState union types, keypairName a key type, created a real date/time, etc
-}


type alias ServerDetails =
    { status : String
    , created : String
    , powerState : Int
    , keypairName : String
    , ipAddresses : List IpAddress
    }


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
