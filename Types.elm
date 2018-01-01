module Types exposing (..)

import Http
import Time


{- App-Level Types -}


type alias Model =
    { authToken : String
    , endpoints : Endpoints
    , creds : Creds
    , messages : List String
    , images : List Image
    , servers : List Server
    , viewState : ViewState
    , flavors : List Flavor
    , keypairs : List Keypair
    , networks : List Network
    , ports : List Port
    }


type Msg
    = Tick Time.Time
    | ChangeViewState ViewState
    | RequestAuth
    | RequestCreateServer CreateServerRequest
    | RequestDeleteServer Server
    | ReceiveAuth (Result Http.Error (Http.Response String))
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
    | InputCreateServerImage CreateServerRequest String
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
    { glance : String
    , nova : String
    , neutron : String
    }



{- Resource-Level Types -}


type alias Image =
    { name : String
    , uuid : ImageUuid
    , size : Int
    , checksum : String
    , diskFormat : String
    , containerFormat : String
    }


type alias ImageUuid =
    Uuid


type alias Server =
    { name : String
    , uuid : ServerUuid
    , details : Maybe ServerDetails
    , floatingIpState : FloatingIpState
    }


type alias ServerUuid =
    Uuid


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
    , imageUuid : ImageUuid
    , count : String
    , flavorUuid : FlavorUuid
    , keypairName : String
    , userData : String
    }


type alias Flavor =
    { uuid : FlavorUuid
    , name : String
    }


type alias FlavorUuid =
    Uuid


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
    Uuid


type alias Port =
    { uuid : PortUuid
    , deviceUuid : ServerUuid
    , adminStateUp : Bool
    , status : String
    }


type alias PortUuid =
    Uuid



{- Helper Types -}


type alias Uuid =
    String
