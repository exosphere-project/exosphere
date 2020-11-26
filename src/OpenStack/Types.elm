module OpenStack.Types exposing
    ( ApplicationCredential
    , ApplicationCredentialSecret
    , ApplicationCredentialUuid
    , AuthTokenString
    , ComputeQuota
    , ConsoleUrl
    , CreateServerRequest
    , CreateVolumeRequest
    , CredentialsForAuthToken(..)
    , Endpoint
    , EndpointInterface(..)
    , Flavor
    , FlavorUuid
    , Image
    , ImageStatus(..)
    , ImageUuid
    , IpAddress
    , IpAddressType(..)
    , IpAddressUuid
    , IpAddressValue
    , Keypair
    , KeystoneUrl
    , MetadataItem
    , NameAndUuid
    , Network
    , NetworkUuid
    , OpenstackLogin
    , Port
    , QuotaItemDetail
    , ScopedAuthToken
    , SecurityGroup
    , Server
    , ServerDetails
    , ServerLockStatus(..)
    , ServerPassword
    , ServerPowerState(..)
    , ServerStatus(..)
    , ServerUuid
    , Service
    , ServiceCatalog
    , SynchronousAPIError
    , UnscopedAuthToken
    , UserUuid
    , Volume
    , VolumeAttachment
    , VolumeAttachmentDevice
    , VolumeName
    , VolumeQuota
    , VolumeSize
    , VolumeStatus(..)
    , VolumeUuid
    )

import Json.Encode
import OpenStack.SecurityGroupRule exposing (SecurityGroupRule)
import RemoteData exposing (RemoteData)
import Time
import Types.Error exposing (HttpErrorWithBody)
import Types.HelperTypes as HelperTypes



{-
   Types that match structure of data returned from OpenStack API, used for
   decoding JSON and representing state of OpenStack resources.
-}


type alias MetadataItem =
    { key : String
    , value : String
    }


type alias QuotaItemDetail =
    -- OpenStack uses -1 for "no limit", but we'll use Nothing for that case
    { inUse : Int
    , limit : Maybe Int
    }


type alias SynchronousAPIError =
    { message : String
    , code : Int
    }



-- Keystone


type alias KeystoneUrl =
    HelperTypes.Url


type alias ScopedAuthToken =
    -- Todo re-order these so it is consistent with the order in UnscopedAuthToken?
    { catalog : ServiceCatalog
    , project : NameAndUuid
    , projectDomain : NameAndUuid
    , user : NameAndUuid
    , userDomain : NameAndUuid
    , expiresAt : Time.Posix
    , tokenValue : AuthTokenString
    }


type alias UnscopedAuthToken =
    { user : NameAndUuid
    , userDomain : NameAndUuid
    , expiresAt : Time.Posix
    , tokenValue : AuthTokenString
    }


type alias AuthTokenString =
    String


type alias ApplicationCredential =
    { uuid : ApplicationCredentialUuid
    , secret : ApplicationCredentialSecret
    }


type alias ApplicationCredentialUuid =
    String


type alias ApplicationCredentialSecret =
    String


type alias NameAndUuid =
    { name : String
    , uuid : HelperTypes.Uuid
    }


type alias UserUuid =
    HelperTypes.Uuid


type alias ServiceCatalog =
    List Service


type alias Service =
    { name : String
    , type_ : String
    , endpoints : List Endpoint
    }


type alias Endpoint =
    { interface : EndpointInterface
    , url : HelperTypes.Url
    }


type EndpointInterface
    = Public
    | Admin
    | Internal


type alias OpenstackLogin =
    { authUrl : KeystoneUrl
    , projectDomain : String
    , projectName : String
    , userDomain : String
    , username : String
    , password : String
    }


type CredentialsForAuthToken
    = PasswordCreds OpenstackLogin
      -- String is a project name
    | AppCreds KeystoneUrl String ApplicationCredential



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
    , projectUuid : HelperTypes.Uuid
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


type alias Keypair =
    { name : String
    , publicKey : String
    , fingerprint : String
    }


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
    , details : ServerDetails
    , consoleUrl : RemoteData HttpErrorWithBody ConsoleUrl
    }


type alias ServerUuid =
    HelperTypes.Uuid


type alias ConsoleUrl =
    HelperTypes.Url


type ServerStatus
    = ServerPaused
    | ServerSuspended
    | ServerActive
    | ServerReboot
    | ServerShutoff
    | ServerStopped
    | ServerSoftDeleted
    | ServerError
    | ServerBuilding
    | ServerRescued
    | ServerShelved
    | ServerShelvedOffloaded
    | ServerDeleted


type ServerPowerState
    = PowerNoState
    | PowerRunning
    | PowerPaused
    | PowerShutdown
    | PowerCrashed
    | PowerSuspended


type ServerLockStatus
    = ServerLocked
    | ServerUnlocked



{- Todo add to ServerDetail:
   - Metadata
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
    , userUuid : UserUuid
    , volumesAttached : List VolumeUuid
    , tags : List ServerTag
    , lockStatus : ServerLockStatus
    }


type alias ComputeQuota =
    { cores : QuotaItemDetail
    , instances : QuotaItemDetail
    , ram : QuotaItemDetail
    }


type alias ServerTag =
    String


type alias ServerPassword =
    String


type alias CreateServerRequest =
    { name : String
    , count : Int
    , imageUuid : ImageUuid
    , flavorUuid : FlavorUuid
    , volBackedSizeGb : Maybe VolumeSize
    , networkUuid : NetworkUuid
    , keypairName : Maybe String
    , userData : String
    , metadata : List ( String, Json.Encode.Value )
    }



-- Cinder


type alias Volume =
    { name : VolumeName
    , uuid : VolumeUuid
    , status : VolumeStatus
    , size : VolumeSize
    , description : Maybe VolumeDescription
    , attachments : List VolumeAttachment
    , imageMetadata : Maybe NameAndUuid
    }


type alias CreateVolumeRequest =
    { name : VolumeName
    , size : VolumeSize
    }


type VolumeStatus
    = Creating
    | Available
    | Reserved
    | Attaching
    | Detaching
    | InUse
    | Maintenance
    | Deleting
    | AwaitingTransfer
    | Error
    | ErrorDeleting
    | BackingUp
    | RestoringBackup
    | ErrorBackingUp
    | ErrorRestoring
    | ErrorExtending
    | Downloading
    | Uploading
    | Retyping
    | Extending


type alias VolumeAttachment =
    { serverUuid : ServerUuid
    , attachmentUuid : AttachmentUuid
    , device : VolumeAttachmentDevice
    }


type alias VolumeUuid =
    HelperTypes.Uuid


type alias VolumeDescription =
    String


type alias VolumeName =
    String


type alias VolumeSize =
    Int


type alias AttachmentUuid =
    HelperTypes.Uuid


type alias VolumeAttachmentDevice =
    String


type alias VolumeQuota =
    { volumes : QuotaItemDetail
    , gigabytes : QuotaItemDetail
    }



-- Neutron


type alias IpAddress =
    { uuid : Maybe IpAddressUuid -- IP addresses returned in server details do not show UUIDs :(
    , address : IpAddressValue
    , openstackType : IpAddressType
    }


type alias IpAddressValue =
    String


type alias IpAddressUuid =
    HelperTypes.Uuid


type IpAddressType
    = IpAddressFixed
    | IpAddressFloating


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
