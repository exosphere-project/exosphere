module OpenStack.Types exposing
    ( AccessRule
    , AccessRuleAccessLevel
    , AccessRuleAccessType
    , AccessRuleState
    , ApplicationCredential
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
    , ExportLocation
    , ExportLocationUuid
    , Flavor
    , FlavorId
    , FloatingIp
    , Image
    , ImageStatus(..)
    , ImageUuid
    , ImageVisibility(..)
    , IpAddressStatus(..)
    , IpAddressUuid
    , IpAddressValue
    , Keypair
    , KeypairFingerprint
    , KeypairIdentifier
    , KeypairName
    , KeystoneUrl
    , MetadataItem
    , MetadataKey
    , MetadataValue
    , NameAndUuid
    , Network
    , NetworkQuota
    , NetworkUuid
    , OpenstackLogin
    , Port
    , PortUuid
    , ProjectDescription
    , ProjectUuid
    , PublicKey
    , QuotaItem
    , QuotaItemLimit(..)
    , Region
    , RegionDescription
    , RegionId
    , ScopedAuthToken
    , SecurityGroup
    , Server
    , ServerDetails
    , ServerEvent
    , ServerFault
    , ServerLockStatus(..)
    , ServerPassword
    , ServerPowerState(..)
    , ServerStatus(..)
    , ServerUuid
    , Service
    , ServiceCatalog
    , Share
    , ShareDescription
    , ShareName
    , ShareProtocol
    , ShareQuota
    , ShareSize
    , ShareStatus(..)
    , ShareTypeName
    , ShareUuid
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
    , accessRuleAccessLevelToString
    , accessRuleAccessTypeToString
    , accessRuleStateToString
    , boolToShareVisibility
    , imageVisibilityToString
    , serverPowerStateToString
    , serverStatusToString
    , shareProtocolToString
    , shareStatusToString
    , shareVisibilityToString
    , stringToAccessRuleAccessLevel
    , stringToAccessRuleAccessType
    , stringToAccessRuleState
    , stringToShareProtocol
    , stringToShareStatus
    , volumeStatusToString
    )

import Dict
import Helpers.RemoteDataPlusPlus as RDPP
import Json.Encode
import OpenStack.HelperTypes as HelperTypes
import OpenStack.SecurityGroupRule exposing (SecurityGroupRule)
import Time
import Types.Error exposing (HttpErrorWithBody)



{-
   Types that match structure of data returned from OpenStack API, used for
   decoding JSON and representing state of OpenStack resources.
-}


type alias MetadataItem =
    { key : MetadataKey
    , value : MetadataValue
    }


type alias MetadataKey =
    String


type alias MetadataValue =
    String


type alias QuotaItem =
    -- OpenStack uses -1 for "no limit", and we'll use the `Unlimited` variant of the `QuotaItemLimit` type in that case
    { inUse : Int
    , limit : QuotaItemLimit
    }


type QuotaItemLimit
    = Limit Int
    | Unlimited


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
    { expiresAt : Time.Posix
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


type alias ProjectUuid =
    HelperTypes.Uuid


type alias ServiceCatalog =
    List Service


type alias ProjectDescription =
    String


type alias Service =
    { name : String
    , type_ : String
    , endpoints : List Endpoint
    }


type alias Endpoint =
    { interface : EndpointInterface
    , url : HelperTypes.Url
    , regionId : RegionId
    }


type EndpointInterface
    = Public
    | Admin
    | Internal


type alias OpenstackLogin =
    { authUrl : KeystoneUrl
    , userDomain : String
    , username : String
    , password : String
    }


type CredentialsForAuthToken
    = TokenCreds KeystoneUrl UnscopedAuthToken ProjectUuid
    | AppCreds KeystoneUrl String ApplicationCredential


type alias Region =
    { id : RegionId
    , description : RegionDescription
    }


type alias RegionId =
    String


type alias RegionDescription =
    String



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
    , visibility : ImageVisibility
    , additionalProperties : Dict.Dict String String
    , createdAt : Time.Posix
    , osDistro : Maybe String
    , osVersion : Maybe String
    , protected : Bool
    , imageType : Maybe String
    , minDiskGB : Maybe Int
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


type ImageVisibility
    = ImagePublic
    | ImageCommunity
    | ImageShared
    | ImagePrivate


imageVisibilityToString : ImageVisibility -> String
imageVisibilityToString imageVisibility =
    case imageVisibility of
        ImagePublic ->
            "Public"

        ImageCommunity ->
            "Community"

        ImageShared ->
            "Shared"

        ImagePrivate ->
            "Private"



-- Nova


type alias Keypair =
    { name : KeypairName
    , publicKey : PublicKey
    , fingerprint : KeypairFingerprint
    }


type alias KeypairName =
    String


type alias KeypairIdentifier =
    ( KeypairName, KeypairFingerprint )


type alias PublicKey =
    String


type alias KeypairFingerprint =
    String


type alias Flavor =
    { id : FlavorId
    , name : String
    , vcpu : Int
    , ram_mb : Int
    , disk_root : Int
    , disk_ephemeral : Int
    , extra_specs : List MetadataItem
    }


type alias FlavorId =
    String


type alias Server =
    { name : String
    , uuid : ServerUuid
    , details : ServerDetails
    , consoleUrl : RDPP.RemoteDataPlusPlus HttpErrorWithBody ConsoleUrl
    }


type alias ServerUuid =
    HelperTypes.Uuid


type alias ConsoleUrl =
    HelperTypes.Url


type ServerStatus
    = ServerActive
    | ServerBuild
    | ServerDeleted
    | ServerError
    | ServerHardReboot
    | ServerMigrating
    | ServerPassword
    | ServerPaused
    | ServerReboot
    | ServerRebuild
    | ServerRescue
    | ServerResize
    | ServerRevertResize
    | ServerShelved
    | ServerShelvedOffloaded
    | ServerShutoff
    | ServerSoftDeleted
    | ServerStopped
    | ServerSuspended
    | ServerUnknown
    | ServerVerifyResize


serverStatusToString : ServerStatus -> String
serverStatusToString serverStatus =
    case serverStatus of
        ServerActive ->
            "Active"

        ServerBuild ->
            "Build"

        ServerDeleted ->
            "Deleted"

        ServerError ->
            "Error"

        ServerHardReboot ->
            "HardReboot"

        ServerMigrating ->
            "Migrating"

        ServerPassword ->
            "Password"

        ServerPaused ->
            "Paused"

        ServerReboot ->
            "Reboot"

        ServerRebuild ->
            "Rebuild"

        ServerRescue ->
            "Rescue"

        ServerResize ->
            "Resize"

        ServerRevertResize ->
            "RevertResize"

        ServerShelved ->
            "Shelved"

        ServerShelvedOffloaded ->
            "ShelvedOffloaded"

        ServerShutoff ->
            "Shutoff"

        ServerSoftDeleted ->
            "SoftDeleted"

        ServerStopped ->
            "Stopped"

        ServerSuspended ->
            "Suspended"

        ServerUnknown ->
            "Unknown"

        ServerVerifyResize ->
            "VerifyResize"


type ServerPowerState
    = PowerNoState
    | PowerRunning
    | PowerPaused
    | PowerShutdown
    | PowerCrashed
    | PowerSuspended


serverPowerStateToString : ServerPowerState -> String
serverPowerStateToString serverPowerState =
    case serverPowerState of
        PowerNoState ->
            "PowerNoState"

        PowerRunning ->
            "PowerRunning"

        PowerPaused ->
            "PowerPaused"

        PowerShutdown ->
            "PowerShutdown"

        PowerCrashed ->
            "PowerCrashed"

        PowerSuspended ->
            "PowerSuspended"


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
    , created : Time.Posix
    , powerState : ServerPowerState
    , imageUuid : ImageUuid
    , flavorId : FlavorId
    , keypairName : Maybe String
    , metadata : List MetadataItem
    , userUuid : UserUuid
    , volumesAttached : List VolumeUuid
    , tags : List ServerTag
    , lockStatus : ServerLockStatus
    , fault : Maybe ServerFault
    }


type alias ServerFault =
    { code : Int
    , created : Time.Posix
    , message : String
    }


type alias ComputeQuota =
    { cores : QuotaItem
    , instances : QuotaItem
    , ram : QuotaItem

    -- OpenStack doesn't tell us a quantity of keypairs in use, only the limit
    , keypairsLimit : Int
    }


type alias ServerTag =
    String


type alias ServerPassword =
    String


type alias CreateServerRequest =
    { name : String
    , count : Int
    , imageUuid : ImageUuid
    , flavorId : FlavorId
    , volBackedSizeGb : Maybe VolumeSize
    , networkUuid : NetworkUuid
    , keypairName : Maybe String
    , userData : String
    , metadata : List ( String, Json.Encode.Value )
    }


type alias ServerEvent =
    { action : String -- This sucks, should use an enumerated type for Server Action
    , errorMessage : Maybe String
    , requestId : String
    , startTime : Time.Posix
    , userId : String
    }



-- Cinder


type alias Volume =
    { name : Maybe VolumeName
    , uuid : VolumeUuid
    , status : VolumeStatus
    , size : VolumeSize
    , description : Maybe VolumeDescription
    , attachments : List VolumeAttachment
    , imageMetadata : Maybe NameAndUuid
    , createdAt : Time.Posix
    , userUuid : UserUuid
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


volumeStatusToString : VolumeStatus -> String
volumeStatusToString volumeStatus =
    case volumeStatus of
        Creating ->
            "Creating"

        Available ->
            "Available"

        Reserved ->
            "Reserved"

        Attaching ->
            "Attaching"

        Detaching ->
            "Detaching"

        InUse ->
            "InUse"

        Maintenance ->
            "Maintenance"

        Deleting ->
            "Deleting"

        AwaitingTransfer ->
            "AwaitingTransfer"

        Error ->
            "Error"

        ErrorDeleting ->
            "ErrorDeleting"

        BackingUp ->
            "BackingUp"

        RestoringBackup ->
            "RestoringBackup"

        ErrorBackingUp ->
            "ErrorBackingUp"

        ErrorRestoring ->
            "ErrorRestoring"

        ErrorExtending ->
            "ErrorExtending"

        Downloading ->
            "Downloading"

        Uploading ->
            "Uploading"

        Retyping ->
            "Retyping"

        Extending ->
            "Extending"


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
    { volumes : QuotaItem
    , gigabytes : QuotaItem
    }



-- Neutron


type alias FloatingIp =
    { uuid : IpAddressUuid
    , address : IpAddressValue
    , status : IpAddressStatus
    , portUuid : Maybe PortUuid
    , dnsName : Maybe String
    , dnsDomain : Maybe String
    }


type alias IpAddressValue =
    String


type alias IpAddressUuid =
    HelperTypes.Uuid


type IpAddressStatus
    = IpAddressActive
    | IpAddressDown
    | IpAddressError


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
    , fixedIps : List IpAddressValue
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


type alias NetworkQuota =
    { floatingIps : QuotaItem
    }



-- Manila


type alias Share =
    { name : Maybe ShareName
    , uuid : ShareUuid
    , status : ShareStatus
    , size : ShareSize
    , description : Maybe ShareDescription
    , metadata : Dict.Dict String String
    , createdAt : Time.Posix
    , userUuid : UserUuid
    , visibility : ShareVisibility
    , shareProtocol : ShareProtocol
    , shareTypeName : ShareTypeName
    }


type alias AccessRule =
    { uuid : AccessRuleUuid
    , accessLevel : AccessRuleAccessLevel
    , accessType : AccessRuleAccessType
    , accessTo : AccessRuleAccessTo
    , accessKey : Maybe AccessRuleAccessKey
    , state : AccessRuleState
    , createdAt : Time.Posix
    }


type alias ShareQuota =
    { gigabytes : QuotaItem
    , snapshots : QuotaItem
    , shares : QuotaItem
    , snapshotGigabytes : QuotaItem
    , shareNetworks : Maybe QuotaItem
    , shareReplicas : Maybe QuotaItem
    , shareReplicaGigabytes : Maybe QuotaItem
    , shareGroups : Maybe QuotaItem
    , shareGroupSnapshots : Maybe QuotaItem
    , perShareGigabytes : Maybe QuotaItem
    }


type alias ExportLocation =
    { uuid : ExportLocationUuid
    , path : String
    , preferred : Bool
    }


type ShareStatus
    = ShareCreating
    | ShareCreatingFromSnapshot
    | ShareDeleting
    | ShareDeleted
    | ShareError
    | ShareErrorDeleting
    | ShareAvailable
    | ShareInactive
    | ShareManageStarting
    | ShareManageError
    | ShareUnmanageStarting
    | ShareUnmanageError
    | ShareUnmanaged
    | ShareExtending
    | ShareExtendingError
    | ShareShrinking
    | ShareShrinkingError
    | ShareShrinkingPossibleDataLossError
    | ShareMigrating
    | ShareMigratingTo
    | ShareReplicationChange
    | ShareReverting
    | ShareRevertingError
    | ShareAwaitingTransfer


type alias ShareName =
    String


type alias ShareUuid =
    String


type alias ShareSize =
    Int


type alias ShareDescription =
    String


type ShareProtocol
    = CephFS
    | UnsupportedShareProtocol String


type alias ShareTypeName =
    String


type ShareVisibility
    = SharePrivate
    | SharePublic


type alias AccessRuleUuid =
    String


type alias AccessRuleAccessTo =
    String


type alias AccessRuleAccessKey =
    String


type AccessRuleAccessLevel
    = RO
    | RW
    | UnsupportedAccessLevel String


type AccessRuleAccessType
    = CephX
    | UnsupportedAccessType String


type AccessRuleState
    = QueuedToApply
    | Applying
    | Active
    | AccessRuleError
    | QueuedToDeny
    | Denying


type alias ExportLocationUuid =
    String


stringToShareStatus : String -> ShareStatus
stringToShareStatus str =
    case str of
        "creating" ->
            ShareCreating

        "creating_from_snapshot" ->
            ShareCreatingFromSnapshot

        "deleting" ->
            ShareDeleting

        "deleted" ->
            ShareDeleted

        "error" ->
            ShareError

        "error_deleting" ->
            ShareErrorDeleting

        "available" ->
            ShareAvailable

        "inactive" ->
            ShareInactive

        "manage_starting" ->
            ShareManageStarting

        "manage_error" ->
            ShareManageError

        "unmanage_starting" ->
            ShareUnmanageStarting

        "unmanaged" ->
            ShareUnmanaged

        "unmanage_error" ->
            ShareUnmanageError

        "extending" ->
            ShareExtending

        "extending_error" ->
            ShareExtendingError

        "shrinking" ->
            ShareShrinking

        "shrinking_error" ->
            ShareShrinkingError

        "shrinking_possible_data_loss_error" ->
            ShareShrinkingPossibleDataLossError

        "migrating" ->
            ShareMigrating

        "migrating_to" ->
            ShareMigratingTo

        "replication_change" ->
            ShareReplicationChange

        "share_reverting" ->
            ShareReverting

        "share_reverting_error" ->
            ShareRevertingError

        "awaiting_transfer" ->
            ShareAwaitingTransfer

        _ ->
            ShareError


shareStatusToString : ShareStatus -> String
shareStatusToString shareStatus =
    case shareStatus of
        ShareCreating ->
            "Creating"

        ShareCreatingFromSnapshot ->
            "Creating from snapshot"

        ShareDeleted ->
            "Deleted"

        ShareError ->
            "Error"

        ShareDeleting ->
            "Deleting"

        ShareErrorDeleting ->
            "Error deleting"

        ShareAvailable ->
            "Available"

        ShareInactive ->
            "Inactive"

        ShareManageStarting ->
            "Manage starting"

        ShareManageError ->
            "Manage error"

        ShareUnmanageStarting ->
            "Unmanage starting"

        ShareUnmanageError ->
            "Unmanage error"

        ShareUnmanaged ->
            "Unmanaged"

        ShareAwaitingTransfer ->
            "AwaitingTransfer"

        ShareExtending ->
            "Extending"

        ShareExtendingError ->
            "Extending error"

        ShareShrinking ->
            "Shrinking"

        ShareShrinkingError ->
            "Shrinking error"

        ShareShrinkingPossibleDataLossError ->
            "Possibile data loss error"

        ShareMigrating ->
            "Migrating"

        ShareMigratingTo ->
            "Migrating to"

        ShareReplicationChange ->
            "Replication change"

        ShareReverting ->
            "Reverting"

        ShareRevertingError ->
            "Reverting error"


boolToShareVisibility : Bool -> ShareVisibility
boolToShareVisibility isPublic =
    if isPublic then
        SharePublic

    else
        SharePrivate


shareVisibilityToString : ShareVisibility -> String
shareVisibilityToString visibility =
    case visibility of
        SharePrivate ->
            "Private"

        SharePublic ->
            "Public"


stringToShareProtocol : String -> ShareProtocol
stringToShareProtocol str =
    case String.toUpper str of
        "CEPHFS" ->
            CephFS

        _ ->
            UnsupportedShareProtocol str


shareProtocolToString : ShareProtocol -> String
shareProtocolToString shareProto =
    case shareProto of
        CephFS ->
            "CephFS"

        UnsupportedShareProtocol str ->
            str


stringToAccessRuleAccessLevel : String -> AccessRuleAccessLevel
stringToAccessRuleAccessLevel str =
    case String.toLower str of
        "ro" ->
            RO

        "rw" ->
            RW

        _ ->
            UnsupportedAccessLevel str


accessRuleAccessLevelToString : AccessRuleAccessLevel -> String
accessRuleAccessLevelToString accessLevel =
    case accessLevel of
        RO ->
            "read-only"

        RW ->
            "read-write"

        UnsupportedAccessLevel str ->
            str


stringToAccessRuleAccessType : String -> AccessRuleAccessType
stringToAccessRuleAccessType str =
    case String.toUpper str of
        "CEPHX" ->
            CephX

        _ ->
            UnsupportedAccessType str


accessRuleAccessTypeToString : AccessRuleAccessType -> String
accessRuleAccessTypeToString accessType =
    case accessType of
        CephX ->
            "CephX"

        UnsupportedAccessType str ->
            str


stringToAccessRuleState : String -> AccessRuleState
stringToAccessRuleState str =
    case String.toLower str of
        "queued_to_apply" ->
            QueuedToApply

        "applying" ->
            Applying

        "active" ->
            Active

        "queued_to_deny" ->
            QueuedToDeny

        "denying" ->
            Denying

        "error" ->
            AccessRuleError

        _ ->
            AccessRuleError


accessRuleStateToString : AccessRuleState -> String
accessRuleStateToString state =
    case state of
        QueuedToApply ->
            "Queued to apply"

        Applying ->
            "Applying"

        Active ->
            "Active"

        QueuedToDeny ->
            "Queued to deny"

        Denying ->
            "Denying"

        AccessRuleError ->
            "Error"
