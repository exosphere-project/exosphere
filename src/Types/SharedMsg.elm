module Types.SharedMsg exposing
    ( ProjectSpecificMsgConstructor(..)
    , ServerSpecificMsgConstructor(..)
    , SharedMsg(..)
    , TickInterval
    )

import Browser
import Browser.Events
import Bytes exposing (Bytes)
import File exposing (File)
import Http
import OpenStack.DnsRecordSet
import OpenStack.ObjectStorage
import OpenStack.SecurityGroupRule as SecurityGroupRule
import OpenStack.ServerActions as ServerActions
import OpenStack.Types as OSTypes
import OpenStack.VolumeSnapshots exposing (VolumeSnapshot)
import Style.Types as ST
import Style.Widgets.Popover.Types exposing (PopoverId)
import Time
import Toasty
import Types.AppVersion exposing (AppVersion)
import Types.Banner as BannerTypes
import Types.Error exposing (ErrorContext, HttpErrorWithBody, Toast)
import Types.Guacamole as GuacTypes
import Types.HelperTypes as HelperTypes
import Types.Interactivity exposing (InteractionLevel)
import Types.Jetstream2Accounting
import Url


type SharedMsg
    = Tick TickInterval Time.Posix
    | ChangeSystemThemePreference ST.Theme
    | DoOrchestration Time.Posix
    | HandleApiErrorWithBody (Maybe HelperTypes.ProjectIdentifier) ErrorContext HttpErrorWithBody
    | Logout
    | Batch (List SharedMsg)
    | RequestUnscopedToken OSTypes.OpenstackLogin
    | ReceiveProjectScopedToken OSTypes.KeystoneUrl ( Http.Metadata, String )
    | ReceiveUnscopedAuthToken OSTypes.KeystoneUrl ( Http.Metadata, String )
    | ReceiveUnscopedProjects OSTypes.KeystoneUrl ErrorContext (Result HttpErrorWithBody (List HelperTypes.UnscopedProviderProject))
    | ReceiveUnscopedRegions OSTypes.KeystoneUrl ErrorContext (Result HttpErrorWithBody (List HelperTypes.UnscopedProviderRegion))
    | RequestProjectScopedToken OSTypes.KeystoneUrl (List HelperTypes.UnscopedProviderProject)
    | CreateProjectsFromRegionSelections OSTypes.KeystoneUrl OSTypes.ProjectUuid (List OSTypes.RegionId)
    | RequestBanners
    | DismissBanner String
    | ReceiveBanners ErrorContext (Result HttpErrorWithBody BannerTypes.Banners)
    | RequestAppVersion
    | ReceiveAppVersion ErrorContext (Result HttpErrorWithBody AppVersion)
    | ProjectMsg HelperTypes.ProjectIdentifier ProjectSpecificMsgConstructor
    | OpenNewWindow String
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ToastMsg (Toasty.Msg Toast)
    | MsgChangeWindowSize Int Int
    | VisibilityChanged Browser.Events.Visibility
    | SelectTheme ST.ThemeChoice
    | SetExperimentalFeaturesEnabled Bool
    | SetAppVersionUpdateNotificationsEnabled Bool
    | TogglePopover PopoverId
    | NetworkConnection Bool
    | ReceiveWebLock ( String, Bool )
    | NoOp


type alias TickInterval =
    Int


type ProjectSpecificMsgConstructor
    = ReceiveAppCredential OSTypes.ApplicationCredential
    | PrepareCredentialedRequest (Maybe HelperTypes.Url -> OSTypes.AuthTokenString -> Cmd SharedMsg) Time.Posix
    | RemoveProject
    | EnsureDefaultSecurityGroup
    | ServerMsg OSTypes.ServerUuid ServerSpecificMsgConstructor
    | RequestCreateServer HelperTypes.CreateServerPageModel OSTypes.NetworkUuid OSTypes.FlavorId
    | RequestDeleteServers (List OSTypes.ServerUuid)
    | RequestShelveServers (List OSTypes.ServerUuid) { deleteFloatingIps : Bool }
    | RequestServerActions (List ( OSTypes.ServerUuid, ServerActions.ServerAction ))
    | RequestCreateShare OSTypes.ShareName OSTypes.ShareDescription OSTypes.ShareSize OSTypes.ShareProtocol OSTypes.ShareTypeName
    | RequestDeleteShare OSTypes.ShareUuid
    | RequestCreateVolume OSTypes.VolumeName OSTypes.VolumeSize
    | RequestDeleteVolume OSTypes.VolumeUuid
    | RequestDeleteVolumeSnapshot HelperTypes.Uuid
    | RequestDetachVolume OSTypes.VolumeUuid
    | RequestCreateKeypair OSTypes.KeypairName OSTypes.PublicKey
    | RequestDeleteKeypair OSTypes.KeypairIdentifier
    | RequestCreateProjectFloatingIp (Maybe OSTypes.IpAddressValue)
    | RequestDeleteFloatingIp ErrorContext OSTypes.IpAddressUuid
    | RequestAssignFloatingIp OSTypes.Port OSTypes.IpAddressUuid
    | RequestUnassignFloatingIp OSTypes.IpAddressUuid
    | RequestDeleteImage OSTypes.ImageUuid
    | RequestCreateSecurityGroup OSTypes.SecurityGroupTemplate (Maybe OSTypes.ServerUuid)
    | RequestDeleteSecurityGroup OSTypes.SecurityGroup
    | RequestUpdateSecurityGroup OSTypes.SecurityGroup OSTypes.SecurityGroupUpdate
    | RequestUpdateSecurityGroupTags OSTypes.SecurityGroupUuid OSTypes.SecurityGroupTagUpdate
    | ReceiveImages (List OSTypes.Image)
    | ReceiveServerImage (Maybe OSTypes.Image)
    | ReceiveServer InteractionLevel OSTypes.ServerUuid ErrorContext (Result HttpErrorWithBody OSTypes.Server)
    | ReceiveServerEvents OSTypes.ServerUuid ErrorContext (Result HttpErrorWithBody (List OSTypes.ServerEvent))
    | ReceiveUserRequestedConsoleLog OSTypes.ServerUuid ErrorContext (Result HttpErrorWithBody String)
    | ReceiveServerSecurityGroups OSTypes.ServerUuid ErrorContext (Result HttpErrorWithBody (List OSTypes.ServerSecurityGroup))
    | ReceiveServerVolumeAttachments OSTypes.ServerUuid ErrorContext (Result HttpErrorWithBody (List OSTypes.VolumeAttachment))
    | ReceiveServers ErrorContext (Result HttpErrorWithBody (List OSTypes.Server))
    | ReceiveCreateServer ErrorContext (Result HttpErrorWithBody OSTypes.ServerUuid)
    | ReceiveFlavors (List OSTypes.Flavor)
    | ReceiveKeypairs ErrorContext (Result HttpErrorWithBody (List OSTypes.Keypair))
    | ReceiveCreateKeypair ErrorContext (Result HttpErrorWithBody OSTypes.Keypair)
    | ReceiveDeleteKeypair ErrorContext OSTypes.KeypairName (Result Http.Error ())
    | ReceiveNetworks ErrorContext (Result HttpErrorWithBody (List OSTypes.Network))
    | ReceiveAutoAllocatedNetwork ErrorContext (Result HttpErrorWithBody OSTypes.NetworkUuid)
    | ReceiveFloatingIps (List OSTypes.FloatingIp)
    | ReceivePorts ErrorContext (Result HttpErrorWithBody (List OSTypes.Port))
    | ReceiveCreateProjectFloatingIp ErrorContext (Result HttpErrorWithBody OSTypes.FloatingIp)
    | ReceiveDeleteFloatingIp OSTypes.IpAddressUuid
    | ReceiveUnassignFloatingIp OSTypes.FloatingIp
    | ReceiveSecurityGroups ErrorContext (Result HttpErrorWithBody (List OSTypes.SecurityGroup))
    | ReceiveDnsRecordSets (List OpenStack.DnsRecordSet.DnsRecordSet)
    | ReceiveCreateDnsRecordSet ErrorContext (Result HttpErrorWithBody OpenStack.DnsRecordSet.DnsRecordSet)
    | ReceiveDeleteDnsRecordSet ErrorContext (Result HttpErrorWithBody OpenStack.DnsRecordSet.DnsRecordSet)
    | ReceiveCreateSecurityGroup ErrorContext OSTypes.SecurityGroupTemplate (Result HttpErrorWithBody OSTypes.SecurityGroup)
    | ReceiveCreateSecurityGroupRule ErrorContext OSTypes.SecurityGroupUuid (Result HttpErrorWithBody SecurityGroupRule.SecurityGroupRule)
    | ReceiveDeleteSecurityGroupRule ErrorContext ( OSTypes.SecurityGroupUuid, SecurityGroupRule.SecurityGroupRuleUuid ) (Result Http.Error ())
    | ReceiveDeleteSecurityGroup ErrorContext OSTypes.SecurityGroupUuid (Result HttpErrorWithBody ())
    | ReceiveUpdateSecurityGroup ErrorContext OSTypes.SecurityGroupUuid (Result HttpErrorWithBody OSTypes.SecurityGroup)
    | ReceiveUpdateSecurityGroupTags ( OSTypes.SecurityGroupUuid, OSTypes.SecurityGroupTagUpdate )
    | ReceiveServerAddSecurityGroup OSTypes.ServerUuid ErrorContext OSTypes.ServerSecurityGroup (Result HttpErrorWithBody String)
    | ReceiveServerRemoveSecurityGroup OSTypes.ServerUuid ErrorContext OSTypes.ServerSecurityGroup (Result HttpErrorWithBody String)
    | ReceiveCreateShare OSTypes.Share
    | ReceiveCreateAccessRule ( OSTypes.ShareUuid, OSTypes.AccessRule )
    | ReceiveShareAccessRules ( OSTypes.ShareUuid, List OSTypes.AccessRule )
    | ReceiveShareExportLocations ( OSTypes.ShareUuid, List OSTypes.ExportLocation )
    | ReceiveShares (List OSTypes.Share)
    | ReceiveShareTypes (List OSTypes.ShareType)
    | ReceiveContainers ErrorContext (Maybe String) (Result HttpErrorWithBody (List OpenStack.ObjectStorage.Container))
    | RequestCreateContainer OpenStack.ObjectStorage.ContainerName
      -- The Bool is `recursive`: when True the container's ordinary objects are deleted first.
    | RequestDeleteContainer OpenStack.ObjectStorage.ContainerName Bool
    | ReceiveCreateContainer ErrorContext (Result HttpErrorWithBody ())
    | ReceiveDeleteContainer ErrorContext (Result HttpErrorWithBody ())
      -- Recursive non-empty-container delete (Int = remaining re-list cycle budget; the object-name
      -- list is threaded through the messages so no loop state lives in the model).
    | ReceiveContainerObjectNamesForDeletion ErrorContext OpenStack.ObjectStorage.ContainerName Int (Result HttpErrorWithBody (List OpenStack.ObjectStorage.ObjectName))
    | ReceiveDeleteContainerObject ErrorContext OpenStack.ObjectStorage.ContainerName Int (List OpenStack.ObjectStorage.ObjectName) (Result HttpErrorWithBody ())
      -- Object listing (container detail). The Maybe Prefix is the pseudo-folder level; the Maybe
      -- String is the marker the page was requested with (Nothing = first page, replaces the cache;
      -- Just = a user-driven "load more" continuation that appends).
    | RequestObjectListingPage OpenStack.ObjectStorage.ContainerName (Maybe OpenStack.ObjectStorage.Prefix) (Maybe String)
    | ReceiveObjectListing ErrorContext OpenStack.ObjectStorage.ContainerName (Maybe OpenStack.ObjectStorage.Prefix) (Maybe String) (Result HttpErrorWithBody OpenStack.ObjectStorage.ObjectListing)
    | RequestDeleteObject OpenStack.ObjectStorage.ContainerName (Maybe OpenStack.ObjectStorage.Prefix) OpenStack.ObjectStorage.ObjectName
    | ReceiveDeleteObject ErrorContext OpenStack.ObjectStorage.ContainerName (Maybe OpenStack.ObjectStorage.Prefix) (Result HttpErrorWithBody ())
    | RequestBulkDeleteObjects OpenStack.ObjectStorage.ContainerName (Maybe OpenStack.ObjectStorage.Prefix) (List OpenStack.ObjectStorage.ObjectName)
    | ReceiveBulkDeleteObjects ErrorContext OpenStack.ObjectStorage.ContainerName (Maybe OpenStack.ObjectStorage.Prefix) (Result HttpErrorWithBody OpenStack.ObjectStorage.BulkDeleteResult)
      -- Upload (container detail). RequestUploadObjects carries the user's picked File values; State
      -- size-guards each (rejecting oversized files WITHOUT reading them), enqueues with a unique id,
      -- then for accepted files runs File.toBytes -> ReceiveUploadObjectBytes (objectName = prefix ++
      -- filename, contentType) -> Rest.Swift PUT -> ReceiveUploadObject. The Int is that unique upload
      -- id, threaded through so a superseded (re-enqueued) upload's late completion is ignored rather
      -- than clobbering the replacement (see OpenStack.ObjectStorage.setUploadStatusById). Status lives
      -- on Project (transient); ClearFinishedUploads drops terminal entries from the queue.
    | RequestUploadObjects OpenStack.ObjectStorage.ContainerName (Maybe OpenStack.ObjectStorage.Prefix) (List File)
    | ReceiveUploadObjectBytes Int OpenStack.ObjectStorage.ContainerName (Maybe OpenStack.ObjectStorage.Prefix) OpenStack.ObjectStorage.ObjectName String Bytes
    | ReceiveUploadObject ErrorContext Int OpenStack.ObjectStorage.ContainerName (Maybe OpenStack.ObjectStorage.Prefix) (Result HttpErrorWithBody ())
    | ClearFinishedUploads
      -- Private download (container detail): GET the object bytes via the proxy (an anchor cannot
      -- carry the token/proxy headers), then File.Download.bytes in State. The object name is FULL
      -- (prefix already included), so no separate prefix is needed.
    | RequestDownloadObject OpenStack.ObjectStorage.ContainerName OpenStack.ObjectStorage.ObjectName
    | ReceiveDownloadObject ErrorContext OpenStack.ObjectStorage.ObjectName (Result HttpErrorWithBody Bytes)
      -- Server-side copy / move (container detail). RequestCopyObject PUTs the destination object with
      -- an X-Copy-From header (source-container, source-prefix (the viewed level, for refresh),
      -- source-object, dest-container, dest-object). The trailing Bool is `isMove`: on a 2xx copy,
      -- ReceiveCopyObject then DELETEs the source (never fire-and-forget both) and refreshes the
      -- affected listings.
    | RequestCopyObject OpenStack.ObjectStorage.ContainerName (Maybe OpenStack.ObjectStorage.Prefix) OpenStack.ObjectStorage.ObjectName OpenStack.ObjectStorage.ContainerName OpenStack.ObjectStorage.ObjectName Bool
    | ReceiveCopyObject ErrorContext OpenStack.ObjectStorage.ContainerName (Maybe OpenStack.ObjectStorage.Prefix) OpenStack.ObjectStorage.ObjectName OpenStack.ObjectStorage.ContainerName OpenStack.ObjectStorage.ObjectName Bool (Result HttpErrorWithBody ())
      -- New pseudo-folder (container detail): PUT a zero-byte `application/directory` object named
      -- `<prefix><name>/`. The Maybe Prefix is the current level (where the folder appears + is
      -- refreshed). On success ReceiveCreateFolder re-lists that level so the new subdir row shows.
    | RequestCreateFolder OpenStack.ObjectStorage.ContainerName (Maybe OpenStack.ObjectStorage.Prefix) OpenStack.ObjectStorage.ObjectName
    | ReceiveCreateFolder ErrorContext OpenStack.ObjectStorage.ContainerName (Maybe OpenStack.ObjectStorage.Prefix) (Result HttpErrorWithBody ())
    | ReceiveDeleteShare OSTypes.ShareUuid
    | ReceiveShareQuota ErrorContext (Result HttpErrorWithBody OSTypes.ShareQuota)
    | ReceiveCreateVolume
    | ReceiveVolumes ErrorContext (Result HttpErrorWithBody (List OSTypes.Volume))
    | ReceiveVolumeSnapshots (List VolumeSnapshot)
    | ReceiveDeleteVolume
    | ReceiveUpdateVolumeName
    | ReceiveDeleteVolumeSnapshot
    | ReceiveAttachVolume ErrorContext ( OSTypes.ServerUuid, OSTypes.VolumeUuid ) (Result HttpErrorWithBody OSTypes.VolumeAttachment)
    | ReceiveDetachVolume ErrorContext ( OSTypes.ServerUuid, OSTypes.VolumeUuid ) (Result HttpErrorWithBody ())
    | ReceiveRegisteredLimits ErrorContext (Result HttpErrorWithBody (List OSTypes.RegisteredLimit))
    | ReceiveProjectLimits ErrorContext (Result HttpErrorWithBody (List OSTypes.ProjectLimit))
    | ReceiveProjectUsages ErrorContext (Result HttpErrorWithBody (List OSTypes.ProjectUsage))
    | ReceiveComputeQuota ErrorContext (Result HttpErrorWithBody OSTypes.ComputeQuota)
    | ReceiveVolumeQuota ErrorContext (Result HttpErrorWithBody OSTypes.VolumeQuota)
    | ReceiveNetworkQuota ErrorContext (Result HttpErrorWithBody OSTypes.NetworkQuota)
    | ReceiveDeleteImage OSTypes.ImageUuid
    | ReceiveJetstream2Allocations (Result HttpErrorWithBody (List Types.Jetstream2Accounting.Allocation))
    | ReceiveImageVisibilityChange OSTypes.ImageUuid OSTypes.ImageVisibility
    | RequestImageVisibilityChange OSTypes.ImageUuid OSTypes.ImageVisibility


type ServerSpecificMsgConstructor
    = RequestDeleteServer Bool
    | RequestShelveServer Bool
    | RequestSetServerName String
    | RequestAttachVolume OSTypes.VolumeUuid
    | RequestCreateServerImage String
    | RequestResizeServer OSTypes.FlavorId
    | RequestServerSecurityGroupUpdates (List OSTypes.ServerSecurityGroupUpdate)
    | RequestCreateServerFloatingIp (Maybe OSTypes.IpAddressValue)
    | RequestCreateServerHostname ( OpenStack.DnsRecordSet.DnsZone, OSTypes.IpAddressValue )
    | ReceiveServerAction ErrorContext (Result HttpErrorWithBody ())
    | ResetServerAction
    | ReceiveConsoleUrl (Result HttpErrorWithBody OSTypes.ConsoleUrl)
    | ReceiveDeleteServer
    | ReceiveCreateServerFloatingIp ErrorContext (Result HttpErrorWithBody OSTypes.FloatingIp)
    | ReceiveAssignServerFloatingIp OSTypes.FloatingIp
    | ReceiveServerPassphrase OSTypes.ServerPassword
    | ReceiveSetServerName ErrorContext (Result HttpErrorWithBody String)
    | ReceiveSetServerMetadata OSTypes.MetadataItem ErrorContext (Result HttpErrorWithBody (List OSTypes.MetadataItem))
    | ReceiveDeleteServerMetadata OSTypes.MetadataKey ErrorContext (Result HttpErrorWithBody String)
    | ReceiveGuacamoleAuthToken (Result Http.Error GuacTypes.GuacamoleAuthToken)
    | RequestServerAction ServerActions.ServerAction
    | ReceiveConsoleLog ErrorContext (Result HttpErrorWithBody String)
    | SetMinimumServerInteractivity InteractionLevel
