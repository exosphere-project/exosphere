module Types.SharedMsg exposing
    ( ProjectSpecificMsgConstructor(..)
    , ServerSpecificMsgConstructor(..)
    , SharedMsg(..)
    , TickInterval
    )

import Browser
import Http
import OpenStack.DnsRecordSet
import OpenStack.SecurityGroupRule as SecurityGroupRule
import OpenStack.Types as OSTypes
import OpenStack.VolumeSnapshots exposing (VolumeSnapshot)
import Style.Types as ST
import Style.Widgets.Popover.Types exposing (PopoverId)
import Time
import Toasty
import Types.Banner as BannerTypes
import Types.Error exposing (ErrorContext, HttpErrorWithBody, Toast)
import Types.Guacamole as GuacTypes
import Types.HelperTypes as HelperTypes
import Types.Jetstream2Accounting
import Url


type SharedMsg
    = Tick TickInterval Time.Posix
    | ChangeSystemThemePreference ST.Theme
    | DoOrchestration Time.Posix
    | HandleApiErrorWithBody ErrorContext HttpErrorWithBody
    | Logout
    | RequestUnscopedToken OSTypes.OpenstackLogin
    | ReceiveProjectScopedToken OSTypes.KeystoneUrl ( Http.Metadata, String )
    | ReceiveUnscopedAuthToken OSTypes.KeystoneUrl ( Http.Metadata, String )
    | ReceiveUnscopedProjects OSTypes.KeystoneUrl ErrorContext (Result HttpErrorWithBody (List HelperTypes.UnscopedProviderProject))
    | ReceiveUnscopedRegions OSTypes.KeystoneUrl ErrorContext (Result HttpErrorWithBody (List HelperTypes.UnscopedProviderRegion))
    | RequestProjectScopedToken OSTypes.KeystoneUrl (List HelperTypes.UnscopedProviderProject)
    | CreateProjectsFromRegionSelections OSTypes.KeystoneUrl OSTypes.ProjectUuid (List OSTypes.RegionId)
    | RequestBanners
    | ReceiveBanners ErrorContext (Result HttpErrorWithBody BannerTypes.Banners)
    | ProjectMsg HelperTypes.ProjectIdentifier ProjectSpecificMsgConstructor
    | OpenNewWindow String
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ToastMsg (Toasty.Msg Toast)
    | MsgChangeWindowSize Int Int
    | SelectTheme ST.ThemeChoice
    | SetExperimentalFeaturesEnabled Bool
    | TogglePopover PopoverId
    | NetworkConnection Bool
    | NoOp


type alias TickInterval =
    Int


type ProjectSpecificMsgConstructor
    = ReceiveAppCredential OSTypes.ApplicationCredential
    | PrepareCredentialedRequest (Maybe HelperTypes.Url -> OSTypes.AuthTokenString -> Cmd SharedMsg) Time.Posix
    | RemoveProject
    | ServerMsg OSTypes.ServerUuid ServerSpecificMsgConstructor
    | RequestCreateServer HelperTypes.CreateServerPageModel OSTypes.NetworkUuid OSTypes.FlavorId
    | RequestDeleteServers (List OSTypes.ServerUuid)
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
    | RequestDeleteSecurityGroup OSTypes.SecurityGroup
    | RequestUpdateSecurityGroup OSTypes.SecurityGroup OSTypes.SecurityGroupUpdate
    | RequestUpdateSecurityGroupTags OSTypes.SecurityGroupUuid (List OSTypes.SecurityGroupTag)
    | ReceiveImages (List OSTypes.Image)
    | ReceiveServerImage (Maybe OSTypes.Image)
    | ReceiveServer OSTypes.ServerUuid ErrorContext (Result HttpErrorWithBody OSTypes.Server)
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
    | ReceiveAssignFloatingIp OSTypes.FloatingIp
    | ReceiveUnassignFloatingIp OSTypes.FloatingIp
    | ReceiveSecurityGroups ErrorContext (Result HttpErrorWithBody (List OSTypes.SecurityGroup))
    | ReceiveDnsRecordSets (List OpenStack.DnsRecordSet.DnsRecordSet)
    | ReceiveCreateDnsRecordSet ErrorContext (Result HttpErrorWithBody OpenStack.DnsRecordSet.DnsRecordSet)
    | ReceiveDeleteDnsRecordSet ErrorContext (Result HttpErrorWithBody OpenStack.DnsRecordSet.DnsRecordSet)
    | ReceiveCreateDefaultSecurityGroup ErrorContext (Result HttpErrorWithBody OSTypes.SecurityGroup) OSTypes.SecurityGroupTemplate
    | ReceiveCreateSecurityGroupRule ErrorContext OSTypes.SecurityGroupUuid (Result HttpErrorWithBody SecurityGroupRule.SecurityGroupRule)
    | ReceiveDeleteSecurityGroupRule ErrorContext ( OSTypes.SecurityGroupUuid, SecurityGroupRule.SecurityGroupRuleUuid ) (Result Http.Error ())
    | ReceiveDeleteSecurityGroup ErrorContext OSTypes.SecurityGroupUuid (Result HttpErrorWithBody ())
    | ReceiveUpdateSecurityGroup ErrorContext OSTypes.SecurityGroupUuid (Result HttpErrorWithBody OSTypes.SecurityGroup)
    | ReceiveUpdateSecurityGroupTags ( OSTypes.SecurityGroupUuid, List OSTypes.SecurityGroupTag )
    | ReceiveCreateShare OSTypes.Share
    | ReceiveCreateAccessRule ( OSTypes.ShareUuid, OSTypes.AccessRule )
    | ReceiveShareAccessRules ( OSTypes.ShareUuid, List OSTypes.AccessRule )
    | ReceiveShareExportLocations ( OSTypes.ShareUuid, List OSTypes.ExportLocation )
    | ReceiveShares (List OSTypes.Share)
    | ReceiveDeleteShare OSTypes.ShareUuid
    | ReceiveShareQuota ErrorContext (Result HttpErrorWithBody OSTypes.ShareQuota)
    | ReceiveCreateVolume
    | ReceiveVolumes ErrorContext (Result HttpErrorWithBody (List OSTypes.Volume))
    | ReceiveVolumeSnapshots (List VolumeSnapshot)
    | ReceiveDeleteVolume
    | ReceiveUpdateVolumeName
    | ReceiveDeleteVolumeSnapshot
    | ReceiveAttachVolume OSTypes.VolumeAttachment
    | ReceiveDetachVolume
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
    | ReceiveServerAction
    | ReceiveServerEvents ErrorContext (Result HttpErrorWithBody (List OSTypes.ServerEvent))
    | ReceiveServerSecurityGroups ErrorContext (Result HttpErrorWithBody (List OSTypes.ServerSecurityGroup))
    | ReceiveServerAddSecurityGroup ErrorContext OSTypes.ServerSecurityGroup (Result HttpErrorWithBody String)
    | ReceiveServerRemoveSecurityGroup ErrorContext OSTypes.ServerSecurityGroup (Result HttpErrorWithBody String)
    | ReceiveConsoleUrl (Result HttpErrorWithBody OSTypes.ConsoleUrl)
    | ReceiveDeleteServer
    | ReceiveCreateServerFloatingIp ErrorContext (Result HttpErrorWithBody OSTypes.FloatingIp)
    | ReceiveServerPassphrase OSTypes.ServerPassword
    | ReceiveSetServerName ErrorContext (Result HttpErrorWithBody String)
    | ReceiveSetServerMetadata OSTypes.MetadataItem ErrorContext (Result HttpErrorWithBody (List OSTypes.MetadataItem))
    | ReceiveDeleteServerMetadata OSTypes.MetadataKey ErrorContext (Result HttpErrorWithBody String)
    | ReceiveGuacamoleAuthToken (Result Http.Error GuacTypes.GuacamoleAuthToken)
    | RequestServerAction (HelperTypes.Url -> Cmd SharedMsg) (Maybe (List OSTypes.ServerStatus)) Bool
    | ReceiveConsoleLog ErrorContext (Result HttpErrorWithBody String)
