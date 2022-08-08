module Types.SharedMsg exposing
    ( ProjectSpecificMsgConstructor(..)
    , ServerSpecificMsgConstructor(..)
    , SharedMsg(..)
    , TickInterval
    )

import Browser
import Http
import OpenStack.Types as OSTypes
import Style.Types as ST
import Style.Widgets.Popover.Types exposing (PopoverId)
import Time
import Toasty
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
    | Jetstream1Login HelperTypes.Jetstream1Creds
    | ReceiveProjectScopedToken OSTypes.KeystoneUrl ( Http.Metadata, String )
    | ReceiveUnscopedAuthToken OSTypes.KeystoneUrl ( Http.Metadata, String )
    | ReceiveUnscopedProjects OSTypes.KeystoneUrl (List HelperTypes.UnscopedProviderProject)
    | ReceiveUnscopedRegions OSTypes.KeystoneUrl (List HelperTypes.UnscopedProviderRegion)
    | RequestProjectScopedToken OSTypes.KeystoneUrl (List HelperTypes.UnscopedProviderProject)
    | CreateProjectsFromRegionSelections OSTypes.KeystoneUrl OSTypes.ProjectUuid (List OSTypes.RegionId)
    | ProjectMsg HelperTypes.ProjectIdentifier ProjectSpecificMsgConstructor
    | OpenNewWindow String
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ToastMsg (Toasty.Msg Toast)
    | MsgChangeWindowSize Int Int
    | SelectTheme ST.ThemeChoice
    | SetExperimentalFeaturesEnabled Bool
    | TogglePopover PopoverId
    | NoOp


type alias TickInterval =
    Int


type ProjectSpecificMsgConstructor
    = ReceiveAppCredential OSTypes.ApplicationCredential
    | PrepareCredentialedRequest (Maybe HelperTypes.Url -> OSTypes.AuthTokenString -> Cmd SharedMsg) Time.Posix
    | RemoveProject
    | ServerMsg OSTypes.ServerUuid ServerSpecificMsgConstructor
    | RequestServers
    | RequestCreateServer HelperTypes.CreateServerPageModel OSTypes.NetworkUuid OSTypes.FlavorId
    | RequestDeleteServers (List OSTypes.ServerUuid)
    | RequestCreateVolume OSTypes.VolumeName OSTypes.VolumeSize
    | RequestDeleteVolume OSTypes.VolumeUuid
    | RequestDetachVolume OSTypes.VolumeUuid
    | RequestKeypairs
    | RequestCreateKeypair OSTypes.KeypairName OSTypes.PublicKey
    | RequestDeleteKeypair OSTypes.KeypairIdentifier
    | RequestDeleteFloatingIp ErrorContext OSTypes.IpAddressUuid
    | RequestAssignFloatingIp OSTypes.Port OSTypes.IpAddressUuid
    | RequestUnassignFloatingIp OSTypes.IpAddressUuid
    | RequestDeleteImage OSTypes.ImageUuid
    | ReceiveImages (List OSTypes.Image)
    | ReceiveServer OSTypes.ServerUuid ErrorContext (Result HttpErrorWithBody OSTypes.Server)
    | ReceiveServers ErrorContext (Result HttpErrorWithBody (List OSTypes.Server))
    | ReceiveCreateServer ErrorContext (Result HttpErrorWithBody OSTypes.ServerUuid)
    | ReceiveFlavors (List OSTypes.Flavor)
    | ReceiveKeypairs (List OSTypes.Keypair)
    | ReceiveCreateKeypair OSTypes.Keypair
    | ReceiveDeleteKeypair ErrorContext OSTypes.KeypairName (Result Http.Error ())
    | ReceiveNetworks ErrorContext (Result HttpErrorWithBody (List OSTypes.Network))
    | ReceiveAutoAllocatedNetwork ErrorContext (Result HttpErrorWithBody OSTypes.NetworkUuid)
    | ReceiveFloatingIps (List OSTypes.FloatingIp)
    | ReceivePorts ErrorContext (Result HttpErrorWithBody (List OSTypes.Port))
    | ReceiveDeleteFloatingIp OSTypes.IpAddressUuid
    | ReceiveAssignFloatingIp OSTypes.FloatingIp
    | ReceiveUnassignFloatingIp OSTypes.FloatingIp
    | ReceiveSecurityGroups (List OSTypes.SecurityGroup)
    | ReceiveCreateExoSecurityGroup OSTypes.SecurityGroup
    | ReceiveCreateVolume
    | ReceiveVolumes (List OSTypes.Volume)
    | ReceiveDeleteVolume
    | ReceiveUpdateVolumeName
    | ReceiveAttachVolume OSTypes.VolumeAttachment
    | ReceiveDetachVolume
    | ReceiveComputeQuota OSTypes.ComputeQuota
    | ReceiveVolumeQuota OSTypes.VolumeQuota
    | ReceiveNetworkQuota OSTypes.NetworkQuota
    | ReceiveRandomServerName String
    | ReceiveDeleteImage OSTypes.ImageUuid
    | ReceiveJetstream2Allocation (Result HttpErrorWithBody (Maybe Types.Jetstream2Accounting.Allocation))


type ServerSpecificMsgConstructor
    = RequestServer
    | RequestDeleteServer Bool
    | RequestSetServerName String
    | RequestAttachVolume OSTypes.VolumeUuid
    | RequestCreateServerImage String
    | RequestResizeServer OSTypes.FlavorId
    | ReceiveServerAction
    | ReceiveServerEvents ErrorContext (Result HttpErrorWithBody (List OSTypes.ServerEvent))
    | ReceiveConsoleUrl (Result HttpErrorWithBody OSTypes.ConsoleUrl)
    | ReceiveDeleteServer
    | ReceiveCreateFloatingIp ErrorContext (Result HttpErrorWithBody OSTypes.FloatingIp)
    | ReceiveServerPassphrase OSTypes.ServerPassword
    | ReceiveSetServerName String ErrorContext (Result HttpErrorWithBody String)
    | ReceiveSetServerMetadata OSTypes.MetadataItem ErrorContext (Result HttpErrorWithBody (List OSTypes.MetadataItem))
    | ReceiveDeleteServerMetadata OSTypes.MetadataKey ErrorContext (Result HttpErrorWithBody String)
    | ReceiveGuacamoleAuthToken (Result Http.Error GuacTypes.GuacamoleAuthToken)
    | RequestServerAction (HelperTypes.Url -> Cmd SharedMsg) (Maybe (List OSTypes.ServerStatus))
    | ReceiveConsoleLog ErrorContext (Result HttpErrorWithBody String)
