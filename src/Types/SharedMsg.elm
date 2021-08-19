module Types.SharedMsg exposing
    ( NavigableView(..)
    , ProjectSpecificMsgConstructor(..)
    , ServerSpecificMsgConstructor(..)
    , SharedMsg(..)
    , TickInterval
    )

import Http
import OpenStack.Types as OSTypes
import Set
import Style.Types
import Time
import Toasty
import Types.Error exposing (ErrorContext, HttpErrorWithBody, Toast)
import Types.Guacamole as GuacTypes
import Types.HelperTypes as HelperTypes
import Url


type SharedMsg
    = Tick TickInterval Time.Posix
    | DoOrchestration Time.Posix
    | HandleApiErrorWithBody ErrorContext HttpErrorWithBody
    | RequestUnscopedToken OSTypes.OpenstackLogin
    | JetstreamLogin HelperTypes.JetstreamCreds
    | ReceiveScopedAuthToken ( Http.Metadata, String )
    | ReceiveUnscopedAuthToken OSTypes.KeystoneUrl ( Http.Metadata, String )
    | ReceiveUnscopedProjects OSTypes.KeystoneUrl (List HelperTypes.UnscopedProviderProject)
    | RequestProjectLoginFromProvider OSTypes.KeystoneUrl (Set.Set HelperTypes.ProjectIdentifier)
    | ProjectMsg HelperTypes.ProjectIdentifier ProjectSpecificMsgConstructor
    | OpenNewWindow String
    | NavigateToView NavigableView
    | NavigateToUrl String
    | ToastyMsg (Toasty.Msg Toast)
    | MsgChangeWindowSize Int Int
    | UrlChange Url.Url
    | SetStyle Style.Types.StyleMode
    | NoOp


type alias TickInterval =
    Int


type ProjectSpecificMsgConstructor
    = ReceiveAppCredential OSTypes.ApplicationCredential
    | PrepareCredentialedRequest (Maybe HelperTypes.Url -> OSTypes.AuthTokenString -> Cmd SharedMsg) Time.Posix
    | ToggleCreatePopup
    | RemoveProject
    | ServerMsg OSTypes.ServerUuid ServerSpecificMsgConstructor
    | RequestServers
    | RequestCreateServer HelperTypes.CreateServerPageModel OSTypes.NetworkUuid
    | RequestDeleteServers (List OSTypes.ServerUuid)
    | RequestCreateVolume OSTypes.VolumeName OSTypes.VolumeSize
    | RequestDeleteVolume OSTypes.VolumeUuid
    | RequestDetachVolume OSTypes.VolumeUuid
    | RequestKeypairs
    | RequestCreateKeypair OSTypes.KeypairName OSTypes.PublicKey
    | RequestDeleteKeypair OSTypes.KeypairIdentifier
    | RequestDeleteFloatingIp OSTypes.IpAddressUuid
    | RequestAssignFloatingIp OSTypes.Port OSTypes.IpAddressUuid
    | RequestUnassignFloatingIp OSTypes.IpAddressUuid
    | ReceiveImages (List OSTypes.Image)
    | ReceiveServer OSTypes.ServerUuid ErrorContext (Result HttpErrorWithBody OSTypes.Server)
    | ReceiveServers ErrorContext (Result HttpErrorWithBody (List OSTypes.Server))
    | ReceiveCreateServer OSTypes.ServerUuid
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
    | ReceiveRandomServerName String


type ServerSpecificMsgConstructor
    = RequestServer
    | RequestDeleteServer Bool
    | RequestSetServerName String
    | RequestAttachVolume OSTypes.VolumeUuid
    | RequestCreateServerImage String
    | ReceiveServerEvents ErrorContext (Result HttpErrorWithBody (List OSTypes.ServerEvent))
    | ReceiveConsoleUrl (Result HttpErrorWithBody OSTypes.ConsoleUrl)
    | ReceiveDeleteServer
    | ReceiveCreateFloatingIp ErrorContext (Result HttpErrorWithBody OSTypes.FloatingIp)
    | ReceiveServerPassword OSTypes.ServerPassword
    | ReceiveSetServerName String ErrorContext (Result HttpErrorWithBody String)
    | ReceiveSetServerMetadata OSTypes.MetadataItem ErrorContext (Result HttpErrorWithBody (List OSTypes.MetadataItem))
    | ReceiveDeleteServerMetadata OSTypes.MetadataKey ErrorContext (Result HttpErrorWithBody String)
    | ReceiveGuacamoleAuthToken (Result Http.Error GuacTypes.GuacamoleAuthToken)
    | RequestServerAction (HelperTypes.Url -> Cmd SharedMsg) (Maybe (List OSTypes.ServerStatus))
    | ReceiveConsoleLog ErrorContext (Result HttpErrorWithBody String)


type
    NavigableView
    -- TODO order these
    = LoginPicker
    | LoginOpenstack
    | LoginJetstream
    | HelpAbout
    | GetSupport (Maybe ( HelperTypes.SupportableItemType, Maybe HelperTypes.Uuid ))
    | ServerList HelperTypes.ProjectIdentifier
    | ServerDetail HelperTypes.ProjectIdentifier OSTypes.ServerUuid
    | ServerCreate HelperTypes.ProjectIdentifier OSTypes.ImageUuid String (Maybe Bool)
    | ServerCreateImage HelperTypes.ProjectIdentifier OSTypes.ServerUuid (Maybe String)
    | FloatingIpList HelperTypes.ProjectIdentifier
    | FloatingIpAssign HelperTypes.ProjectIdentifier (Maybe OSTypes.IpAddressUuid) (Maybe OSTypes.ServerUuid)
    | KeypairList HelperTypes.ProjectIdentifier
    | KeypairCreate HelperTypes.ProjectIdentifier
    | VolumeList HelperTypes.ProjectIdentifier
    | VolumeCreate HelperTypes.ProjectIdentifier
    | VolumeAttach HelperTypes.ProjectIdentifier (Maybe OSTypes.ServerUuid) (Maybe OSTypes.VolumeUuid)
    | VolumeDetail HelperTypes.ProjectIdentifier OSTypes.VolumeUuid
