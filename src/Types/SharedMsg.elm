module Types.SharedMsg exposing
    ( ProjectSpecificMsgConstructor(..)
    , ServerSpecificMsgConstructor(..)
    , SharedMsg(..)
    , TickInterval
    )

import Browser
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
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ToastyMsg (Toasty.Msg Toast)
    | MsgChangeWindowSize Int Int
    | SetStyle Style.Types.StyleMode
    | SetExperimentalFeaturesEnabled Bool
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
