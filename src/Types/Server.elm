module Types.Server exposing
    ( ExoServerProps
    , ExoServerVersion
    , ExoSetupStatus(..)
    , NewServerNetworkOptions(..)
    , ResourceUsageRDPP
    , Server
    , ServerFromExoProps
    , ServerOrigin(..)
    , ServerUiStatus(..)
    , currentExoServerVersion
    )

import Helpers.RemoteDataPlusPlus as RDPP
import OpenStack.Types as OSTypes
import RemoteData exposing (WebData)
import Time
import Types.Error exposing (HttpErrorWithBody)
import Types.Guacamole as GuacTypes
import Types.HelperTypes as HelperTypes
import Types.ServerResourceUsage
import Types.Workflow as WorkflowTypes


type alias Server =
    { osProps : OSTypes.Server
    , exoProps : ExoServerProps
    , events : WebData (List OSTypes.ServerEvent)
    }


type alias ExoServerProps =
    { floatingIpCreationOption : HelperTypes.FloatingIpOption
    , deletionAttempted : Bool
    , targetOpenstackStatus : Maybe (List OSTypes.ServerStatus) -- Maybe we have performed an instance action and are waiting for server to reflect that
    , serverOrigin : ServerOrigin
    , receivedTime : Maybe Time.Posix -- Used only if this server was polled more recently than the other servers in the project
    , loadingSeparately : Bool -- Again, used only if server was polled more recently on its own.
    }


type ServerOrigin
    = ServerFromExo ServerFromExoProps
    | ServerNotFromExo


type alias ServerFromExoProps =
    { exoServerVersion : ExoServerVersion
    , exoSetupStatus : RDPP.RemoteDataPlusPlus HttpErrorWithBody ( ExoSetupStatus, Maybe Time.Posix )
    , resourceUsage : ResourceUsageRDPP
    , guacamoleStatus : GuacTypes.ServerGuacamoleStatus
    , customWorkflowStatus : WorkflowTypes.ServerCustomWorkflowStatus
    , exoCreatorUsername : Maybe String
    }


type alias ResourceUsageRDPP =
    RDPP.RemoteDataPlusPlus HttpErrorWithBody Types.ServerResourceUsage.History


type alias ExoServerVersion =
    Int


currentExoServerVersion : ExoServerVersion
currentExoServerVersion =
    4


type ServerUiStatus
    = ServerUiStatusUnknown
    | ServerUiStatusBuilding
    | ServerUiStatusRunningSetup
    | ServerUiStatusReady
    | ServerUiStatusPaused
    | ServerUiStatusUnpausing
    | ServerUiStatusRebooting
    | ServerUiStatusSuspending
    | ServerUiStatusSuspended
    | ServerUiStatusResuming
    | ServerUiStatusShutoff
    | ServerUiStatusStopped
    | ServerUiStatusStarting
    | ServerUiStatusDeleting
    | ServerUiStatusSoftDeleted
    | ServerUiStatusError
    | ServerUiStatusRescued
    | ServerUiStatusShelving
    | ServerUiStatusShelved
    | ServerUiStatusUnshelving
    | ServerUiStatusDeleted
    | ServerUiStatusResizing
    | ServerUiStatusVerifyResize
    | ServerUiStatusRevertingResize
    | ServerUiStatusMigrating
    | ServerUiStatusPassword


type ExoSetupStatus
    = ExoSetupWaiting
    | ExoSetupRunning
    | ExoSetupComplete
    | ExoSetupError
    | ExoSetupTimeout
    | ExoSetupUnknown


type NewServerNetworkOptions
    = NetworksLoading
    | AutoSelectedNetwork OSTypes.NetworkUuid
    | ManualNetworkSelection
    | NoneAvailable
