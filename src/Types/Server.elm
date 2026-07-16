module Types.Server exposing
    ( ExoFeature(..)
    , ExoServerProps
    , ExoServerVersion
    , ExoSetupStatus(..)
    , NewServerNetworkOptions(..)
    , ResourceUsageRDPP
    , Server
    , ServerExoActions
    , ServerFromExoProps
    , ServerOrigin(..)
    , ServerUiStatus(..)
    , clearActionTargetIfTargetStatusReached
    , clearQuotaRefreshIntentIfTargetStatusReached
    , currentExoServerVersion
    , exoSetupStatusToString
    , exoVersionSupportsFeature
    , initServerExoActions
    , resizeQuotaRefreshTargetOpenstackStatus
    , resizeTargetOpenstackStatus
    , serverActionQuotaRefreshTargetOpenstackStatus
    , serverActionTargetOpenstackStatus
    , shelveQuotaRefreshTargetOpenstackStatus
    , shelveTargetOpenstackStatus
    , shouldRefreshQuotaOnTargetStatus
    )

import Helpers.RemoteDataPlusPlus as RDPP
import OpenStack.ServerActions as ServerActions
import OpenStack.Types as OSTypes
import Time
import Types.Error exposing (HttpErrorWithBody)
import Types.Guacamole as GuacTypes
import Types.HelperTypes as HelperTypes
import Types.Interactivity exposing (InteractionLevel)
import Types.ServerResourceUsage
import Types.Workflow as WorkflowTypes


type alias Server =
    { osProps : OSTypes.Server
    , exoProps : ExoServerProps
    , interaction : InteractionLevel
    }


type alias ExoServerProps =
    { floatingIpCreationOption : HelperTypes.FloatingIpOption
    , deletionAttempted : Bool
    , serverOrigin : ServerOrigin
    , receivedTime : Maybe Time.Posix -- Used only if this server was polled more recently than the other servers in the project
    , loadingSeparately : Bool -- Again, used only if server was polled more recently on its own.
    }


type alias ServerExoActions =
    { targetOpenstackStatus : Maybe (List OSTypes.ServerStatus) -- Maybe we have performed an instance action and are waiting for server to reflect that
    , quotaRefreshTargetOpenstackStatus : Maybe (List OSTypes.ServerStatus)
    , request : RDPP.RemoteDataPlusPlus HttpErrorWithBody ()
    }


initServerExoActions : ServerExoActions
initServerExoActions =
    { targetOpenstackStatus = Nothing
    , quotaRefreshTargetOpenstackStatus = Nothing
    , request = RDPP.empty
    }


shouldRefreshQuotaOnTargetStatus : ServerExoActions -> OSTypes.ServerStatus -> Bool
shouldRefreshQuotaOnTargetStatus exoActions currentStatus =
    exoActions.quotaRefreshTargetOpenstackStatus
        |> Maybe.map (List.member currentStatus)
        |> Maybe.withDefault False


clearQuotaRefreshIntentIfTargetStatusReached : ServerExoActions -> OSTypes.ServerStatus -> ServerExoActions
clearQuotaRefreshIntentIfTargetStatusReached exoActions currentStatus =
    if shouldRefreshQuotaOnTargetStatus exoActions currentStatus then
        { exoActions
            | quotaRefreshTargetOpenstackStatus = Nothing
        }

    else
        exoActions


clearActionTargetIfTargetStatusReached : ServerExoActions -> OSTypes.ServerStatus -> ServerExoActions
clearActionTargetIfTargetStatusReached exoActions currentStatus =
    let
        targetReached =
            exoActions.targetOpenstackStatus
                |> Maybe.map (List.member currentStatus)
                |> Maybe.withDefault False
    in
    if targetReached then
        { exoActions
            | targetOpenstackStatus = Nothing
            , request = RDPP.empty
        }

    else
        exoActions


shelveTargetOpenstackStatus : Maybe (List OSTypes.ServerStatus)
shelveTargetOpenstackStatus =
    Just [ OSTypes.ServerShelved, OSTypes.ServerShelvedOffloaded ]


shelveQuotaRefreshTargetOpenstackStatus : Maybe (List OSTypes.ServerStatus)
shelveQuotaRefreshTargetOpenstackStatus =
    Just [ OSTypes.ServerShelvedOffloaded ]


resizeTargetOpenstackStatus : Maybe (List OSTypes.ServerStatus)
resizeTargetOpenstackStatus =
    Just [ OSTypes.ServerResize ]


resizeQuotaRefreshTargetOpenstackStatus : Maybe (List OSTypes.ServerStatus)
resizeQuotaRefreshTargetOpenstackStatus =
    -- Nova holds Placement allocations on both source and destination during VERIFY_RESIZE until confirmed or reverted.
    -- So we refresh the quota again after resizing settles.
    Just [ OSTypes.ServerVerifyResize, OSTypes.ServerActive, OSTypes.ServerShutoff ]


serverActionTargetOpenstackStatus : ServerActions.ServerAction -> Maybe (List OSTypes.ServerStatus)
serverActionTargetOpenstackStatus action =
    case action of
        ServerActions.Unshelve ->
            Just [ OSTypes.ServerActive ]

        ServerActions.ConfirmResize ->
            -- Servers can be resized from shutoff, in which case they return to that status.
            Just [ OSTypes.ServerActive, OSTypes.ServerShutoff ]

        ServerActions.RevertResize ->
            Just [ OSTypes.ServerActive, OSTypes.ServerShutoff ]

        ServerActions.Start ->
            Just [ OSTypes.ServerActive ]

        ServerActions.Unpause ->
            Just [ OSTypes.ServerActive ]

        ServerActions.Resume ->
            Just [ OSTypes.ServerActive ]

        ServerActions.Suspend ->
            Just [ OSTypes.ServerSuspended ]

        ServerActions.Reboot ->
            Just [ OSTypes.ServerActive ]

        _ ->
            Nothing


serverActionQuotaRefreshTargetOpenstackStatus : ServerActions.ServerAction -> Maybe (List OSTypes.ServerStatus)
serverActionQuotaRefreshTargetOpenstackStatus action =
    case action of
        ServerActions.ConfirmResize ->
            Just [ OSTypes.ServerActive, OSTypes.ServerShutoff ]

        ServerActions.RevertResize ->
            Just [ OSTypes.ServerActive, OSTypes.ServerShutoff ]

        ServerActions.Unshelve ->
            Just [ OSTypes.ServerActive ]

        _ ->
            Nothing


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
    6


type ExoFeature
    = NamedMountpoints


exoVersionSupportsFeature : ExoFeature -> ExoServerVersion -> Bool
exoVersionSupportsFeature feature version =
    case feature of
        NamedMountpoints ->
            version >= 5


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
    | ExoSetupStarting
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


exoSetupStatusToString : ExoSetupStatus -> String
exoSetupStatusToString status =
    case status of
        ExoSetupWaiting ->
            "Waiting"

        ExoSetupStarting ->
            "Starting"

        ExoSetupRunning ->
            "Running"

        ExoSetupComplete ->
            "Complete"

        ExoSetupError ->
            "Error"

        ExoSetupTimeout ->
            "Timeout"

        ExoSetupUnknown ->
            "Unknown"
