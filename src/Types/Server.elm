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
    , UnreachedTarget
    , abandonActionTargets
    , abandonTargetIfActionEnded
    , actionIsResumable
    , clearActionTargetIfTargetStatusReached
    , clearQuotaRefreshIntentIfTargetStatusReached
    , currentExoServerVersion
    , deleteTargetOpenstackStatus
    , exoSetupStatusToString
    , exoVersionSupportsFeature
    , failAction
    , initServerExoActions
    , isAwaitingConfirmation
    , requestOutcomeUnknown
    , resizeQuotaRefreshTargetOpenstackStatus
    , resizeTargetOpenstackStatus
    , serverActionQuotaRefreshTargetOpenstackStatus
    , serverActionTargetOpenstackStatus
    , shelveQuotaRefreshTargetOpenstackStatus
    , shelveTargetOpenstackStatus
    , shouldRefreshQuotaOnTargetStatus
    , taskStateUiStatus
    )

import Helpers.RemoteDataPlusPlus as RDPP
import Http
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
    , unreachedTarget : Maybe UnreachedTarget -- Nova accepted the action, then finished without reaching its target
    }


{-| The statuses an accepted action was waiting for when Nova finished working on it without reaching any of them.
-}
type alias UnreachedTarget =
    { statuses : List OSTypes.ServerStatus
    , noticedAt : Time.Posix
    }


initServerExoActions : ServerExoActions
initServerExoActions =
    { targetOpenstackStatus = Nothing
    , quotaRefreshTargetOpenstackStatus = Nothing
    , request = RDPP.empty
    , unreachedTarget = Nothing
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


abandonActionTargets : ServerExoActions -> ServerExoActions
abandonActionTargets exoActions =
    { exoActions
        | targetOpenstackStatus = Nothing
        , quotaRefreshTargetOpenstackStatus = Nothing
    }


{-| A timeout or network error says nothing about whether OpenStack received the request. Nova may have accepted the
action, and only the response was lost.
-}
requestOutcomeUnknown : HttpErrorWithBody -> Bool
requestOutcomeUnknown httpError =
    case httpError.error of
        Http.Timeout ->
            True

        Http.NetworkError ->
            True

        _ ->
            False


{-| The action's request failed in a way that does not tell us whether OpenStack received it, and we are still waiting
for the server to show whether it did.
-}
isAwaitingConfirmation : ServerExoActions -> Bool
isAwaitingConfirmation exoActions =
    case ( exoActions.targetOpenstackStatus, exoActions.request.refreshStatus ) of
        ( Just _, RDPP.NotLoading (Just ( httpError, _ )) ) ->
            requestOutcomeUnknown httpError

        _ ->
            False


{-| Record a failed action's request.

When OpenStack rejected it, the instance will not reach the target statuses, so the UI falls back to the status
OpenStack reports. When we cannot tell whether OpenStack received it, keep the targets: polling either sees the target
reached, or sees that no action is underway and abandons them (see `abandonTargetIfActionEnded`).

-}
failAction : HttpErrorWithBody -> Time.Posix -> ServerExoActions -> ServerExoActions
failAction httpError failedAt exoActions =
    let
        failed =
            { exoActions
                | request = RDPP.setNotLoading (Just ( httpError, failedAt )) exoActions.request
                , unreachedTarget = Nothing
            }
    in
    if requestOutcomeUnknown httpError then
        failed

    else
        abandonActionTargets failed


{-| How long after Nova accepts an action before we trust that a server response reflects it.

A poll sent before the action was accepted can arrive after it, showing the server as it was. Until the time a request
was sent is tracked (#1127), wait out any such response.

-}
actionSettleMillis : Int
actionSettleMillis =
    15 * 1000


{-| Nova accepted the action (HTTP 202) but may later fail it asynchronously. For example, an unshelve that finds no
valid host returns the instance to SHELVED\_OFFLOADED with no task in progress, and records no fault.

Once Nova has no task in progress and the server is not in a target status, the action has ended without reaching its
target. Stop waiting, and record what was missed so the UI can say so.

The same check settles a request whose outcome is unknown (see `requestOutcomeUnknown`). If no action is underway once
the error has settled, OpenStack never received the request, or received it and has finished without reaching the
target. Either way, stop waiting and leave the error for the UI to report.

-}
abandonTargetIfActionEnded : Time.Posix -> OSTypes.ServerStatus -> Maybe String -> ServerExoActions -> ServerExoActions
abandonTargetIfActionEnded receivedAt currentStatus taskState exoActions =
    let
        actionEnded targets since =
            let
                settled =
                    Time.posixToMillis receivedAt - Time.posixToMillis since >= actionSettleMillis

                -- Nova confirms a resize without setting a task state, so VERIFY_RESIZE with no task is expected while
                -- a confirmation is underway.
                confirmingResize =
                    currentStatus == OSTypes.ServerVerifyResize
            in
            settled
                && (taskState == Nothing)
                && not (List.member currentStatus targets)
                && not confirmingResize
    in
    case ( exoActions.targetOpenstackStatus, exoActions.request.refreshStatus, exoActions.request.data ) of
        ( Just targets, RDPP.NotLoading Nothing, RDPP.DoHave () acceptedAt ) ->
            if actionEnded targets acceptedAt then
                let
                    abandoned =
                        abandonActionTargets exoActions
                in
                { abandoned | unreachedTarget = Just { statuses = targets, noticedAt = receivedAt } }

            else
                exoActions

        ( Just targets, RDPP.NotLoading (Just ( httpError, erroredAt )), _ ) ->
            -- Any request sent before the error was sent before the action, too, so settle from the error.
            if requestOutcomeUnknown httpError && actionEnded targets erroredAt then
                abandonActionTargets exoActions

            else
                exoActions

        _ ->
            exoActions


{-| The in-progress UI status that Nova's task state implies, regardless of what Exosphere asked for. This shows
transitions started outside Exosphere, and ones Exosphere has stopped waiting for.
-}
taskStateUiStatus : OSTypes.ServerStatus -> Maybe String -> Maybe ServerUiStatus
taskStateUiStatus status taskState =
    case ( status, taskState ) of
        ( OSTypes.ServerShelved, Just "unshelving" ) ->
            Just ServerUiStatusUnshelving

        ( OSTypes.ServerShelved, Just "powering-on" ) ->
            Just ServerUiStatusUnshelving

        ( OSTypes.ServerShelvedOffloaded, Just "unshelving" ) ->
            Just ServerUiStatusUnshelving

        ( OSTypes.ServerShelvedOffloaded, Just "spawning" ) ->
            Just ServerUiStatusUnshelving

        ( OSTypes.ServerShelved, _ ) ->
            -- Includes shelving_offloading, which we continue to show as shelved.
            Nothing

        ( OSTypes.ServerShelvedOffloaded, _ ) ->
            Nothing

        ( _, Just "shelving" ) ->
            Just ServerUiStatusShelving

        ( _, Just "shelving_image_pending_upload" ) ->
            Just ServerUiStatusShelving

        ( _, Just "shelving_image_uploading" ) ->
            Just ServerUiStatusShelving

        _ ->
            Nothing


{-| An action that was left loading was probably abandoned in flight.
Do not restore it when rehydrating state. Errors, however, are a definite
outcome & we can show them in the UI.
-}
actionIsResumable : ServerExoActions -> Bool
actionIsResumable exoActions =
    case exoActions.request.refreshStatus of
        RDPP.Loading ->
            False

        RDPP.NotLoading (Just _) ->
            True

        RDPP.NotLoading Nothing ->
            let
                awaitingTransition =
                    RDPP.gotData exoActions.request && exoActions.targetOpenstackStatus /= Nothing

                awaitingQuotaRefresh =
                    exoActions.quotaRefreshTargetOpenstackStatus /= Nothing

                -- Reported to the user until they dismiss it, like an error.
                reportingUnreachedTarget =
                    exoActions.unreachedTarget /= Nothing
            in
            awaitingTransition || awaitingQuotaRefresh || reportingUnreachedTarget


{-| A deleted server leaves the server list, so reaching these is rarely observed. They let polling settle a delete
whose outcome is unknown.
-}
deleteTargetOpenstackStatus : Maybe (List OSTypes.ServerStatus)
deleteTargetOpenstackStatus =
    Just [ OSTypes.ServerDeleted, OSTypes.ServerSoftDeleted ]


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
