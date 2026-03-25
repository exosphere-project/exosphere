module OpenStack.ServerActions exposing (ApplicableServerStatuses(..), ServerAction(..), ServerActionName, allServerActions, allowedLockStatus, allowedServerStatuses, serverActionToJsonBody, serverActionToString, stringToServerAction)

import Json.Encode as Encode
import OpenStack.Types exposing (ServerLockStatus(..), ServerStatus(..))


type ServerAction
    = ConfirmResize
    | RevertResize
    | Lock
    | Unlock
    | Start
    | Stop
    | Unpause
    | Pause
    | Resume
    | Suspend
    | Shelve
    | Unshelve
    | Reboot
    | Resize
    | CreateImage
    | Delete
    | UnsupportedAction String


allServerActions : List ServerAction
allServerActions =
    [ ConfirmResize
    , RevertResize
    , Lock
    , Unlock
    , Start
    , Stop
    , Unpause
    , Pause
    , Resume
    , Suspend
    , Shelve
    , Unshelve
    , Reboot
    , Resize
    , CreateImage
    , Delete
    ]


type alias ServerActionName =
    String


type ApplicableServerStatuses
    = AnyServerStatus
    | SpecificServerStatuses (List ServerStatus)
    | NoServerStatuses


allowedServerStatuses : ServerAction -> ApplicableServerStatuses
allowedServerStatuses action =
    case action of
        ConfirmResize ->
            SpecificServerStatuses [ ServerVerifyResize ]

        RevertResize ->
            SpecificServerStatuses [ ServerVerifyResize ]

        Lock ->
            AnyServerStatus

        Unlock ->
            AnyServerStatus

        Start ->
            SpecificServerStatuses [ ServerShutoff, ServerStopped ]

        Stop ->
            SpecificServerStatuses [ ServerActive ]

        Unpause ->
            SpecificServerStatuses [ ServerPaused ]

        Pause ->
            SpecificServerStatuses [ ServerActive ]

        Resume ->
            SpecificServerStatuses [ ServerSuspended ]

        Suspend ->
            SpecificServerStatuses [ ServerActive ]

        Shelve ->
            SpecificServerStatuses [ ServerActive, ServerPaused, ServerShutoff, ServerSuspended ]

        Unshelve ->
            SpecificServerStatuses [ ServerShelved, ServerShelvedOffloaded ]

        Reboot ->
            SpecificServerStatuses [ ServerActive, ServerShutoff ]

        Resize ->
            SpecificServerStatuses [ ServerActive, ServerShutoff ]

        CreateImage ->
            SpecificServerStatuses [ ServerActive, ServerPaused, ServerShutoff, ServerSuspended ]

        Delete ->
            SpecificServerStatuses
                [ ServerActive
                , ServerBuild
                , ServerError
                , ServerHardReboot
                , ServerMigrating
                , ServerPassword
                , ServerPaused
                , ServerReboot
                , ServerRebuild
                , ServerRescue
                , ServerResize
                , ServerRevertResize
                , ServerShelved
                , ServerShelvedOffloaded
                , ServerShutoff
                , ServerStopped
                , ServerSuspended
                , ServerUnknown
                , ServerVerifyResize
                ]

        UnsupportedAction _ ->
            NoServerStatuses


allowedLockStatus : ServerAction -> Maybe ServerLockStatus
allowedLockStatus action =
    case action of
        Unlock ->
            Just ServerLocked

        CreateImage ->
            Nothing

        _ ->
            Just ServerUnlocked


serverActionToJsonBody : ServerAction -> Encode.Value
serverActionToJsonBody action =
    case action of
        Reboot ->
            Encode.object
                [ ( "reboot"
                  , Encode.object
                        [ ( "type", Encode.string "SOFT" ) ]
                  )
                ]

        _ ->
            Encode.object [ ( serverActionToString action, Encode.null ) ]


serverActionToString : ServerAction -> String
serverActionToString serverAction =
    case serverAction of
        ConfirmResize ->
            "confirmResize"

        RevertResize ->
            "revertResize"

        Lock ->
            "lock"

        Unlock ->
            "unlock"

        Start ->
            "os-start"

        Stop ->
            "os-stop"

        Unpause ->
            "unpause"

        Pause ->
            "pause"

        Resume ->
            "resume"

        Suspend ->
            "suspend"

        Shelve ->
            "shelve"

        Unshelve ->
            "unshelve"

        Reboot ->
            "reboot"

        Resize ->
            "resize"

        CreateImage ->
            "createImage"

        Delete ->
            "delete"

        UnsupportedAction str ->
            str


stringToServerAction : String -> ServerAction
stringToServerAction str =
    case str of
        "confirmResize" ->
            ConfirmResize

        "revertResize" ->
            RevertResize

        "lock" ->
            Lock

        "unlock" ->
            Unlock

        "os-start" ->
            Start

        "os-stop" ->
            Stop

        "unpause" ->
            Unpause

        "pause" ->
            Pause

        "resume" ->
            Resume

        "suspend" ->
            Suspend

        "shelve" ->
            Shelve

        "unshelve" ->
            Unshelve

        "reboot" ->
            Reboot

        "resize" ->
            Resize

        "createImage" ->
            CreateImage

        "delete" ->
            Delete

        _ ->
            UnsupportedAction str
