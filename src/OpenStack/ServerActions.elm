module OpenStack.ServerActions exposing (ServerAction(..), ServerActionName, serverActionToString, stringToServerAction)


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
    | UnsupportedAction String


type alias ServerActionName =
    String


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

        _ ->
            UnsupportedAction str
