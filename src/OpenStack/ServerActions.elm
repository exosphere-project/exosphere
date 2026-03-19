module OpenStack.ServerActions exposing (ServerAction(..), ServerActionName, serverActionToJsonBody, serverActionToString, stringToServerAction)

import Json.Encode as Encode


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
