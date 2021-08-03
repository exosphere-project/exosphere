module OpenStack.ServerActions exposing
    ( ActionType(..)
    , SelectMod(..)
    , ServerAction
    , getAllowed
    )

import Helpers.String
import Http
import Json.Encode
import OpenStack.Types as OSTypes
import Rest.Helpers exposing (expectStringWithErrorBody, openstackCredentialedRequest, resultToMsgErrorBody)
import Rest.Nova
import Types.Error exposing (ErrorContext, ErrorLevel(..))
import Types.HelperTypes exposing (ProjectIdentifier, Url)
import Types.Types
    exposing
        ( HttpRequestMethod(..)
        , Msg(..)
        , ProjectSpecificMsgConstructor(..)
        , Server
        , ServerSpecificMsgConstructor(..)
        )
import Types.View exposing (ProjectViewConstructor(..))


getAllowed : Maybe String -> Maybe String -> OSTypes.ServerStatus -> OSTypes.ServerLockStatus -> List ServerAction
getAllowed maybeWordForServer maybeWordForImage serverStatus serverLockStatus =
    let
        allowedByServerStatus action =
            case action.allowedStatuses of
                Nothing ->
                    True

                Just allowedStatuses ->
                    List.member serverStatus allowedStatuses

        allowedByLockStatus action =
            case action.allowedLockStatus of
                Nothing ->
                    True

                Just allowedLockStatus_ ->
                    serverLockStatus == allowedLockStatus_
    in
    actions maybeWordForServer maybeWordForImage
        |> List.filter allowedByServerStatus
        |> List.filter allowedByLockStatus


type alias ServerAction =
    { name : String
    , description : String
    , allowedStatuses : Maybe (List OSTypes.ServerStatus)
    , allowedLockStatus : Maybe OSTypes.ServerLockStatus
    , action : ActionType
    , selectMod : SelectMod
    , targetStatus : Maybe (List OSTypes.ServerStatus)
    , confirmable : Bool
    }


type ActionType
    = CmdAction (ProjectIdentifier -> Url -> OSTypes.ServerUuid -> Cmd Msg)
    | UpdateAction (ProjectIdentifier -> Server -> Msg)


type SelectMod
    = NoMod
    | Primary
    | Warning
    | Danger


actions : Maybe String -> Maybe String -> List ServerAction
actions maybeWordForServer maybeWordForImage =
    let
        wordForServer =
            maybeWordForServer
                |> Maybe.withDefault "server"

        wordForImage =
            maybeWordForImage
                |> Maybe.withDefault "image"
    in
    [ { name = "Lock"
      , description =
            String.join " "
                [ "Prevent further"
                , wordForServer
                , "actions until it is unlocked"
                ]
      , allowedStatuses = Nothing
      , allowedLockStatus = Just OSTypes.ServerUnlocked
      , action =
            CmdAction <|
                doAction (Json.Encode.object [ ( "lock", Json.Encode.null ) ])
      , selectMod = NoMod
      , targetStatus = Nothing
      , confirmable = False
      }
    , { name = "Unlock"
      , description =
            String.join " "
                [ "Allow further"
                , wordForServer
                , "actions"
                ]
      , allowedStatuses = Nothing
      , allowedLockStatus = Just OSTypes.ServerLocked
      , action =
            CmdAction <|
                doAction (Json.Encode.object [ ( "unlock", Json.Encode.null ) ])
      , selectMod = Warning
      , targetStatus = Nothing
      , confirmable = False
      }
    , { name = "Start"
      , description =
            String.join " "
                [ "Start stopped"
                , wordForServer
                ]
      , allowedStatuses = Just [ OSTypes.ServerStopped, OSTypes.ServerShutoff ]
      , allowedLockStatus = Just OSTypes.ServerUnlocked
      , action =
            CmdAction <|
                doAction <|
                    Json.Encode.object [ ( "os-start", Json.Encode.null ) ]
      , selectMod = Primary
      , targetStatus = Just [ OSTypes.ServerActive ]
      , confirmable = False
      }
    , { name = "Unpause"
      , description =
            String.join " "
                [ "Restore paused"
                , wordForServer
                ]
      , allowedStatuses = Just [ OSTypes.ServerPaused ]
      , allowedLockStatus = Just OSTypes.ServerUnlocked
      , action =
            CmdAction <|
                doAction <|
                    Json.Encode.object [ ( "unpause", Json.Encode.null ) ]
      , selectMod = Primary
      , targetStatus = Just [ OSTypes.ServerActive ]
      , confirmable = False
      }
    , { name = "Resume"
      , description =
            String.join " "
                [ "Resume suspended"
                , wordForServer
                ]
      , allowedStatuses = Just [ OSTypes.ServerSuspended ]
      , allowedLockStatus = Just OSTypes.ServerUnlocked
      , action =
            CmdAction <|
                doAction <|
                    Json.Encode.object [ ( "resume", Json.Encode.null ) ]
      , selectMod = Primary
      , targetStatus = Just [ OSTypes.ServerActive ]
      , confirmable = False
      }
    , { name = "Unshelve"
      , description =
            String.join " "
                [ "Restore shelved"
                , wordForServer
                ]
      , allowedStatuses = Just [ OSTypes.ServerShelved, OSTypes.ServerShelvedOffloaded ]
      , allowedLockStatus = Just OSTypes.ServerUnlocked
      , action =
            CmdAction <|
                doAction (Json.Encode.object [ ( "unshelve", Json.Encode.null ) ])
      , selectMod = Primary
      , targetStatus = Just [ OSTypes.ServerActive ]
      , confirmable = False
      }
    , { name = "Suspend"
      , description = "Save execution state to disk"
      , allowedStatuses = Just [ OSTypes.ServerActive ]
      , allowedLockStatus = Just OSTypes.ServerUnlocked
      , action =
            CmdAction <|
                doAction <|
                    Json.Encode.object [ ( "suspend", Json.Encode.null ) ]
      , selectMod = NoMod
      , targetStatus = Just [ OSTypes.ServerSuspended ]
      , confirmable = False
      }
    , { name = "Shelve"
      , description =
            String.join " "
                [ "Shut down"
                , wordForServer
                , "and offload it from compute host"
                ]
      , allowedStatuses = Just [ OSTypes.ServerActive, OSTypes.ServerShutoff, OSTypes.ServerPaused, OSTypes.ServerSuspended ]
      , allowedLockStatus = Just OSTypes.ServerUnlocked
      , action =
            CmdAction <|
                doAction (Json.Encode.object [ ( "shelve", Json.Encode.null ) ])
      , selectMod = NoMod
      , targetStatus = Just [ OSTypes.ServerShelved, OSTypes.ServerShelvedOffloaded ]
      , confirmable = False
      }
    , { name =
            wordForImage
                |> Helpers.String.toTitleCase
      , description =
            String.join " "
                [ "Create snapshot image of"
                , wordForServer
                ]
      , allowedStatuses = Just [ OSTypes.ServerActive, OSTypes.ServerShutoff, OSTypes.ServerPaused, OSTypes.ServerSuspended ]
      , allowedLockStatus = Nothing
      , action =
            UpdateAction <|
                \projectId server ->
                    ProjectMsg
                        projectId
                        (SetProjectView
                            (CreateServerImage
                                server.osProps.uuid
                                (server.osProps.name ++ "-image")
                            )
                        )
      , selectMod = NoMod
      , targetStatus = Just [ OSTypes.ServerActive ]
      , confirmable = False
      }
    , { name = "Reboot"
      , description =
            String.join " "
                [ "Restart"
                , wordForServer
                ]
      , allowedStatuses = Just [ OSTypes.ServerActive, OSTypes.ServerShutoff ]
      , allowedLockStatus = Just OSTypes.ServerUnlocked

      -- TODO soft and hard reboot? Call hard reboot "reset"?
      , action =
            CmdAction <|
                doAction <|
                    Json.Encode.object
                        [ ( "reboot"
                          , Json.Encode.object
                                [ ( "type", Json.Encode.string "SOFT" ) ]
                          )
                        ]
      , selectMod = Warning
      , targetStatus = Just [ OSTypes.ServerActive ]
      , confirmable = False
      }
    , { name = "Delete"
      , description =
            String.join " "
                [ "Destroy"
                , wordForServer
                ]
      , allowedStatuses =
            Just
                [ OSTypes.ServerPaused
                , OSTypes.ServerSuspended
                , OSTypes.ServerActive
                , OSTypes.ServerReboot
                , OSTypes.ServerShutoff
                , OSTypes.ServerStopped
                , OSTypes.ServerError
                , OSTypes.ServerBuilding
                , OSTypes.ServerRescued
                , OSTypes.ServerShelved
                , OSTypes.ServerShelvedOffloaded
                ]
      , allowedLockStatus = Just OSTypes.ServerUnlocked
      , action =
            CmdAction <| Rest.Nova.requestDeleteServer
      , selectMod = Danger
      , targetStatus = Just [ OSTypes.ServerSoftDeleted ]
      , confirmable = True
      }

    {-
       -- Not showing to users
       , { name = "Pause"
         , description = "Stop server execution but persist memory state"
         , allowedStatuses = [ OSTypes.ServerActive ]
         , action = doAction <| Json.Encode.object [ ( "pause", Json.Encode.null ) ]
         , selectMod = None
         , targetStatus = [ OSTypes.ServerPaused ]
         }
    -}
    {-
       -- Not showing to users
       , { name = "Stop"
         , description = "Shut down server"
         , allowedStatuses = [ OSTypes.ServerActive ]
         , action = doAction <| Json.Encode.object [ ( "os-stop", Json.Encode.null ) ]
         , selectMod = None
         , targetStatus = [ OSTypes.ServerStopped ]
         }
    -}
    ]


doAction : Json.Encode.Value -> ProjectIdentifier -> Url -> OSTypes.ServerUuid -> Cmd Msg
doAction body projectId novaUrl serverId =
    let
        errorContext =
            ErrorContext
                ("perform action for server " ++ serverId)
                ErrorCrit
                Nothing
    in
    openstackCredentialedRequest
        projectId
        Post
        Nothing
        (novaUrl ++ "/servers/" ++ serverId ++ "/action")
        (Http.jsonBody body)
        (expectStringWithErrorBody
            (resultToMsgErrorBody
                errorContext
                (\_ ->
                    ProjectMsg projectId <|
                        ServerMsg serverId <|
                            RequestServer
                )
            )
        )
