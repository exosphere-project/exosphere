module OpenStack.ServerActions exposing (getAllowed)

import Error exposing (ErrorContext, ErrorLevel(..))
import Framework.Modifier as Modifier
import Http
import Json.Encode
import OpenStack.Types as OSTypes
import Rest.Helpers exposing (openstackCredentialedRequest, resultToMsg)
import Rest.Rest as Rest
import Types.Types
    exposing
        ( ActionType(..)
        , HttpRequestMethod(..)
        , Msg(..)
        , Project
        , ProjectSpecificMsgConstructor(..)
        , ProjectViewConstructor(..)
        , Server
        , ServerAction
        )


getAllowed : OSTypes.ServerStatus -> List ServerAction
getAllowed serverStatus =
    List.filter
        (\action ->
            List.member serverStatus action.allowedStatuses
        )
        actions


actions : List Types.Types.ServerAction
actions =
    [ { name = "Start"
      , description = "Start stopped server"
      , allowedStatuses = [ OSTypes.ServerStopped, OSTypes.ServerShutoff ]
      , action =
            CmdAction <|
                doAction <|
                    Json.Encode.object [ ( "os-start", Json.Encode.null ) ]
      , selectMods = [ Modifier.Primary ]
      , targetStatus = [ OSTypes.ServerActive ]
      , confirmable = False
      }
    , { name = "Unpause"
      , description = "Restore paused server"
      , allowedStatuses = [ OSTypes.ServerPaused ]
      , action =
            CmdAction <|
                doAction <|
                    Json.Encode.object [ ( "unpause", Json.Encode.null ) ]
      , selectMods = [ Modifier.Primary ]
      , targetStatus = [ OSTypes.ServerActive ]
      , confirmable = False
      }
    , { name = "Resume"
      , description = "Resume suspended server"
      , allowedStatuses = [ OSTypes.ServerSuspended ]
      , action =
            CmdAction <|
                doAction <|
                    Json.Encode.object [ ( "resume", Json.Encode.null ) ]
      , selectMods = [ Modifier.Primary ]
      , targetStatus = [ OSTypes.ServerActive ]
      , confirmable = False
      }
    , { name = "Unshelve"
      , description = "Restore shelved server"
      , allowedStatuses = [ OSTypes.ServerShelved, OSTypes.ServerShelvedOffloaded ]
      , action =
            CmdAction <|
                doAction (Json.Encode.object [ ( "unshelve", Json.Encode.null ) ])
      , selectMods = [ Modifier.Primary ]
      , targetStatus = [ OSTypes.ServerActive ]
      , confirmable = False
      }
    , { name = "Suspend"
      , description = "Save execution state to disk"
      , allowedStatuses = [ OSTypes.ServerActive ]
      , action =
            CmdAction <|
                doAction <|
                    Json.Encode.object [ ( "suspend", Json.Encode.null ) ]
      , selectMods = []
      , targetStatus = [ OSTypes.ServerSuspended ]
      , confirmable = False
      }
    , { name = "Shelve"
      , description = "Shut down server and offload it from compute host"
      , allowedStatuses = [ OSTypes.ServerActive, OSTypes.ServerShutoff, OSTypes.ServerPaused, OSTypes.ServerSuspended ]
      , action =
            CmdAction <|
                doAction (Json.Encode.object [ ( "shelve", Json.Encode.null ) ])
      , selectMods = []
      , targetStatus = [ OSTypes.ServerShelved, OSTypes.ServerShelvedOffloaded ]
      , confirmable = False
      }
    , { name = "Image"
      , description = "Create snapshot image of server"
      , allowedStatuses = [ OSTypes.ServerActive, OSTypes.ServerShutoff, OSTypes.ServerPaused, OSTypes.ServerSuspended ]
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
      , selectMods = []
      , targetStatus = [ OSTypes.ServerActive ]
      , confirmable = False
      }
    , { name = "Reboot"
      , description = "Restart server"
      , allowedStatuses = [ OSTypes.ServerActive, OSTypes.ServerShutoff ]

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
      , selectMods = [ Modifier.Warning ]
      , targetStatus = [ OSTypes.ServerActive ]
      , confirmable = False
      }
    , { name = "Delete"
      , description = "Destroy server"
      , allowedStatuses =
            [ OSTypes.ServerPaused
            , OSTypes.ServerSuspended
            , OSTypes.ServerActive
            , OSTypes.ServerShutoff
            , OSTypes.ServerStopped
            , OSTypes.ServerError
            , OSTypes.ServerBuilding
            , OSTypes.ServerRescued
            , OSTypes.ServerShelved
            , OSTypes.ServerShelvedOffloaded
            ]
      , action =
            CmdAction <| Rest.requestDeleteServer
      , selectMods = [ Modifier.Danger ]
      , targetStatus = [ OSTypes.ServerSoftDeleted ]
      , confirmable = True
      }

    {-
       -- Not showing to users
       , { name = "Pause"
         , description = "Stop server execution but persist memory state"
         , allowedStatuses = [ OSTypes.ServerActive ]
         , action = doAction <| Json.Encode.object [ ( "pause", Json.Encode.null ) ]
         , selectMods = []
         , targetStatus = [ OSTypes.ServerPaused ]
         }
    -}
    {-
       -- Not showing to users
       , { name = "Stop"
         , description = "Shut down server"
         , allowedStatuses = [ OSTypes.ServerActive ]
         , action = doAction <| Json.Encode.object [ ( "os-stop", Json.Encode.null ) ]
         , selectMods = []
         , targetStatus = [ OSTypes.ServerStopped ]
         }
    -}
    ]


doAction : Json.Encode.Value -> Project -> Server -> Cmd Msg
doAction body project server =
    let
        errorContext =
            ErrorContext
                ("perform action for server " ++ server.osProps.uuid)
                ErrorCrit
                Nothing
    in
    openstackCredentialedRequest
        project
        Post
        Nothing
        (project.endpoints.nova ++ "/servers/" ++ server.osProps.uuid ++ "/action")
        (Http.jsonBody body)
        (Http.expectString
            (resultToMsg errorContext (\_ -> NoOp))
        )
