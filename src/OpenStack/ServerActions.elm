module OpenStack.ServerActions exposing (getAllowed)

import Framework.Modifier as Modifier
import Helpers.Helpers as Helpers
import Http
import Json.Encode
import OpenStack.Types as OSTypes
import Rest.Helpers exposing (openstackCredentialedRequest)
import Rest.Rest as Rest
import Types.Types exposing (..)


type alias ServerAction =
    { name : String
    , description : String

    -- Todo enforce uniqueness by using a different collection (something like a Set, except ServerAction types aren't "comparable")
    , allowedStatus : List OSTypes.ServerStatus
    , action : Project -> Server -> Cmd Msg
    , selectMods : List Modifier.Modifier
    , targetStatus : List OSTypes.ServerStatus
    }


getAllowed : OSTypes.ServerStatus -> List ServerAction
getAllowed serverStatus =
    -- TODO write tests for this? Or make impossible state impossible, i.e. can't call server actions that are not allowed based on server status?
    let
        actionIsAllowed : OSTypes.ServerStatus -> ServerAction -> Bool
        actionIsAllowed status action =
            List.member status action.allowedStatus
    in
    List.filter (actionIsAllowed serverStatus) actions


actions : List ServerAction
actions =
    [ { name = "Start"
      , description = "Start stopped server"
      , allowedStatus = [ OSTypes.ServerStopped, OSTypes.ServerShutoff ]
      , action =
            doAction <|
                Json.Encode.object [ ( "os-start", Json.Encode.null ) ]
      , selectMods = [ Modifier.Primary ]
      , targetStatus = [ OSTypes.ServerActive ]
      }
    , { name = "Unpause"
      , description = "Restore paused server"
      , allowedStatus = [ OSTypes.ServerPaused ]
      , action =
            doAction <|
                Json.Encode.object [ ( "unpause", Json.Encode.null ) ]
      , selectMods = [ Modifier.Primary ]
      , targetStatus = [ OSTypes.ServerActive ]
      }
    , { name = "Resume"
      , description = "Resume suspended server"
      , allowedStatus = [ OSTypes.ServerSuspended ]
      , action = doAction <| Json.Encode.object [ ( "resume", Json.Encode.null ) ]
      , selectMods = [ Modifier.Primary ]
      , targetStatus = [ OSTypes.ServerActive ]
      }
    , { name = "Unshelve"
      , description = "Restore shelved server"
      , allowedStatus = [ OSTypes.ServerShelved, OSTypes.ServerShelvedOffloaded ]
      , action = doAction (Json.Encode.object [ ( "unshelve", Json.Encode.null ) ])
      , selectMods = [ Modifier.Primary ]
      , targetStatus = [ OSTypes.ServerActive ]
      }
    , { name = "Suspend"
      , description = "Save execution state to disk"
      , allowedStatus = [ OSTypes.ServerActive ]
      , action = doAction <| Json.Encode.object [ ( "suspend", Json.Encode.null ) ]
      , selectMods = []
      , targetStatus = [ OSTypes.ServerSuspended ]
      }
    , { name = "Shelve"
      , description = "Shut down server and offload it from compute host"
      , allowedStatus = [ OSTypes.ServerActive, OSTypes.ServerShutoff, OSTypes.ServerPaused, OSTypes.ServerSuspended ]
      , action = doAction (Json.Encode.object [ ( "shelve", Json.Encode.null ) ])
      , selectMods = []
      , targetStatus = [ OSTypes.ServerShelved, OSTypes.ServerShelvedOffloaded ]
      }
    , { name = "Reboot"
      , description = "Restart server"
      , allowedStatus = [ OSTypes.ServerActive, OSTypes.ServerShutoff ]

      -- TODO soft and hard reboot? Call hard reboot "reset"?
      , action =
            doAction <|
                Json.Encode.object
                    [ ( "reboot"
                      , Json.Encode.object
                            [ ( "type", Json.Encode.string "SOFT" ) ]
                      )
                    ]
      , selectMods = [ Modifier.Warning ]
      , targetStatus = [ OSTypes.ServerActive ]
      }
    , { name = "Delete"
      , description = "Destroy server"
      , allowedStatus =
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
      , action = Rest.requestDeleteServer
      , selectMods = [ Modifier.Danger ]
      , targetStatus = [ OSTypes.ServerSoftDeleted ]
      }

    {-
       -- Not showing to users
       , { name = "Pause"
         , description = "Stop server execution but persist memory state"
         , allowedStatus = [ OSTypes.ServerActive ]
         , action = doAction <| Json.Encode.object [ ( "pause", Json.Encode.null ) ]
         , selectMods = []
         , targetStatus = [ OSTypes.ServerPaused ]
         }
    -}
    {-
       -- Not showing to users
       , { name = "Stop"
         , description = "Shut down server"
         , allowedStatus = [ OSTypes.ServerActive ]
         , action = doAction <| Json.Encode.object [ ( "os-stop", Json.Encode.null ) ]
         , selectMods = []
         , targetStatus = [ OSTypes.ServerStopped ]
         }
    -}
    ]


doAction : Json.Encode.Value -> Project -> Server -> Cmd Msg
doAction body project server =
    openstackCredentialedRequest
        project
        Post
        (project.endpoints.nova ++ "/servers/" ++ server.osProps.uuid ++ "/action")
        (Http.jsonBody body)
        (Http.expectString
            (\result -> ProjectMsg (Helpers.getProjectId project) (ReceiveServerAction server.osProps.uuid result))
        )
