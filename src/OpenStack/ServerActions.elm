module OpenStack.ServerActions exposing
    ( SelectMod(..)
    , ServerAction
    , getAllowed
    )

import Helpers.String
import Http
import Json.Encode
import OpenStack.Types as OSTypes
import Rest.Helpers exposing (expectStringWithErrorBody, openstackCredentialedRequest, resultToMsgErrorBody)
import Types.Error exposing (ErrorContext, ErrorLevel(..))
import Types.HelperTypes exposing (HttpRequestMethod(..), ProjectIdentifier, Url)
import Types.Server exposing (Server)
import Types.SharedMsg as SharedMsg
    exposing
        ( ProjectSpecificMsgConstructor(..)
        , ServerSpecificMsgConstructor(..)
        , SharedMsg(..)
        )


getAllowed : Maybe String -> Maybe String -> Maybe String -> OSTypes.ServerStatus -> OSTypes.ServerLockStatus -> List ServerAction
getAllowed maybeWordForServer maybeWordForImage maybeWordForFlavor serverStatus serverLockStatus =
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
    actions maybeWordForServer maybeWordForImage maybeWordForFlavor
        |> List.filter allowedByServerStatus
        |> List.filter allowedByLockStatus


type alias ServerAction =
    { name : String
    , description : String
    , allowedStatuses : Maybe (List OSTypes.ServerStatus)
    , allowedLockStatus : Maybe OSTypes.ServerLockStatus
    , action : ProjectIdentifier -> Server -> Bool -> SharedMsg
    , selectMod : SelectMod
    , confirmable : Bool
    }


type SelectMod
    = NoMod
    | Primary
    | Warning
    | Danger


actions : Maybe String -> Maybe String -> Maybe String -> List ServerAction
actions maybeWordForServer maybeWordForImage maybeWordForFlavor =
    let
        wordForServer =
            maybeWordForServer
                |> Maybe.withDefault "server"

        wordForImage =
            maybeWordForImage
                |> Maybe.withDefault "image"

        wordForFlavor =
            maybeWordForFlavor
                |> Maybe.withDefault "flavor"
    in
    [ { name = "Confirm"
      , description =
            String.join " "
                [ "Finish"
                , wordForServer
                , "resize operation"
                ]
      , allowedStatuses = Just [ OSTypes.ServerVerifyResize ]
      , allowedLockStatus = Just OSTypes.ServerUnlocked
      , action =
            doAction (Json.Encode.object [ ( "confirmResize", Json.Encode.null ) ]) (Just [ OSTypes.ServerActive ])
      , selectMod = Primary
      , confirmable = False
      }
    , { name = "Revert"
      , description =
            String.join " "
                [ "Abort"
                , wordForServer
                , "resize operation"
                ]
      , allowedStatuses = Just [ OSTypes.ServerVerifyResize ]
      , allowedLockStatus = Just OSTypes.ServerUnlocked
      , action =
            doAction (Json.Encode.object [ ( "revertResize", Json.Encode.null ) ]) (Just [ OSTypes.ServerActive ])
      , selectMod = NoMod
      , confirmable = False
      }
    , { name = "Lock"
      , description =
            String.join " "
                [ "Prevent further"
                , wordForServer
                , "actions until it is unlocked"
                ]
      , allowedStatuses = Nothing
      , allowedLockStatus = Just OSTypes.ServerUnlocked
      , action =
            doAction (Json.Encode.object [ ( "lock", Json.Encode.null ) ]) Nothing
      , selectMod = NoMod
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
            doAction (Json.Encode.object [ ( "unlock", Json.Encode.null ) ]) Nothing
      , selectMod = Warning
      , confirmable = False
      }
    , { name = "Start"
      , description =
            String.join " "
                [ "Start stopped"
                , wordForServer
                ]
      , allowedStatuses = Just [ OSTypes.ServerShutoff, OSTypes.ServerStopped ]
      , allowedLockStatus = Just OSTypes.ServerUnlocked
      , action =
            doAction (Json.Encode.object [ ( "os-start", Json.Encode.null ) ]) (Just [ OSTypes.ServerActive ])
      , selectMod = Primary
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
            doAction (Json.Encode.object [ ( "unpause", Json.Encode.null ) ]) (Just [ OSTypes.ServerActive ])
      , selectMod = Primary
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
            doAction (Json.Encode.object [ ( "resume", Json.Encode.null ) ]) (Just [ OSTypes.ServerActive ])
      , selectMod = Primary
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
            doAction (Json.Encode.object [ ( "unshelve", Json.Encode.null ) ]) (Just [ OSTypes.ServerActive ])
      , selectMod = Primary
      , confirmable = False
      }
    , { name = "Suspend"
      , description = "Save execution state to disk"
      , allowedStatuses = Just [ OSTypes.ServerActive ]
      , allowedLockStatus = Just OSTypes.ServerUnlocked
      , action =
            doAction (Json.Encode.object [ ( "suspend", Json.Encode.null ) ]) (Just [ OSTypes.ServerSuspended ])
      , selectMod = NoMod
      , confirmable = False
      }
    , { name = "Shelve"
      , description =
            String.join " "
                [ "Shut down"
                , wordForServer
                , "and offload it from compute host"
                ]
      , allowedStatuses = Just [ OSTypes.ServerActive, OSTypes.ServerPaused, OSTypes.ServerShutoff, OSTypes.ServerSuspended ]
      , allowedLockStatus = Just OSTypes.ServerUnlocked
      , action =
            doAction (Json.Encode.object [ ( "shelve", Json.Encode.null ) ])
                (Just [ OSTypes.ServerShelved, OSTypes.ServerShelvedOffloaded ])
      , selectMod = NoMod
      , confirmable = False
      }
    , { name = "Resize"
      , description =
            String.join " "
                [ "Change"
                , wordForServer
                , "to a different"
                , wordForFlavor
                ]
      , allowedStatuses = Just [ OSTypes.ServerActive, OSTypes.ServerShutoff ]
      , allowedLockStatus = Just OSTypes.ServerUnlocked
      , action =
            -- This must be overridden in the Page to do anything
            \_ _ _ -> NoOp
      , selectMod = NoMod
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
      , allowedStatuses = Just [ OSTypes.ServerActive, OSTypes.ServerPaused, OSTypes.ServerShutoff, OSTypes.ServerSuspended ]
      , allowedLockStatus = Nothing
      , action =
            -- This must be overridden in the Page to do anything
            \_ _ _ -> NoOp
      , selectMod = NoMod
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
            doAction
                (Json.Encode.object
                    [ ( "reboot"
                      , Json.Encode.object
                            [ ( "type", Json.Encode.string "SOFT" ) ]
                      )
                    ]
                )
                (Just [ OSTypes.ServerActive ])
      , selectMod = Warning
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
                -- TODO add new statuses like resize-whatever, migrating, etc
                [ OSTypes.ServerActive
                , OSTypes.ServerBuild
                , OSTypes.ServerError
                , OSTypes.ServerPaused
                , OSTypes.ServerReboot
                , OSTypes.ServerRescue
                , OSTypes.ServerSuspended
                , OSTypes.ServerShelved
                , OSTypes.ServerShelvedOffloaded
                , OSTypes.ServerShutoff
                , OSTypes.ServerStopped
                ]
      , allowedLockStatus = Just OSTypes.ServerUnlocked
      , action =
            \projectId server retainFloatingIp ->
                ProjectMsg projectId <| ServerMsg server.osProps.uuid <| RequestDeleteServer retainFloatingIp
      , selectMod = Danger
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


doAction : Json.Encode.Value -> Maybe (List OSTypes.ServerStatus) -> ProjectIdentifier -> Server -> Bool -> SharedMsg
doAction jsonBody maybeTargetStatuses projectId server _ =
    let
        credentialedRequest : Url -> Cmd SharedMsg
        credentialedRequest novaUrl =
            let
                errorContext =
                    ErrorContext
                        ("perform action for server " ++ server.osProps.uuid)
                        ErrorCrit
                        Nothing
            in
            openstackCredentialedRequest
                projectId
                Post
                Nothing
                (novaUrl ++ "/servers/" ++ server.osProps.uuid ++ "/action")
                (Http.jsonBody jsonBody)
                (expectStringWithErrorBody
                    (resultToMsgErrorBody
                        errorContext
                        (\_ ->
                            ProjectMsg projectId <|
                                ServerMsg server.osProps.uuid <|
                                    RequestServer
                        )
                    )
                )
    in
    SharedMsg.ProjectMsg projectId <|
        SharedMsg.ServerMsg server.osProps.uuid <|
            SharedMsg.RequestServerAction
                credentialedRequest
                maybeTargetStatuses
