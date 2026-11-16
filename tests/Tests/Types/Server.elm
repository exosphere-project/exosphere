module Tests.Types.Server exposing (serverActionEndedWithoutTargetSuite, serverActionLifecycleSuite, serverActionOutcomeUnknownSuite, serverExoActionsSuite)

import Dict
import Expect
import Helpers.RemoteDataPlusPlus as RDPP
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import LocalStorage.LocalStorage as LocalStorage
import OpenStack.ServerActions as ServerActions
import OpenStack.Types as OSTypes
import Test exposing (Test, describe, test)
import Time
import Types.Server as Server


serverExoActionsSuite : Test
serverExoActionsSuite =
    let
        actionWithRefreshIntent =
            { targetOpenstackStatus = Server.shelveTargetOpenstackStatus
            , quotaRefreshTargetOpenstackStatus = Server.shelveQuotaRefreshTargetOpenstackStatus
            , request = RDPP.empty
            , unreachedTarget = Nothing
            }

        actionAfterShelved =
            Server.clearQuotaRefreshIntentIfTargetStatusReached actionWithRefreshIntent OSTypes.ServerShelved

        actionAfterShelvedOffloaded =
            Server.clearQuotaRefreshIntentIfTargetStatusReached actionAfterShelved OSTypes.ServerShelvedOffloaded

        actionAfterBulkShelvedOffloaded =
            Server.clearActionTargetIfTargetStatusReached actionWithRefreshIntent OSTypes.ServerShelvedOffloaded

        resizeActionWithRefreshIntent =
            { targetOpenstackStatus = Server.resizeTargetOpenstackStatus
            , quotaRefreshTargetOpenstackStatus = Server.resizeQuotaRefreshTargetOpenstackStatus
            , request = RDPP.empty
            , unreachedTarget = Nothing
            }

        actionsWithoutGenericQuotaRefreshTargets =
            [ ServerActions.Lock
            , ServerActions.Unlock
            , ServerActions.Start
            , ServerActions.Stop
            , ServerActions.Pause
            , ServerActions.Unpause
            , ServerActions.Suspend
            , ServerActions.Resume
            , ServerActions.Reboot
            , ServerActions.CreateImage
            , ServerActions.Delete
            , ServerActions.Shelve
            , ServerActions.UnsupportedAction "unsupported"
            ]

        actionWithoutGenericQuotaRefreshTargetTests =
            List.map
                (\action ->
                    test (ServerActions.serverActionToString action ++ " has no generic quota refresh target") <|
                        \_ ->
                            Server.serverActionQuotaRefreshTargetOpenstackStatus action
                                |> Expect.equal Nothing
                )
                actionsWithoutGenericQuotaRefreshTargets

        resizeQuotaRefreshStatuses =
            [ ( OSTypes.ServerResize, resizeActionWithRefreshIntent )
            , ( OSTypes.ServerVerifyResize
              , { resizeActionWithRefreshIntent
                    | quotaRefreshTargetOpenstackStatus = Nothing
                }
              )
            , ( OSTypes.ServerActive
              , { resizeActionWithRefreshIntent
                    | quotaRefreshTargetOpenstackStatus = Nothing
                }
              )
            , ( OSTypes.ServerShutoff
              , { resizeActionWithRefreshIntent
                    | quotaRefreshTargetOpenstackStatus = Nothing
                }
              )
            ]

        resizeQuotaRefreshStatusTests =
            List.map
                (\( status, expected ) ->
                    test (OSTypes.serverStatusToString status ++ " handles resize quota refresh intent") <|
                        \_ ->
                            Server.clearQuotaRefreshIntentIfTargetStatusReached resizeActionWithRefreshIntent status
                                |> Expect.equal expected
                )
                resizeQuotaRefreshStatuses

        actionTargetCases =
            [ ( ServerActions.ConfirmResize
              , Just [ OSTypes.ServerActive, OSTypes.ServerShutoff ]
              , Just [ OSTypes.ServerActive, OSTypes.ServerShutoff ]
              )
            , ( ServerActions.RevertResize
              , Just [ OSTypes.ServerActive, OSTypes.ServerShutoff ]
              , Just [ OSTypes.ServerActive, OSTypes.ServerShutoff ]
              )
            , ( ServerActions.Unshelve
              , Just [ OSTypes.ServerActive ]
              , Just [ OSTypes.ServerActive ]
              )
            ]

        actionTargetTests =
            List.map
                (\( action, expectedActionTarget, expectedQuotaTarget ) ->
                    test (ServerActions.serverActionToString action ++ " has the expected completion targets") <|
                        \_ ->
                            ( Server.serverActionTargetOpenstackStatus action
                            , Server.serverActionQuotaRefreshTargetOpenstackStatus action
                            )
                                |> Expect.equal ( expectedActionTarget, expectedQuotaTarget )
                )
                actionTargetCases

        legacyStoredAction =
            Encode.object
                [ ( "targetOpenstackStatus", Encode.null )
                , ( "request", RDPP.encode (\() -> Encode.null) (\_ -> Encode.null) RDPP.empty )
                ]

        tests =
            [ test "resize keeps RESIZE as its established action target" <|
                \_ ->
                    Server.resizeTargetOpenstackStatus
                        |> Expect.equal (Just [ OSTypes.ServerResize ])
            , test "generic Resize has no quota target because resizing uses the flavor-aware dedicated path" <|
                \_ ->
                    Server.serverActionQuotaRefreshTargetOpenstackStatus ServerActions.Resize
                        |> Expect.equal Nothing
            , test "resize refreshes quota at VERIFY_RESIZE or fast-confirmed ACTIVE/SHUTOFF" <|
                \_ ->
                    Server.resizeQuotaRefreshTargetOpenstackStatus
                        |> Expect.equal
                            (Just
                                [ OSTypes.ServerVerifyResize
                                , OSTypes.ServerActive
                                , OSTypes.ServerShutoff
                                ]
                            )
            , test "shelve keeps its established action completion targets" <|
                \_ ->
                    Expect.equal
                        Server.shelveTargetOpenstackStatus
                        (Just [ OSTypes.ServerShelved, OSTypes.ServerShelvedOffloaded ])
            , test "shelve waits for ShelvedOffloaded before refreshing quota" <|
                \_ ->
                    Expect.equal
                        Server.shelveQuotaRefreshTargetOpenstackStatus
                        (Just [ OSTypes.ServerShelvedOffloaded ])
            , test "Shelved preserves the pending refresh intent until ShelvedOffloaded" <|
                \_ ->
                    Expect.equal actionAfterShelved actionWithRefreshIntent
            , test "ShelvedOffloaded consumes the pending refresh intent" <|
                \_ ->
                    Expect.equal
                        actionAfterShelvedOffloaded
                        { actionWithRefreshIntent
                            | quotaRefreshTargetOpenstackStatus = Nothing
                        }
            , test "bulk server updates clear the UI target but preserve quota refresh intent" <|
                \_ ->
                    Expect.equal
                        actionAfterBulkShelvedOffloaded
                        { actionWithRefreshIntent | targetOpenstackStatus = Nothing }
            , test "stored actions without quota refresh fields remain valid" <|
                \_ ->
                    Decode.decodeValue LocalStorage.serverExoActionDecoder legacyStoredAction
                        |> Expect.equal (Ok Server.initServerExoActions)
            ]
                ++ resizeQuotaRefreshStatusTests
                ++ actionTargetTests
                ++ actionWithoutGenericQuotaRefreshTargetTests
    in
    describe "server quota refresh actions" tests


serverActionLifecycleSuite : Test
serverActionLifecycleSuite =
    let
        acknowledgedAt =
            Time.millisToPosix 1000

        failedAt =
            Time.millisToPosix 2000

        httpError =
            { error = Http.BadStatus 503, body = "service unavailable" }

        pendingShelve =
            { targetOpenstackStatus = Server.shelveTargetOpenstackStatus
            , quotaRefreshTargetOpenstackStatus = Server.shelveQuotaRefreshTargetOpenstackStatus
            , request = RDPP.setLoading RDPP.empty
            , unreachedTarget = Nothing
            }

        acknowledgedShelve =
            { pendingShelve
                | request =
                    RDPP.RemoteDataPlusPlus
                        (RDPP.DoHave () acknowledgedAt)
                        (RDPP.NotLoading Nothing)
            }

        -- Dispatching keeps any earlier response, so an in-flight request can carry one.
        reDispatchedShelve =
            { acknowledgedShelve | request = RDPP.setLoading acknowledgedShelve.request }

        -- Locking has no status to wait for.
        acknowledgedLock =
            { targetOpenstackStatus = Nothing
            , quotaRefreshTargetOpenstackStatus = Nothing
            , request =
                RDPP.RemoteDataPlusPlus
                    (RDPP.DoHave () acknowledgedAt)
                    (RDPP.NotLoading Nothing)
            , unreachedTarget = Nothing
            }

        failedShelve =
            Server.failAction httpError failedAt pendingShelve

        failedAfterAnEarlierAction =
            Server.failAction httpError failedAt reDispatchedShelve

        -- Shelving reaches SHELVED first, leaving the quota refresh owed until SHELVED_OFFLOADED.
        awaitingQuotaRefreshOnly =
            Server.clearActionTargetIfTargetStatusReached acknowledgedShelve OSTypes.ServerShelved

        legacyFailedShelve =
            { pendingShelve
                | request = RDPP.setNotLoading (Just ( httpError, failedAt )) RDPP.empty
            }

        -- Hand-written: the encoder emits only shapes the application still writes.
        storedFailure requestedTargets =
            Encode.object
                (requestedTargets
                    ++ [ ( "request"
                         , Encode.object
                            [ ( "haveness", Encode.bool False )
                            , ( "loading", Encode.bool False )
                            , ( "errored", Encode.bool True )
                            , ( "error"
                              , Encode.object
                                    [ ( "error"
                                      , Encode.object
                                            [ ( "type", Encode.string "badStatus" )
                                            , ( "status", Encode.int 503 )
                                            ]
                                      )
                                    , ( "body", Encode.string "service unavailable" )
                                    ]
                              )
                            , ( "errorReceivedTime", Encode.int 2000 )
                            ]
                         )
                       ]
                )

        storedAcknowledged targetFields =
            Encode.object
                (targetFields
                    ++ [ ( "request"
                         , Encode.object
                            [ ( "haveness", Encode.bool True )
                            , ( "data", Encode.null )
                            , ( "dataReceivedTime", Encode.int 1000 )
                            , ( "loading", Encode.bool False )
                            , ( "errored", Encode.bool False )
                            ]
                         )
                       ]
                )

        storedFailureNamingUnknownStatus =
            storedFailure
                [ ( "targetOpenstackStatus", Encode.list Encode.string [ "Shelved", "NoSuchStatus" ] ) ]

        storedFailureWithoutTargets =
            storedFailure []

        decodedFailure =
            { targetOpenstackStatus = Nothing
            , quotaRefreshTargetOpenstackStatus = Nothing
            , request =
                RDPP.setNotLoading
                    (Just ( { error = Http.BadStatus 503, body = "service unavailable" }, Time.millisToPosix 2000 ))
                    RDPP.empty
            , unreachedTarget = Nothing
            }

        storedPendingShelve =
            Encode.object
                [ ( "targetOpenstackStatus"
                  , Encode.list (Encode.string << OSTypes.serverStatusToString)
                        [ OSTypes.ServerShelved, OSTypes.ServerShelvedOffloaded ]
                  )
                , ( "quotaRefreshTargetOpenstackStatus", Encode.null )
                , ( "request"
                  , RDPP.encode (\() -> Encode.null) (\_ -> Encode.null) (RDPP.setLoading RDPP.empty)
                  )
                ]

        storeAndRead exoAction =
            LocalStorage.encodeServerExoActions (Dict.fromList [ ( "server-uuid", exoAction ) ])
                |> Decode.decodeValue LocalStorage.serverExoActionsDecoder

        tests =
            [ test "a failed action gives up the statuses it was waiting for" <|
                \_ ->
                    ( failedShelve.targetOpenstackStatus, failedShelve.quotaRefreshTargetOpenstackStatus )
                        |> Expect.equal ( Nothing, Nothing )
            , test "a failed action keeps the error explaining why it did not happen" <|
                \_ ->
                    failedShelve.request.refreshStatus
                        |> Expect.equal (RDPP.NotLoading (Just ( httpError, failedAt )))
            , test "an action still in flight does not survive being stored" <|
                \_ ->
                    storeAndRead pendingShelve
                        |> Expect.equal (Ok Dict.empty)
            , test "an in-flight action does not survive on the strength of an earlier response" <|
                \_ ->
                    storeAndRead reDispatchedShelve
                        |> Expect.equal (Ok Dict.empty)
            , test "an action with no status left to wait for does not survive being stored" <|
                \_ ->
                    storeAndRead acknowledgedLock
                        |> Expect.equal (Ok Dict.empty)
            , test "a server with no action on record does not survive being stored" <|
                \_ ->
                    storeAndRead Server.initServerExoActions
                        |> Expect.equal (Ok Dict.empty)
            , test "an acknowledged action still waiting on a status survives being stored" <|
                \_ ->
                    storeAndRead acknowledgedShelve
                        |> Expect.equal (Ok (Dict.fromList [ ( "server-uuid", acknowledgedShelve ) ]))
            , test "an action still owed a quota refresh survives being stored" <|
                \_ ->
                    storeAndRead awaitingQuotaRefreshOnly
                        |> Expect.equal (Ok (Dict.fromList [ ( "server-uuid", awaitingQuotaRefreshOnly ) ]))
            , test "a failure survives being stored so it can still be reported" <|
                \_ ->
                    storeAndRead failedShelve
                        |> Expect.equal (Ok (Dict.fromList [ ( "server-uuid", failedShelve ) ]))
            , test "a failure survives when an earlier action had already been acknowledged" <|
                \_ ->
                    storeAndRead failedAfterAnEarlierAction
                        |> Expect.equal (Ok (Dict.fromList [ ( "server-uuid", failedAfterAnEarlierAction ) ]))
            , test "a failure stored with the statuses it was waiting for gives them up when read" <|
                \_ ->
                    storeAndRead legacyFailedShelve
                        |> Expect.equal
                            (Ok
                                (Dict.fromList
                                    [ ( "server-uuid"
                                      , { legacyFailedShelve
                                            | targetOpenstackStatus = Nothing
                                            , quotaRefreshTargetOpenstackStatus = Nothing
                                        }
                                      )
                                    ]
                                )
                            )
            , test "an action left in flight by an earlier version of the application is dropped" <|
                \_ ->
                    Decode.decodeValue LocalStorage.serverExoActionsDecoder
                        (Encode.object [ ( "server-uuid", storedPendingShelve ) ])
                        |> Expect.equal (Ok Dict.empty)
            , test "an action stored without any target statuses still decodes" <|
                \_ ->
                    Decode.decodeValue LocalStorage.serverExoActionsDecoder
                        (Encode.object [ ( "server-uuid", storedFailureWithoutTargets ) ])
                        |> Expect.equal (Ok (Dict.fromList [ ( "server-uuid", decodedFailure ) ]))
            , test "a failure naming a status the application cannot read keeps the failure" <|
                \_ ->
                    Decode.decodeValue LocalStorage.serverExoActionsDecoder
                        (Encode.object [ ( "server-uuid", storedFailureNamingUnknownStatus ) ])
                        |> Expect.equal (Ok (Dict.fromList [ ( "server-uuid", decodedFailure ) ]))
            , test "an action that cannot be read does not discard the others" <|
                \_ ->
                    Decode.decodeValue LocalStorage.serverExoActionsDecoder
                        (Encode.object
                            [ ( "unreadable", Encode.object [ ( "targetOpenstackStatus", Encode.null ) ] )
                            , ( "server-uuid", storedFailureWithoutTargets )
                            ]
                        )
                        |> Expect.equal (Ok (Dict.fromList [ ( "server-uuid", decodedFailure ) ]))
            , test "an acknowledged action naming a status the application cannot read is dropped alone" <|
                \_ ->
                    Decode.decodeValue LocalStorage.serverExoActionsDecoder
                        (Encode.object
                            [ ( "unreadable-target"
                              , storedAcknowledged
                                    [ ( "targetOpenstackStatus"
                                      , Encode.list Encode.string [ "Active", "NoSuchStatus" ]
                                      )
                                    ]
                              )
                            , ( "server-uuid", storedFailureWithoutTargets )
                            ]
                        )
                        |> Expect.equal (Ok (Dict.fromList [ ( "server-uuid", decodedFailure ) ]))
            , test "an acknowledged action keeps a readable target when its quota target cannot be read" <|
                \_ ->
                    Decode.decodeValue LocalStorage.serverExoActionsDecoder
                        (Encode.object
                            [ ( "server-uuid"
                              , storedAcknowledged
                                    [ ( "targetOpenstackStatus", Encode.list Encode.string [ "Active" ] )
                                    , ( "quotaRefreshTargetOpenstackStatus"
                                      , Encode.list Encode.string [ "NoSuchStatus" ]
                                      )
                                    ]
                              )
                            ]
                        )
                        |> Expect.equal
                            (Ok
                                (Dict.fromList
                                    [ ( "server-uuid"
                                      , { targetOpenstackStatus = Just [ OSTypes.ServerActive ]
                                        , quotaRefreshTargetOpenstackStatus = Nothing
                                        , request =
                                            RDPP.RemoteDataPlusPlus
                                                (RDPP.DoHave () (Time.millisToPosix 1000))
                                                (RDPP.NotLoading Nothing)
                                        , unreachedTarget = Nothing
                                        }
                                      )
                                    ]
                                )
                            )
            ]
    in
    describe "instance action lifecycle" tests


{-| Nova can accept an action (HTTP 202) and then fail it asynchronously, e.g. an unshelve that
finds no valid host. The instance then returns to its prior status with no task in progress, and
will never reach the status Exosphere is waiting for.
-}
serverActionEndedWithoutTargetSuite : Test
serverActionEndedWithoutTargetSuite =
    let
        acceptedAt =
            Time.millisToPosix 1000

        -- Comfortably after the grace period for responses to polls sent before the action.
        settledAt =
            Time.millisToPosix 31000

        -- Soon enough after acceptance that the response may predate the action.
        tooSoonAt =
            Time.millisToPosix 3000

        acceptedUnshelve =
            { targetOpenstackStatus = Server.serverActionTargetOpenstackStatus ServerActions.Unshelve
            , quotaRefreshTargetOpenstackStatus = Server.serverActionQuotaRefreshTargetOpenstackStatus ServerActions.Unshelve
            , request =
                RDPP.RemoteDataPlusPlus
                    (RDPP.DoHave () acceptedAt)
                    (RDPP.NotLoading Nothing)
            , unreachedTarget = Nothing
            }

        noTask =
            Nothing

        abandonedUnshelve =
            Server.abandonTargetIfActionEnded settledAt OSTypes.ServerShelvedOffloaded noTask acceptedUnshelve

        storeAndRead exoAction =
            LocalStorage.encodeServerExoActions (Dict.fromList [ ( "server-uuid", exoAction ) ])
                |> Decode.decodeValue LocalStorage.serverExoActionsDecoder

        unchangedCases =
            [ ( "Nova is still working on it"
              , Server.abandonTargetIfActionEnded settledAt OSTypes.ServerShelvedOffloaded (Just "spawning") acceptedUnshelve
              , acceptedUnshelve
              )
            , ( "the response may predate the action"
              , Server.abandonTargetIfActionEnded tooSoonAt OSTypes.ServerShelvedOffloaded noTask acceptedUnshelve
              , acceptedUnshelve
              )
            , ( "the action request is still in flight"
              , Server.abandonTargetIfActionEnded settledAt
                    OSTypes.ServerShelvedOffloaded
                    noTask
                    { acceptedUnshelve | request = RDPP.setLoading acceptedUnshelve.request }
              , { acceptedUnshelve | request = RDPP.setLoading acceptedUnshelve.request }
              )
            , ( "the action has no status to wait for"
              , Server.abandonTargetIfActionEnded settledAt
                    OSTypes.ServerActive
                    noTask
                    { acceptedUnshelve | targetOpenstackStatus = Nothing, quotaRefreshTargetOpenstackStatus = Nothing }
              , { acceptedUnshelve | targetOpenstackStatus = Nothing, quotaRefreshTargetOpenstackStatus = Nothing }
              )
            , ( "the target status was reached"
              , Server.abandonTargetIfActionEnded settledAt OSTypes.ServerActive noTask acceptedUnshelve
              , acceptedUnshelve
              )
            , ( "a resize confirmation is underway, which Nova runs without a task state"
              , Server.abandonTargetIfActionEnded settledAt
                    OSTypes.ServerVerifyResize
                    noTask
                    { acceptedUnshelve
                        | targetOpenstackStatus = Server.serverActionTargetOpenstackStatus ServerActions.ConfirmResize
                    }
              , { acceptedUnshelve
                    | targetOpenstackStatus = Server.serverActionTargetOpenstackStatus ServerActions.ConfirmResize
                }
              )
            ]

        unchangedTests =
            List.map
                (\( name, actual, expected ) ->
                    test ("the target is kept when " ++ name) <|
                        \_ -> Expect.equal expected actual
                )
                unchangedCases

        uiStatusCases =
            [ ( OSTypes.ServerShelvedOffloaded, Just "unshelving", Just Server.ServerUiStatusUnshelving )
            , ( OSTypes.ServerShelvedOffloaded, Just "spawning", Just Server.ServerUiStatusUnshelving )
            , ( OSTypes.ServerShelved, Just "powering-on", Just Server.ServerUiStatusUnshelving )
            , ( OSTypes.ServerShelvedOffloaded, Nothing, Nothing )
            , ( OSTypes.ServerShutoff, Just "shelving", Just Server.ServerUiStatusShelving )
            , ( OSTypes.ServerActive, Just "shelving_image_uploading", Just Server.ServerUiStatusShelving )
            , ( OSTypes.ServerActive, Just "shelving_image_pending_upload", Just Server.ServerUiStatusShelving )

            -- Offloading an already-shelved instance is still shown as shelved.
            , ( OSTypes.ServerShelved, Just "shelving_offloading", Nothing )
            , ( OSTypes.ServerActive, Just "rebooting", Nothing )
            , ( OSTypes.ServerActive, Nothing, Nothing )
            ]

        uiStatusTests =
            List.map
                (\( status, taskState, expected ) ->
                    test
                        (OSTypes.serverStatusToString status
                            ++ " with task state "
                            ++ Maybe.withDefault "none" taskState
                            ++ " has the expected in-progress status"
                        )
                    <|
                        \_ ->
                            Server.taskStateUiStatus status taskState
                                |> Expect.equal expected
                )
                uiStatusCases

        tests =
            [ test "an accepted unshelve that Nova abandoned gives up the statuses it was waiting for" <|
                \_ ->
                    ( abandonedUnshelve.targetOpenstackStatus, abandonedUnshelve.quotaRefreshTargetOpenstackStatus )
                        |> Expect.equal ( Nothing, Nothing )
            , test "an abandoned action records what it was waiting for and when that was noticed" <|
                \_ ->
                    abandonedUnshelve.unreachedTarget
                        |> Expect.equal (Just { statuses = [ OSTypes.ServerActive ], noticedAt = settledAt })
            , test "an abandoned action is not reported as an HTTP error" <|
                \_ ->
                    abandonedUnshelve.request.refreshStatus
                        |> Expect.equal (RDPP.NotLoading Nothing)
            , test "an abandoned action survives being stored so it can still be reported" <|
                \_ ->
                    storeAndRead abandonedUnshelve
                        |> Expect.equal (Ok (Dict.fromList [ ( "server-uuid", abandonedUnshelve ) ]))
            ]
                ++ unchangedTests
                ++ uiStatusTests
    in
    describe "instance actions Nova accepted but did not complete" tests


{-| A timeout or network error does not say whether OpenStack received the request. Nova may have accepted the action
and only the response was lost, so Exosphere keeps waiting until polling shows whether the action took effect.
-}
serverActionOutcomeUnknownSuite : Test
serverActionOutcomeUnknownSuite =
    let
        erroredAt =
            Time.millisToPosix 1000

        settledAt =
            Time.millisToPosix 31000

        tooSoonAt =
            Time.millisToPosix 3000

        timeout =
            { error = Http.Timeout, body = "" }

        networkError =
            { error = Http.NetworkError, body = "" }

        rejected =
            { error = Http.BadStatus 409, body = "conflict" }

        badGateway =
            { error = Http.BadStatus 502, body = "" }

        gatewayTimeout =
            { error = Http.BadStatus 504, body = "" }

        -- A gateway with no backend to send to returns 503 without forwarding the request.
        unavailable =
            { error = Http.BadStatus 503, body = "" }

        unconfirmedDelete =
            Server.failAction networkError
                erroredAt
                { initServerExoActionsLoading | targetOpenstackStatus = Server.deleteTargetOpenstackStatus }

        abandonedDelete =
            Server.abandonTargetIfActionEnded settledAt OSTypes.ServerActive noTask unconfirmedDelete

        deletedAfterAll =
            Server.clearActionTargetIfTargetStatusReached unconfirmedDelete OSTypes.ServerDeleted

        details status taskState taskStateReported =
            { openstackStatus = status
            , created = Time.millisToPosix 0
            , powerState = OSTypes.PowerRunning
            , imageUuid = "image-uuid"
            , flavorId = "flavor-id"
            , keypairName = Nothing
            , metadata = []
            , userUuid = "user-uuid"
            , volumesAttached = []
            , tags = []
            , lockStatus = OSTypes.ServerUnlocked
            , fault = Nothing
            , taskState = taskState
            , taskStateReported = taskStateReported
            }

        acceptedUnshelve =
            { pendingUnshelve
                | request =
                    RDPP.RemoteDataPlusPlus
                        (RDPP.DoHave () erroredAt)
                        (RDPP.NotLoading Nothing)
            }

        pendingUnshelve =
            { targetOpenstackStatus = Server.serverActionTargetOpenstackStatus ServerActions.Unshelve
            , quotaRefreshTargetOpenstackStatus = Server.serverActionQuotaRefreshTargetOpenstackStatus ServerActions.Unshelve
            , request = RDPP.setLoading RDPP.empty
            , unreachedTarget = Nothing
            }

        unconfirmedUnshelve =
            Server.failAction timeout erroredAt pendingUnshelve

        rejectedUnshelve =
            Server.failAction rejected erroredAt pendingUnshelve

        unshelvedAfterAll =
            Server.clearActionTargetIfTargetStatusReached unconfirmedUnshelve OSTypes.ServerActive

        noTask =
            Nothing

        storeAndRead exoAction =
            LocalStorage.encodeServerExoActions (Dict.fromList [ ( "server-uuid", exoAction ) ])
                |> Decode.decodeValue LocalStorage.serverExoActionsDecoder

        tests =
            [ test "timeouts, network errors and gateway failures leave the outcome unknown" <|
                \_ ->
                    List.map Server.requestOutcomeUnknown [ timeout, networkError, badGateway, gatewayTimeout ]
                        |> Expect.equal [ True, True, True, True ]
            , test "errors OpenStack returned are definite" <|
                \_ ->
                    List.map Server.requestOutcomeUnknown [ rejected, unavailable ]
                        |> Expect.equal [ False, False ]
            , test "an unconfirmed action keeps the statuses it is waiting for" <|
                \_ ->
                    ( unconfirmedUnshelve.targetOpenstackStatus, unconfirmedUnshelve.quotaRefreshTargetOpenstackStatus )
                        |> Expect.equal ( pendingUnshelve.targetOpenstackStatus, pendingUnshelve.quotaRefreshTargetOpenstackStatus )
            , test "an unconfirmed action is awaiting confirmation" <|
                \_ ->
                    Server.isAwaitingConfirmation unconfirmedUnshelve
                        |> Expect.equal True
            , test "an action OpenStack rejected gives up its statuses" <|
                \_ ->
                    ( rejectedUnshelve.targetOpenstackStatus, Server.isAwaitingConfirmation rejectedUnshelve )
                        |> Expect.equal ( Nothing, False )
            , test "an unconfirmed action that reaches its target clears the error" <|
                \_ ->
                    ( unshelvedAfterAll.targetOpenstackStatus, unshelvedAfterAll.request )
                        |> Expect.equal ( Nothing, RDPP.empty )
            , test "an unconfirmed action is kept while Nova is working on it" <|
                \_ ->
                    Server.abandonTargetIfActionEnded settledAt OSTypes.ServerShelvedOffloaded (Just "unshelving") unconfirmedUnshelve
                        |> Expect.equal unconfirmedUnshelve
            , test "an unconfirmed action is kept while a response may predate the error" <|
                \_ ->
                    Server.abandonTargetIfActionEnded tooSoonAt OSTypes.ServerShelvedOffloaded noTask unconfirmedUnshelve
                        |> Expect.equal unconfirmedUnshelve
            , test "an unconfirmed action with nothing underway once settled gives up its statuses and keeps the error" <|
                \_ ->
                    Server.abandonTargetIfActionEnded settledAt OSTypes.ServerShelvedOffloaded noTask unconfirmedUnshelve
                        |> Expect.equal
                            { unconfirmedUnshelve
                                | targetOpenstackStatus = Nothing
                                , quotaRefreshTargetOpenstackStatus = Nothing
                            }
            , test "an unconfirmed action keeps its statuses across a reload" <|
                \_ ->
                    storeAndRead unconfirmedUnshelve
                        |> Expect.equal (Ok (Dict.fromList [ ( "server-uuid", unconfirmedUnshelve ) ]))
            , test "an unconfirmed delete waits for the server to be deleted" <|
                \_ ->
                    unconfirmedDelete
                        |> Server.abandonTargetIfActionEnded settledAt OSTypes.ServerActive (Just "deleting")
                        |> Server.isAwaitingDeleteConfirmation
                        |> Expect.equal True
            , test "a server with an unconfirmed delete shows as deleting again after a reload" <|
                \_ ->
                    Server.deletionAttemptedAfterSettling unconfirmedDelete unconfirmedDelete False
                        |> Expect.equal True
            , test "a server whose unconfirmed delete did not take effect stops showing as deleting" <|
                \_ ->
                    Server.deletionAttemptedAfterSettling unconfirmedDelete abandonedDelete True
                        |> Expect.equal False
            , test "a server whose unconfirmed delete took effect still shows as deleting" <|
                \_ ->
                    Server.deletionAttemptedAfterSettling unconfirmedDelete deletedAfterAll True
                        |> Expect.equal True
            , test "other actions leave whether a server shows as deleting alone" <|
                \_ ->
                    [ Server.deletionAttemptedAfterSettling unconfirmedUnshelve unconfirmedUnshelve False
                    , Server.deletionAttemptedAfterSettling unconfirmedUnshelve unconfirmedUnshelve True
                    ]
                        |> Expect.equal [ False, True ]
            , test "settling an action abandons it once Nova reports no task in progress" <|
                \_ ->
                    Server.settleAction settledAt (details OSTypes.ServerShelvedOffloaded noTask True) acceptedUnshelve
                        |> .targetOpenstackStatus
                        |> Expect.equal Nothing
            , test "settling an action keeps waiting when the server leaves out its task state" <|
                \_ ->
                    Server.settleAction settledAt (details OSTypes.ServerShelvedOffloaded noTask False) acceptedUnshelve
                        |> Expect.equal acceptedUnshelve
            , test "settling an action clears it once the target is reached, task state or not" <|
                \_ ->
                    Server.settleAction settledAt (details OSTypes.ServerActive noTask False) acceptedUnshelve
                        |> .targetOpenstackStatus
                        |> Expect.equal Nothing
            ]

        initServerExoActionsLoading =
            { pendingUnshelve
                | targetOpenstackStatus = Nothing
                , quotaRefreshTargetOpenstackStatus = Nothing
            }
    in
    describe "instance actions whose request outcome is unknown" tests
