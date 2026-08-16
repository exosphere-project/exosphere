module Tests.Types.Server exposing (serverExoActionsSuite)

import Expect
import Helpers.RemoteDataPlusPlus as RDPP
import Json.Decode as Decode
import Json.Encode as Encode
import LocalStorage.LocalStorage as LocalStorage
import OpenStack.ServerActions as ServerActions
import OpenStack.Types as OSTypes
import Test exposing (Test, describe, test)
import Types.Server as Server


serverExoActionsSuite : Test
serverExoActionsSuite =
    let
        actionWithRefreshIntent =
            { targetOpenstackStatus = Server.shelveTargetOpenstackStatus
            , quotaRefreshTargetOpenstackStatus = Server.shelveQuotaRefreshTargetOpenstackStatus
            , request = RDPP.empty
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
