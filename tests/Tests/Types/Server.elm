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

        legacyStoredAction =
            Encode.object
                [ ( "targetOpenstackStatus", Encode.null )
                , ( "request", RDPP.encode (\() -> Encode.null) (\_ -> Encode.null) RDPP.empty )
                ]
    in
    describe "server quota refresh actions"
        [ test "shelve keeps its established action completion targets" <|
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
        , test "the actual unshelve action refreshes quota only after Active" <|
            \_ ->
                ( Server.serverActionTargetOpenstackStatus ServerActions.Unshelve
                , Server.serverActionQuotaRefreshTargetOpenstackStatus ServerActions.Unshelve
                )
                    |> Expect.equal
                        ( Just [ OSTypes.ServerActive ]
                        , Just [ OSTypes.ServerActive ]
                        )
        , test "bulk server updates clear the UI target but preserve quota refresh intent" <|
            \_ ->
                Expect.equal
                    actionAfterBulkShelvedOffloaded
                    { actionWithRefreshIntent | targetOpenstackStatus = Nothing }
        , test "reboot does not request a quota refresh" <|
            \_ ->
                Server.serverActionQuotaRefreshTargetOpenstackStatus ServerActions.Reboot
                    |> Expect.equal Nothing
        , test "stored actions without quota refresh fields remain valid" <|
            \_ ->
                Decode.decodeValue LocalStorage.serverExoActionDecoder legacyStoredAction
                    |> Expect.equal (Ok Server.initServerExoActions)
        ]
