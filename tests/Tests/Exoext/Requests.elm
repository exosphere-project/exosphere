module Tests.Exoext.Requests exposing
    ( historyRefetchSuite
    , inFlightSuite
    , navigationSuite
    , preEchoBadgeSuite
    , removalSuite
    )

{-| The host's side of the §7.1 request slot as of 0.4.0: the one in-flight guard that covers every
verb, the removal round-trip, the pre-echo run reaching the row badge, and a history refetch that
retries after a failure.

These are host behaviors rather than adapter ones — what the host does with a slot, a marker and a
clock — so they sit here beside the rest of `Tests.Exoext.Host` rather than with the CloudShield
payloads that happen to fill them.

-}

import CloudShield.Card as Card
import Exoext.Lifecycle as Lifecycle
import Expect
import Helpers.RemoteDataPlusPlus as RDPP
import Http
import Json.Decode as Decode
import OpenStack.Types as OSTypes
import Page.ServerDetail as ServerDetail
import Route
import Test exposing (Test, describe, test)
import Tests.Exoext.Fixtures exposing (modelAfterStartScan, polledAt, project, projectPublishing, projectWithTargets, runSlot, runSlotFor, serverPublishing, writeRequestAt)
import Time
import Types.Project
import Types.SharedMsg as SharedMsg


meta : List ( String, String ) -> List OSTypes.MetadataItem
meta pairs =
    pairs |> List.map (\( key, value ) -> { key = key, value = value })


idle : ServerDetail.Model
idle =
    ServerDetail.init "self"


{-| A model whose clock has been set by a poll, which is where `exoextRequestBlocked` reads the time
from. Every in-flight test starts from one, because a marker's age is half of what the guard decides.
-}
atClock : Int -> ServerDetail.Model -> ServerDetail.Model
atClock millis model =
    { model | exoextClock = Time.millisToPosix millis }


{-| The model a message leaves behind. The page's `update` returns a triple; every test here is
about the model half of it.
-}
updatedBy : ServerDetail.Msg -> Types.Project.Project -> ServerDetail.Model -> ServerDetail.Model
updatedBy msg proj model =
    let
        ( updated, _, _ ) =
            ServerDetail.update msg proj model
    in
    updated


pending : Int -> String -> Lifecycle.PendingRequest
pending millis subject =
    { seq = millis
    , requestId = "exoext-req-" ++ String.fromInt millis
    , kind = "getEmbed"
    , subject = subject
    , since = Time.millisToPosix millis
    }



-- THE ONE IN-FLIGHT GUARD


{-| One request slot, one guard. The old pair each protected its own verb from the other's requests
and neither protected a verb from its own kind, so the two ways a press could destroy work were
exactly the two nobody watched.
-}
inFlightSuite : Test
inFlightSuite =
    let
        blocked model =
            ServerDetail.exoextRequestBlocked (projectPublishing []) model
    in
    describe "exoextRequestBlocked covers every outstanding request"
        [ test "an untouched page blocks nothing" <|
            \_ ->
                Expect.equal False (blocked (atClock 1000 idle))
        , test "a session request in flight blocks a scan — the seq bump would supersede it" <|
            \_ ->
                Expect.equal True
                    (blocked (atClock 1000 { idle | exoextPendingEmbed = Just (pending 1000 "r-1") }))
        , test "a removal in flight blocks just the same" <|
            \_ ->
                Expect.equal True
                    (blocked (atClock 1000 { idle | exoextPendingDelete = Just (pending 1000 "r-1") }))
        , test "a marker past the request timeout stops blocking — a guard must not outlive its request" <|
            \_ ->
                -- The wedge this closes: a publisher that never answers left the marker set
                -- forever, and with one slot that is a page nothing can be pressed on again.
                Expect.equal ( False, False )
                    ( blocked (atClock (1000 + Lifecycle.requestTimeoutMillis + 1) { idle | exoextPendingEmbed = Just (pending 1000 "r-1") })
                    , blocked (atClock (1000 + Lifecycle.requestTimeoutMillis + 1) { idle | exoextPendingDelete = Just (pending 1000 "r-1") })
                    )
        , test "a tracked scan whose run has not settled blocks" <|
            \_ ->
                Expect.equal True
                    (ServerDetail.exoextRequestBlocked (projectPublishing (runSlot 1 "running")) (atClock 1000 modelAfterStartScan))
        , test "a batch with targets left blocks even between two settled siblings" <|
            \_ ->
                -- The tracked run reads terminal here, so a run-only guard would open exactly the
                -- window in which a press strands the rest of the batch.
                Expect.equal True
                    (ServerDetail.exoextRequestBlocked (projectPublishing (runSlot 1700 "done"))
                        (atClock 1700 (writeRequestAt 1700 { subject = "i-1", batchId = Nothing } modelAfterStartScan))
                    )
        , test "an unclaimed request on the wire blocks, even with nothing tracked in this tab" <|
            \_ ->
                -- The only clause that can see a request a DIFFERENT browser tab wrote.
                Expect.equal True
                    (ServerDetail.exoextRequestBlocked
                        (projectPublishing (meta [ ( "exoext.v1.req.seq", "1700" ) ]))
                        (atClock 1700 idle)
                    )
        , test "once the publisher claims it, the slot stops blocking" <|
            \_ ->
                Expect.equal False
                    (ServerDetail.exoextRequestBlocked
                        (projectPublishing (meta [ ( "exoext.v1.req.seq", "1700" ), ( "exoext.v1.req.claimed", "1700" ) ]))
                        (atClock 1700 idle)
                    )
        , test "an unclaimed request older than the timeout stops blocking too" <|
            \_ ->
                -- A publisher that is not answering must not be able to lock the page on its behalf.
                Expect.equal False
                    (ServerDetail.exoextRequestBlocked
                        (projectPublishing (meta [ ( "exoext.v1.req.seq", "1700" ) ]))
                        (atClock (1700 + Lifecycle.requestTimeoutMillis + 1) idle)
                    )
        , test "no server to write against is blocked, which is the fail-closed answer" <|
            \_ ->
                Expect.equal True (ServerDetail.exoextRequestBlocked project (atClock 1000 idle))
        ]



-- REMOVAL


{-| The `deleteResult` round-trip: write the request, read the acknowledgement, and let the row go
(or say why it did not).
-}
removalSuite : Test
removalSuite =
    let
        written =
            updatedBy
                (ServerDetail.ExoextWriteDeleteRequest
                    { kind = "deleteResult", resultId = "exoext-req-9", batchId = "b-1" }
                    (Time.millisToPosix 1700)
                )
                (projectPublishing [])
                { idle | exoextHistory = loadedHistory }

        ack status errorField =
            meta
                [ ( "exoext.v1.res.body.n", "1" )
                , ( "exoext.v1.res.body.0"
                  , """{"schemaVersion":"1.0","requestId":"exoext-req-1700","batchId":"b-1","resultId":"exoext-req-9","kind":"action","action":"deleteResult","status":\""""
                        ++ status
                        ++ "\","
                        ++ errorField
                        ++ "}"
                  )
                ]

        rowIds model =
            RDPP.withDefault [] model.exoextHistory |> List.map (.requestId >> Maybe.withDefault "?")
    in
    describe "the deleteResult round-trip"
        [ test "the write records an in-flight removal keyed to the result it names" <|
            \_ ->
                Expect.equal (Just ( "exoext-req-1700", "exoext-req-9" ))
                    (written.exoextPendingDelete |> Maybe.map (\p -> ( p.requestId, p.subject )))
        , test "the row reads as removing from the press, and offers no other action" <|
            \_ ->
                Expect.equal ( Just "exoext-req-9", Nothing )
                    (ServerDetail.exoextRemovalState (Time.millisToPosix 1700) written)
        , test "an ok acknowledgement drops the row and asks for a fresh index" <|
            \_ ->
                let
                    settled =
                        polledAt (Time.millisToPosix 1800) (ack "ok" "\"error\":null") written
                in
                Expect.equal ( [ "exoext-req-8" ], Nothing, ( Nothing, 1 ) )
                    ( rowIds settled
                    , settled.exoextPendingDelete
                      -- The generation is what makes that refetch a NEW URL: a removal moves
                      -- neither the manifest etag nor the run slot, so without it the read would go
                      -- out unchanged and could be answered from cache with the row still in it.
                    , ( settled.exoextHistoryRequestKey, settled.exoextHistoryGeneration )
                    )
        , test "a refused removal keeps the row and records the publisher's reason on it" <|
            \_ ->
                let
                    refused =
                        polledAt (Time.millisToPosix 1800)
                            (ack "error" "\"error\":{\"code\":\"active\",\"message\":\"that scan is still running\"}")
                            written
                in
                Expect.equal
                    ( [ "exoext-req-8", "exoext-req-9" ]
                    , Nothing
                    , Just { resultId = "exoext-req-9", message = "that scan is still running" }
                    )
                    ( rowIds refused, refused.exoextPendingDelete, refused.exoextDeleteError )
        , test "an acknowledgement of a DIFFERENT request settles nothing" <|
            \_ ->
                let
                    stale =
                        polledAt (Time.millisToPosix 1800)
                            (meta
                                [ ( "exoext.v1.res.body.n", "1" )
                                , ( "exoext.v1.res.body.0", """{"requestId":"exoext-req-1","kind":"action","action":"deleteResult","status":"ok"}""" )
                                ]
                            )
                            written
                in
                Expect.equal ( [ "exoext-req-8", "exoext-req-9" ], True )
                    ( rowIds stale, stale.exoextPendingDelete /= Nothing )
        , test "a removal that is never answered becomes a failure, not a row stuck removing" <|
            \_ ->
                Expect.equal ( Nothing, Just { resultId = "exoext-req-9", message = Lifecycle.requestTimedOutMessage } )
                    (ServerDetail.exoextRemovalState
                        (Time.millisToPosix (1700 + Lifecycle.requestTimeoutMillis + 1))
                        written
                    )
        ]


{-| Two archived rows, the newer of which is the one the removal names.
-}
loadedHistory : RDPP.RemoteDataPlusPlus String (List CloudShieldRow)
loadedHistory =
    RDPP.RemoteDataPlusPlus
        (RDPP.DoHave
            [ row "exoext-req-8", row "exoext-req-9" ]
            (Time.millisToPosix 0)
        )
        (RDPP.NotLoading Nothing)


type alias CloudShieldRow =
    { batchId : String
    , requestId : Maybe String
    , targetId : String
    , targetName : String
    , completedAt : String
    , status : String
    , counts : { critical : Int, high : Int, medium : Int, low : Int, info : Int }
    , error : Maybe String
    }


row : String -> CloudShieldRow
row requestId =
    { batchId = "b-1"
    , requestId = Just requestId
    , targetId = "i-1"
    , targetName = "alpha"
    , completedAt = "2026-08-01T00:00:00Z"
    , status = "ok"
    , counts = { critical = 0, high = 1, medium = 0, low = 0, info = 0 }
    , error = Nothing
    }



-- NAVIGATION


{-| The §5.4 check applied to the first verb through which an extension moves the app.
-}
navigationSuite : Test
navigationSuite =
    describe "exoext.navigate goes only where the project already is"
        [ test "an instance of this project is navigated to" <|
            \_ ->
                Expect.equal
                    (SharedMsg.NavigateToRoute
                        (Route.ProjectRoute { projectUuid = "project-uuid", regionId = Just "RegionOne" }
                            (Route.ServerDetail "i-1")
                        )
                    )
                    (ServerDetail.exoextNavigation (projectWithTargets [ "i-1" ] []) "i-1")
        , test "an id this project does not have is ignored, not guessed at" <|
            \_ ->
                Expect.equal SharedMsg.NoOp
                    (ServerDetail.exoextNavigation (projectWithTargets [ "i-1" ] []) "i-elsewhere")
        , test "an empty id goes nowhere either" <|
            \_ ->
                Expect.equal SharedMsg.NoOp
                    (ServerDetail.exoextNavigation (projectWithTargets [ "i-1" ] []) "")
        ]



-- THE PRE-ECHO BADGE


{-| A request the wire has not echoed yet still has to show on its row.

Before this, the window between writing a request and the publisher reporting it left the row
reading **idle** while the same host record was offering that row a Stop control and greying every
Scan button. Three controls, three accounts of what was happening.

-}
preEchoBadgeSuite : Test
preEchoBadgeSuite =
    let
        justWritten =
            writeRequestAt 1700 { subject = "i-1", batchId = Nothing } modelAfterStartScan

        configWith metadata model =
            ServerDetail.exoextViewConfig True
                (projectPublishing metadata)
                model
                (Time.millisToPosix 1700)
                (serverPublishing metadata)
    in
    describe "a request the wire has not echoed yet"
        [ test "shows on its own row instead of leaving it idle" <|
            \_ ->
                Expect.equal (Just { targetId = "i-1", state = "queued" })
                    (configWith [] justWritten).statusOverride
        , test "and is stoppable at the same moment, so the row and the control agree" <|
            \_ ->
                Expect.equal
                    ( Just { targetId = "i-1", requestId = "exoext-req-1700" }, True )
                    ( (configWith [] justWritten).cancellableRun
                    , (configWith [] justWritten).scanBusy
                    )
        , test "the row projected to the manifest is queued, never idle-with-a-Stop" <|
            \_ ->
                Expect.equal (Ok [ "queued" ])
                    (Card.projection Time.utc
                        (configWith [] justWritten)
                        [ { id = "i-1", name = "alpha", status = "ACTIVE" } ]
                        justWritten.exoextCard
                        |> Decode.decodeValue
                            (Decode.field "instances" (Decode.list (Decode.field "scanState" Decode.string)))
                        |> Result.mapError Decode.errorToString
                    )
        , test "once the wire echoes the run, the wire is what shows" <|
            \_ ->
                Expect.equal (Just { targetId = "i-1", state = "done" })
                    (configWith (runSlotFor 1700 "done" "i-1") justWritten).statusOverride
        ]



-- HISTORY REFETCH


{-| A history fetch that fails must leave itself retryable.

The refresh key is a "this generation has been fetched" marker, so stamping it on a failure records
a fetch that never happened: nothing asks again until the run slot moves, which for a settled scan
can be hours away. That is the bug where history froze until the researcher reloaded the page.

-}
historyRefetchSuite : Test
historyRefetchSuite =
    let
        metadata =
            runSlot 1700 "done"

        key =
            ":1700:done"

        received result =
            updatedBy
                (ServerDetail.GotExoextIndexObject (Time.millisToPosix 1800) key result)
                (projectPublishing metadata)
                { idle | exoextHistoryRequestKey = Just key }
    in
    describe "the history index refetch"
        [ test "a good fetch stamps its generation, so a steady state refetches nothing" <|
            \_ ->
                Expect.equal (Just key) (received (Ok "[]")).exoextHistoryRequestKey
        , test "a failed fetch does NOT stamp it, so the next poll tries again" <|
            \_ ->
                Expect.equal Nothing
                    (received (Err { error = Http.NetworkError, body = "" })).exoextHistoryRequestKey
        , test "a failed FIRST fetch still resolves to empty history rather than an error card" <|
            \_ ->
                Expect.equal (Just 0)
                    (case (received (Err { error = Http.NetworkError, body = "" })).exoextHistory.data of
                        RDPP.DoHave rows _ ->
                            Just (List.length rows)

                        RDPP.DontHave ->
                            Nothing
                    )
        ]
