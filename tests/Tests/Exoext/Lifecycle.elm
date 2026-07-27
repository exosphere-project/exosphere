module Tests.Exoext.Lifecycle exposing (suite)

{-| Focused coverage for the generic `Exoext.Lifecycle` request/response + session model: the
`sessionState` derivation (opening / open / stale / failed / idle, with the expiry boundary,
the 30s timeout, and Opening superseding a prior Open), session freshness, run-status
correlation, the §7.1 batch pacing step, and the declarative verb alias table.
-}

import Exoext.Lifecycle as Lifecycle
import Expect
import Test exposing (Test, describe, test)
import Time


timeoutMillis : Int
timeoutMillis =
    30 * 1000


{-| A session request pending for `subject` "b1", written at wall-clock `sinceMillis`.
-}
pendingAt : Int -> Lifecycle.PendingRequest
pendingAt sinceMillis =
    { seq = 0
    , requestId = "req-1"
    , kind = "getEmbed"
    , subject = "b1"
    , since = Time.millisToPosix sinceMillis
    }


{-| An `"ok"` wire result for request "req-1", opening a session for "b1" that expires at
`expiresMillis`.
-}
okResultExpiring : Int -> Lifecycle.ResultInput
okResultExpiring expiresMillis =
    { requestId = "req-1"
    , subject = "b1"
    , status = "ok"
    , url = "https://vm.example/embed"
    , expiresAt = Just (Time.millisToPosix expiresMillis)
    , message = ""
    }


errorResult : Lifecycle.ResultInput
errorResult =
    { requestId = "req-1"
    , subject = "b1"
    , status = "error"
    , url = ""
    , expiresAt = Nothing
    , message = "remint failed"
    }


{-| A batch of `remaining` subjects in its steady state: the last decided-on write has been issued,
so only the seq correlation is guarding the request slot.
-}
batchOf : List String -> Lifecycle.Batch
batchOf remaining =
    { batchId = Just "b", remaining = remaining, awaitingWrite = False }


runSlot : Int -> String -> List { key : String, value : String }
runSlot seq state =
    [ { key = "exoext.v1.run.seq", value = String.fromInt seq }
    , { key = "exoext.v1.run.state", value = state }
    ]


suite : Test
suite =
    describe "Exoext.Lifecycle"
        [ describe "sessionFreshness"
            [ test "before expiry is Fresh" <|
                \_ ->
                    Lifecycle.sessionFreshness (Time.millisToPosix 999)
                        { resultId = "b1", url = "u", expiresAt = Just (Time.millisToPosix 1000) }
                        |> Expect.equal Lifecycle.Fresh
            , test "exactly at expiry is Stale" <|
                \_ ->
                    Lifecycle.sessionFreshness (Time.millisToPosix 1000)
                        { resultId = "b1", url = "u", expiresAt = Just (Time.millisToPosix 1000) }
                        |> Expect.equal Lifecycle.Stale
            , test "no expiry is Fresh (never hide on a missing timestamp)" <|
                \_ ->
                    Lifecycle.sessionFreshness (Time.millisToPosix 10000000)
                        { resultId = "b1", url = "u", expiresAt = Nothing }
                        |> Expect.equal Lifecycle.Fresh
            ]
        , describe "sessionState"
            [ test "nothing pending and no result is NoSession" <|
                \_ ->
                    Lifecycle.sessionState timeoutMillis (Time.millisToPosix 0) Nothing Nothing
                        |> Expect.equal Lifecycle.NoSession
            , test "an ok, still-fresh result (no pending) is Open" <|
                \_ ->
                    Lifecycle.sessionState timeoutMillis (Time.millisToPosix 500) Nothing (Just (okResultExpiring 1000))
                        |> Expect.equal (Lifecycle.Open { resultId = "b1", url = "https://vm.example/embed", expiresAt = Just (Time.millisToPosix 1000) })
            , test "Open flips to OpenStale exactly at expiry" <|
                \_ ->
                    ( Lifecycle.sessionState timeoutMillis (Time.millisToPosix 999) Nothing (Just (okResultExpiring 1000))
                    , Lifecycle.sessionState timeoutMillis (Time.millisToPosix 1000) Nothing (Just (okResultExpiring 1000))
                    )
                        |> Expect.equal
                            ( Lifecycle.Open { resultId = "b1", url = "https://vm.example/embed", expiresAt = Just (Time.millisToPosix 1000) }
                            , Lifecycle.OpenStale { resultId = "b1", url = "https://vm.example/embed", expiresAt = Just (Time.millisToPosix 1000) }
                            )
            , test "an error result is Failed with the unwrapped message and its subject" <|
                \_ ->
                    Lifecycle.sessionState timeoutMillis (Time.millisToPosix 0) Nothing (Just errorResult)
                        |> Expect.equal (Lifecycle.Failed { subject = "b1", message = "remint failed" })
            , test "a pending request with no result yet (within the timeout) is Opening for its subject" <|
                \_ ->
                    Lifecycle.sessionState timeoutMillis (Time.millisToPosix 5000) (Just (pendingAt 0)) Nothing
                        |> Expect.equal (Lifecycle.Opening { subject = "b1" })
            , test "a pending request past the 30s timeout is Failed, attributed to the clicked subject" <|
                \_ ->
                    Lifecycle.sessionState timeoutMillis (Time.millisToPosix 31000) (Just (pendingAt 0)) Nothing
                        |> Expect.equal (Lifecycle.Failed { subject = "b1", message = "the request timed out" })
            , test "a new Opening supersedes a previous Open (result in slot is an earlier request)" <|
                \_ ->
                    -- The slot still holds a fresh ok result, but from an EARLIER request (requestId
                    -- "req-0" != the pending "req-1"): our new getEmbed is in flight, so Opening wins.
                    let
                        earlierResult =
                            { requestId = "req-0"
                            , subject = "b0"
                            , status = "ok"
                            , url = "https://vm.example/old"
                            , expiresAt = Just (Time.millisToPosix 100000)
                            , message = ""
                            }
                    in
                    Lifecycle.sessionState timeoutMillis (Time.millisToPosix 5000) (Just (pendingAt 0)) (Just earlierResult)
                        |> Expect.equal (Lifecycle.Opening { subject = "b1" })
            , test "a matching error result (pending requestId matches) is Failed, not Opening" <|
                \_ ->
                    Lifecycle.sessionState timeoutMillis (Time.millisToPosix 5000) (Just (pendingAt 0)) (Just errorResult)
                        |> Expect.equal (Lifecycle.Failed { subject = "b1", message = "remint failed" })
            ]
        , describe "run status correlation"
            [ test "isTerminalRunState covers done/error/cancelled/expired only" <|
                \_ ->
                    List.map Lifecycle.isTerminalRunState [ "done", "error", "cancelled", "expired", "queued", "running" ]
                        |> Expect.equal [ True, True, True, True, False, False ]
            , test "correlatedRunState returns the state when the seq matches" <|
                \_ ->
                    Lifecycle.correlatedRunState 7 (runSlot 7 "running")
                        |> Expect.equal "running"
            , test "correlatedRunState defaults to queued on a seq mismatch" <|
                \_ ->
                    Lifecycle.correlatedRunState 7 (runSlot 9 "done")
                        |> Expect.equal "queued"
            , test "correlatedRunState defaults to queued with no run slot" <|
                \_ ->
                    Lifecycle.correlatedRunState 7 []
                        |> Expect.equal "queued"
            , test "requestStillPending: a tracked, non-terminal run is pending" <|
                \_ ->
                    Lifecycle.requestStillPending (Just (pendingWithSeq 7)) (runSlot 7 "running")
                        |> Expect.equal True
            , test "requestStillPending: a terminal run is not pending" <|
                \_ ->
                    Lifecycle.requestStillPending (Just (pendingWithSeq 7)) (runSlot 7 "done")
                        |> Expect.equal False
            , test "requestStillPending: no tracked request is not pending" <|
                \_ ->
                    Lifecycle.requestStillPending Nothing (runSlot 7 "running")
                        |> Expect.equal False
            ]
        , describe "advanceBatch"
            [ test "no tracked request waits" <|
                \_ ->
                    Lifecycle.advanceBatch Nothing (Just (batchOf [ "i-2" ])) (runSlot 7 "done")
                        |> Expect.equal Lifecycle.BatchWaiting
            , test "a non-terminal correlated run waits" <|
                \_ ->
                    Lifecycle.advanceBatch (Just (pendingWithSeq 7)) (Just (batchOf [ "i-2" ])) (runSlot 7 "running")
                        |> Expect.equal Lifecycle.BatchWaiting
            , test "a decided-on write that has not been issued yet waits, even on a settled run" <|
                \_ ->
                    -- The pre-write guard: `next` was popped on an earlier poll and the write is
                    -- still on its way, so the metadata is byte-identical to what already settled.
                    -- Without this, a second poll landing in that gap pops a second subject and
                    -- races two requests into the one §7.1 slot.
                    Lifecycle.advanceBatch (Just (pendingWithSeq 7))
                        (Just { batchId = Just "b", remaining = [ "i-3" ], awaitingWrite = True })
                        (runSlot 7 "done")
                        |> Expect.equal Lifecycle.BatchWaiting
            , test "each terminal state settles, reporting the finished subject and that state" <|
                \_ ->
                    [ "done", "error", "cancelled", "expired" ]
                        |> List.map
                            (\state ->
                                Lifecycle.advanceBatch (Just (pendingWithSeq 7)) Nothing (runSlot 7 state)
                            )
                        |> Expect.equal
                            ([ "done", "error", "cancelled", "expired" ]
                                |> List.map
                                    (\state ->
                                        Lifecycle.BatchSettled { subject = "i-1", state = state, next = Nothing, remaining = [] }
                                    )
                            )
            , test "a settled run with subjects left pops the head and reports the rest" <|
                \_ ->
                    Lifecycle.advanceBatch (Just (pendingWithSeq 7)) (Just (batchOf [ "i-2", "i-3" ])) (runSlot 7 "done")
                        |> Expect.equal (Lifecycle.BatchSettled { subject = "i-1", state = "done", next = Just "i-2", remaining = [ "i-3" ] })
            , test "a settled run with an exhausted batch reports no next subject" <|
                \_ ->
                    Lifecycle.advanceBatch (Just (pendingWithSeq 7)) (Just (batchOf [])) (runSlot 7 "done")
                        |> Expect.equal (Lifecycle.BatchSettled { subject = "i-1", state = "done", next = Nothing, remaining = [] })
            , test "a run slot still echoing the PREVIOUS seq waits — the no-double-fire property" <|
                \_ ->
                    -- The continuation for i-2 has just been written (pending.seq bumped to 8) while
                    -- the status slot still reports run 7 as done. The seq mismatch reads as
                    -- "queued", so this poll must not fire a second continuation.
                    Lifecycle.advanceBatch (Just (pendingWithSeq 8)) (Just (batchOf [ "i-3" ])) (runSlot 7 "done")
                        |> Expect.equal Lifecycle.BatchWaiting
            ]
        , describe "declarative verbs"
            [ test "resolveVerb maps an aliased action name to its generic verb + kind" <|
                \_ ->
                    Lifecycle.resolveVerb aliases "cloudshield.startScan"
                        |> Maybe.map (\a -> ( a.verb, a.kind ))
                        |> Expect.equal (Just ( Lifecycle.verbWriteRequest, "scan" ))
            , test "resolveVerb maps getEmbed to openSession" <|
                \_ ->
                    Lifecycle.resolveVerb aliases "cloudshield.getEmbed"
                        |> Maybe.map .verb
                        |> Expect.equal (Just Lifecycle.verbOpenSession)
            , test "resolveVerb returns Nothing for an off-table action (fail-closed)" <|
                \_ ->
                    Lifecycle.resolveVerb aliases "cloudshield.deleteEverything"
                        |> Expect.equal Nothing
            ]
        ]


pendingWithSeq : Int -> Lifecycle.PendingRequest
pendingWithSeq seq =
    { seq = seq, requestId = "", kind = "scan", subject = "i-1", since = Time.millisToPosix 0 }


aliases : List Lifecycle.VerbAlias
aliases =
    [ { name = "cloudshield.startScan", verb = Lifecycle.verbWriteRequest, kind = "scan" }
    , { name = "cloudshield.getEmbed", verb = Lifecycle.verbOpenSession, kind = "" }
    ]
