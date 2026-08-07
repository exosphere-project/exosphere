module Tests.Exoext.Lifecycle exposing (suite)

{-| Focused coverage for the generic `Exoext.Lifecycle` request/response + session model: the
`sessionState` derivation (opening / open / stale / failed / idle, with the expiry boundary,
the 30s timeout, and Opening superseding a prior Open), session freshness, run-status
correlation, run staleness (the safety valve on a run left non-terminal forever), the §7.1 batch
pacing step, and the declarative verb alias table.
-}

import Exoext.Lifecycle as Lifecycle
import Expect
import Test exposing (Test, describe, test)
import Time


meta : List ( String, String ) -> List { key : String, value : String }
meta pairs =
    List.map (\( k, v ) -> { key = k, value = v }) pairs


timeoutMillis : Int
timeoutMillis =
    30 * 1000


{-| The client clock as the run-status fixtures see it. Every fixture run `seq` is a small number
below it, so no fixture run is anywhere near `staleRunAfterMillis` old — staleness is exercised
deliberately, in its own describe, and never falls out of another test by accident.
-}
pollTime : Time.Posix
pollTime =
    Time.millisToPosix 10000


{-| The clock as it reads when a run written at wall-clock `seq` is `ageMillis` old.
-}
clockAfter : Int -> Int -> Time.Posix
clockAfter seq ageMillis =
    Time.millisToPosix (seq + ageMillis)


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


{-| The same §7.1 status slot, plus the §4.3 descriptors that name WHICH run it is — what a
WP8-or-later publisher writes, and the whole input recovery works from.
-}
runSlotFor : Int -> String -> List { key : String, value : String }
runSlotFor seq state =
    runSlot seq state
        ++ [ { key = "exoext.v1.run.target", value = "i-9" }
           , { key = "exoext.v1.run.requestId", value = "exoext-req-" ++ String.fromInt seq }
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
        , describe "runStale (the safety valve on a run left non-terminal forever)"
            [ test "the threshold is six hours" <|
                \_ ->
                    -- Pinned deliberately. Every other case here is stated relative to the constant
                    -- so a retune does not make them vacuous; this one asserts the number the doc
                    -- comment argues for, so a retune has to be a decision rather than a typo.
                    Lifecycle.staleRunAfterMillis
                        |> Expect.equal (6 * 60 * 60 * 1000)
            , test "a run that started moments ago is not stale, whatever non-terminal state it is in" <|
                \_ ->
                    -- The regression guard that matters most: a healthy run must never be called
                    -- dead, because the host's answer to a dead run is to stop displaying it and
                    -- let the user supersede it.
                    [ "queued", "running", "scanning" ]
                        |> List.map (\state -> Lifecycle.runStale (clockAfter 1700 1000) { seq = 1700, state = state })
                        |> Expect.equal [ False, False, False ]
            , test "a healthy long scan, well past the reference 45-minute deadline, is still not stale" <|
                \_ ->
                    -- The threshold has to clear a publisher tuned far more generously than the
                    -- §4.4 reference, since the host does not know any publisher's timeouts.
                    Lifecycle.runStale (clockAfter 1700 (90 * 60 * 1000)) { seq = 1700, state = "running" }
                        |> Expect.equal False
            , test "a non-terminal run older than the threshold is stale" <|
                \_ ->
                    Lifecycle.runStale (clockAfter 1700 (Lifecycle.staleRunAfterMillis + 60 * 1000))
                        { seq = 1700, state = "running" }
                        |> Expect.equal True
            , test "the boundary is exclusive in both directions: exactly at the threshold is live, one ms past is stale" <|
                \_ ->
                    ( Lifecycle.runStale (clockAfter 1700 Lifecycle.staleRunAfterMillis) { seq = 1700, state = "running" }
                    , Lifecycle.runStale (clockAfter 1700 (Lifecycle.staleRunAfterMillis + 1)) { seq = 1700, state = "running" }
                    )
                        |> Expect.equal ( False, True )
            , test "a terminal run is never stale, however old — it is a settled record, not a live run" <|
                \_ ->
                    -- Ageing terminal runs out would break the one place an old terminal run is
                    -- load-bearing: an undrained batch tail adopting it in `recoverRun`.
                    [ "done", "error", "cancelled", "expired" ]
                        |> List.map
                            (\state ->
                                Lifecycle.runStale (clockAfter 1700 (100 * Lifecycle.staleRunAfterMillis))
                                    { seq = 1700, state = state }
                            )
                        |> Expect.equal [ False, False, False, False ]
            , test "a run whose seq is ahead of the client clock is not stale — clock skew must not condemn a run" <|
                \_ ->
                    Lifecycle.runStale (Time.millisToPosix 0) { seq = 1700, state = "running" }
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
        , describe "recoverRun (adopting a run this session never wrote)"
            [ test "a live run naming its target becomes a tracked request, dated by its own seq" <|
                \_ ->
                    -- `since` is the run's seq, i.e. wall-clock millis of the write, so the elapsed
                    -- clock survives a reload instead of restarting from it.
                    Lifecycle.recoverRun
                        { now = pollTime, tracked = Nothing, tailPending = False, metadata = runSlotFor 1700 "running" }
                        |> Expect.equal
                            (Lifecycle.RecoverPending
                                { seq = 1700
                                , requestId = "exoext-req-1700"
                                , kind = ""
                                , subject = "i-9"
                                , since = Time.millisToPosix 1700
                                }
                            )
            , test "a live run is recovered for every non-terminal state" <|
                \_ ->
                    [ "queued", "running", "scanning" ]
                        |> List.map
                            (\state ->
                                Lifecycle.recoverRun
                                    { now = pollTime, tracked = Nothing, tailPending = False, metadata = runSlotFor 1700 state }
                                    /= Lifecycle.NoRecovery
                            )
                        |> Expect.equal [ True, True, True ]
            , test "a LIVE tracked request is never overwritten, even by a run slot naming another row" <|
                \_ ->
                    -- The precedence rule. The tracker is the reader's own newer truth (a request
                    -- just written is not echoed yet); adopting the wire here would rewind it.
                    Lifecycle.recoverRun
                        { now = pollTime, tracked = Just (pendingWithSeq 9000), tailPending = False, metadata = runSlotFor 1700 "running" }
                        |> Expect.equal Lifecycle.NoRecovery
            , test "a tracked request whose own run already settled is still not overwritten" <|
                \_ ->
                    Lifecycle.recoverRun
                        { now = pollTime, tracked = Just (pendingWithSeq 1700), tailPending = False, metadata = runSlotFor 1700 "done" }
                        |> Expect.equal Lifecycle.NoRecovery
            , test "no run slot at all recovers nothing" <|
                \_ ->
                    Lifecycle.recoverRun { now = pollTime, tracked = Nothing, tailPending = False, metadata = [] }
                        |> Expect.equal Lifecycle.NoRecovery
            , test "a run with no target recovers nothing — an unattributable run is not adoptable" <|
                \_ ->
                    Lifecycle.recoverRun
                        { now = pollTime, tracked = Nothing, tailPending = False, metadata = runSlot 1700 "running" }
                        |> Expect.equal Lifecycle.NoRecovery
            , test "a finished run with nothing waiting on it recovers NOTHING" <|
                \_ ->
                    -- The run slot is overwritten in place and never cleared, so a terminal run is a
                    -- record of the LAST run, not of a current one. Adopting one means re-adopting
                    -- the same finished run on every later page load, which is exactly how a scan
                    -- that finished once came to read as just-finished forever.
                    [ "done", "error", "cancelled", "expired" ]
                        |> List.map
                            (\state ->
                                Lifecycle.recoverRun
                                    { now = pollTime, tracked = Nothing, tailPending = False, metadata = runSlotFor 1700 state }
                            )
                        |> Expect.equal
                            ([ "done", "error", "cancelled", "expired" ]
                                |> List.map (\_ -> Lifecycle.NoRecovery)
                            )
            , test "a finished run WITH an undrained tail is tracked, so the batch can resume" <|
                \_ ->
                    Lifecycle.recoverRun
                        { now = pollTime, tracked = Nothing, tailPending = True, metadata = runSlotFor 1700 "done" }
                        |> Expect.equal
                            (Lifecycle.RecoverPending
                                { seq = 1700
                                , requestId = "exoext-req-1700"
                                , kind = ""
                                , subject = "i-9"
                                , since = Time.millisToPosix 1700
                                }
                            )
            , test "a STALE live run is not adopted — an abandoned run must not resurrect as a tracked request" <|
                \_ ->
                    -- The reload half of the safety valve. A publisher restarted mid-run leaves
                    -- `run.state` non-terminal forever; adopting it would re-arm the block on every
                    -- page load, which is the trap this exists to open.
                    [ "queued", "running", "scanning" ]
                        |> List.map
                            (\state ->
                                Lifecycle.recoverRun
                                    { now = clockAfter 1700 (Lifecycle.staleRunAfterMillis + 1)
                                    , tracked = Nothing
                                    , tailPending = False
                                    , metadata = runSlotFor 1700 state
                                    }
                            )
                        |> Expect.equal [ Lifecycle.NoRecovery, Lifecycle.NoRecovery, Lifecycle.NoRecovery ]
            , test "recovery's staleness boundary matches runStale: at the threshold it still adopts, one ms past it does not" <|
                \_ ->
                    ( Lifecycle.recoverRun
                        { now = clockAfter 1700 Lifecycle.staleRunAfterMillis
                        , tracked = Nothing
                        , tailPending = False
                        , metadata = runSlotFor 1700 "running"
                        }
                        /= Lifecycle.NoRecovery
                    , Lifecycle.recoverRun
                        { now = clockAfter 1700 (Lifecycle.staleRunAfterMillis + 1)
                        , tracked = Nothing
                        , tailPending = False
                        , metadata = runSlotFor 1700 "running"
                        }
                        /= Lifecycle.NoRecovery
                    )
                        |> Expect.equal ( True, False )
            , test "an ancient TERMINAL run with an undrained tail is still adopted — staleness must not strand a batch" <|
                \_ ->
                    -- Adopting a terminal run is not about believing it; it is the token
                    -- `advanceBatch` needs to pop the next subject. Staleness applies only to runs
                    -- claiming to be live, so this path is untouched.
                    Lifecycle.recoverRun
                        { now = clockAfter 1700 (100 * Lifecycle.staleRunAfterMillis)
                        , tracked = Nothing
                        , tailPending = True
                        , metadata = runSlotFor 1700 "done"
                        }
                        |> Expect.equal
                            (Lifecycle.RecoverPending
                                { seq = 1700
                                , requestId = "exoext-req-1700"
                                , kind = ""
                                , subject = "i-9"
                                , since = Time.millisToPosix 1700
                                }
                            )
            , test "a publisher that omits run.requestId still recovers, with an empty requestId" <|
                \_ ->
                    -- A scan correlates by seq, so the id is inert for tracking; it only matters to
                    -- a cancel, which falls back to the host's own minting.
                    Lifecycle.recoverRun
                        { now = pollTime
                        , tracked = Nothing
                        , tailPending = False
                        , metadata = runSlot 1700 "running" ++ [ { key = "exoext.v1.run.target", value = "i-9" } ]
                        }
                        |> Expect.equal
                            (Lifecycle.RecoverPending
                                { seq = 1700
                                , requestId = ""
                                , kind = ""
                                , subject = "i-9"
                                , since = Time.millisToPosix 1700
                                }
                            )
            , test "a recovered run names no verb, because §4.3 never told this layer one" <|
                \_ ->
                    -- The run slot carries seq/state/target and no verb, so any `kind` here would be
                    -- this layer inventing one — and the only value it ever invented was one
                    -- extension's word. Correlation is by seq, so empty costs nothing.
                    case Lifecycle.recoverRun { now = pollTime, tracked = Nothing, tailPending = False, metadata = runSlotFor 1700 "running" } of
                        Lifecycle.RecoverPending request ->
                            Expect.equal "" request.kind

                        Lifecycle.NoRecovery ->
                            Expect.fail "expected a live run to be recovered"
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
        , describe "runStopping (a stop asked for and not yet answered)"
            [ test "the cancel channel naming this run, on a non-terminal run, is stopping" <|
                \_ ->
                    Lifecycle.runStopping { requestId = "req-1", state = "running" }
                        (meta [ ( "exoext.v1.req.cancel", "req-1" ) ])
                        |> Expect.equal True
            , test "a channel naming a different run is not this run stopping" <|
                \_ ->
                    Lifecycle.runStopping { requestId = "req-1", state = "running" }
                        (meta [ ( "exoext.v1.req.cancel", "req-2" ) ])
                        |> Expect.equal False
            , test "an empty channel is not a stop, whatever the run is called" <|
                \_ ->
                    -- The cleared value a superseding request write leaves behind. Matching it
                    -- against a run whose id the host could not resolve (also "") would invent one.
                    Lifecycle.runStopping { requestId = "", state = "running" }
                        (meta [ ( "exoext.v1.req.cancel", "" ) ])
                        |> Expect.equal False
            , test "no cancel channel at all is not stopping" <|
                \_ ->
                    Lifecycle.runStopping { requestId = "req-1", state = "running" } []
                        |> Expect.equal False
            , test "every terminal state is past stopping, cancelled included" <|
                \_ ->
                    -- `cancelled` is the one a reader might expect to be stopping: it is the state a
                    -- stop LEADS to, and by the time it is on the wire the stop is answered.
                    Lifecycle.terminalRunStates
                        |> List.filter
                            (\state ->
                                Lifecycle.runStopping { requestId = "req-1", state = state }
                                    (meta [ ( "exoext.v1.req.cancel", "req-1" ) ])
                            )
                        |> Expect.equal []
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
