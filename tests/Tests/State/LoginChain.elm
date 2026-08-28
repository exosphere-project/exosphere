module Tests.State.LoginChain exposing (suite)

import Expect
import State.LoginChain
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "credential-file login chain"
        [ test "two region questions run in file order" <|
            \_ ->
                let
                    ( first, afterFirst ) =
                        State.LoginChain.popNext [ "cloud-a", "cloud-b" ]

                    ( second, afterSecond ) =
                        State.LoginChain.popNext afterFirst
                in
                Expect.equal
                    ( Just "cloud-a", Just "cloud-b", [] )
                    ( first, second, afterSecond )
        , test "cancel skips the current login and exposes the next one" <|
            \_ ->
                State.LoginChain.popNext [ "cloud-b" ]
                    |> Expect.equal ( Just "cloud-b", [] )
        , test "leaving region selection abandons the held login and remaining chain" <|
            \_ ->
                let
                    abandoned =
                        State.LoginChain.shouldAbandon True False

                    oldPending =
                        if abandoned then
                            []

                        else
                            [ "cloud-b" ]

                    ( freshFirst, freshRemaining ) =
                        State.LoginChain.popNext ("fresh-cloud" :: oldPending)
                in
                Expect.equal ( Just "fresh-cloud", [] ) ( freshFirst, freshRemaining )
        , test "a failed login advances to the next intent" <|
            \_ ->
                State.LoginChain.popNext [ "cloud-after-failure" ]
                    |> Expect.equal ( Just "cloud-after-failure", [] )
        , test "a stale response from an identical replacement chain is dropped" <|
            \_ ->
                let
                    firstGeneration =
                        State.LoginChain.nextGeneration 0

                    replacementGeneration =
                        State.LoginChain.nextGeneration firstGeneration
                in
                Expect.equal
                    ( False, True )
                    ( State.LoginChain.acceptsResponse replacementGeneration firstGeneration
                    , State.LoginChain.acceptsResponse replacementGeneration replacementGeneration
                    )
        , test "abandonment invalidates an in-flight response before it can restore state" <|
            \_ ->
                let
                    requestGeneration =
                        7

                    generationAfterAbandonment =
                        State.LoginChain.nextGeneration requestGeneration
                in
                State.LoginChain.acceptsResponse generationAfterAbandonment requestGeneration
                    |> Expect.equal False
        , test "an already-added notice survives a region question and remains deduplicated" <|
            \_ ->
                []
                    |> State.LoginChain.addNotice "cloud-riders"
                    |> State.LoginChain.addNotice "rogue-squadron"
                    |> State.LoginChain.addNotice "cloud-riders"
                    |> Expect.equal [ "cloud-riders", "rogue-squadron" ]
        ]
