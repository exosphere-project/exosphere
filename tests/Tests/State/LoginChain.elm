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
        ]
