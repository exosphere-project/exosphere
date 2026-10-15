module Tests.Helpers.GetterSetters exposing (loginAlreadyImportedSuite)

import Expect
import Helpers.GetterSetters
import Test exposing (Test, describe, test)


loginAlreadyImportedSuite : Test
loginAlreadyImportedSuite =
    describe "deciding whether a scoped login is already held"
        [ describe "when the incoming region is settled"
            [ test "a project held in that region is the same project" <|
                \() ->
                    Helpers.GetterSetters.loginAlreadyImported (Just "IU") [ Just "IU" ]
                        |> Expect.equal True
            , test "the same project UUID held only in another region still needs importing" <|
                \() ->
                    Helpers.GetterSetters.loginAlreadyImported (Just "TACC") [ Just "IU" ]
                        |> Expect.equal False
            , test "one of several held regions matching is enough" <|
                \() ->
                    Helpers.GetterSetters.loginAlreadyImported (Just "TACC") [ Just "IU", Just "TACC" ]
                        |> Expect.equal True
            , test "none of several held regions matching still needs importing" <|
                \() ->
                    Helpers.GetterSetters.loginAlreadyImported (Just "JETSTREAM") [ Just "IU", Just "TACC" ]
                        |> Expect.equal False
            , test "holding nothing needs importing" <|
                \() ->
                    Helpers.GetterSetters.loginAlreadyImported (Just "IU") []
                        |> Expect.equal False
            , test "a project stored before regions were recorded counts as the same project" <|
                \() ->
                    Helpers.GetterSetters.loginAlreadyImported (Just "IU") [ Nothing ]
                        |> Expect.equal True
            ]
        , describe "when the incoming region is not settled"
            [ test "a project stored before regions were recorded counts, which is what refreshes rely on" <|
                \() ->
                    Helpers.GetterSetters.loginAlreadyImported Nothing [ Nothing ]
                        |> Expect.equal True
            , test "a project held in a named region still leaves another region to ask about" <|
                \() ->
                    Helpers.GetterSetters.loginAlreadyImported Nothing [ Just "IU" ]
                        |> Expect.equal False
            , test "several named regions still leave another region to ask about" <|
                \() ->
                    Helpers.GetterSetters.loginAlreadyImported Nothing [ Just "IU", Just "TACC" ]
                        |> Expect.equal False
            , test "one region-less project among named ones is enough" <|
                \() ->
                    Helpers.GetterSetters.loginAlreadyImported Nothing [ Just "IU", Nothing ]
                        |> Expect.equal True
            , test "holding nothing needs importing" <|
                \() ->
                    Helpers.GetterSetters.loginAlreadyImported Nothing []
                        |> Expect.equal False
            ]
        ]
