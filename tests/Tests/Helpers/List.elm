module Tests.Helpers.List exposing (listSuite)

import Expect
import Helpers.List
import Test exposing (Test, describe, test)


listSuite : Test
listSuite =
    describe "List Helpers Tests"
        [ describe "duplicatedValuesBy"
            [ test "returns duplicate field values only once" <|
                \_ ->
                    [ { name = "a", id = "1" }
                    , { name = "b", id = "2" }
                    , { name = "a", id = "3" }
                    ]
                        |> Helpers.List.duplicatedValuesBy .name
                        |> Expect.equal [ "a" ]
            ]
        ]
