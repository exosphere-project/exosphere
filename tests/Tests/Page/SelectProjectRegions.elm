module Tests.Page.SelectProjectRegions exposing (canSubmitSuite)

import Expect
import Page.SelectProjectRegions
import Set
import Test exposing (Test, describe, test)


model : List String -> Page.SelectProjectRegions.Model
model selectedRegions =
    let
        initial =
            Page.SelectProjectRegions.init
                "https://cell.alliance.rebel:5000/v3"
                "1d00d4b1de1d00d4b1de1d00d4b1de"
    in
    { initial | selectedRegions = Set.fromList selectedRegions }


canSubmitSuite : Test
canSubmitSuite =
    describe "deciding whether there is a region selection to submit"
        [ test "a page the user has not touched has nothing to submit" <|
            \() ->
                Page.SelectProjectRegions.init
                    "https://cell.alliance.rebel:5000/v3"
                    "1d00d4b1de1d00d4b1de1d00d4b1de"
                    |> Page.SelectProjectRegions.canSubmit
                    |> Expect.equal False
        , test "unticking the last region leaves nothing to submit" <|
            \() ->
                model []
                    |> Page.SelectProjectRegions.canSubmit
                    |> Expect.equal False
        , test "one region is enough" <|
            \() ->
                model [ "IU" ]
                    |> Page.SelectProjectRegions.canSubmit
                    |> Expect.equal True
        , test "several regions are enough" <|
            \() ->
                model [ "IU", "TACC" ]
                    |> Page.SelectProjectRegions.canSubmit
                    |> Expect.equal True
        ]
