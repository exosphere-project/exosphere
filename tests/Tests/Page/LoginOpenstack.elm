module Tests.Page.LoginOpenstack exposing (cloudSelectionSuite)

import Expect
import OpenStack.CloudsYaml
import OpenStack.Types exposing (ApplicationCredential)
import Page.LoginOpenstack
import Set
import Test exposing (Test, describe, test)


cloud : String -> OpenStack.CloudsYaml.CloudEntry
cloud name =
    { name = name
    , authUrl = "https://cell.alliance.rebel:5000/v3"
    , appCredential = ApplicationCredential (name ++ "-id") (name ++ "-secret")
    , regionName = Nothing
    }


modelWithClouds : List String -> List String -> Page.LoginOpenstack.Model
modelWithClouds names selected =
    let
        model =
            Page.LoginOpenstack.init Nothing
    in
    { model
        | clouds = List.map cloud names
        , selectedClouds = Set.fromList selected
    }


cloudSelectionSuite : Test
cloudSelectionSuite =
    describe "picking which clouds to log in to"
        [ test "selects nothing when nothing is ticked" <|
            \() ->
                modelWithClouds [ "one", "two" ] []
                    |> Page.LoginOpenstack.selectedClouds
                    |> Expect.equal []
        , test "selects only the ticked clouds" <|
            \() ->
                modelWithClouds [ "one", "two", "three" ] [ "two" ]
                    |> Page.LoginOpenstack.selectedClouds
                    |> List.map .name
                    |> Expect.equal [ "two" ]
        , test "keeps the order of the file rather than the order of the selection" <|
            \() ->
                modelWithClouds [ "one", "two", "three" ] [ "three", "one" ]
                    |> Page.LoginOpenstack.selectedClouds
                    |> List.map .name
                    |> Expect.equal [ "one", "three" ]
        , test "carries the application credential of each selected cloud" <|
            \() ->
                modelWithClouds [ "one", "two" ] [ "one", "two" ]
                    |> Page.LoginOpenstack.selectedClouds
                    |> List.map (.appCredential >> .uuid)
                    |> Expect.equal [ "one-id", "two-id" ]
        , test "ignores a selected name that is not in the file" <|
            \() ->
                modelWithClouds [ "one" ] [ "one", "ghost" ]
                    |> Page.LoginOpenstack.selectedClouds
                    |> List.map .name
                    |> Expect.equal [ "one" ]
        ]
