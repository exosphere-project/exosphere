module Tests.State.Navigation exposing (navigationSuite)

import Expect
import Page.SelectProjectRegions
import Route
import State.Navigation exposing (Navigation(..))
import Test exposing (Test, describe, test)
import Types.View exposing (NonProjectViewConstructor(..), ViewState(..))


navigationSuite : Test
navigationSuite =
    describe "classifying a URL change against the current page"
        [ credentialFileChainSuite
        ]



-- Fixtures


keystoneUrl : String
keystoneUrl =
    "https://keystone.example:5000/v3"


selectingRegions : String -> ViewState
selectingRegions projectUuid =
    NonProjectView (SelectProjectRegions (Page.SelectProjectRegions.init keystoneUrl projectUuid))


classify : ViewState -> Route.Route -> Navigation
classify =
    State.Navigation.classify { credentialFileLoginChainIsActive = False }


classifyDuringChain : ViewState -> Route.Route -> Navigation
classifyDuringChain =
    State.Navigation.classify { credentialFileLoginChainIsActive = True }



-- Suites


credentialFileChainSuite : Test
credentialFileChainSuite =
    describe "a credential-file login chain waiting on region selection"
        [ test "is abandoned when navigation leaves region selection" <|
            \_ ->
                Expect.equal NewPageAbandoningCredentialFileChain
                    (classifyDuringChain (selectingRegions "proj-1") Route.Home)
        , test "survives moving on to the next project's region question" <|
            \_ ->
                Expect.equal NewPage
                    (classifyDuringChain (selectingRegions "proj-1") (Route.SelectProjectRegions keystoneUrl "proj-2"))
        , test "survives arriving at region selection" <|
            \_ ->
                Expect.equal NewPage
                    (classifyDuringChain (NonProjectView HelpAbout) (Route.SelectProjectRegions keystoneUrl "proj-1"))
        , test "is not abandoned when no chain is active" <|
            \_ ->
                Expect.equal NewPage
                    (classify (selectingRegions "proj-1") Route.Home)
        ]
