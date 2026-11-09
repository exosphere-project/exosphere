module Tests.State.Navigation exposing (navigationSuite)

import Dict
import Expect
import Page.FloatingIpAssign
import Page.LoginOpenstack
import Page.MessageLog
import Page.SelectProjectRegions
import Page.ServerCreateImage
import Page.VolumeAttach
import Route
import Set
import State.Navigation exposing (Navigation(..))
import Style.Widgets.DataList as DataList
import Test exposing (Test, describe, test)
import Types.HelperTypes as HelperTypes
import Types.View exposing (LoginView(..), NonProjectViewConstructor(..), ProjectViewConstructor(..), ViewState(..))
import Url


navigationSuite : Test
navigationSuite =
    describe "classifying a URL change against the current page"
        [ echoSuite
        , echoThroughUrlSuite
        , newPageSuite
        , credentialFileChainSuite
        ]



-- Fixtures


keystoneUrl : String
keystoneUrl =
    "https://keystone.example:5000/v3"


projectIdentifier : HelperTypes.ProjectIdentifier
projectIdentifier =
    { projectUuid = "proj-1", regionId = Just "RegionOne" }


projectRoute : Route.ProjectRouteConstructor -> Route.Route
projectRoute =
    Route.ProjectRoute projectIdentifier


volumeAttach : ViewState
volumeAttach =
    ProjectView projectIdentifier (VolumeAttach (Page.VolumeAttach.init (Just "srv-1") (Just "vol-1")))


floatingIpAssign : ViewState
floatingIpAssign =
    ProjectView projectIdentifier (FloatingIpAssign (Page.FloatingIpAssign.init (Just "ip-1") (Just "srv-1")))


serverCreateImage : ViewState
serverCreateImage =
    ProjectView projectIdentifier (ServerCreateImage (Page.ServerCreateImage.init "srv-1" (Just "nightly")))


{-| The server list's own filters need a view context to build, so these stand in for them.
-}
serverListDefaults : DataList.SelectedFilterOptions
serverListDefaults =
    DataList.getDefaultFilterOptions
        [ { id = "creator"
          , label = "Creator"
          , chipPrefix = "Created by "
          , filterOptions = \_ -> Dict.empty
          , filterTypeAndDefaultValue = DataList.MultiselectOption (Set.singleton "alice")
          , onFilter = \_ _ -> True
          }
        , { id = "status"
          , label = "Status of"
          , chipPrefix = "Status of "
          , filterOptions = \_ -> Dict.empty
          , filterTypeAndDefaultValue = DataList.MultiselectOption Set.empty
          , onFilter = \_ _ -> True
          }
        ]


serverList : HelperTypes.ListQuery -> ViewState
serverList listQuery =
    ProjectView projectIdentifier
        (ServerList
            { showHeading = True
            , dataListModel = DataList.initWithQuery serverListDefaults listQuery
            , defaultFilterOptions = serverListDefaults
            , retainFloatingIpsWhenDeleting = False
            , deleteFloatingIpsWhenShelving = True
            , selectedBulkAction = Nothing
            }
        )


filteredServers : HelperTypes.ListQuery
filteredServers =
    { search = "web a+b é"
    , filters = Dict.fromList [ ( "creator", [ "" ] ), ( "status", [ "Active", "Shelved" ] ) ]
    }


messageLog : ViewState
messageLog =
    NonProjectView (MessageLog (Page.MessageLog.init (Just True)))


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


echoSuite : Test
echoSuite =
    let
        testCases =
            [ ( "Attach Volume", volumeAttach, projectRoute (Route.VolumeAttach (Just "srv-1") (Just "vol-1")) )
            , ( "Assign Floating IP", floatingIpAssign, projectRoute (Route.FloatingIpAssign (Just "ip-1") (Just "srv-1")) )
            , ( "Create Image", serverCreateImage, projectRoute (Route.ServerCreateImage "srv-1" (Just "nightly")) )
            , ( "message log", messageLog, Route.MessageLog True )
            ]
    in
    describe "a page that already shows what the URL says stays as it is"
        (List.concatMap
            (\( description, viewState, route ) ->
                [ test description <|
                    \_ -> Expect.equal SamePage (classify viewState route)
                , test (description ++ " during a credential-file login chain") <|
                    \_ -> Expect.equal SamePage (classifyDuringChain viewState route)
                ]
            )
            testCases
        )


{-| A page's own `Route.replaceUrl` reaches `classify` only after `Route.toUrl` and `Route.fromUrl`,
so the echo is recognised only if the page's route survives that round trip.
-}
echoThroughUrlSuite : Test
echoThroughUrlSuite =
    let
        testCases =
            [ ( "Attach Volume", volumeAttach )
            , ( "Attach Volume with nothing chosen", ProjectView projectIdentifier (VolumeAttach (Page.VolumeAttach.init Nothing Nothing)) )
            , ( "Assign Floating IP", floatingIpAssign )
            , ( "Create Image", serverCreateImage )
            , ( "Create Image with an empty name", ProjectView projectIdentifier (ServerCreateImage (Page.ServerCreateImage.init "srv-1" (Just ""))) )
            , ( "Create Image with a name that needs escaping", ProjectView projectIdentifier (ServerCreateImage (Page.ServerCreateImage.init "srv-1" (Just "a+b & c=d"))) )
            , ( "message log", messageLog )
            , ( "server list with default filters", serverList Route.emptyListQuery )
            , ( "server list with search and filters", serverList filteredServers )
            , ( "message log hiding debug messages", NonProjectView (MessageLog (Page.MessageLog.init (Just False))) )
            ]

        throughUrl : Maybe String -> Route.Route -> Maybe Route.Route
        throughUrl maybePathPrefix route =
            Url.fromString ("http://example.com" ++ Route.toUrl maybePathPrefix route)
                |> Maybe.map (Route.fromUrl maybePathPrefix Route.PageNotFound)
    in
    describe "a page's own URL change is recognised after the URL round trip"
        (List.concatMap
            (\( description, viewState ) ->
                List.map
                    (\maybePathPrefix ->
                        test (description ++ Maybe.withDefault "" (Maybe.map (\prefix -> " under /" ++ prefix) maybePathPrefix)) <|
                            \_ ->
                                case State.Navigation.viewStateToRoute viewState of
                                    Just route ->
                                        throughUrl maybePathPrefix route
                                            |> Maybe.map (classify viewState)
                                            |> Expect.equal (Just SamePage)

                                    Nothing ->
                                        Expect.fail "expected the page to describe itself as a route"
                    )
                    [ Nothing, Just "exosphere" ]
            )
            testCases
        )


newPageSuite : Test
newPageSuite =
    let
        testCases =
            [ ( "a different volume", volumeAttach, projectRoute (Route.VolumeAttach (Just "srv-1") (Just "vol-2")) )
            , ( "a cleared server", volumeAttach, projectRoute (Route.VolumeAttach Nothing (Just "vol-1")) )
            , ( "a different image name", serverCreateImage, projectRoute (Route.ServerCreateImage "srv-1" (Just "weekly")) )
            , ( "a different server's Create Image", serverCreateImage, projectRoute (Route.ServerCreateImage "srv-2" (Just "nightly")) )
            , ( "the other debug-message setting", messageLog, Route.MessageLog False )
            , ( "the same page in another region"
              , volumeAttach
              , Route.ProjectRoute { projectIdentifier | regionId = Just "RegionTwo" } (Route.VolumeAttach (Just "srv-1") (Just "vol-1"))
              )
            , ( "the same page without a region"
              , volumeAttach
              , Route.ProjectRoute { projectIdentifier | regionId = Nothing } (Route.VolumeAttach (Just "srv-1") (Just "vol-1"))
              )
            , ( "the same page in another project"
              , volumeAttach
              , Route.ProjectRoute { projectIdentifier | projectUuid = "proj-2" } (Route.VolumeAttach (Just "srv-1") (Just "vol-1"))
              )
            , ( "another page", volumeAttach, projectRoute Route.VolumeList )
            , ( "different server list filters", serverList filteredServers, projectRoute (Route.ServerList Route.emptyListQuery) )
            , ( "a page that has not opted in, at its own URL", NonProjectView HelpAbout, Route.HelpAbout )
            , ( "the start-up placeholder at its own URL", NonProjectView LoginPicker, Route.LoginPicker )
            , ( "OpenStack login at its own URL"
              , NonProjectView (Login (LoginOpenstack (Page.LoginOpenstack.init Nothing)))
              , Route.LoginOpenstack (Just { authUrl = "", userDomain = "", username = "", password = "" })
              )
            , ( "Back to an earlier volume choice", volumeAttach, projectRoute (Route.VolumeAttach (Just "srv-1") (Just "vol-0")) )
            ]
    in
    describe "any other URL change rebuilds the page"
        (List.map
            (\( description, viewState, route ) ->
                test description <|
                    \_ -> Expect.equal NewPage (classify viewState route)
            )
            testCases
        )


credentialFileChainSuite : Test
credentialFileChainSuite =
    describe "a credential-file login chain waiting on region selection"
        [ test "is abandoned when navigation leaves region selection" <|
            \_ ->
                Expect.equal NewPageAbandoningCredentialFileChain
                    (classifyDuringChain (selectingRegions "proj-1") Route.Home)
        , test "is abandoned even when the destination is a page that would recognise its own URL" <|
            \_ ->
                Expect.equal NewPageAbandoningCredentialFileChain
                    (classifyDuringChain (selectingRegions "proj-1") (Route.MessageLog True))
        , test "survives moving on to the next project's region question" <|
            \_ ->
                Expect.equal NewPage
                    (classifyDuringChain (selectingRegions "proj-1") (Route.SelectProjectRegions keystoneUrl "proj-2"))
        , test "survives arriving at region selection" <|
            \_ ->
                Expect.equal NewPage
                    (classifyDuringChain volumeAttach (Route.SelectProjectRegions keystoneUrl "proj-1"))
        , test "is not abandoned when no chain is active" <|
            \_ ->
                Expect.equal NewPage
                    (classify (selectingRegions "proj-1") Route.Home)
        , test "region selection rebuilds even for its own URL" <|
            \_ ->
                Expect.equal NewPage
                    (classifyDuringChain (selectingRegions "proj-1") (Route.SelectProjectRegions keystoneUrl "proj-1"))
        ]
