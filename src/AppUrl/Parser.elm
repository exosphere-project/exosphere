module AppUrl.Parser exposing (urlToViewState)

import Dict
import OpenStack.Types as OSTypes
import Types.Defaults as Defaults
import Types.Types
    exposing
        ( JetstreamCreds
        , JetstreamProvider(..)
        , LoginView(..)
        , NonProjectViewConstructor(..)
        , ProjectViewConstructor(..)
        , ViewState(..)
        )
import Url
import Url.Parser
    exposing
        ( (</>)
        , (<?>)
        , Parser
        , map
        , oneOf
        , parse
        , s
        , string
        , top
        )
import Url.Parser.Query as Query


urlToViewState : Maybe String -> ViewState -> Url.Url -> Maybe ViewState
urlToViewState maybePathPrefix defaultViewState url =
    case maybePathPrefix of
        Nothing ->
            parse
                (oneOf
                    (pathParsers defaultViewState)
                )
                url

        Just pathPrefix ->
            parse
                (s
                    pathPrefix
                    </> oneOf
                            (pathParsers defaultViewState)
                )
                url


pathParsers : ViewState -> List (Parser (ViewState -> b) b)
pathParsers defaultViewState =
    [ -- Non-project-specific views
      map defaultViewState top
    , map
        (\creds -> NonProjectView <| Login <| LoginOpenstack creds)
        (let
            queryParser =
                Query.map6
                    OSTypes.OpenstackLogin
                    (Query.string "authurl"
                        |> Query.map (Maybe.withDefault "")
                    )
                    (Query.string "pdomain"
                        |> Query.map (Maybe.withDefault "")
                    )
                    (Query.string "pname"
                        |> Query.map (Maybe.withDefault "")
                    )
                    (Query.string "udomain"
                        |> Query.map (Maybe.withDefault "")
                    )
                    (Query.string "uname"
                        |> Query.map (Maybe.withDefault "")
                    )
                    -- This parses into a blank password, ugly I know
                    (Query.string ""
                        |> Query.map (\_ -> "")
                    )
         in
         s "login" </> s "openstack" <?> queryParser
        )
    , map
        (\creds -> NonProjectView <| Login <| LoginJetstream creds)
        (let
            providerEnumDict =
                Dict.fromList
                    [ ( "iu", IUCloud )
                    , ( "tacc", TACCCloud )
                    , ( "both", BothJetstreamClouds )
                    ]

            queryParser =
                Query.map4
                    JetstreamCreds
                    (Query.enum "provider" providerEnumDict
                        |> Query.map (Maybe.withDefault BothJetstreamClouds)
                    )
                    (Query.string "pname"
                        |> Query.map (Maybe.withDefault "")
                    )
                    (Query.string "taccuname"
                        |> Query.map (Maybe.withDefault "")
                    )
                    -- This parses into a blank password, ugly I know
                    (Query.string ""
                        |> Query.map (\_ -> "")
                    )
         in
         s "login" </> s "jetstream" <?> queryParser
        )
    , map
        (NonProjectView LoginPicker)
        (s "loginpicker")
    , map
        (\maybeTokenValue ->
            case maybeTokenValue of
                Just tokenValue ->
                    NonProjectView <| LoadingUnscopedProjects tokenValue

                Nothing ->
                    NonProjectView PageNotFound
        )
        (s "auth" </> s "oidc-login" <?> Query.string "token")

    -- Not bothering to decode the SelectProjects view, because you can't currently navigate there on a fresh page load and see anything useful
    , map
        (NonProjectView MessageLog)
        (s "msglog")
    , map
        (NonProjectView Settings)
        (s "settings")
    , map
        (NonProjectView <| GetSupport Nothing "" False)
        (s "getsupport")
    , map
        (NonProjectView HelpAbout)
        (s "helpabout")
    , map
        (NonProjectView PageNotFound)
        (s "pagenotfound")
    , map
        (\uuid projectViewConstructor -> ProjectView uuid { createPopup = False } <| projectViewConstructor)
        (s "projects" </> string </> oneOf projectViewConstructorParsers)
    ]


projectViewConstructorParsers : List (Parser (ProjectViewConstructor -> b) b)
projectViewConstructorParsers =
    [ map
        (ListImages Defaults.imageListViewParams Defaults.sortTableParams)
        (s "images")
    , map
        (AllResources Defaults.allResourcesListViewParams)
        (s "resources")
    , map
        (\svrUuid imageName ->
            CreateServerImage svrUuid imageName
        )
        (let
            queryParser =
                Query.string "name"
                    |> Query.map (Maybe.withDefault "")
         in
         s "servers" </> string </> s "image" <?> queryParser
        )
    , map
        (\svrUuid ->
            ServerDetail svrUuid Defaults.serverDetailViewParams
        )
        (s "servers" </> string)
    , map
        (ListProjectServers Defaults.serverListViewParams)
        (s "servers")
    , map
        (\volUuid ->
            VolumeDetail volUuid []
        )
        (s "volumes" </> string)
    , map
        (ListProjectVolumes Defaults.volumeListViewParams)
        (s "volumes")
    , map
        (ListKeypairs Defaults.keypairListViewParams)
        (s "keypairs")
    , map
        (CreateKeypair "" "")
        (s "uploadkeypair")
    , map
        ListQuotaUsage
        (s "quotausage")
    , map
        (\params ->
            CreateServer params
        )
        (let
            maybeBoolEnumDict =
                Dict.fromList
                    [ ( "justtrue", Just True )
                    , ( "justfalse", Just False )
                    , ( "nothing", Nothing )
                    ]

            queryParser =
                Query.map3
                    Defaults.createServerViewParams
                    (Query.string "imageuuid"
                        |> Query.map (Maybe.withDefault "")
                    )
                    (Query.string "imagename"
                        |> Query.map (Maybe.withDefault "")
                    )
                    (Query.enum "deployguac" maybeBoolEnumDict
                        |> Query.map (Maybe.withDefault Nothing)
                    )
         in
         s "createserver" <?> queryParser
        )
    , map
        Defaults.createVolumeView
        (s "createvolume")
    , map
        (\( maybeServerUuid, maybeVolUuid ) ->
            AttachVolumeModal maybeServerUuid maybeVolUuid
        )
        (let
            queryParser =
                Query.map2
                    Tuple.pair
                    (Query.string "serveruuid")
                    (Query.string "voluuid")
         in
         s "attachvol" <?> queryParser
        )
    , map
        (\attachment ->
            MountVolInstructions attachment
        )
        (let
            queryParser =
                Query.map3
                    OSTypes.VolumeAttachment
                    (Query.string "serveruuid"
                        |> Query.map (Maybe.withDefault "")
                    )
                    (Query.string "attachmentuuid"
                        |> Query.map (Maybe.withDefault "")
                    )
                    (Query.string "device"
                        |> Query.map (Maybe.withDefault "")
                    )
         in
         s "attachvolinstructions" <?> queryParser
        )
    ]
