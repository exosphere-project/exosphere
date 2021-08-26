module AppUrl.Parser exposing (urlToRoute)

import Dict
import OpenStack.Types as OSTypes
import Route exposing (NavigablePage(..), NavigableProjectPage(..))
import Types.HelperTypes exposing (JetstreamCreds, JetstreamProvider(..))
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


urlToRoute : Maybe String -> NavigablePage -> Url.Url -> Maybe NavigablePage
urlToRoute maybePathPrefix defaultRoute url =
    case maybePathPrefix of
        Nothing ->
            parse
                (oneOf
                    (pathParsers defaultRoute)
                )
                url

        Just pathPrefix ->
            parse
                (s
                    pathPrefix
                    </> oneOf
                            (pathParsers defaultRoute)
                )
                url


pathParsers : NavigablePage -> List (Parser (NavigablePage -> b) b)
pathParsers defaultRoute =
    [ -- Non-project-specific pages
      map defaultRoute top
    , map
        (\creds ->
            LoginOpenstack (Just creds)
        )
        (let
            queryParser =
                Query.map4
                    OSTypes.OpenstackLogin
                    (Query.string "authurl"
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
        (\creds -> LoginJetstream (Just creds))
        (let
            providerEnumDict =
                Dict.fromList
                    [ ( "iu", IUCloud )
                    , ( "tacc", TACCCloud )
                    , ( "both", BothJetstreamClouds )
                    ]

            queryParser =
                Query.map3
                    JetstreamCreds
                    (Query.enum "provider" providerEnumDict
                        |> Query.map (Maybe.withDefault BothJetstreamClouds)
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
        LoginPicker
        (s "loginpicker")
    , map
        (\maybeTokenValue ->
            case maybeTokenValue of
                Just tokenValue ->
                    LoadingUnscopedProjects tokenValue

                Nothing ->
                    PageNotFound
        )
        (s "auth" </> s "oidc-login" <?> Query.string "token")

    -- Not bothering to decode the SelectProjects page, because you can't currently navigate there on a fresh page load and see anything useful
    , map
        (\maybeShowDebugMsgs ->
            let
                showDebugMsgs =
                    case maybeShowDebugMsgs of
                        Just str ->
                            case str of
                                "true" ->
                                    True

                                "false" ->
                                    False

                                _ ->
                                    False

                        Nothing ->
                            False
            in
            MessageLog showDebugMsgs
        )
        (s "msglog" <?> Query.string "showdebug")
    , map
        Settings
        (s "settings")
    , map
        (GetSupport Nothing)
        (s "getsupport")
    , map
        HelpAbout
        (s "helpabout")
    , map
        PageNotFound
        (s "pagenotfound")
    , map
        (\uuid projectRoute -> ProjectPage uuid <| projectRoute)
        (s "projects" </> string </> oneOf projectRouteParsers)
    ]


projectRouteParsers : List (Parser (NavigableProjectPage -> b) b)
projectRouteParsers =
    [ map
        ImageList
        (s "images")
    , map
        AllResourcesList
        (s "resources")
    , map
        (\svrUuid imageName ->
            ServerCreateImage svrUuid (Just imageName)
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
            ServerDetail svrUuid
        )
        (s "servers" </> string)
    , map
        ServerList
        (s "servers")
    , map
        (\volUuid ->
            VolumeDetail volUuid
        )
        (s "volumes" </> string)
    , map
        VolumeList
        (s "volumes")
    , map
        FloatingIpList
        (s "floatingips")
    , map
        (FloatingIpAssign Nothing Nothing)
        (s "assignfloatingip")
    , map
        KeypairList
        (s "keypairs")
    , map
        KeypairCreate
        (s "uploadkeypair")
    , map
        identity
        (let
            maybeBoolEnumDict =
                Dict.fromList
                    [ ( "justtrue", Just True )
                    , ( "justfalse", Just False )
                    , ( "nothing", Nothing )
                    ]

            queryParser =
                Query.map3
                    ServerCreate
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
        VolumeCreate
        (s "createvolume")
    , map
        (\( maybeServerUuid, maybeVolUuid ) ->
            VolumeAttach maybeServerUuid maybeVolUuid
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
            VolumeMountInstructions attachment
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
