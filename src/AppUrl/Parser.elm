module AppUrl.Parser exposing (urlToViewState)

-- TODO the Types.SharedMsg import is temporary. Perhaps in the future, this model should call the NavigateToView Msg instead of `init`ing pages directly?

import Dict
import OpenStack.Types as OSTypes
import Page.AllResourcesList
import Page.FloatingIpAssign
import Page.FloatingIpList
import Page.GetSupport
import Page.ImageList
import Page.KeypairCreate
import Page.KeypairList
import Page.LoginOpenstack
import Page.ServerCreate
import Page.ServerCreateImage
import Page.ServerDetail
import Page.ServerList
import Page.VolumeAttach
import Page.VolumeCreate
import Page.VolumeDetail
import Page.VolumeList
import Types.HelperTypes exposing (JetstreamCreds, JetstreamProvider(..))
import Types.SharedMsg as SharedMsg
import Types.View
    exposing
        ( LoginView(..)
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


urlToViewState : Maybe String -> ViewState -> Url.Url -> Maybe ( ViewState, Cmd SharedMsg.SharedMsg )
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


pathParsers : ViewState -> List (Parser (( ViewState, Cmd SharedMsg.SharedMsg ) -> b) b)
pathParsers defaultViewState =
    [ -- Non-project-specific views
      map ( defaultViewState, Cmd.none ) top
    , map
        (\creds ->
            let
                init =
                    Page.LoginOpenstack.init
            in
            ( NonProjectView <| Login <| LoginOpenstack <| { init | creds = creds }, Cmd.none )
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
        (\creds -> ( NonProjectView <| Login <| LoginJetstream creds, Cmd.none ))
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
        ( NonProjectView LoginPicker, Cmd.none )
        (s "loginpicker")
    , map
        (\maybeTokenValue ->
            case maybeTokenValue of
                Just tokenValue ->
                    ( NonProjectView <| LoadingUnscopedProjects tokenValue, Cmd.none )

                Nothing ->
                    ( NonProjectView PageNotFound, Cmd.none )
        )
        (s "auth" </> s "oidc-login" <?> Query.string "token")

    -- Not bothering to decode the SelectProjects view, because you can't currently navigate there on a fresh page load and see anything useful
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
            ( NonProjectView <| MessageLog { showDebugMsgs = showDebugMsgs }, Cmd.none )
        )
        (s "msglog" <?> Query.string "showdebug")
    , map
        ( NonProjectView Settings, Cmd.none )
        (s "settings")
    , map
        (let
            ( pageModel, cmd ) =
                Page.GetSupport.init Nothing
         in
         ( NonProjectView <| GetSupport pageModel, cmd )
        )
        (s "getsupport")
    , map
        ( NonProjectView HelpAbout, Cmd.none )
        (s "helpabout")
    , map
        ( NonProjectView PageNotFound, Cmd.none )
        (s "pagenotfound")
    , map
        (\uuid projectViewConstructor -> ( ProjectView uuid { createPopup = False } <| projectViewConstructor, Cmd.none ))
        (s "projects" </> string </> oneOf projectViewConstructorParsers)
    ]


projectViewConstructorParsers : List (Parser (ProjectViewConstructor -> b) b)
projectViewConstructorParsers =
    [ map
        (ImageList Page.ImageList.init)
        (s "images")
    , map
        (AllResourcesList Page.AllResourcesList.init)
        (s "resources")
    , map
        (\svrUuid imageName ->
            ServerCreateImage (Page.ServerCreateImage.init svrUuid (Just imageName))
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
            ServerDetail (Page.ServerDetail.init svrUuid)
        )
        (s "servers" </> string)
    , map
        (ServerList <| Page.ServerList.init True)
        (s "servers")
    , map
        (\volUuid ->
            VolumeDetail (Page.VolumeDetail.init True volUuid)
        )
        (s "volumes" </> string)
    , map
        (VolumeList <| Page.VolumeList.init True)
        (s "volumes")
    , map
        (FloatingIpList <| Page.FloatingIpList.init True)
        (s "floatingips")
    , map
        (FloatingIpAssign <| Page.FloatingIpAssign.init Nothing Nothing)
        (s "assignfloatingip")
    , map
        (KeypairList <| Page.KeypairList.init True)
        (s "keypairs")
    , map
        (KeypairCreate Page.KeypairCreate.init)
        (s "uploadkeypair")
    , map
        ServerCreate
        (let
            maybeBoolEnumDict =
                Dict.fromList
                    [ ( "justtrue", Just True )
                    , ( "justfalse", Just False )
                    , ( "nothing", Nothing )
                    ]

            queryParser =
                Query.map3
                    Page.ServerCreate.init
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
        (VolumeCreate Page.VolumeCreate.init)
        (s "createvolume")
    , map
        (\( maybeServerUuid, maybeVolUuid ) ->
            VolumeAttach (Page.VolumeAttach.init maybeServerUuid maybeVolUuid)
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
