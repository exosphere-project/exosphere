module Route exposing (ProjectRouteConstructor(..), Route(..), replaceUrl, routeToUrl, urlToRoute)

import Browser.Navigation
import Dict
import OpenStack.Types as OSTypes
import Types.HelperTypes as HelperTypes
import Url
import Url.Builder as UB
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


type Route
    = GetSupport (Maybe ( HelperTypes.SupportableItemType, Maybe HelperTypes.Uuid ))
    | HelpAbout
    | LoadingUnscopedProjects OSTypes.AuthTokenString
    | LoginJetstream (Maybe HelperTypes.JetstreamCreds)
    | LoginOpenstack (Maybe OSTypes.OpenstackLogin)
    | LoginPicker
    | MessageLog Bool
    | PageNotFound
    | ProjectRoute HelperTypes.ProjectIdentifier ProjectRouteConstructor
    | SelectProjects OSTypes.KeystoneUrl
    | Settings


type ProjectRouteConstructor
    = AllResourcesList
    | FloatingIpAssign (Maybe OSTypes.IpAddressUuid) (Maybe OSTypes.ServerUuid)
    | FloatingIpList
    | ImageList
    | KeypairCreate
    | KeypairList
    | ServerCreate OSTypes.ImageUuid String (Maybe Bool)
    | ServerCreateImage OSTypes.ServerUuid (Maybe String)
    | ServerDetail OSTypes.ServerUuid
    | ServerList
    | VolumeAttach (Maybe OSTypes.ServerUuid) (Maybe OSTypes.VolumeUuid)
    | VolumeCreate
    | VolumeDetail OSTypes.VolumeUuid
    | VolumeList
    | VolumeMountInstructions OSTypes.VolumeAttachment


routeToUrl : Maybe String -> Route -> String
routeToUrl maybePathPrefix route =
    let
        buildUrlFunc =
            buildPrefixedUrl maybePathPrefix
    in
    case route of
        GetSupport _ ->
            buildUrlFunc
                [ "getsupport" ]
                []

        HelpAbout ->
            buildUrlFunc
                [ "helpabout" ]
                []

        LoadingUnscopedProjects _ ->
            buildUrlFunc
                [ "loadingprojs"
                ]
                []

        LoginJetstream _ ->
            buildUrlFunc
                [ "login"
                , "jetstream"
                ]
                []

        LoginOpenstack _ ->
            buildUrlFunc
                [ "login"
                , "openstack"
                ]
                []

        LoginPicker ->
            buildUrlFunc
                [ "loginpicker" ]
                []

        MessageLog showDebugMsgs ->
            buildUrlFunc
                [ "msglog" ]
                [ UB.string "showdebug"
                    (if showDebugMsgs then
                        "true"

                     else
                        "false"
                    )
                ]

        PageNotFound ->
            buildUrlFunc
                [ "pagenotfound" ]
                []

        ProjectRoute projectIdentifier projectRouteConstructor ->
            let
                projectIdentifierPath =
                    [ "projects"
                    , projectIdentifier
                    ]

                ( projectSpecificPath, projectSpecificQuery ) =
                    case projectRouteConstructor of
                        AllResourcesList ->
                            ( [ "resources" ]
                            , []
                            )

                        FloatingIpAssign maybeIpUuid maybeServerUuid ->
                            let
                                ipUuidQP =
                                    case maybeIpUuid of
                                        Just ipUuid ->
                                            [ UB.string "ipuuid" ipUuid ]

                                        Nothing ->
                                            []

                                serverUuidQP =
                                    case maybeServerUuid of
                                        Just serverUuid ->
                                            [ UB.string "serveruuid" serverUuid ]

                                        Nothing ->
                                            []
                            in
                            ( [ "assignfloatingip" ]
                            , List.concat [ ipUuidQP, serverUuidQP ]
                            )

                        FloatingIpList ->
                            ( [ "floatingips" ]
                            , []
                            )

                        ImageList ->
                            ( [ "images" ]
                            , []
                            )

                        KeypairCreate ->
                            ( [ "uploadkeypair" ]
                            , []
                            )

                        KeypairList ->
                            ( [ "keypairs" ]
                            , []
                            )

                        ServerCreate imageUuid imageName maybeDeployGuac ->
                            ( [ "createserver"
                              ]
                            , [ UB.string "imageuuid" imageUuid
                              , UB.string "imagename" imageName
                              , UB.string "deployguac"
                                    (case maybeDeployGuac of
                                        Just bool ->
                                            if bool then
                                                "justtrue"

                                            else
                                                "justfalse"

                                        Nothing ->
                                            "nothing"
                                    )
                              ]
                            )

                        ServerCreateImage serverUuid maybeImageName ->
                            ( [ "servers"
                              , serverUuid
                              , "image"
                              ]
                            , case maybeImageName of
                                Just imageName ->
                                    [ UB.string "name" imageName
                                    ]

                                Nothing ->
                                    []
                            )

                        ServerDetail serverUuid ->
                            ( [ "servers"
                              , serverUuid
                              ]
                            , []
                            )

                        ServerList ->
                            ( [ "servers" ]
                            , []
                            )

                        VolumeAttach maybeServerUuid maybeVolumeUuid ->
                            let
                                volUuidQP =
                                    case maybeVolumeUuid of
                                        Just volUuid ->
                                            [ UB.string "voluuid" volUuid ]

                                        Nothing ->
                                            []

                                serverUuidQP =
                                    case maybeServerUuid of
                                        Just serverUuid ->
                                            [ UB.string "serveruuid" serverUuid ]

                                        Nothing ->
                                            []
                            in
                            ( [ "attachvol"
                              ]
                            , List.concat [ volUuidQP, serverUuidQP ]
                            )

                        VolumeCreate ->
                            ( [ "createvolume"
                              ]
                            , []
                            )

                        VolumeDetail volumeUuid ->
                            ( [ "volumes"
                              , volumeUuid
                              ]
                            , []
                            )

                        VolumeList ->
                            ( [ "volumes" ]
                            , []
                            )

                        VolumeMountInstructions attachment ->
                            ( [ "attachvolinstructions" ]
                            , [ UB.string "serveruuid" attachment.serverUuid
                              , UB.string "attachmentuuid" attachment.attachmentUuid
                              , UB.string "device" attachment.device
                              ]
                            )
            in
            buildUrlFunc (projectIdentifierPath ++ projectSpecificPath) projectSpecificQuery

        SelectProjects keystoneUrl ->
            buildUrlFunc
                [ "selectprojs"
                ]
                [ UB.string "keystoneurl" keystoneUrl
                ]

        Settings ->
            buildUrlFunc
                [ "settings" ]
                []


buildPrefixedUrl : Maybe String -> List String -> List UB.QueryParameter -> String
buildPrefixedUrl maybePathPrefix pathParts queryParams =
    let
        prefixedPathParts =
            case maybePathPrefix of
                Just pathPrefix ->
                    pathPrefix :: pathParts

                Nothing ->
                    pathParts
    in
    UB.absolute prefixedPathParts queryParams


replaceUrl : Browser.Navigation.Key -> Maybe String -> Route -> Cmd msg
replaceUrl key maybePathPrefix route =
    Browser.Navigation.replaceUrl key (routeToUrl maybePathPrefix route)


urlToRoute : Maybe String -> Route -> Url.Url -> Maybe Route
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


pathParsers : Route -> List (Parser (Route -> b) b)
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
                    [ ( "iu", HelperTypes.IUCloud )
                    , ( "tacc", HelperTypes.TACCCloud )
                    , ( "both", HelperTypes.BothJetstreamClouds )
                    ]

            queryParser =
                Query.map3
                    HelperTypes.JetstreamCreds
                    (Query.enum "provider" providerEnumDict
                        |> Query.map (Maybe.withDefault HelperTypes.BothJetstreamClouds)
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
        (\uuid projectRoute -> ProjectRoute uuid <| projectRoute)
        (s "projects" </> string </> oneOf projectRouteParsers)
    ]


projectRouteParsers : List (Parser (ProjectRouteConstructor -> b) b)
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
        (\( maybeIpUuid, maybeServerUuid ) -> FloatingIpAssign maybeIpUuid maybeServerUuid)
        (let
            queryparser =
                Query.map2
                    Tuple.pair
                    (Query.string "ipuuid")
                    (Query.string "serveruuid")
         in
         s "assignfloatingip" <?> queryparser
        )
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
