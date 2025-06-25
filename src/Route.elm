module Route exposing
    ( ProjectRouteConstructor(..)
    , Route(..)
    , defaultLoginPage
    , defaultRoute
    , fromUrl
    , pushUrl
    , replaceUrl
    , toUrl
    , withReplaceUrl
    )

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
    | Home
    | LoadingUnscopedProjects OSTypes.AuthTokenString
    | LoginOpenstack (Maybe OSTypes.OpenstackLogin)
    | LoginOpenIdConnect
    | LoginPicker
    | MessageLog Bool
    | PageNotFound
    | ProjectRoute HelperTypes.ProjectIdentifier ProjectRouteConstructor
    | SelectProjectRegions OSTypes.KeystoneUrl OSTypes.ProjectUuid
    | SelectProjects OSTypes.KeystoneUrl
    | Settings


type ProjectRouteConstructor
    = ProjectOverview
    | FloatingIpAssign (Maybe OSTypes.IpAddressUuid) (Maybe OSTypes.ServerUuid)
    | FloatingIpList
    | ImageList
    | InstanceSourcePicker
    | FloatingIpCreate (Maybe OSTypes.ServerUuid)
    | KeypairCreate
    | KeypairList
    | SecurityGroupDetail OSTypes.SecurityGroupUuid
    | SecurityGroupList
    | ServerCreate OSTypes.ImageUuid String (Maybe (List OSTypes.FlavorId)) (Maybe Bool)
    | ServerCreateImage OSTypes.ServerUuid (Maybe String)
    | ServerDetail OSTypes.ServerUuid
    | ServerList
    | ServerResize OSTypes.ServerUuid
    | ServerSecurityGroups OSTypes.ServerUuid
    | ShareCreate
    | ShareList
    | ShareDetail OSTypes.ShareUuid
    | VolumeAttach (Maybe OSTypes.ServerUuid) (Maybe OSTypes.VolumeUuid)
    | VolumeCreate
    | VolumeDetail OSTypes.VolumeUuid
    | VolumeList
    | VolumeMountInstructions OSTypes.VolumeAttachment
    | Credentials


{-| Generates a url to the given route (including the project's path prefix if the app lives in a subdirectory).

    e.g. https://try.exosphere.app/exosphere/helpabout

-}
toUrl : Maybe String -> Route -> String
toUrl maybePathPrefix route =
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

        Home ->
            buildUrlFunc
                [ "home" ]
                []

        LoadingUnscopedProjects _ ->
            buildUrlFunc
                [ "loadingprojs"
                ]
                []

        LoginOpenstack _ ->
            buildUrlFunc
                [ "login"
                , "openstack"
                ]
                []

        LoginOpenIdConnect ->
            buildUrlFunc
                [ "login"
                , "openidconnect"
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
                    List.concat
                        [ [ "projects"
                          , projectIdentifier.projectUuid
                          ]
                        , case projectIdentifier.regionId of
                            Just regionId ->
                                [ "regions", regionId ]

                            Nothing ->
                                []
                        ]

                ( projectSpecificPath, projectSpecificQuery ) =
                    case projectRouteConstructor of
                        ProjectOverview ->
                            ( [ "overview" ]
                            , []
                            )

                        Credentials ->
                            ( [ "credentials" ]
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

                        InstanceSourcePicker ->
                            ( [ "instancesource" ]
                            , []
                            )

                        FloatingIpCreate maybeServerUuid ->
                            let
                                serverUuidQP =
                                    case maybeServerUuid of
                                        Just serverUuid ->
                                            [ UB.string "serveruuid" serverUuid ]

                                        Nothing ->
                                            []
                            in
                            ( [ "createfloatingip" ]
                            , serverUuidQP
                            )

                        KeypairCreate ->
                            ( [ "uploadkeypair" ]
                            , []
                            )

                        KeypairList ->
                            ( [ "keypairs" ]
                            , []
                            )

                        SecurityGroupDetail securityGroupUuid ->
                            ( [ "securitygroups"
                              , securityGroupUuid
                              ]
                            , []
                            )

                        SecurityGroupList ->
                            ( [ "securitygroups" ]
                            , []
                            )

                        ServerCreate imageUuid imageName restrictFlavorIds maybeDeployGuac ->
                            ( [ "createserver"
                              ]
                            , [ UB.string "imageuuid" imageUuid
                              , UB.string "imagename" imageName
                              , UB.string "restrictflavorids"
                                    (case restrictFlavorIds of
                                        Nothing ->
                                            "false"

                                        Just flavorIds ->
                                            -- OpenStack doesn't allow flavor IDs to contain the @ symbol,
                                            -- so use it to separate them in this query parameter value
                                            String.join "@" ("true" :: flavorIds)
                                    )
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

                        ServerResize serverUuid ->
                            ( [ "servers"
                              , serverUuid
                              , "resize"
                              ]
                            , []
                            )

                        ServerSecurityGroups serverUuid ->
                            ( [ "servers"
                              , serverUuid
                              , "securitygroups"
                              ]
                            , []
                            )

                        ShareCreate ->
                            ( [ "createshare" ]
                            , []
                            )

                        ShareDetail shareUuid ->
                            ( [ "shares"
                              , shareUuid
                              ]
                            , []
                            )

                        ShareList ->
                            ( [ "shares" ]
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
                            , [ UB.string "voluuid" attachment.volumeUuid
                              , UB.string "serveruuid" attachment.serverUuid
                              , UB.string "attachmentuuid" attachment.attachmentUuid
                              ]
                                ++ (case attachment.device of
                                        Just device ->
                                            [ UB.string "device" device ]

                                        Nothing ->
                                            []
                                   )
                            )
            in
            buildUrlFunc (projectIdentifierPath ++ projectSpecificPath) projectSpecificQuery

        SelectProjectRegions keystoneUrl projectUuid ->
            buildUrlFunc
                [ "selectprojectregions" ]
                [ UB.string "keystoneurl" keystoneUrl
                , UB.string "projectuuid" projectUuid
                ]

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


pushUrl :
    { r
        | navigationKey : Browser.Navigation.Key
        , urlPathPrefix : Maybe String
    }
    -> Route
    -> Cmd msg
pushUrl { navigationKey, urlPathPrefix } route =
    Browser.Navigation.pushUrl navigationKey (toUrl urlPathPrefix route)


replaceUrl :
    { r
        | navigationKey : Browser.Navigation.Key
        , urlPathPrefix : Maybe String
    }
    -> Route
    -> Cmd msg
replaceUrl { navigationKey, urlPathPrefix } route =
    Browser.Navigation.replaceUrl navigationKey (toUrl urlPathPrefix route)


withReplaceUrl :
    { r
        | navigationKey : Browser.Navigation.Key
        , urlPathPrefix : Maybe String
    }
    -> Route
    -> ( model, Cmd msg, sharedMsg )
    -> ( model, Cmd msg, sharedMsg )
withReplaceUrl viewContext route ( model, cmd, sharedMsg ) =
    -- Helper for use in pages
    ( model
    , Cmd.batch
        [ cmd
        , replaceUrl viewContext route
        ]
    , sharedMsg
    )


fromUrl : Maybe String -> Route -> Url.Url -> Route
fromUrl maybePathPrefix defaultRoute_ url =
    (case maybePathPrefix of
        Nothing ->
            parse
                (oneOf
                    (pathParsers defaultRoute_)
                )
                url

        Just pathPrefix ->
            parse
                (s
                    pathPrefix
                    </> oneOf
                            (pathParsers defaultRoute_)
                )
                url
    )
        |> Maybe.withDefault defaultRoute_


pathParsers : Route -> List (Parser (Route -> b) b)
pathParsers defaultRoute_ =
    [ -- Non-project-specific pages
      map defaultRoute_ top
    , map
        Home
        (s "home")
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

    -- We no longer set this URL in the app but someone may still try to use it
    , map
        LoginPicker
        (s "login" </> s "jetstream1")

    -- We no longer set this URL in the app but someone may still try to use it
    , map
        LoginPicker
        (s "login" </> s "jetstream")
    , map
        LoginOpenIdConnect
        (s "login" </> s "openidconnect")
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
    , map
        (\maybeKeystoneUrl maybeProjectUuid ->
            case ( maybeKeystoneUrl, maybeProjectUuid ) of
                ( Just keystoneUrl, Just projectUuid ) ->
                    SelectProjectRegions keystoneUrl projectUuid

                _ ->
                    PageNotFound
        )
        (s "selectprojectregions" <?> Query.string "keystoneurl" <?> Query.string "projectuuid")
    , map
        (\maybeKeystoneUrl ->
            case maybeKeystoneUrl of
                Just keystoneUrl ->
                    SelectProjects keystoneUrl

                Nothing ->
                    PageNotFound
        )
        (s "selectprojs" <?> Query.string "keystoneurl")
    , map
        (\maybeShowDebugMsgs -> MessageLog (maybeShowDebugMsgs == Just "true"))
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
        (\projectUuid regionId projectRoute ->
            ProjectRoute
                { projectUuid = projectUuid
                , regionId = Just regionId
                }
                projectRoute
        )
        (s "projects" </> string </> s "regions" </> string </> oneOf projectRouteParsers)
    , map
        (\uuid projectRoute ->
            ProjectRoute
                { projectUuid = uuid, regionId = Nothing }
                projectRoute
        )
        (s "projects" </> string </> oneOf projectRouteParsers)
    ]


projectRouteParsers : List (Parser (ProjectRouteConstructor -> b) b)
projectRouteParsers =
    [ map
        InstanceSourcePicker
        (s "instancesource")

    -- Legacy URLs, keeping parsers around to handle any old links
    , map
        ProjectOverview
        (s "resources")

    -- Current URLs
    , map
        ProjectOverview
        (s "overview")
    , map
        Credentials
        (s "credentials")
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
        SecurityGroupDetail
        (s "securitygroups" </> string)
    , map
        SecurityGroupList
        (s "securitygroups")
    , map
        ServerDetail
        (s "servers" </> string)
    , map
        ServerList
        (s "servers")
    , map
        ServerResize
        (s "servers" </> string </> s "resize")
    , map
        ServerSecurityGroups
        (s "servers" </> string </> s "securitygroups")
    , map
        ShareCreate
        (s "createshare")
    , map
        ShareDetail
        (s "shares" </> string)
    , map
        ShareList
        (s "shares")
    , map
        VolumeDetail
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
        (\maybeServerUuid -> FloatingIpCreate maybeServerUuid)
        (let
            queryParser =
                Query.string "serveruuid"
         in
         s "createfloatingip" <?> queryParser
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
                Query.map4
                    ServerCreate
                    (Query.string "imageuuid"
                        |> Query.map (Maybe.withDefault "")
                    )
                    (Query.string "imagename"
                        |> Query.map (Maybe.withDefault "")
                    )
                    (Query.string "restrictflavorids"
                        |> Query.map
                            (\value ->
                                -- Nothing if this query parameter doesn't exist. Sorry for ugly factoring.
                                -- elm-analyse doesn't let you map Nothing to Nothing in a case statement.
                                value
                                    |> Maybe.andThen
                                        (\value_ ->
                                            if value_ == "false" then
                                                Nothing

                                            else
                                                let
                                                    -- The beginning of the value will be "true@";
                                                    -- this is not to be interpreted as a flavor ID,
                                                    -- it just indicates we are passing flavor IDs
                                                    flavorIds =
                                                        String.dropLeft 5 value_
                                                in
                                                Just
                                                    (String.split "@" flavorIds)
                                        )
                            )
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
        VolumeMountInstructions
        (let
            queryParser =
                Query.map4
                    OSTypes.VolumeAttachment
                    (Query.string "voluuid"
                        |> Query.map (Maybe.withDefault "")
                    )
                    (Query.string "serveruuid"
                        |> Query.map (Maybe.withDefault "")
                    )
                    (Query.string "attachmentuuid"
                        |> Query.map (Maybe.withDefault "")
                    )
                    (Query.string "device")
         in
         s "attachvolinstructions" <?> queryParser
        )
    , map
        ImageList
        (s "images")
    ]


defaultRoute : Route
defaultRoute =
    Home


defaultLoginPage : Maybe HelperTypes.DefaultLoginView -> Route
defaultLoginPage maybeDefaultLoginView =
    case maybeDefaultLoginView of
        Nothing ->
            LoginPicker

        Just defaultLoginView ->
            case defaultLoginView of
                HelperTypes.DefaultLoginOpenstack ->
                    LoginOpenstack Nothing

                HelperTypes.DefaultLoginOpenIdConnect ->
                    LoginOpenIdConnect
