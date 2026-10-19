module Route exposing
    ( ProjectRouteConstructor(..)
    , Route(..)
    , defaultLoginPage
    , defaultRoute
    , fromUrl
    , pushUrl
    , replaceUrl
    , toAnalyticsUrl
    , toUrl
    )

import AppUrl exposing (AppUrl)
import Browser.Navigation
import Dict
import OpenStack.ObjectStorage as ObjectStorage
import OpenStack.Types as OSTypes
import Types.HelperTypes as HelperTypes
import Url


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
    | ObjectStorageList
    | ObjectStorageContainerCreate
    | ObjectStorageContainerDetail ObjectStorage.ContainerName (Maybe ObjectStorage.Prefix)
    | SecurityGroupDetail OSTypes.SecurityGroupUuid
    | SecurityGroupList
    | ServerCreate OSTypes.ImageUuid String (Maybe (List OSTypes.FlavorId)) (Maybe Bool)
    | ServerCreateImage OSTypes.ServerUuid (Maybe String)
    | ServerConsoleLog OSTypes.ServerUuid
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
            buildUrlFunc [ "getsupport" ] []

        HelpAbout ->
            buildUrlFunc [ "helpabout" ] []

        Home ->
            buildUrlFunc [ "home" ] []

        LoadingUnscopedProjects _ ->
            buildUrlFunc [ "loadingprojs" ] []

        LoginOpenstack _ ->
            buildUrlFunc [ "login", "openstack" ] []

        LoginOpenIdConnect ->
            buildUrlFunc [ "login", "openidconnect" ] []

        LoginPicker ->
            buildUrlFunc [ "loginpicker" ] []

        MessageLog showDebugMsgs ->
            buildUrlFunc [ "msglog" ]
                [ ( "showdebug"
                  , [ if showDebugMsgs then
                        "true"

                      else
                        "false"
                    ]
                  )
                ]

        PageNotFound ->
            buildUrlFunc [ "pagenotfound" ] []

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
                            ( [ "overview" ], [] )

                        Credentials ->
                            ( [ "credentials" ], [] )

                        FloatingIpAssign maybeIpUuid maybeServerUuid ->
                            ( [ "assignfloatingip" ]
                            , [ ( "ipuuid", maybeToList maybeIpUuid )
                              , ( "serveruuid", maybeToList maybeServerUuid )
                              ]
                            )

                        FloatingIpList ->
                            ( [ "floatingips" ], [] )

                        ImageList ->
                            ( [ "images" ], [] )

                        InstanceSourcePicker ->
                            ( [ "instancesource" ], [] )

                        FloatingIpCreate maybeServerUuid ->
                            ( [ "createfloatingip" ]
                            , [ ( "serveruuid", maybeToList maybeServerUuid ) ]
                            )

                        KeypairCreate ->
                            ( [ "uploadkeypair" ], [] )

                        KeypairList ->
                            ( [ "keypairs" ], [] )

                        ObjectStorageList ->
                            ( [ "objectstorage" ], [] )

                        ObjectStorageContainerCreate ->
                            ( [ "createcontainer" ], [] )

                        ObjectStorageContainerDetail containerName maybePrefix ->
                            ( [ "objectstorage", containerName ]
                            , [ ( "prefix", maybeToList (normalizePrefix maybePrefix) ) ]
                            )

                        SecurityGroupDetail securityGroupUuid ->
                            ( [ "securitygroups", securityGroupUuid ], [] )

                        SecurityGroupList ->
                            ( [ "securitygroups" ], [] )

                        ServerCreate imageUuid imageName restrictFlavorIds maybeDeployGuac ->
                            ( [ "createserver" ]
                            , [ ( "imageuuid", [ imageUuid ] )
                              , ( "imagename", [ imageName ] )
                              , ( "restrictflavorids"
                                , [ case restrictFlavorIds of
                                        Nothing ->
                                            "false"

                                        Just flavorIds ->
                                            String.join "@" ("true" :: flavorIds)
                                  ]
                                )
                              , ( "deployguac"
                                , [ case maybeDeployGuac of
                                        Just True ->
                                            "justtrue"

                                        Just False ->
                                            "justfalse"

                                        Nothing ->
                                            "nothing"
                                  ]
                                )
                              ]
                            )

                        ServerCreateImage serverUuid maybeImageName ->
                            ( [ "servers", serverUuid, "image" ]
                            , [ ( "name", maybeToList maybeImageName ) ]
                            )

                        ServerConsoleLog serverUuid ->
                            ( [ "servers", serverUuid, "console-log" ], [] )

                        ServerDetail serverUuid ->
                            ( [ "servers", serverUuid ], [] )

                        ServerList ->
                            ( [ "servers" ], [] )

                        ServerResize serverUuid ->
                            ( [ "servers", serverUuid, "resize" ], [] )

                        ServerSecurityGroups serverUuid ->
                            ( [ "servers", serverUuid, "securitygroups" ], [] )

                        ShareCreate ->
                            ( [ "createshare" ], [] )

                        ShareDetail shareUuid ->
                            ( [ "shares", shareUuid ], [] )

                        ShareList ->
                            ( [ "shares" ], [] )

                        VolumeAttach maybeServerUuid maybeVolumeUuid ->
                            ( [ "attachvol" ]
                            , [ ( "voluuid", maybeToList maybeVolumeUuid )
                              , ( "serveruuid", maybeToList maybeServerUuid )
                              ]
                            )

                        VolumeCreate ->
                            ( [ "createvolume" ], [] )

                        VolumeDetail volumeUuid ->
                            ( [ "volumes", volumeUuid ], [] )

                        VolumeList ->
                            ( [ "volumes" ], [] )

                        VolumeMountInstructions attachment ->
                            ( [ "attachvolinstructions" ]
                            , [ ( "voluuid", [ attachment.volumeUuid ] )
                              , ( "serveruuid", [ attachment.serverUuid ] )
                              , ( "attachmentuuid", [ attachment.attachmentUuid ] )
                              , ( "device", maybeToList attachment.device )
                              ]
                            )
            in
            buildUrlFunc (projectIdentifierPath ++ projectSpecificPath) projectSpecificQuery

        SelectProjectRegions keystoneUrl projectUuid ->
            buildUrlFunc [ "selectprojectregions" ]
                [ ( "keystoneurl", [ keystoneUrl ] )
                , ( "projectuuid", [ projectUuid ] )
                ]

        SelectProjects keystoneUrl ->
            buildUrlFunc [ "selectprojs" ] [ ( "keystoneurl", [ keystoneUrl ] ) ]

        Settings ->
            buildUrlFunc [ "settings" ] []


{-| The URL to report to analytics: the route's path with every project, resource and container
identifier replaced by a placeholder such as `:server`, and no query string, so analytics never
receives a UUID, a resource name, a Keystone URL or a token. The path prefix and region stay, since
they tell deployments and regions apart rather than identifying anyone.

    toAnalyticsUrl Nothing (ProjectRoute { projectUuid = "…", regionId = Just "IU" } (ServerDetail "…"))
        == "/projects/:project/regions/IU/servers/:server"

-}
toAnalyticsUrl : Maybe String -> Route -> String
toAnalyticsUrl maybePathPrefix route =
    toUrl maybePathPrefix (withoutIdentifiers route)
        |> String.split "?"
        |> List.head
        |> Maybe.withDefault ""


{-| Every constructor is listed, so a new route cannot reach analytics without deciding what it
carries.
-}
withoutIdentifiers : Route -> Route
withoutIdentifiers route =
    case route of
        GetSupport _ ->
            GetSupport Nothing

        HelpAbout ->
            HelpAbout

        Home ->
            Home

        LoadingUnscopedProjects _ ->
            LoadingUnscopedProjects ""

        LoginOpenstack _ ->
            LoginOpenstack Nothing

        LoginOpenIdConnect ->
            LoginOpenIdConnect

        LoginPicker ->
            LoginPicker

        MessageLog showDebugMsgs ->
            MessageLog showDebugMsgs

        PageNotFound ->
            PageNotFound

        ProjectRoute projectIdentifier projectRouteConstructor ->
            ProjectRoute { projectIdentifier | projectUuid = ":project" }
                (projectRouteWithoutIdentifiers projectRouteConstructor)

        SelectProjectRegions _ _ ->
            SelectProjectRegions "" ""

        SelectProjects _ ->
            SelectProjects ""

        Settings ->
            Settings


projectRouteWithoutIdentifiers : ProjectRouteConstructor -> ProjectRouteConstructor
projectRouteWithoutIdentifiers projectRouteConstructor =
    case projectRouteConstructor of
        ProjectOverview ->
            ProjectOverview

        FloatingIpAssign _ _ ->
            FloatingIpAssign Nothing Nothing

        FloatingIpList ->
            FloatingIpList

        ImageList ->
            ImageList

        InstanceSourcePicker ->
            InstanceSourcePicker

        FloatingIpCreate _ ->
            FloatingIpCreate Nothing

        KeypairCreate ->
            KeypairCreate

        KeypairList ->
            KeypairList

        ObjectStorageList ->
            ObjectStorageList

        ObjectStorageContainerCreate ->
            ObjectStorageContainerCreate

        ObjectStorageContainerDetail _ _ ->
            ObjectStorageContainerDetail ":container" Nothing

        SecurityGroupDetail _ ->
            SecurityGroupDetail ":security-group"

        SecurityGroupList ->
            SecurityGroupList

        ServerCreate _ _ _ _ ->
            ServerCreate "" "" Nothing Nothing

        ServerCreateImage _ _ ->
            ServerCreateImage ":server" Nothing

        ServerConsoleLog _ ->
            ServerConsoleLog ":server"

        ServerDetail _ ->
            ServerDetail ":server"

        ServerList ->
            ServerList

        ServerResize _ ->
            ServerResize ":server"

        ServerSecurityGroups _ ->
            ServerSecurityGroups ":server"

        ShareCreate ->
            ShareCreate

        ShareList ->
            ShareList

        ShareDetail _ ->
            ShareDetail ":share"

        VolumeAttach _ _ ->
            VolumeAttach Nothing Nothing

        VolumeCreate ->
            VolumeCreate

        VolumeDetail _ ->
            VolumeDetail ":volume"

        VolumeList ->
            VolumeList

        VolumeMountInstructions _ ->
            VolumeMountInstructions { volumeUuid = "", serverUuid = "", attachmentUuid = "", device = Nothing }

        Credentials ->
            Credentials


{-| Normalize an object-storage prefix query value: treat `Just ""` as `Nothing`.

The route carries `prefix` as a query parameter (prefixes contain `/`, unicode, and spaces, which
round-trip more safely in the query than in a path segment). An empty prefix and an absent prefix
are the same thing, the top level of a container, so both collapse to `Nothing` on the way in and
out.

-}
normalizePrefix : Maybe String -> Maybe ObjectStorage.Prefix
normalizePrefix maybePrefix =
    case maybePrefix of
        Just "" ->
            Nothing

        other ->
            other


{-| A query parameter given no values is left out of the URL.

AppUrl leaves `\` unescaped, but browsers read a `\` in an http(s) path as `/` (WHATWG URL
Standard, path state), which would split a Swift container name (Swift allows `\` in one) across
two segments. Writing it as `%5C` keeps it whole, and AppUrl decodes it back.

-}
buildPrefixedUrl : Maybe String -> List String -> List ( String, List String ) -> String
buildPrefixedUrl maybePathPrefix pathParts queryParams =
    AppUrl.toString
        { path = maybeToList maybePathPrefix ++ pathParts
        , queryParameters = Dict.fromList queryParams
        , fragment = Nothing
        }
        |> String.replace "\\" "%5C"


maybeToList : Maybe a -> List a
maybeToList maybe =
    case maybe of
        Just value ->
            [ value ]

        Nothing ->
            []


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


fromUrl : Maybe String -> Route -> Url.Url -> Route
fromUrl maybePathPrefix defaultRoute_ url =
    let
        appUrl =
            AppUrl.fromUrl url
    in
    appUrl.path
        |> dropOneTrailingEmptySegment
        |> stripPathPrefix maybePathPrefix
        |> Maybe.andThen (\path -> parseRoute { appUrl | path = path })
        |> Maybe.withDefault defaultRoute_


{-| `AppUrl.fromUrl` drops one trailing slash, and elm/url, which this router used before AppUrl,
also accepted a second one (`/servers/s//`). Dropping one more empty segment keeps those links
working.
-}
dropOneTrailingEmptySegment : List String -> List String
dropOneTrailingEmptySegment path =
    case List.reverse path of
        "" :: rest ->
            List.reverse rest

        _ ->
            path


stripPathPrefix : Maybe String -> List String -> Maybe (List String)
stripPathPrefix maybePathPrefix path =
    case ( maybePathPrefix, path ) of
        ( Nothing, _ ) ->
            Just path

        ( Just pathPrefix, first :: rest ) ->
            if first == pathPrefix then
                Just rest

            else
                Nothing

        ( Just _, [] ) ->
            Nothing


{-| The first value given for a query parameter, percent-decoded, with `+` read as a space.
-}
queryValue : String -> AppUrl -> Maybe String
queryValue key appUrl =
    Dict.get key appUrl.queryParameters
        |> Maybe.andThen List.head


parseRoute : AppUrl -> Maybe Route
parseRoute appUrl =
    let
        query key =
            queryValue key appUrl

        queryOrEmpty key =
            Maybe.withDefault "" (query key)
    in
    case appUrl.path of
        [ "home" ] ->
            Just Home

        [ "login", "openstack" ] ->
            -- A login link may prefill everything but the password.
            Just
                (LoginOpenstack
                    (Just
                        { authUrl = queryOrEmpty "authurl"
                        , userDomain = queryOrEmpty "udomain"
                        , username = queryOrEmpty "uname"
                        , password = ""
                        }
                    )
                )

        -- We no longer set this URL in the app but someone may still try to use it
        [ "login", "jetstream1" ] ->
            Just LoginPicker

        -- We no longer set this URL in the app but someone may still try to use it
        [ "login", "jetstream" ] ->
            Just LoginPicker

        [ "login", "openidconnect" ] ->
            Just LoginOpenIdConnect

        [ "loginpicker" ] ->
            Just LoginPicker

        [ "auth", "oidc-login" ] ->
            Just (Maybe.withDefault PageNotFound (Maybe.map LoadingUnscopedProjects (query "token")))

        [ "selectprojectregions" ] ->
            Just
                (Maybe.map2 SelectProjectRegions (query "keystoneurl") (query "projectuuid")
                    |> Maybe.withDefault PageNotFound
                )

        [ "selectprojs" ] ->
            Just (Maybe.withDefault PageNotFound (Maybe.map SelectProjects (query "keystoneurl")))

        [ "msglog" ] ->
            Just (MessageLog (query "showdebug" == Just "true"))

        [ "settings" ] ->
            Just Settings

        [ "getsupport" ] ->
            Just (GetSupport Nothing)

        [ "helpabout" ] ->
            Just HelpAbout

        [ "pagenotfound" ] ->
            Just PageNotFound

        "projects" :: projectUuid :: "regions" :: regionId :: projectPath ->
            parseProjectRoute appUrl projectPath
                |> Maybe.map (ProjectRoute { projectUuid = projectUuid, regionId = Just regionId })

        "projects" :: projectUuid :: projectPath ->
            parseProjectRoute appUrl projectPath
                |> Maybe.map (ProjectRoute { projectUuid = projectUuid, regionId = Nothing })

        _ ->
            Nothing


parseProjectRoute : AppUrl -> List String -> Maybe ProjectRouteConstructor
parseProjectRoute appUrl projectPath =
    let
        query key =
            queryValue key appUrl

        queryOrEmpty key =
            Maybe.withDefault "" (query key)
    in
    case projectPath of
        [ "instancesource" ] ->
            Just InstanceSourcePicker

        -- Legacy URL, kept to handle any old links
        [ "resources" ] ->
            Just ProjectOverview

        [ "overview" ] ->
            Just ProjectOverview

        [ "credentials" ] ->
            Just Credentials

        [ "servers", serverUuid, "image" ] ->
            Just (ServerCreateImage serverUuid (Just (queryOrEmpty "name")))

        [ "securitygroups", securityGroupUuid ] ->
            Just (SecurityGroupDetail securityGroupUuid)

        [ "securitygroups" ] ->
            Just SecurityGroupList

        [ "servers", serverUuid, "console-log" ] ->
            Just (ServerConsoleLog serverUuid)

        [ "servers", serverUuid ] ->
            Just (ServerDetail serverUuid)

        [ "servers" ] ->
            Just ServerList

        [ "servers", serverUuid, "resize" ] ->
            Just (ServerResize serverUuid)

        [ "servers", serverUuid, "securitygroups" ] ->
            Just (ServerSecurityGroups serverUuid)

        [ "createshare" ] ->
            Just ShareCreate

        [ "shares", shareUuid ] ->
            Just (ShareDetail shareUuid)

        [ "shares" ] ->
            Just ShareList

        [ "volumes", volumeUuid ] ->
            Just (VolumeDetail volumeUuid)

        [ "volumes" ] ->
            Just VolumeList

        [ "floatingips" ] ->
            Just FloatingIpList

        [ "assignfloatingip" ] ->
            Just (FloatingIpAssign (query "ipuuid") (query "serveruuid"))

        [ "createfloatingip" ] ->
            Just (FloatingIpCreate (query "serveruuid"))

        [ "keypairs" ] ->
            Just KeypairList

        [ "uploadkeypair" ] ->
            Just KeypairCreate

        [ "objectstorage", containerName ] ->
            Just (ObjectStorageContainerDetail containerName (normalizePrefix (query "prefix")))

        [ "objectstorage" ] ->
            Just ObjectStorageList

        [ "createcontainer" ] ->
            Just ObjectStorageContainerCreate

        [ "createserver" ] ->
            Just
                (ServerCreate
                    (queryOrEmpty "imageuuid")
                    (queryOrEmpty "imagename")
                    (query "restrictflavorids" |> Maybe.andThen parseRestrictFlavorIds)
                    (query "deployguac" |> Maybe.andThen parseDeployGuac)
                )

        [ "createvolume" ] ->
            Just VolumeCreate

        [ "attachvol" ] ->
            Just (VolumeAttach (query "serveruuid") (query "voluuid"))

        [ "attachvolinstructions" ] ->
            Just
                (VolumeMountInstructions
                    { volumeUuid = queryOrEmpty "voluuid"
                    , serverUuid = queryOrEmpty "serveruuid"
                    , attachmentUuid = queryOrEmpty "attachmentuuid"
                    , device = query "device"
                    }
                )

        [ "images" ] ->
            Just ImageList

        _ ->
            Nothing


{-| The value is `false`, or `true` followed by the flavor IDs, each after an `@`, so the IDs start
after the five characters of `true@`. OpenStack forbids `@` in flavor IDs, which makes it a safe
separator.
-}
parseRestrictFlavorIds : String -> Maybe (List OSTypes.FlavorId)
parseRestrictFlavorIds value =
    if value == "false" then
        Nothing

    else
        Just (String.split "@" (String.dropLeft 5 value))


parseDeployGuac : String -> Maybe Bool
parseDeployGuac value =
    case value of
        "justtrue" ->
            Just True

        "justfalse" ->
            Just False

        _ ->
            Nothing


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
