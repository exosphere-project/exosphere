module Tests.Route exposing (routeSuite)

import Expect
import OpenStack.Types as OSTypes
import Route
import Test exposing (Test, describe, test)
import Types.HelperTypes as HelperTypes
import Url


routeSuite : Test
routeSuite =
    describe "Route"
        [ roundTripSuite
        , lossyRouteSuite
        , toUrlSuite
        , parseSuite
        , earlierUrlFormSuite
        , rootSuite
        , queryValueDecodingSuite
        , analyticsUrlSuite
        ]



-- Fixtures


keystoneUrl : OSTypes.KeystoneUrl
keystoneUrl =
    "https://keystone.example:5000/v3"


projectRoute : Route.ProjectRouteConstructor -> Route.Route
projectRoute constructor =
    Route.ProjectRoute { projectUuid = "proj-1", regionId = Nothing } constructor


regionalProjectRoute : Route.ProjectRouteConstructor -> Route.Route
regionalProjectRoute constructor =
    Route.ProjectRoute { projectUuid = "proj-1", regionId = Just "RegionOne" } constructor


volumeAttachment : OSTypes.VolumeAttachment
volumeAttachment =
    { volumeUuid = "vol-1"
    , serverUuid = "srv-1"
    , attachmentUuid = "att-1"
    , device = Just "/dev/sdb"
    }


losslessNonProjectRoutes : List ( String, Route.Route )
losslessNonProjectRoutes =
    [ ( "GetSupport without an item", Route.GetSupport Nothing )
    , ( "HelpAbout", Route.HelpAbout )
    , ( "Home", Route.Home )
    , ( "LoginOpenIdConnect", Route.LoginOpenIdConnect )
    , ( "LoginPicker", Route.LoginPicker )
    , ( "MessageLog showing debug messages", Route.MessageLog True )
    , ( "MessageLog hiding debug messages", Route.MessageLog False )
    , ( "PageNotFound", Route.PageNotFound )
    , ( "SelectProjectRegions", Route.SelectProjectRegions keystoneUrl "proj-1" )
    , ( "SelectProjects", Route.SelectProjects keystoneUrl )
    , ( "Settings", Route.Settings )
    ]


losslessProjectConstructors : List ( String, Route.ProjectRouteConstructor )
losslessProjectConstructors =
    [ ( "ProjectOverview", Route.ProjectOverview )
    , ( "Credentials", Route.Credentials )
    , ( "FloatingIpAssign with both UUIDs", Route.FloatingIpAssign (Just "ip-1") (Just "srv-1") )
    , ( "FloatingIpAssign with neither UUID", Route.FloatingIpAssign Nothing Nothing )
    , ( "FloatingIpList", Route.FloatingIpList )
    , ( "ImageList", Route.ImageList )
    , ( "InstanceSourcePicker", Route.InstanceSourcePicker )
    , ( "FloatingIpCreate for a server", Route.FloatingIpCreate (Just "srv-1") )
    , ( "FloatingIpCreate without a server", Route.FloatingIpCreate Nothing )
    , ( "KeypairCreate", Route.KeypairCreate )
    , ( "KeypairList", Route.KeypairList )
    , ( "ObjectStorageList", Route.ObjectStorageList )
    , ( "ObjectStorageContainerCreate", Route.ObjectStorageContainerCreate )
    , ( "ObjectStorageContainerDetail without a prefix", Route.ObjectStorageContainerDetail "photos" Nothing )
    , ( "ObjectStorageContainerDetail, prefix with slashes and spaces", Route.ObjectStorageContainerDetail "photos" (Just "a/b c/") )
    , ( "ObjectStorageContainerDetail, unicode name and prefix", Route.ObjectStorageContainerDetail "café-données" (Just "dossier/£/") )
    , ( "ObjectStorageContainerDetail, name with '#', '%' and spaces", Route.ObjectStorageContainerDetail "my #1 100% container" Nothing )
    , ( "ObjectStorageContainerDetail, name holding a literal percent escape", Route.ObjectStorageContainerDetail "literal%20name" Nothing )
    , ( "ObjectStorageContainerDetail, name and prefix with a backslash", Route.ObjectStorageContainerDetail "back\\slash" (Just "dir\\sub/") )
    , ( "SecurityGroupDetail", Route.SecurityGroupDetail "sg-1" )
    , ( "SecurityGroupList", Route.SecurityGroupList )
    , ( "ServerCreate with restricted flavors and Guacamole", Route.ServerCreate "img-1" "My Image+1 & co" (Just [ "f1", "f2" ]) (Just True) )
    , ( "ServerCreate without Guacamole", Route.ServerCreate "img-1" "Ubuntu" Nothing (Just False) )
    , ( "ServerCreate with defaults", Route.ServerCreate "img-1" "Ubuntu" Nothing Nothing )
    , ( "ServerCreateImage with a name", Route.ServerCreateImage "srv-1" (Just "snap shot") )
    , ( "ServerCreateImage with an empty name", Route.ServerCreateImage "srv-1" (Just "") )
    , ( "ServerConsoleLog", Route.ServerConsoleLog "srv-1" )
    , ( "ServerDetail", Route.ServerDetail "srv-1" )
    , ( "ServerList", Route.ServerList )
    , ( "ServerResize", Route.ServerResize "srv-1" )
    , ( "ServerSecurityGroups", Route.ServerSecurityGroups "srv-1" )
    , ( "ShareCreate", Route.ShareCreate )
    , ( "ShareList", Route.ShareList )
    , ( "ShareDetail", Route.ShareDetail "share-1" )
    , ( "VolumeAttach with both UUIDs", Route.VolumeAttach (Just "srv-1") (Just "vol-1") )
    , ( "VolumeAttach with neither UUID", Route.VolumeAttach Nothing Nothing )
    , ( "VolumeCreate", Route.VolumeCreate )
    , ( "VolumeDetail", Route.VolumeDetail "vol-1" )
    , ( "VolumeList", Route.VolumeList )
    , ( "VolumeMountInstructions with a device", Route.VolumeMountInstructions volumeAttachment )
    , ( "VolumeMountInstructions without a device", Route.VolumeMountInstructions { volumeAttachment | device = Nothing } )
    ]


losslessRoutes : List ( String, Route.Route )
losslessRoutes =
    losslessNonProjectRoutes
        ++ List.map (Tuple.mapSecond projectRoute) losslessProjectConstructors
        ++ List.map
            (\( description, constructor ) -> ( description ++ " in a region", regionalProjectRoute constructor ))
            losslessProjectConstructors



-- Helpers


{-| `Route.fromUrl` falls back to its default route both for the root path and for a URL that no
parser matches. Parsing with two different defaults separates those fallbacks, reported as
`Nothing`, from a real match. A string `Url.fromString` rejects is also `Nothing`.
-}
parse : Maybe String -> String -> Maybe Route.Route
parse maybePathPrefix path =
    Url.fromString ("http://example.com" ++ path)
        |> Maybe.andThen
            (\url ->
                let
                    route =
                        Route.fromUrl maybePathPrefix Route.Home url
                in
                if route == Route.fromUrl maybePathPrefix Route.Settings url then
                    Just route

                else
                    Nothing
            )


roundTrip : Maybe String -> Route.Route -> Maybe Route.Route
roundTrip maybePathPrefix route =
    parse maybePathPrefix (Route.toUrl maybePathPrefix route)


{-| A deployment served from a subdirectory sets a path prefix, and `fromUrl` and `toUrl` handle a
prefixed URL through separate code, so every case runs both ways.
-}
withAndWithoutPathPrefix : (Maybe String -> List Test) -> List Test
withAndWithoutPathPrefix testsFor =
    [ describe "without a path prefix" (testsFor Nothing)
    , describe "with a path prefix" (testsFor (Just "exosphere"))
    ]


prefixPath : Maybe String -> String -> String
prefixPath maybePathPrefix path =
    case maybePathPrefix of
        Just pathPrefix ->
            "/" ++ pathPrefix ++ path

        Nothing ->
            path



-- Suites


roundTripSuite : Test
roundTripSuite =
    describe "lossless routes survive toUrl then fromUrl"
        (withAndWithoutPathPrefix
            (\maybePathPrefix ->
                List.map
                    (\( description, route ) ->
                        test description <|
                            \_ -> Expect.equal (Just route) (roundTrip maybePathPrefix route)
                    )
                    losslessRoutes
            )
        )


{-| These pin what the current router does with routes that cannot survive a round trip, not what
it should do. A change here alters where an existing link or redirect lands, so make it on purpose.
-}
lossyRouteSuite : Test
lossyRouteSuite =
    let
        testCases =
            [ { description = "LoadingUnscopedProjects builds a URL that no parser matches"
              , route = Route.LoadingUnscopedProjects "token-1"
              , expected = Nothing
              }
            , { description = "LoginOpenstack drops the credentials it was given"
              , route =
                    Route.LoginOpenstack
                        (Just { authUrl = keystoneUrl, userDomain = "Default", username = "alice", password = "secret" })
              , expected =
                    Just (Route.LoginOpenstack (Just { authUrl = "", userDomain = "", username = "", password = "" }))
              }
            , { description = "LoginOpenstack without credentials comes back with empty ones"
              , route = Route.LoginOpenstack Nothing
              , expected =
                    Just (Route.LoginOpenstack (Just { authUrl = "", userDomain = "", username = "", password = "" }))
              }
            , { description = "GetSupport drops the item it was given"
              , route = Route.GetSupport (Just ( HelperTypes.SupportableServer, Just "srv-1" ))
              , expected = Just (Route.GetSupport Nothing)
              }
            , { description = "ServerCreateImage without a name comes back with an empty one"
              , route = projectRoute (Route.ServerCreateImage "srv-1" Nothing)
              , expected = Just (projectRoute (Route.ServerCreateImage "srv-1" (Just "")))
              }
            , { description = "ServerCreate restricted to no flavors comes back restricted to one empty flavor ID"
              , route = projectRoute (Route.ServerCreate "img-1" "Ubuntu" (Just []) Nothing)
              , expected = Just (projectRoute (Route.ServerCreate "img-1" "Ubuntu" (Just [ "" ]) Nothing))
              }
            , { description = "ObjectStorageContainerDetail with an empty prefix comes back without one"
              , route = projectRoute (Route.ObjectStorageContainerDetail "photos" (Just ""))
              , expected = Just (projectRoute (Route.ObjectStorageContainerDetail "photos" Nothing))
              }
            ]
    in
    describe "lossy routes normalise through toUrl then fromUrl"
        (withAndWithoutPathPrefix
            (\maybePathPrefix ->
                List.map
                    (\{ description, route, expected } ->
                        test description <|
                            \_ -> Expect.equal expected (roundTrip maybePathPrefix route)
                    )
                    testCases
            )
        )


toUrlSuite : Test
toUrlSuite =
    let
        testCases =
            [ ( Nothing, Route.GetSupport (Just ( HelperTypes.SupportableServer, Just "srv-1" )), "/getsupport" )
            , ( Nothing, Route.HelpAbout, "/helpabout" )
            , ( Nothing, Route.Home, "/home" )
            , ( Nothing, Route.LoadingUnscopedProjects "token-1", "/loadingprojs" )
            , ( Nothing, Route.LoginOpenstack Nothing, "/login/openstack" )
            , ( Nothing, Route.LoginOpenIdConnect, "/login/openidconnect" )
            , ( Nothing, Route.LoginPicker, "/loginpicker" )
            , ( Nothing, Route.MessageLog True, "/msglog?showdebug=true" )
            , ( Nothing, Route.MessageLog False, "/msglog?showdebug=false" )
            , ( Nothing, Route.PageNotFound, "/pagenotfound" )
            , ( Nothing
              , Route.SelectProjectRegions keystoneUrl "proj-1"
              , "/selectprojectregions?keystoneurl=https://keystone.example:5000/v3&projectuuid=proj-1"
              )
            , ( Nothing, Route.SelectProjects keystoneUrl, "/selectprojs?keystoneurl=https://keystone.example:5000/v3" )
            , ( Nothing, Route.Settings, "/settings" )
            , ( Nothing, projectRoute Route.ProjectOverview, "/projects/proj-1/overview" )
            , ( Nothing, projectRoute Route.Credentials, "/projects/proj-1/credentials" )
            , ( Nothing
              , projectRoute (Route.FloatingIpAssign (Just "ip-1") (Just "srv-1"))
              , "/projects/proj-1/assignfloatingip?ipuuid=ip-1&serveruuid=srv-1"
              )
            , ( Nothing, projectRoute (Route.FloatingIpAssign Nothing Nothing), "/projects/proj-1/assignfloatingip" )
            , ( Nothing, projectRoute Route.FloatingIpList, "/projects/proj-1/floatingips" )
            , ( Nothing, projectRoute Route.ImageList, "/projects/proj-1/images" )
            , ( Nothing, projectRoute Route.InstanceSourcePicker, "/projects/proj-1/instancesource" )
            , ( Nothing, projectRoute (Route.FloatingIpCreate (Just "srv-1")), "/projects/proj-1/createfloatingip?serveruuid=srv-1" )
            , ( Nothing, projectRoute Route.KeypairCreate, "/projects/proj-1/uploadkeypair" )
            , ( Nothing, projectRoute Route.KeypairList, "/projects/proj-1/keypairs" )
            , ( Nothing, projectRoute Route.ObjectStorageList, "/projects/proj-1/objectstorage" )
            , ( Nothing, projectRoute Route.ObjectStorageContainerCreate, "/projects/proj-1/createcontainer" )
            , ( Nothing
              , projectRoute (Route.ObjectStorageContainerDetail "my #1 café" (Just "a/b c/"))
              , "/projects/proj-1/objectstorage/my%20%231%20café?prefix=a/b+c/"
              )
            , ( Nothing
              , projectRoute (Route.ObjectStorageContainerDetail "literal%20name" Nothing)
              , "/projects/proj-1/objectstorage/literal%2520name"
              )
            , ( Nothing
              , projectRoute (Route.ObjectStorageContainerDetail "back\\slash" (Just "dir\\sub/"))
              , "/projects/proj-1/objectstorage/back%5Cslash?prefix=dir%5Csub/"
              )
            , ( Nothing, projectRoute (Route.SecurityGroupDetail "sg-1"), "/projects/proj-1/securitygroups/sg-1" )
            , ( Nothing, projectRoute Route.SecurityGroupList, "/projects/proj-1/securitygroups" )
            , ( Nothing
              , projectRoute (Route.ServerCreate "img-1" "My Image+1" (Just [ "f1", "f2" ]) (Just True))
              , "/projects/proj-1/createserver?deployguac=justtrue&imagename=My+Image%2B1&imageuuid=img-1&restrictflavorids=true@f1@f2"
              )
            , ( Nothing
              , projectRoute (Route.ServerCreate "img-1" "Ubuntu" Nothing (Just False))
              , "/projects/proj-1/createserver?deployguac=justfalse&imagename=Ubuntu&imageuuid=img-1&restrictflavorids=false"
              )
            , ( Nothing
              , projectRoute (Route.ServerCreate "img-1" "Ubuntu" Nothing Nothing)
              , "/projects/proj-1/createserver?deployguac=nothing&imagename=Ubuntu&imageuuid=img-1&restrictflavorids=false"
              )
            , ( Nothing, projectRoute (Route.ServerCreateImage "srv-1" (Just "snap shot")), "/projects/proj-1/servers/srv-1/image?name=snap+shot" )
            , ( Nothing, projectRoute (Route.ServerCreateImage "srv-1" (Just "")), "/projects/proj-1/servers/srv-1/image?name" )
            , ( Nothing, projectRoute (Route.ServerCreateImage "srv-1" Nothing), "/projects/proj-1/servers/srv-1/image" )
            , ( Nothing, projectRoute (Route.ServerConsoleLog "srv-1"), "/projects/proj-1/servers/srv-1/console-log" )
            , ( Nothing, projectRoute (Route.ServerDetail "srv-1"), "/projects/proj-1/servers/srv-1" )
            , ( Nothing, projectRoute Route.ServerList, "/projects/proj-1/servers" )
            , ( Nothing, projectRoute (Route.ServerResize "srv-1"), "/projects/proj-1/servers/srv-1/resize" )
            , ( Nothing, projectRoute (Route.ServerSecurityGroups "srv-1"), "/projects/proj-1/servers/srv-1/securitygroups" )
            , ( Nothing, projectRoute Route.ShareCreate, "/projects/proj-1/createshare" )
            , ( Nothing, projectRoute Route.ShareList, "/projects/proj-1/shares" )
            , ( Nothing, projectRoute (Route.ShareDetail "share-1"), "/projects/proj-1/shares/share-1" )
            , ( Nothing
              , projectRoute (Route.VolumeAttach (Just "srv-1") (Just "vol-1"))
              , "/projects/proj-1/attachvol?serveruuid=srv-1&voluuid=vol-1"
              )
            , ( Nothing, projectRoute (Route.VolumeAttach Nothing Nothing), "/projects/proj-1/attachvol" )
            , ( Nothing, projectRoute Route.VolumeCreate, "/projects/proj-1/createvolume" )
            , ( Nothing, projectRoute (Route.VolumeDetail "vol-1"), "/projects/proj-1/volumes/vol-1" )
            , ( Nothing, projectRoute Route.VolumeList, "/projects/proj-1/volumes" )
            , ( Nothing
              , projectRoute (Route.VolumeMountInstructions volumeAttachment)
              , "/projects/proj-1/attachvolinstructions?attachmentuuid=att-1&device=/dev/sdb&serveruuid=srv-1&voluuid=vol-1"
              )
            , ( Nothing
              , projectRoute (Route.VolumeMountInstructions { volumeAttachment | device = Nothing })
              , "/projects/proj-1/attachvolinstructions?attachmentuuid=att-1&serveruuid=srv-1&voluuid=vol-1"
              )
            , ( Nothing, regionalProjectRoute Route.ServerList, "/projects/proj-1/regions/RegionOne/servers" )
            , ( Just "exosphere", Route.Home, "/exosphere/home" )
            , ( Just "exosphere"
              , regionalProjectRoute (Route.ServerDetail "srv-1")
              , "/exosphere/projects/proj-1/regions/RegionOne/servers/srv-1"
              )
            ]
    in
    describe "toUrl builds exactly these strings"
        (List.map
            (\( maybePathPrefix, route, expected ) ->
                test expected <|
                    \_ -> Expect.equal expected (Route.toUrl maybePathPrefix route)
            )
            testCases
        )


parseSuite : Test
parseSuite =
    let
        testCases =
            [ { description = "legacy project overview path"
              , path = "/projects/proj-1/resources"
              , expected = Just (projectRoute Route.ProjectOverview)
              }
            , { description = "legacy Jetstream login path"
              , path = "/login/jetstream"
              , expected = Just Route.LoginPicker
              }
            , { description = "legacy Jetstream1 login path"
              , path = "/login/jetstream1"
              , expected = Just Route.LoginPicker
              }
            , { description = "OIDC callback carrying a token"
              , path = "/auth/oidc-login?token=gAAAA-token_1"
              , expected = Just (Route.LoadingUnscopedProjects "gAAAA-token_1")
              }
            , { description = "OIDC callback without a token"
              , path = "/auth/oidc-login"
              , expected = Just Route.PageNotFound
              }
            , { description = "project selection without a Keystone URL"
              , path = "/selectprojs"
              , expected = Just Route.PageNotFound
              }
            , { description = "region selection without a project UUID"
              , path = "/selectprojectregions?keystoneurl=https%3A%2F%2Fkeystone.example"
              , expected = Just Route.PageNotFound
              }
            , { description = "OpenStack login prefilled from the query, never with a password"
              , path = "/login/openstack?authurl=https%3A%2F%2Fkeystone.example&udomain=Default&uname=alice"
              , expected =
                    Just
                        (Route.LoginOpenstack
                            (Just { authUrl = "https://keystone.example", userDomain = "Default", username = "alice", password = "" })
                        )
              }
            , { description = "message log without showdebug"
              , path = "/msglog"
              , expected = Just (Route.MessageLog False)
              }
            , { description = "message log with a showdebug value other than true"
              , path = "/msglog?showdebug=yes"
              , expected = Just (Route.MessageLog False)
              }
            , { description = "create server without query params"
              , path = "/projects/proj-1/createserver"
              , expected = Just (projectRoute (Route.ServerCreate "" "" Nothing Nothing))
              }
            , { description = "create server with an unknown deployguac value"
              , path = "/projects/proj-1/createserver?imageuuid=img-1&imagename=Ubuntu&restrictflavorids=false&deployguac=maybe"
              , expected = Just (projectRoute (Route.ServerCreate "img-1" "Ubuntu" Nothing Nothing))
              }
            , { description = "create server with unencoded '@' between flavor IDs"
              , path = "/projects/proj-1/createserver?imageuuid=img-1&imagename=Ubuntu&restrictflavorids=true@f1@f2&deployguac=nothing"
              , expected = Just (projectRoute (Route.ServerCreate "img-1" "Ubuntu" (Just [ "f1", "f2" ]) Nothing))
              }
            , { description = "mount instructions without query params"
              , path = "/projects/proj-1/attachvolinstructions"
              , expected =
                    Just
                        (projectRoute
                            (Route.VolumeMountInstructions { volumeUuid = "", serverUuid = "", attachmentUuid = "", device = Nothing })
                        )
              }
            , { description = "trailing slash"
              , path = "/projects/proj-1/servers/"
              , expected = Just (projectRoute Route.ServerList)
              }
            , { description = "double trailing slash"
              , path = "/projects/proj-1/servers/srv-1//"
              , expected = Just (projectRoute (Route.ServerDetail "srv-1"))
              }
            , { description = "percent escapes in a project UUID segment decode"
              , path = "/projects/proj%201/servers"
              , expected = Just (Route.ProjectRoute { projectUuid = "proj 1", regionId = Nothing } Route.ServerList)
              }
            , { description = "a container name that is not a valid percent escape is kept as written"
              , path = "/projects/proj-1/objectstorage/100%"
              , expected = Just (projectRoute (Route.ObjectStorageContainerDetail "100%" Nothing))
              }
            , { description = "an unknown path falls back to the default route"
              , path = "/nope"
              , expected = Nothing
              }
            , { description = "regional legacy project overview path"
              , path = "/projects/proj-1/regions/RegionOne/resources"
              , expected = Just (regionalProjectRoute Route.ProjectOverview)
              }
            ]
    in
    describe "fromUrl parses these URLs"
        (withAndWithoutPathPrefix
            (\maybePathPrefix ->
                List.map
                    (\{ description, path, expected } ->
                        test description <|
                            \_ -> Expect.equal expected (parse maybePathPrefix (prefixPath maybePathPrefix path))
                    )
                    testCases
            )
            ++ [ test "a path missing the configured prefix falls back to the default route" <|
                    \_ -> Expect.equal Nothing (parse (Just "exosphere") "/home")
               ]
        )


{-| Before the router moved to AppUrl, elm/url's `Url.Builder` wrote these URLs, so links and
bookmarks in this form are still around.
-}
earlierUrlFormSuite : Test
earlierUrlFormSuite =
    let
        testCases =
            [ ( "/selectprojectregions?keystoneurl=https%3A%2F%2Fkeystone.example%3A5000%2Fv3&projectuuid=proj-1"
              , Route.SelectProjectRegions keystoneUrl "proj-1"
              )
            , ( "/selectprojs?keystoneurl=https%3A%2F%2Fkeystone.example%3A5000%2Fv3", Route.SelectProjects keystoneUrl )
            , ( "/projects/proj-1/objectstorage/my%20%231%20caf%C3%A9?prefix=a%2Fb%20c%2F"
              , projectRoute (Route.ObjectStorageContainerDetail "my #1 café" (Just "a/b c/"))
              )
            , ( "/projects/proj-1/createserver?imageuuid=img-1&imagename=My%20Image%2B1&restrictflavorids=true%40f1%40f2&deployguac=justtrue"
              , projectRoute (Route.ServerCreate "img-1" "My Image+1" (Just [ "f1", "f2" ]) (Just True))
              )
            , ( "/projects/proj-1/createserver?imageuuid=img-1&imagename=Ubuntu&restrictflavorids=false&deployguac=justfalse"
              , projectRoute (Route.ServerCreate "img-1" "Ubuntu" Nothing (Just False))
              )
            , ( "/projects/proj-1/createserver?imageuuid=img-1&imagename=Ubuntu&restrictflavorids=false&deployguac=nothing"
              , projectRoute (Route.ServerCreate "img-1" "Ubuntu" Nothing Nothing)
              )
            , ( "/projects/proj-1/servers/srv-1/image?name=snap%20shot", projectRoute (Route.ServerCreateImage "srv-1" (Just "snap shot")) )
            , ( "/projects/proj-1/servers/srv-1/image?name=", projectRoute (Route.ServerCreateImage "srv-1" (Just "")) )
            , ( "/projects/proj-1/attachvolinstructions?voluuid=vol-1&serveruuid=srv-1&attachmentuuid=att-1&device=%2Fdev%2Fsdb"
              , projectRoute (Route.VolumeMountInstructions volumeAttachment)
              )
            , ( "/projects/proj-1/attachvolinstructions?voluuid=vol-1&serveruuid=srv-1&attachmentuuid=att-1"
              , projectRoute (Route.VolumeMountInstructions { volumeAttachment | device = Nothing })
              )
            , ( "/projects/proj-1/attachvol?voluuid=vol-1&serveruuid=srv-1", projectRoute (Route.VolumeAttach (Just "srv-1") (Just "vol-1")) )
            ]
    in
    describe "URLs in the earlier elm/url form still parse"
        (List.map
            (\( path, expected ) ->
                test path <|
                    \_ -> Expect.equal (Just expected) (parse Nothing path)
            )
            testCases
        )


rootSuite : Test
rootSuite =
    let
        testCases =
            [ ( Nothing, "/" )
            , ( Just "exosphere", "/exosphere" )
            , ( Just "exosphere", "/exosphere/" )
            ]
    in
    describe "the root path parses as the default route"
        (List.map
            (\( maybePathPrefix, path ) ->
                test path <|
                    \_ ->
                        Expect.equal (Just Route.HelpAbout)
                            (Url.fromString ("http://example.com" ++ path)
                                |> Maybe.map (Route.fromUrl maybePathPrefix Route.HelpAbout)
                            )
            )
            testCases
        )


{-| AppUrl reads `+` as a space, keeps a raw `=` in the value, gives the first of repeated values,
reads a key without `=` as empty, and keeps an invalid percent escape as written. `FloatingIpCreate`
keeps its query value as a `Maybe`, so an absent value and an empty one stay distinguishable.
-}
queryValueDecodingSuite : Test
queryValueDecodingSuite =
    let
        testCases =
            [ ( "a literal '+' reads as a space", "?serveruuid=a+b", Just "a b" )
            , ( "%20 decodes to a space", "?serveruuid=a%20b", Just "a b" )
            , ( "%2B decodes to a '+'", "?serveruuid=a%2Bb", Just "a+b" )
            , ( "%3D decodes to a '='", "?serveruuid=a%3Db", Just "a=b" )
            , ( "a raw '=' stays in the value", "?serveruuid=a=b", Just "a=b" )
            , ( "a repeated key yields its first value", "?serveruuid=a&serveruuid=b", Just "a" )
            , ( "an empty value is empty", "?serveruuid=", Just "" )
            , ( "a key without '=' is empty", "?serveruuid", Just "" )
            , ( "an absent key yields no value", "", Nothing )
            , ( "an invalid percent escape is kept as written", "?serveruuid=%zz", Just "%zz" )
            , ( "percent-encoded UTF-8 decodes", "?serveruuid=caf%C3%A9", Just "café" )
            ]
    in
    describe "query values decode as AppUrl decodes them"
        (List.map
            (\( description, query, expectedServerUuid ) ->
                test description <|
                    \_ ->
                        Expect.equal
                            (Just (projectRoute (Route.FloatingIpCreate expectedServerUuid)))
                            (parse Nothing ("/projects/proj-1/createfloatingip" ++ query))
            )
            testCases
        )


analyticsUrlSuite : Test
analyticsUrlSuite =
    let
        templateCases =
            [ ( Nothing, regionalProjectRoute (Route.ServerDetail "srv-1"), "/projects/:project/regions/RegionOne/servers/:server" )
            , ( Nothing, projectRoute Route.ServerList, "/projects/:project/servers" )
            , ( Just "exosphere"
              , regionalProjectRoute (Route.ServerCreateImage "srv-1" (Just "snap shot"))
              , "/exosphere/projects/:project/regions/RegionOne/servers/:server/image"
              )
            , ( Nothing, projectRoute (Route.ObjectStorageContainerDetail "photos" (Just "a/b c/")), "/projects/:project/objectstorage/:container" )
            , ( Nothing, projectRoute (Route.SecurityGroupDetail "sg-1"), "/projects/:project/securitygroups/:security-group" )
            , ( Nothing, projectRoute (Route.ShareDetail "share-1"), "/projects/:project/shares/:share" )
            , ( Nothing, projectRoute (Route.VolumeDetail "vol-1"), "/projects/:project/volumes/:volume" )
            , ( Nothing, projectRoute (Route.VolumeAttach (Just "srv-1") (Just "vol-1")), "/projects/:project/attachvol" )
            , ( Nothing, Route.SelectProjectRegions keystoneUrl "proj-1", "/selectprojectregions" )
            , ( Nothing, Route.LoadingUnscopedProjects "token-1", "/loadingprojs" )
            , ( Nothing, Route.MessageLog True, "/msglog" )
            ]

        identifyingFixtures =
            [ "proj-1", "srv-1", "vol-1", "ip-1", "sg-1", "share-1", "img-1", "att-1", "f1", "f2", "photos", "café", "données", "dossier", "my #1", "literal", "back", "keystone.example", "token-1", "alice", "secret", "Default", "Ubuntu", "My Image", "snap", "sdb" ]

        lossyRoutes =
            [ ( "LoadingUnscopedProjects", Route.LoadingUnscopedProjects "token-1" )
            , ( "LoginOpenstack with credentials"
              , Route.LoginOpenstack (Just { authUrl = keystoneUrl, userDomain = "Default", username = "alice", password = "secret" })
              )
            , ( "GetSupport with an item", Route.GetSupport (Just ( HelperTypes.SupportableServer, Just "srv-1" )) )
            ]

        leaks : String -> List String
        leaks analyticsUrl =
            List.filter (\fixture -> String.contains fixture analyticsUrl) ("?" :: identifyingFixtures)
    in
    describe "toAnalyticsUrl"
        [ describe "reports the route's shape"
            (List.map
                (\( maybePathPrefix, route, expected ) ->
                    test expected <|
                        \_ -> Expect.equal expected (Route.toAnalyticsUrl maybePathPrefix route)
                )
                templateCases
            )
        , describe "never reports an identifier, name, credential or query string"
            (withAndWithoutPathPrefix
                (\maybePathPrefix ->
                    List.map
                        (\( description, route ) ->
                            test description <|
                                \_ -> Expect.equal [] (leaks (Route.toAnalyticsUrl maybePathPrefix route))
                        )
                        (losslessRoutes ++ lossyRoutes)
                )
            )
        ]
