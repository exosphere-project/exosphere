module Tests.Route exposing (objectStorageRouteRoundTripSuite)

import Expect
import Route
import Test exposing (Test, describe, test)
import Url


{-| Wrap a project-scoped route constructor in a full `Route` for round-trip testing.
-}
projectRoute : Route.ProjectRouteConstructor -> Route.Route
projectRoute constructor =
    Route.ProjectRoute { projectUuid = "proj-1", regionId = Nothing } constructor


{-| Build a URL from `toUrl` and parse it back with `fromUrl`, using the given path prefix on
BOTH sides. `Route.PageNotFound` is the sentinel default: if parsing falls through, the result is
`PageNotFound`, which won't equal any real route and so fails the test loudly.
-}
roundTrip : Maybe String -> Route.Route -> Route.Route
roundTrip maybePathPrefix route =
    case Url.fromString ("http://example.com" ++ Route.toUrl maybePathPrefix route) of
        Just url ->
            Route.fromUrl maybePathPrefix Route.PageNotFound url

        Nothing ->
            -- An unparseable URL is itself a failure; return a value that won't match.
            Route.PageNotFound


objectStorageRouteRoundTripSuite : Test
objectStorageRouteRoundTripSuite =
    describe "Object Storage routes round-trip through toUrl / fromUrl"
        [ test "(a) container list" <|
            \_ ->
                let
                    route =
                        projectRoute Route.ObjectStorageList
                in
                Expect.equal route (roundTrip Nothing route)
        , test "(b) container detail, no prefix" <|
            \_ ->
                let
                    route =
                        projectRoute (Route.ObjectStorageContainerDetail "photos" Nothing)
                in
                Expect.equal route (roundTrip Nothing route)
        , test "(c) container detail, prefix with slashes and spaces" <|
            \_ ->
                let
                    route =
                        projectRoute (Route.ObjectStorageContainerDetail "photos" (Just "a/b c/"))
                in
                Expect.equal route (roundTrip Nothing route)
        , test "(d) unicode container name + unicode prefix" <|
            \_ ->
                let
                    route =
                        projectRoute (Route.ObjectStorageContainerDetail "café-données" (Just "dossier/£/"))
                in
                Expect.equal route (roundTrip Nothing route)
        , test "(e) container name with '#' and spaces (percent-encode/decode through the segment)" <|
            \_ ->
                let
                    route =
                        projectRoute (Route.ObjectStorageContainerDetail "my #1 container" Nothing)
                in
                Expect.equal route (roundTrip Nothing route)
        , test "(f) path prefix threaded through both toUrl and fromUrl" <|
            \_ ->
                let
                    route =
                        projectRoute (Route.ObjectStorageContainerDetail "photos" (Just "a/b c/"))
                in
                Expect.equal route (roundTrip (Just "exosphere") route)
        , test "(g) container create" <|
            \_ ->
                let
                    route =
                        projectRoute Route.ObjectStorageContainerCreate
                in
                Expect.equal route (roundTrip Nothing route)
        , test "(g2) container create with path prefix" <|
            \_ ->
                let
                    route =
                        projectRoute Route.ObjectStorageContainerCreate
                in
                Expect.equal route (roundTrip (Just "exosphere") route)
        , test "(f2) container list with path prefix" <|
            \_ ->
                let
                    route =
                        projectRoute Route.ObjectStorageList
                in
                Expect.equal route (roundTrip (Just "exosphere") route)
        ]
