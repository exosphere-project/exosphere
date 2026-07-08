module Rest.Swift exposing
    ( containerPageLimit
    , recursiveDeleteMaxCycles
    , requestContainerObjectNames
    , requestContainers
    , requestContainersPage
    , requestCreateContainer
    , requestDeleteContainer
    , requestDeleteContainerObject
    )

{-| HTTP wiring for OpenStack Object Storage (Swift).

Every request goes through `Rest.Helpers.openstackCredentialedRequest` (Keystone token + CORS
proxy). The base URL is the project's catalog endpoint (`project.endpoints.swift`), passed in by
the caller. Swift-specific wiring stays behind the backend-agnostic `OpenStack.ObjectStorage`
types and `SharedMsg` constructors, so an S3 implementation can live alongside it later.

PROVISIONAL: verified against devstack Swift 2.37 and live against Jetstream2 Ceph RGW (2026-07)
through an Exosphere-style proxy. A proxy must expose the Swift response headers (deployed proxies
often expose only `X-Subject-Token`) and lift its 1 MiB body cap for uploads/HEAD reads.

-}

import Bytes exposing (Bytes)
import Helpers.GetterSetters as GetterSetters
import Http
import Json.Decode
import OpenStack.ObjectStorage as ObjectStorage
import Rest.Helpers
    exposing
        ( expectBytesWithErrorBody
        , expectMetadataWithErrorBody
        , expectVoidWithErrorBody
        , httpResponseStringToResult
        , openstackCredentialedRequest
        )
import Time
import Types.Error exposing (ErrorContext, ErrorLevel(..), HttpErrorWithBody)
import Types.HelperTypes exposing (Headers, HttpRequestMethod(..), Url)
import Types.Project exposing (Project)
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))
import Url
import Url.Builder


containerPageLimit : Int
containerPageLimit =
    10000


requestContainers : Project -> Url -> Time.Posix -> Cmd SharedMsg
requestContainers project url currentTime =
    requestContainersPage project url currentTime Nothing


requestContainersPage : Project -> Url -> Time.Posix -> Maybe String -> Cmd SharedMsg
requestContainersPage project url currentTime maybeMarker =
    let
        errorContext =
            ErrorContext
                "get a list of object storage containers"
                ErrorCrit
                Nothing

        -- Swift sends Last-Modified without Cache-Control, so browser heuristic caching can stale
        -- listing reads; match Rest.AppVersion's timestamp query-param precedent.
        queryParams =
            [ Url.Builder.string "format" "json"
            , Url.Builder.int "limit" containerPageLimit
            , Url.Builder.int "t" (Time.posixToMillis currentTime)
            ]
                ++ (case maybeMarker of
                        Just marker ->
                            [ Url.Builder.string "marker" marker ]

                        Nothing ->
                            []
                   )

        resultToMsg_ result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveContainers errorContext maybeMarker result)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        []
        ( url, [], queryParams )
        Http.emptyBody
        (expectSwiftJsonOrEmpty resultToMsg_ ObjectStorage.parseContainersResponse)


expectSwiftJsonOrEmpty :
    (Result HttpErrorWithBody a -> SharedMsg)
    -> (String -> Result Json.Decode.Error a)
    -> Http.Expect SharedMsg
expectSwiftJsonOrEmpty toMsg parse =
    Http.expectStringResponse toMsg <|
        httpResponseStringToResult <|
            \body ->
                case parse body of
                    Ok value ->
                        Ok value

                    Err err ->
                        Err <| HttpErrorWithBody (Http.BadBody (Json.Decode.errorToString err)) body


{-| The recursive "delete a non-empty container" flow deletes objects a page at a time and re-lists
between pages. This bounds the number of re-list cycles so a persistent server race / error can
never spin forever; each cycle can clear up to a full listing page (`containerPageLimit`) of
objects. See the `ReceiveContainerObjectNamesForDeletion` / `ReceiveDeleteContainerObject` handlers
in `State.State`.
-}
recursiveDeleteMaxCycles : Int
recursiveDeleteMaxCycles =
    100


containerPath : ObjectStorage.ContainerName -> List String
containerPath containerName =
    [ Url.percentEncode containerName ]


requestCreateContainer : Project -> Url -> ObjectStorage.ContainerName -> Cmd SharedMsg
requestCreateContainer project url containerName =
    let
        errorContext =
            ErrorContext
                ("create object storage container " ++ containerName)
                ErrorCrit
                Nothing

        resultToMsg_ result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveCreateContainer errorContext result)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Put
        Nothing
        []
        ( url, containerPath containerName, [] )
        Http.emptyBody
        (expectVoidWithErrorBody resultToMsg_)


requestDeleteContainer : Project -> Url -> ObjectStorage.ContainerName -> Cmd SharedMsg
requestDeleteContainer project url containerName =
    let
        errorContext =
            ErrorContext
                ("delete object storage container " ++ containerName)
                ErrorCrit
                (Just "If the container is not empty, delete its objects first, then try again.")

        resultToMsg_ result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveDeleteContainer errorContext result)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Delete
        Nothing
        []
        ( url, containerPath containerName, [] )
        Http.emptyBody
        (expectSwiftDeleteOrGone resultToMsg_)


{-| List the **object names** in a container for the recursive-delete flow: a plain
`GET <container>?format=json` with **no** `delimiter`, so every ordinary object (at any depth) is
returned as a flat name. `budget` is the remaining re-list cycle allowance, threaded back through
the `ReceiveContainerObjectNamesForDeletion` message so `State.State` can stop a runaway loop.

NOTE (Out Of Scope, per plan): this cannot tell SLO/DLO manifests apart from ordinary objects, so
the recursive delete makes **no large-object guarantee** — segments of a large object may be
orphaned. That is warned about in the UI; large-object cleanup is CLI/rclone territory.

-}
requestContainerObjectNames : Project -> Url -> Time.Posix -> ObjectStorage.ContainerName -> Int -> Cmd SharedMsg
requestContainerObjectNames project url currentTime containerName budget =
    let
        errorContext =
            ErrorContext
                ("list objects in container " ++ containerName ++ " for deletion")
                ErrorCrit
                Nothing

        queryParams =
            [ Url.Builder.string "format" "json"
            , Url.Builder.int "limit" containerPageLimit
            , Url.Builder.int "t" (Time.posixToMillis currentTime)
            ]

        resultToMsg_ result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveContainerObjectNamesForDeletion errorContext containerName budget result)

        parseNames body =
            ObjectStorage.parseObjectListingResponse body
                |> Result.map (\listing -> List.map .name listing.objects)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        []
        ( url, containerPath containerName, queryParams )
        Http.emptyBody
        (expectSwiftJsonOrEmpty resultToMsg_ parseNames)


{-| Delete a single ordinary object with a `DELETE` to `<container>/<object path>`.

`204`/`404` are both success (idempotent). `budget` and `remaining` (the object names still queued
for this cycle) are threaded back through `ReceiveDeleteContainerObject` so the handler can delete
the next one without holding loop state in the model.

-}
requestDeleteContainerObject : Project -> Url -> ObjectStorage.ContainerName -> Int -> List ObjectStorage.ObjectName -> ObjectStorage.ObjectName -> Cmd SharedMsg
requestDeleteContainerObject project url containerName budget remaining objectName =
    let
        errorContext =
            ErrorContext
                ("delete object " ++ objectName ++ " in container " ++ containerName)
                ErrorCrit
                Nothing

        resultToMsg_ result =
            ProjectMsg
                (GetterSetters.projectIdentifier project)
                (ReceiveDeleteContainerObject errorContext containerName budget remaining result)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Delete
        Nothing
        []
        ( url, ObjectStorage.objectPath containerName objectName |> List.map Url.percentEncode, [] )
        Http.emptyBody
        (expectSwiftDeleteOrGone resultToMsg_)


{-| Expect a Swift `DELETE`: any 2xx (`204`) **or** `404` is success (idempotent delete); every
other status is surfaced as an error carrying the response body (e.g. `409` = container not empty).
-}
expectSwiftDeleteOrGone : (Result HttpErrorWithBody () -> SharedMsg) -> Http.Expect SharedMsg
expectSwiftDeleteOrGone toMsg =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.GoodStatus_ _ _ ->
                    Ok ()

                Http.BadStatus_ metadata body ->
                    if metadata.statusCode == 404 then
                        Ok ()

                    else
                        Err <| HttpErrorWithBody (Http.BadStatus metadata.statusCode) body

                Http.BadUrl_ badUrl ->
                    Err <| HttpErrorWithBody (Http.BadUrl badUrl) ""

                Http.Timeout_ ->
                    Err <| HttpErrorWithBody Http.Timeout ""

                Http.NetworkError_ ->
                    Err <| HttpErrorWithBody Http.NetworkError ""
