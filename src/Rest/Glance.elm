module Rest.Glance exposing
    ( receiveDeleteImage
    , receiveImages
    , requestChangeVisibility
    , requestDeleteImage
    , requestImage
    , requestImages
    )

import Dict
import Helpers.GetterSetters as GetterSetters
import Helpers.Json exposing (resultToDecoder)
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.Time exposing (makeIso8601StringToPosixDecoder)
import Helpers.Url as UrlHelpers
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode
import OpenStack.Types as OSTypes
import Rest.Helpers exposing (expectJsonWithErrorBody, openstackCredentialedRequest, resultToMsgErrorBody)
import Types.Error exposing (ErrorContext, ErrorLevel(..))
import Types.HelperTypes exposing (HttpRequestMethod(..), MetadataFilter)
import Types.Project exposing (Project)
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))
import Url.Builder



{- HTTP Requests -}


requestImages : SharedModel -> Project -> Cmd SharedMsg
requestImages model project =
    Cmd.batch
        [ requestImagesWithVisibility Nothing model project
        , requestImagesWithVisibility (Just OSTypes.ImagePublic) model project
        , requestImagesWithVisibility (Just OSTypes.ImageCommunity) model project
        , requestImagesWithVisibility (Just OSTypes.ImageShared) model project
        , requestImagesWithVisibility (Just OSTypes.ImagePrivate) model project
        ]


requestImagesWithVisibility : Maybe OSTypes.ImageVisibility -> SharedModel -> Project -> Cmd SharedMsg
requestImagesWithVisibility maybeVisibility model project =
    let
        query =
            Url.Builder.int "limit" 999999
                :: (case maybeVisibility of
                        Nothing ->
                            []

                        Just OSTypes.ImageCommunity ->
                            [ Url.Builder.string "visibility" "community"
                            , Url.Builder.string "status" "active"
                            ]

                        Just OSTypes.ImagePrivate ->
                            [ Url.Builder.string "visibility" "private"
                            , Url.Builder.string "status" "active"
                            ]

                        Just OSTypes.ImagePublic ->
                            [ Url.Builder.string "visibility" "public"
                            , Url.Builder.string "status" "active"
                            ]

                        Just OSTypes.ImageShared ->
                            [ Url.Builder.string "visibility" "shared"
                            , Url.Builder.string "status" "active"
                            ]
                   )

        projectKeystoneHostname =
            UrlHelpers.hostnameFromUrl project.endpoints.keystone

        maybeExcludeFilter : Maybe MetadataFilter
        maybeExcludeFilter =
            Dict.get projectKeystoneHostname model.viewContext.cloudSpecificConfigs
                |> Maybe.andThen (\csc -> csc.imageExcludeFilter)

        errorContext =
            ErrorContext
                ("get a list of images for project \"" ++ project.auth.project.name ++ "\"")
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\images -> ProjectMsg (GetterSetters.projectIdentifier project) <| ReceiveImages images)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        []
        ( project.endpoints.glance, [ "v2", "images" ], query )
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg_
            (makeImagesDecoder maybeExcludeFilter)
        )


requestImage : OSTypes.ImageUuid -> Project -> ErrorContext -> Cmd SharedMsg
requestImage imageId project errorContext =
    let
        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\image -> ProjectMsg (GetterSetters.projectIdentifier project) <| Types.SharedMsg.ReceiveServerImage image)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        []
        ( project.endpoints.glance, [ "v2", "images", imageId ], [] )
        Http.emptyBody
        (Rest.Helpers.expectJsonWithErrorBody
            resultToMsg_
            (makeImageDecoder Nothing)
        )


requestChangeVisibility : Project -> OSTypes.ImageUuid -> OSTypes.ImageVisibility -> Cmd SharedMsg
requestChangeVisibility project imageUuid imageVisibility =
    let
        errorContext =
            ErrorContext
                ("replace image visibility with " ++ OSTypes.imageVisibilityToString imageVisibility)
                ErrorCrit
                Nothing

        operation : Json.Encode.Value
        operation =
            Json.Encode.object
                [ ( "op", Json.Encode.string "replace" )
                , ( "path", Json.Encode.string "/visibility" )
                , ( "value", Json.Encode.string (OSTypes.imageVisibilityToString imageVisibility |> String.toLower) )
                ]

        body =
            Json.Encode.list identity [ operation ]

        resultToMsg_ : Result Types.Error.HttpErrorWithBody a -> SharedMsg
        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\_ ->
                    ProjectMsg
                        (GetterSetters.projectIdentifier project)
                        (ReceiveImageVisibilityChange imageUuid imageVisibility)
                )

        receiveImageVisibilityChangeDecoder : Decode.Decoder (Maybe ProjectSpecificMsgConstructor)
        receiveImageVisibilityChangeDecoder =
            Decode.maybe (Decode.map (\image -> ReceiveImageVisibilityChange image.uuid image.visibility) imageDecoderHelper)
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Patch
        Nothing
        []
        ( project.endpoints.glance, [ "v2", "images", imageUuid ], [] )
        (Http.stringBody "application/openstack-images-v2.1-json-patch" (Json.Encode.encode 0 body))
        (expectJsonWithErrorBody
            resultToMsg_
            receiveImageVisibilityChangeDecoder
        )


requestDeleteImage : Project -> OSTypes.ImageUuid -> Cmd SharedMsg
requestDeleteImage project imageUuid =
    let
        errorContext =
            ErrorContext
                ("delete image with UUID " ++ imageUuid)
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\_ ->
                    ProjectMsg
                        (GetterSetters.projectIdentifier project)
                        (ReceiveDeleteImage imageUuid)
                )
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Delete
        Nothing
        []
        ( project.endpoints.glance, [ "v2", "images", imageUuid ], [] )
        Http.emptyBody
        (Rest.Helpers.expectStringWithErrorBody
            resultToMsg_
        )



{- HTTP Response Handling -}


receiveImages : SharedModel -> Project -> List OSTypes.Image -> ( SharedModel, Cmd SharedMsg )
receiveImages model project newImages =
    let
        insertOrReplaceImage : OSTypes.Image -> List OSTypes.Image -> List OSTypes.Image
        insertOrReplaceImage image imageList =
            image :: List.filter (\image_ -> image_.uuid /= image.uuid) imageList

        updateImages : List OSTypes.Image -> List OSTypes.Image -> List OSTypes.Image
        updateImages newImages_ oldImages =
            List.foldl (\image_ acc -> insertOrReplaceImage image_ acc) oldImages newImages_

        initialImages =
            -- We need initialImages to have content for the case when the request comes
            -- when a project is opened. Otherwise the "Loading ..." spinner will spin forever.
            case project.images.data of
                RDPP.DontHave ->
                    { data = RDPP.DoHave [] model.clientCurrentTime, refreshStatus = RDPP.NotLoading Nothing }

                RDPP.DoHave _ _ ->
                    project.images

        updatedImages : RDPP.RemoteDataPlusPlus Types.Error.HttpErrorWithBody (List OSTypes.Image)
        updatedImages =
            GetterSetters.transformRDPP (updateImages newImages) initialImages

        newProject =
            { project | images = updatedImages }

        newModel =
            GetterSetters.modelUpdateProject model newProject
    in
    ( newModel, Cmd.none )


receiveDeleteImage : SharedModel -> Project -> OSTypes.IpAddressUuid -> ( SharedModel, Cmd SharedMsg )
receiveDeleteImage model project imageUuid =
    case project.images.data of
        RDPP.DoHave images _ ->
            let
                newImages =
                    List.filter (\i -> i.uuid /= imageUuid) images

                newProject =
                    { project
                        | images =
                            RDPP.RemoteDataPlusPlus
                                (RDPP.DoHave newImages model.clientCurrentTime)
                                (RDPP.NotLoading Nothing)
                    }

                newModel =
                    GetterSetters.modelUpdateProject model newProject
            in
            ( newModel, Cmd.none )

        _ ->
            ( model, Cmd.none )



{- JSON Decoders -}


catMaybes : List (Maybe a) -> List a
catMaybes =
    List.filterMap identity


makeImagesDecoder : Maybe MetadataFilter -> Decode.Decoder (List OSTypes.Image)
makeImagesDecoder maybeExcludeFilter =
    Decode.field "images" (Decode.map catMaybes (Decode.list <| makeImageDecoder maybeExcludeFilter))


makeAdditionalPropertiesDecoder : List String -> Decode.Decoder (Dict.Dict String String)
makeAdditionalPropertiesDecoder basePropertyNames =
    let
        fromBool val =
            if val then
                "True"

            else
                "False"

        asString =
            Decode.oneOf
                [ Decode.string
                , Decode.map String.fromInt Decode.int
                , Decode.map String.fromFloat Decode.float
                , Decode.map fromBool Decode.bool
                , Decode.null ""
                , Decode.succeed "IGNORE"
                ]
    in
    Decode.dict asString
        |> Decode.map
            (\someDict ->
                Dict.filter (\k _ -> not (List.member k basePropertyNames)) someDict
            )


makeImageDecoder : Maybe MetadataFilter -> Decode.Decoder (Maybe OSTypes.Image)
makeImageDecoder maybeExcludeFilter =
    case maybeExcludeFilter of
        Nothing ->
            Decode.map Just imageDecoderHelper

        Just excludeFilter ->
            Decode.andThen
                (\maybeFilterValue ->
                    case maybeFilterValue of
                        Nothing ->
                            Decode.map Just imageDecoderHelper

                        Just filterValue ->
                            if filterValue == excludeFilter.filterValue then
                                Decode.succeed Nothing

                            else
                                Decode.map Just imageDecoderHelper
                )
                (Decode.maybe (Decode.field excludeFilter.filterKey Decode.string))


imageDecoderHelper : Decode.Decoder OSTypes.Image
imageDecoderHelper =
    let
        -- Currently hard-coded. TODO: Load these from Glance image schema endpoint
        basePropertyNames =
            [ "architecture"
            , "backend"
            , "checksum"
            , "container_format"
            , "created_at"
            , "direct_url"
            , "disk_format"
            , "file"
            , "id"
            , "image_type"
            , "instance_uuid"
            , "kernel_id"
            , "locations"
            , "min_disk"
            , "min_ram"
            , "name"
            , "os_distro"
            , "os_hash_algo"
            , "os_hash_value"
            , "os_hidden"
            , "os_version"
            , "owner"
            , "protected"
            , "ramdisk_id"
            , "schema"
            , "self"
            , "size"
            , "status"
            , "tags"
            , "updated_at"
            , "virtual_size"
            , "visibility"
            ]
    in
    Decode.succeed
        OSTypes.Image
        |> Pipeline.required "name" Decode.string
        |> Pipeline.required "status" (Decode.string |> Decode.map parseImageStatus |> Decode.andThen resultToDecoder)
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "size" (Decode.oneOf [ Decode.int, Decode.null 0 ] |> Decode.andThen (\i -> Decode.succeed <| Just i))
        |> Pipeline.optional "checksum" (Decode.string |> Decode.andThen (\s -> Decode.succeed <| Just s)) Nothing
        |> Pipeline.optional "disk_format" (Decode.string |> Decode.andThen (\s -> Decode.succeed <| Just s)) Nothing
        |> Pipeline.optional "container_format" (Decode.string |> Decode.andThen (\s -> Decode.succeed <| Just s)) Nothing
        |> Pipeline.required "tags" (Decode.list Decode.string)
        |> Pipeline.required "owner" Decode.string
        |> Pipeline.required "visibility" (Decode.string |> Decode.map parseImageVisibility |> Decode.andThen resultToDecoder)
        |> Pipeline.custom (makeAdditionalPropertiesDecoder basePropertyNames)
        |> Pipeline.required "created_at" (Decode.string |> Decode.andThen makeIso8601StringToPosixDecoder)
        |> Pipeline.optional "os_distro" (Decode.string |> Decode.andThen (\s -> Decode.succeed <| Just s)) Nothing
        |> Pipeline.optional "os_version" (Decode.string |> Decode.andThen (\s -> Decode.succeed <| Just s)) Nothing
        |> Pipeline.required "protected" Decode.bool
        |> Pipeline.optional "image_type" (Decode.string |> Decode.andThen (\s -> Decode.succeed <| Just s)) Nothing
        |> Pipeline.optional "min_disk" (Decode.nullable Decode.int) Nothing


parseImageVisibility : String -> Result String OSTypes.ImageVisibility
parseImageVisibility visibility =
    case visibility of
        "public" ->
            Result.Ok OSTypes.ImagePublic

        "community" ->
            Result.Ok OSTypes.ImageCommunity

        "shared" ->
            Result.Ok OSTypes.ImageShared

        "private" ->
            Result.Ok OSTypes.ImagePrivate

        _ ->
            Result.Err "Unrecognized image visibility value"


parseImageStatus : String -> Result String OSTypes.ImageStatus
parseImageStatus status =
    case status of
        "queued" ->
            Result.Ok OSTypes.ImageQueued

        "saving" ->
            Result.Ok OSTypes.ImageSaving

        "active" ->
            Result.Ok OSTypes.ImageActive

        "killed" ->
            Result.Ok OSTypes.ImageKilled

        "deleted" ->
            Result.Ok OSTypes.ImageDeleted

        "pending_delete" ->
            Result.Ok OSTypes.ImagePendingDelete

        "deactivated" ->
            Result.Ok OSTypes.ImageDeactivated

        _ ->
            Result.Err "Unrecognized image status"
