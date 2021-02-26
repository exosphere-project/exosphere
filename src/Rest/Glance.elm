module Rest.Glance exposing
    ( decodeImages
    , imageDecoder
    , imageStatusDecoder
    , imageVisibilityDecoder
    , receiveImages
    , requestImages
    )

import Dict
import Helpers.GetterSetters as GetterSetters
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import OpenStack.Types as OSTypes
import Rest.Helpers exposing (expectJsonWithErrorBody, openstackCredentialedRequest, resultToMsgErrorBody)
import Types.Error exposing (ErrorContext, ErrorLevel(..))
import Types.Types
    exposing
        ( ExcludeFilter
        , FloatingIpState(..)
        , HttpRequestMethod(..)
        , Model
        , Msg(..)
        , NewServerNetworkOptions(..)
        , Project
        , ProjectSpecificMsgConstructor(..)
        , ProjectViewConstructor(..)
        , ServerOrigin(..)
        , ViewState(..)
        )



{- HTTP Requests -}


requestImages : Project -> Maybe ExcludeFilter -> Cmd Msg
requestImages project maybeExcludeFilter =
    let
        errorContext =
            ErrorContext
                ("get a list of images for project \"" ++ project.auth.project.name ++ "\"")
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
                (\images -> ProjectMsg project.auth.project.uuid <| ReceiveImages images)
    in
    openstackCredentialedRequest
        project
        Get
        Nothing
        (project.endpoints.glance ++ "/v2/images?limit=999999")
        Http.emptyBody
        (expectJsonWithErrorBody
            resultToMsg_
            (decodeImages maybeExcludeFilter)
        )



{- HTTP Response Handling -}


receiveImages : Model -> Project -> List OSTypes.Image -> ( Model, Cmd Msg )
receiveImages model project images =
    let
        set_image_featured_flag : OSTypes.Image -> OSTypes.Image
        set_image_featured_flag image =
            let
                new_featured_flag =
                    case model.style.featuredImageNamePrefix of
                        Nothing ->
                            False

                        Just featuredImageNamePrefix ->
                            case ( String.startsWith featuredImageNamePrefix image.name, image.visibility ) of
                                ( True, OSTypes.ImagePublic ) ->
                                    True

                                ( _, _ ) ->
                                    False
            in
            { image | featured = new_featured_flag }

        images_with_featured_flags =
            List.map set_image_featured_flag images

        newProject =
            { project | images = images_with_featured_flags }

        newModel =
            GetterSetters.modelUpdateProject model newProject
    in
    ( newModel, Cmd.none )



{- JSON Decoders -}


decodeImages : Maybe ExcludeFilter -> Decode.Decoder (List OSTypes.Image)
decodeImages maybeExcludeFilter =
    Decode.field "images" (Decode.list (imageDecoder maybeExcludeFilter))


setFilteredOutBasedOnAttribute : Maybe ExcludeFilter -> Decode.Decoder Bool
setFilteredOutBasedOnAttribute maybeExcludeFilter =
    case maybeExcludeFilter of
        Just excludeFilter ->
            Decode.dict (Decode.oneOf [ Decode.string, Decode.succeed "not a string" ])
                |> Decode.map
                    (\someDict ->
                        Dict.get excludeFilter.filterKey someDict
                            |> Maybe.map (\x -> x == excludeFilter.filterValue)
                            |> Maybe.withDefault False
                    )

        Nothing ->
            Decode.succeed False


decodeAdditionalProperties : List String -> Decode.Decoder (Dict.Dict String String)
decodeAdditionalProperties basePropertyNames =
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
                , Decode.map (\_ -> "IGNORE") (Decode.succeed "IGNORE")
                ]
    in
    Decode.dict asString
        |> Decode.map
            (\someDict ->
                Dict.filter (\k _ -> not (List.member k basePropertyNames)) someDict
            )


imageDecoder : Maybe ExcludeFilter -> Decode.Decoder OSTypes.Image
imageDecoder maybeExcludeFilter =
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
    Decode.succeed OSTypes.Image
        |> Pipeline.required "name" Decode.string
        |> Pipeline.required "status" (Decode.string |> Decode.andThen imageStatusDecoder)
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "size" (Decode.oneOf [ Decode.int, Decode.null 0 ] |> Decode.andThen (\i -> Decode.succeed <| Just i))
        |> Pipeline.optional "checksum" (Decode.string |> Decode.andThen (\s -> Decode.succeed <| Just s)) Nothing
        |> Pipeline.optional "disk_format" (Decode.string |> Decode.andThen (\s -> Decode.succeed <| Just s)) Nothing
        |> Pipeline.optional "container_format" (Decode.string |> Decode.andThen (\s -> Decode.succeed <| Just s)) Nothing
        |> Pipeline.required "tags" (Decode.list Decode.string)
        |> Pipeline.required "owner" Decode.string
        |> Pipeline.custom (setFilteredOutBasedOnAttribute maybeExcludeFilter)
        |> Pipeline.required "visibility" (Decode.string |> Decode.andThen imageVisibilityDecoder)
        |> Pipeline.custom (Decode.succeed False)
        |> Pipeline.custom (decodeAdditionalProperties basePropertyNames)


imageVisibilityDecoder : String -> Decode.Decoder OSTypes.ImageVisibility
imageVisibilityDecoder visibility =
    case visibility of
        "public" ->
            Decode.succeed OSTypes.ImagePublic

        "community" ->
            Decode.succeed OSTypes.ImageCommunity

        "shared" ->
            Decode.succeed OSTypes.ImageShared

        "private" ->
            Decode.succeed OSTypes.ImagePrivate

        _ ->
            Decode.fail "Unrecognized image visibility value"


imageStatusDecoder : String -> Decode.Decoder OSTypes.ImageStatus
imageStatusDecoder status =
    case status of
        "queued" ->
            Decode.succeed OSTypes.ImageQueued

        "saving" ->
            Decode.succeed OSTypes.ImageSaving

        "active" ->
            Decode.succeed OSTypes.ImageActive

        "killed" ->
            Decode.succeed OSTypes.ImageKilled

        "deleted" ->
            Decode.succeed OSTypes.ImageDeleted

        "pending_delete" ->
            Decode.succeed OSTypes.ImagePendingDelete

        "deactivated" ->
            Decode.succeed OSTypes.ImageDeactivated

        _ ->
            Decode.fail "Unrecognized image status"
