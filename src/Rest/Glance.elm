module Rest.Glance exposing
    ( decodeImages
    , imageDecoder
    , imageStatusDecoder
    , receiveImages
    , requestImages
    )

import Dict exposing (Dict)
import Helpers.GetterSetters as GetterSetters
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import OpenStack.Types as OSTypes
import Rest.Helpers exposing (expectJsonWithErrorBody, openstackCredentialedRequest, resultToMsgErrorBody)
import Types.Error exposing (ErrorContext, ErrorLevel(..))
import Types.Types
    exposing
        ( FloatingIpState(..)
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


requestImages : Project -> String -> String -> Cmd Msg
requestImages project filterKey filterValue =
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
            (decodeImages filterKey filterValue)
        )



{- HTTP Response Handling -}


receiveImages : Model -> Project -> List OSTypes.Image -> ( Model, Cmd Msg )
receiveImages model project images =
    let
        newProject =
            { project | images = images }

        newModel =
            GetterSetters.modelUpdateProject model newProject
    in
    ( newModel, Cmd.none )



{- JSON Decoders -}


decodeImages : String -> String -> Decode.Decoder (List OSTypes.Image)
decodeImages filterKey filterValue =
    Decode.field "images" (Decode.list (imageDecoder filterKey filterValue))


setFilteredOutBasedOnAttribute : String -> String -> Decode.Decoder Bool
setFilteredOutBasedOnAttribute filterKey filterValue =
    Decode.dict (Decode.oneOf [ Decode.string, Decode.succeed "not a string" ])
        |> Decode.map
            (\someDict ->
                Dict.get filterKey someDict
                    |> Maybe.map (\x -> x == filterValue)
                    |> Maybe.withDefault False
            )


imageDecoder : String -> String -> Decode.Decoder OSTypes.Image
imageDecoder filterKey filterValue =
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
        |> Pipeline.custom (setFilteredOutBasedOnAttribute filterKey filterValue)


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
