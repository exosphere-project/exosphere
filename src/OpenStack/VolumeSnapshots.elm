module OpenStack.VolumeSnapshots exposing
    ( Status(..)
    , VolumeSnapshot
    , isTransitioning
    , volumeSnapshotDecoder
    )

import Helpers.Time exposing (iso8601StringToPosixDecodeError)
import Json.Decode exposing (Decoder, andThen, fail, int, map, string, succeed)
import Json.Decode.Pipeline as Pipeline
import OpenStack.HelperTypes exposing (Uuid)
import Time


type alias VolumeSnapshot =
    { uuid : Uuid
    , name : Maybe String
    , description : String
    , volumeId : String
    , sizeInGiB : Int
    , createdAt : Time.Posix
    , status : Status
    }


type Status
    = Available
    | BackingUp
    | Creating
    | Deleted
    | Deleting
    | Error
    | ErrorDeleting
    | Restoring
    | UnManaging


isTransitioning : VolumeSnapshot -> Bool
isTransitioning { status } =
    List.member status
        [ BackingUp
        , Creating
        , Deleting
        , Restoring
        ]


volumeSnapshotDecoder : Decoder VolumeSnapshot
volumeSnapshotDecoder =
    succeed VolumeSnapshot
        |> Pipeline.required "id" string
        |> Pipeline.optional "name" (string |> map Maybe.Just) Maybe.Nothing
        |> Pipeline.required "description" string
        |> Pipeline.required "volume_id" string
        |> Pipeline.required "size" int
        |> Pipeline.required "created_at" (string |> andThen iso8601StringToPosixDecodeError)
        |> Pipeline.required "status" (string |> andThen statusDecoder)


statusDecoder : String -> Decoder Status
statusDecoder status =
    case status of
        "available" ->
            succeed Available

        "backing-up" ->
            succeed BackingUp

        "creating" ->
            succeed Creating

        "deleted" ->
            succeed Deleted

        "deleting" ->
            succeed Deleting

        "error" ->
            succeed Error

        "error_deleting" ->
            succeed ErrorDeleting

        "restoring" ->
            succeed Restoring

        "unmanaging" ->
            succeed UnManaging

        _ ->
            fail "Unrecognized volume status"
