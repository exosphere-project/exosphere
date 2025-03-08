module Rest.Jetstream2Accounting exposing (requestAllocations)

import Helpers.GetterSetters as GetterSetters
import Helpers.Json exposing (resultToDecoder)
import Helpers.Time exposing (makeIso8601StringToPosixDecoder)
import Http
import Json.Decode as Decode
import Rest.Helpers
import Types.Error
import Types.HelperTypes exposing (HttpRequestMethod(..), Url)
import Types.Jetstream2Accounting
import Types.Project exposing (Project)
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))


requestAllocations : Project -> Url -> Cmd SharedMsg
requestAllocations project url =
    let
        resultToMsg : Result Types.Error.HttpErrorWithBody (List Types.Jetstream2Accounting.Allocation) -> SharedMsg
        resultToMsg result =
            ProjectMsg (GetterSetters.projectIdentifier project) <| ReceiveJetstream2Allocations result
    in
    Rest.Helpers.openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        []
        ( url, [], [] )
        Http.emptyBody
        (Rest.Helpers.expectJsonWithErrorBody resultToMsg allocationsDecoder)


allocationsDecoder : Decode.Decoder (List Types.Jetstream2Accounting.Allocation)
allocationsDecoder =
    Decode.list allocationDecoder


allocationDecoder : Decode.Decoder Types.Jetstream2Accounting.Allocation
allocationDecoder =
    Decode.map8 Types.Jetstream2Accounting.Allocation
        (Decode.field "description" Decode.string)
        (Decode.field "abstract" Decode.string)
        (Decode.field "service_units_allocated" Decode.float)
        (Decode.field "service_units_used" (Decode.nullable Decode.float))
        (Decode.field "start_date" Decode.string |> Decode.andThen makeIso8601StringToPosixDecoder)
        (Decode.field "end_date" Decode.string |> Decode.andThen makeIso8601StringToPosixDecoder)
        (Decode.field "resource" resourceDecoder)
        (Decode.field "active" statusDecoder)


resourceDecoder : Decode.Decoder Types.Jetstream2Accounting.Resource
resourceDecoder =
    Decode.string |> Decode.andThen makeResourceDecoder


makeResourceDecoder : String -> Decode.Decoder Types.Jetstream2Accounting.Resource
makeResourceDecoder str =
    case Types.Jetstream2Accounting.resourceFromStr str of
        Just resource ->
            Decode.succeed resource

        Nothing ->
            Decode.fail "Could not decode Jetstream2 allocation, unrecognized resource type"


statusDecoder : Decode.Decoder Types.Jetstream2Accounting.AllocationStatus
statusDecoder =
    Decode.int
        |> Decode.map parseStatusInt
        |> Decode.andThen resultToDecoder


parseStatusInt : Int -> Result String Types.Jetstream2Accounting.AllocationStatus
parseStatusInt i =
    case i of
        1 ->
            Result.Ok Types.Jetstream2Accounting.Active

        0 ->
            Result.Ok Types.Jetstream2Accounting.Inactive

        _ ->
            Result.Err "unrecognized value for active field"
