module Rest.Jetstream2Accounting exposing (requestAllocations)

import Helpers.GetterSetters as GetterSetters
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
        url
        Http.emptyBody
        (Rest.Helpers.expectJsonWithErrorBody resultToMsg decodeAllocations)


decodeAllocations : Decode.Decoder (List Types.Jetstream2Accounting.Allocation)
decodeAllocations =
    Decode.list decodeAllocation


decodeAllocation : Decode.Decoder Types.Jetstream2Accounting.Allocation
decodeAllocation =
    Decode.map7 Types.Jetstream2Accounting.Allocation
        (Decode.field "description" Decode.string)
        (Decode.field "abstract" Decode.string)
        (Decode.field "service_units_allocated" Decode.float)
        (Decode.field "service_units_used" (Decode.nullable Decode.float))
        (Decode.field "start_date" Decode.string |> Decode.andThen Rest.Helpers.iso8601StringToPosixDecodeError)
        (Decode.field "end_date" Decode.string |> Decode.andThen Rest.Helpers.iso8601StringToPosixDecodeError)
        (Decode.field "resource" decodeResource)


decodeResource : Decode.Decoder Types.Jetstream2Accounting.Resource
decodeResource =
    Decode.string |> Decode.andThen decodeResource_


decodeResource_ : String -> Decode.Decoder Types.Jetstream2Accounting.Resource
decodeResource_ str =
    case Types.Jetstream2Accounting.resourceFromStr str of
        Just resource ->
            Decode.succeed resource

        Nothing ->
            Decode.fail "Could not decode Jetstream2 allocation, unrecognized resource type"
