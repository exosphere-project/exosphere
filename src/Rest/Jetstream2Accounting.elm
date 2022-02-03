module Rest.Jetstream2Accounting exposing (requestAllocation)

import Helpers.GetterSetters as GetterSetters
import Http
import Json.Decode as Decode
import Rest.Helpers
import Types.Error
import Types.HelperTypes exposing (HttpRequestMethod(..), Url)
import Types.Jetstream2Accounting
import Types.Project exposing (Project)
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))


requestAllocation : Project -> Url -> Cmd SharedMsg
requestAllocation project url =
    let
        resultToMsg : Result Types.Error.HttpErrorWithBody (Maybe Types.Jetstream2Accounting.Allocation) -> SharedMsg
        resultToMsg result =
            ProjectMsg (GetterSetters.projectIdentifier project) <| ReceiveJetstream2Allocation result
    in
    Rest.Helpers.openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        url
        Http.emptyBody
        (Rest.Helpers.expectJsonWithErrorBody resultToMsg decodeFirstAllocation)


decodeFirstAllocation : Decode.Decoder (Maybe Types.Jetstream2Accounting.Allocation)
decodeFirstAllocation =
    Decode.list decodeAllocation
        |> Decode.map List.head


decodeAllocation : Decode.Decoder Types.Jetstream2Accounting.Allocation
decodeAllocation =
    Decode.map6 Types.Jetstream2Accounting.Allocation
        (Decode.field "description" Decode.string)
        (Decode.field "abstract" Decode.string)
        (Decode.field "service_units_allocated" Decode.float)
        (Decode.field "service_units_used" (Decode.nullable Decode.float))
        (Decode.field "start_date" Decode.string |> Decode.andThen Rest.Helpers.iso8601StringToPosixDecodeError)
        (Decode.field "end_date" Decode.string |> Decode.andThen Rest.Helpers.iso8601StringToPosixDecodeError)
