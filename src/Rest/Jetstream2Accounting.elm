module Rest.Jetstream2Accounting exposing (requestAllocation)

import Helpers.GetterSetters as GetterSetters
import Http
import Json.Decode as Decode
import Rest.Helpers
import Types.Error
import Types.HelperTypes exposing (HttpRequestMethod(..), Url)
import Types.Jetstream2Accounting
import Types.Project exposing (Project)
import Types.SharedMsg exposing (SharedMsg)



-- TODO consider another function to request all allocations using an unscoped token


requestAllocation : Project -> Url -> (Result Types.Error.HttpErrorWithBody Types.Jetstream2Accounting.Allocation -> SharedMsg) -> Cmd SharedMsg
requestAllocation project url resultToMsg =
    Rest.Helpers.openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Get
        Nothing
        url
        Http.emptyBody
        (Rest.Helpers.expectJsonWithErrorBody resultToMsg decodeFirstAllocation)


decodeAllocations : Decode.Decoder (List Types.Jetstream2Accounting.Allocation)
decodeAllocations =
    Decode.list decodeAllocation


decodeFirstAllocation : Decode.Decoder Types.Jetstream2Accounting.Allocation
decodeFirstAllocation =
    Decode.list decodeAllocation
        |> Decode.andThen
            (\allocations ->
                case List.head allocations of
                    Just firstAllocation ->
                        Decode.succeed firstAllocation

                    Nothing ->
                        Decode.fail "Could not decode first allocation in list"
            )


decodeAllocation : Decode.Decoder Types.Jetstream2Accounting.Allocation
decodeAllocation =
    Decode.map6 Types.Jetstream2Accounting.Allocation
        (Decode.field "description" Decode.string)
        (Decode.field "abstract" Decode.string)
        (Decode.field "service_units_allocated" Decode.float)
        (Decode.field "service_units_used" (Decode.nullable Decode.float))
        (Decode.field "start_date" Decode.string |> Decode.andThen Rest.Helpers.iso8601StringToPosixDecodeError)
        (Decode.field "end_date" Decode.string |> Decode.andThen Rest.Helpers.iso8601StringToPosixDecodeError)
