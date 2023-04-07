module Rest.Designate exposing (receiveRecordSets, requestRecordSets)

import Helpers.GetterSetters
import Helpers.RemoteDataPlusPlus
import Http
import Json.Decode
import OpenStack.DnsRecordSet
import Rest.Helpers
import Set
import Types.Error
import Types.HelperTypes
import Types.Project
import Types.SharedModel
import Types.SharedMsg


requestRecordSets : Types.Project.Project -> Cmd Types.SharedMsg.SharedMsg
requestRecordSets project =
    case project.endpoints.designate of
        Just designateUrl ->
            let
                errorContext =
                    Types.Error.ErrorContext
                        ("get a list of record sets for project " ++ project.auth.project.name)
                        Types.Error.ErrorCrit
                        Nothing

                resultToMsg_ =
                    Rest.Helpers.resultToMsgErrorBody
                        errorContext
                        (\groups ->
                            Types.SharedMsg.ProjectMsg
                                (Helpers.GetterSetters.projectIdentifier project)
                                (Types.SharedMsg.ReceiveDnsRecordSets groups)
                        )
            in
            Rest.Helpers.openstackCredentialedRequest
                (Helpers.GetterSetters.projectIdentifier project)
                Types.HelperTypes.Get
                Nothing
                []
                (designateUrl ++ "/recordsets")
                Http.emptyBody
                (Rest.Helpers.expectJsonWithErrorBody
                    resultToMsg_
                    recordSetsDecoder
                )

        Nothing ->
            Cmd.none


receiveRecordSets : Types.SharedModel.SharedModel -> Types.Project.Project -> List OpenStack.DnsRecordSet.DnsRecordSet -> ( Types.SharedModel.SharedModel, Cmd Types.SharedMsg.SharedMsg )
receiveRecordSets model project dnsRecordSets =
    let
        newProject =
            { project
                | dnsRecordSets =
                    Helpers.RemoteDataPlusPlus.RemoteDataPlusPlus
                        (Helpers.RemoteDataPlusPlus.DoHave dnsRecordSets model.clientCurrentTime)
                        (Helpers.RemoteDataPlusPlus.NotLoading Nothing)
            }

        newModel =
            Helpers.GetterSetters.modelUpdateProject model newProject
    in
    ( newModel, Cmd.none )


recordSetsDecoder : Json.Decode.Decoder (List OpenStack.DnsRecordSet.DnsRecordSet)
recordSetsDecoder =
    Json.Decode.field "recordsets"
        (Json.Decode.list
            (Json.Decode.map5
                (\id name type_ ttl records ->
                    { id = id
                    , name = name
                    , type_ = type_
                    , ttl = ttl
                    , records = Set.fromList records
                    }
                )
                (Json.Decode.field "id" Json.Decode.string)
                (Json.Decode.field "name" Json.Decode.string)
                (Json.Decode.field "type" Json.Decode.string
                    |> Json.Decode.map OpenStack.DnsRecordSet.fromStringToRecordType
                    |> Json.Decode.andThen
                        (\value ->
                            case value of
                                Err _ ->
                                    Json.Decode.fail "Failed to parse dns record type"

                                Ok z ->
                                    Json.Decode.succeed z
                        )
                )
                (Json.Decode.field "ttl" (Json.Decode.nullable Json.Decode.int))
                (Json.Decode.field "records" (Json.Decode.list Json.Decode.string))
            )
        )
