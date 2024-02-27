module Rest.Designate exposing (receiveRecordSets, requestRecordSets)

import Helpers.GetterSetters
import Helpers.RemoteDataPlusPlus
import Http
import Json.Decode
import OpenStack.DnsRecordSet
import OpenStack.HelperTypes
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
                (designateUrl ++ "v2/recordsets?limit=1000")
                Http.emptyBody
                (Rest.Helpers.expectJsonWithErrorBody
                    resultToMsg_
                    recordSetsDecoder
                )

        Nothing ->
            Cmd.none


{-| Create a RecordSet

    {
        "action": "NONE",
        "created_at": "2024-02-27T18:52:08.000000",
        "description": null,
        "id": "493151ca-6140-4f53-9b35-b2ed7ec6b22f",
        "name": "jointly-finer-bluebird.ccr190024.projects.jetstream-cloud.org.",
        "project_id": "95e98332bb62488eba5fee7c5849e13c",
        "records": "149.165.169.236",
        "status": "ACTIVE",
        "ttl": null,
        "type": "A",
        "updated_at": null,
        "version": 1,
        "zone_id": "01dd555c-87bc-4c03-9b34-642fe66a428a",
        "zone_name": "ccr190024.projects.jetstream-cloud.org."
    }

-}
requestCreateRecordSet : Types.Project.Project -> {} -> Cmd Types.SharedMsg.SharedMsg
requestCreateRecordSet project recordSetData =
    Cmd.none


{-| Delete a recordset by ID
-}
requestDeleteRecordSet : Types.Project.Project -> OpenStack.HelperTypes.Uuid -> OpenStack.HelperTypes.Uuid -> Cmd Types.SharedMsg.SharedMsg
requestDeleteRecordSet _ _ _ =
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


recordSetDecoder : Json.Decode.Decoder OpenStack.DnsRecordSet.DnsRecordSet
recordSetDecoder =
    Json.Decode.map6 OpenStack.DnsRecordSet.DnsRecordSet
        (Json.Decode.field "zone_id" Json.Decode.string)
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
        (Json.Decode.field "records"
            (Json.Decode.list Json.Decode.string)
            |> Json.Decode.map Set.fromList
        )


recordSetsDecoder : Json.Decode.Decoder (List OpenStack.DnsRecordSet.DnsRecordSet)
recordSetsDecoder =
    Json.Decode.field "recordsets"
        (Json.Decode.list recordSetDecoder)
