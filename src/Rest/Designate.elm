module Rest.Designate exposing
    ( DnsRecordSetRequest
    , receiveCreateRecordSet
    , receiveDeleteRecordSet
    , receiveRecordSets
    , requestCreateRecordSet
    , requestDeleteRecordSet
    , requestRecordSets
    )

import Helpers.GetterSetters
import Helpers.RemoteDataPlusPlus as RDPP
import Http
import Json.Decode as Decode
import Json.Encode as Encode
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


receiveRecordSets : Types.SharedModel.SharedModel -> Types.Project.Project -> List OpenStack.DnsRecordSet.DnsRecordSet -> ( Types.SharedModel.SharedModel, Cmd Types.SharedMsg.SharedMsg )
receiveRecordSets model project dnsRecordSets =
    let
        newProject =
            { project
                | dnsRecordSets =
                    RDPP.RemoteDataPlusPlus
                        (RDPP.DoHave dnsRecordSets model.clientCurrentTime)
                        (RDPP.NotLoading Nothing)
            }

        newModel =
            Helpers.GetterSetters.modelUpdateProject model newProject
    in
    ( newModel, Cmd.none )


{-| Create a RecordSet
-}
type alias DnsRecordSetRequest =
    { zone_id : OpenStack.HelperTypes.Uuid
    , name : String
    , description : String
    , type_ : OpenStack.DnsRecordSet.RecordType
    , records : Set.Set String
    , ttl : Maybe Int
    }


requestCreateRecordSet : Types.Project.Project -> DnsRecordSetRequest -> Cmd Types.SharedMsg.SharedMsg
requestCreateRecordSet project request =
    case project.endpoints.designate of
        Nothing ->
            Cmd.none

        Just designateUrl ->
            let
                encodedRequest =
                    Encode.object
                        [ ( "name", Encode.string request.name )
                        , ( "description", Encode.string request.description )
                        , ( "type", Encode.string <| OpenStack.DnsRecordSet.recordTypeToString request.type_ )
                        , ( "records", Encode.list Encode.string <| Set.toList <| request.records )
                        , ( "ttl", Encode.int <| Maybe.withDefault 3600 <| request.ttl )
                        ]

                errorContext =
                    Types.Error.ErrorContext
                        (String.join " "
                            [ "Create a DNS record"
                            , request.name
                            , OpenStack.DnsRecordSet.recordTypeToString request.type_
                            , String.join ", " <| Set.toList request.records
                            ]
                        )
                        Types.Error.ErrorCrit
                        Nothing

                resultToMsg =
                    Types.SharedMsg.ReceiveCreateDnsRecordSet errorContext
                        >> Types.SharedMsg.ProjectMsg (Helpers.GetterSetters.projectIdentifier project)
            in
            Rest.Helpers.openstackCredentialedRequest
                (Helpers.GetterSetters.projectIdentifier project)
                Types.HelperTypes.Post
                Nothing
                []
                (designateUrl ++ "v2/zones/" ++ request.zone_id ++ "/recordsets")
                (Http.jsonBody encodedRequest)
                (Rest.Helpers.expectJsonWithErrorBody
                    resultToMsg
                    recordSetDecoder
                )


receiveCreateRecordSet : Types.SharedModel.SharedModel -> Types.Project.Project -> OpenStack.DnsRecordSet.DnsRecordSet -> ( Types.SharedModel.SharedModel, Cmd Types.SharedMsg.SharedMsg )
receiveCreateRecordSet model project recordSet =
    let
        newDnsRecordSets =
            project.dnsRecordSets
                |> RDPP.map (List.filter (\e -> e.id /= recordSet.id))
                |> RDPP.map ((::) recordSet)
    in
    ( Helpers.GetterSetters.modelUpdateProject model
        { project | dnsRecordSets = newDnsRecordSets }
    , Cmd.none
    )


{-| Delete a recordset by ID
-}
requestDeleteRecordSet : Types.Project.Project -> OpenStack.DnsRecordSet.DnsRecordSet -> Cmd Types.SharedMsg.SharedMsg
requestDeleteRecordSet project { zone_id, id } =
    case project.endpoints.designate of
        Nothing ->
            Cmd.none

        Just designateUrl ->
            let
                errorContext =
                    Types.Error.ErrorContext
                        ("Delete DNS record " ++ id ++ " in zone " ++ zone_id)
                        Types.Error.ErrorCrit
                        Nothing

                resultToMsg =
                    Types.SharedMsg.ReceiveDeleteDnsRecordSet errorContext
                        >> Types.SharedMsg.ProjectMsg (Helpers.GetterSetters.projectIdentifier project)
            in
            Rest.Helpers.openstackCredentialedRequest
                (Helpers.GetterSetters.projectIdentifier project)
                Types.HelperTypes.Delete
                Nothing
                []
                (designateUrl ++ "v2/zones/" ++ zone_id ++ "/recordsets/" ++ id)
                Http.emptyBody
                (Rest.Helpers.expectJsonWithErrorBody
                    resultToMsg
                    recordSetDecoder
                )


receiveDeleteRecordSet : Types.SharedModel.SharedModel -> Types.Project.Project -> OpenStack.DnsRecordSet.DnsRecordSet -> ( Types.SharedModel.SharedModel, Cmd Types.SharedMsg.SharedMsg )
receiveDeleteRecordSet model project recordSet =
    let
        newDnsRecordSets =
            project.dnsRecordSets
                |> RDPP.map (List.filter (\r -> r.id /= recordSet.id))
    in
    ( Helpers.GetterSetters.modelUpdateProject model
        { project | dnsRecordSets = newDnsRecordSets }
    , Cmd.none
    )


{-| Decode a Designate RecordSet

Example:

```json
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
```

-}
recordSetDecoder : Decode.Decoder OpenStack.DnsRecordSet.DnsRecordSet
recordSetDecoder =
    Decode.map7 OpenStack.DnsRecordSet.DnsRecordSet
        (Decode.field "zone_id" Decode.string)
        (Decode.field "zone_name" Decode.string)
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "type" Decode.string
            |> Decode.map OpenStack.DnsRecordSet.fromStringToRecordType
            |> Decode.andThen
                (\value ->
                    case value of
                        Err _ ->
                            Decode.fail "Failed to parse DNS record type"

                        Ok z ->
                            Decode.succeed z
                )
        )
        (Decode.field "ttl" (Decode.nullable Decode.int))
        (Decode.field "records" (Decode.list Decode.string) |> Decode.map Set.fromList)


recordSetsDecoder : Decode.Decoder (List OpenStack.DnsRecordSet.DnsRecordSet)
recordSetsDecoder =
    Decode.field "recordsets"
        (Decode.list recordSetDecoder)
