module Rest.Designate exposing (..)

import Helpers.GetterSetters as GetterSetters
import Http
import Json.Decode as Decode
import OpenStack.Types as OSTypes
import Rest.Helpers
    exposing
        ( expectJsonWithErrorBody
        , openstackCredentialedRequest
        , resultToMsgErrorBody
        )
import Types.Error exposing (ErrorContext, ErrorLevel(..))
import Types.HelperTypes exposing (FloatingIpOption(..), HttpRequestMethod(..))
import Types.Project exposing (Project)
import Types.Server exposing (NewServerNetworkOptions(..), ServerOrigin(..))
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), ServerSpecificMsgConstructor(..), SharedMsg(..))


requestRecordSets : Project -> Cmd SharedMsg
requestRecordSets project =
    case project.endpoints.designate of
        Just p ->
            let
                errorContext =
                    ErrorContext
                        ("get a list of record sets for project " ++ project.auth.project.name)
                        ErrorCrit
                        Nothing

                resultToMsg_ =
                    resultToMsgErrorBody
                        errorContext
                        (\groups ->
                            ProjectMsg
                                (GetterSetters.projectIdentifier project)
                                (ReceiveRecordSets groups)
                        )
            in
            openstackCredentialedRequest
                (GetterSetters.projectIdentifier project)
                Get
                Nothing
                []
                (p ++ "/v2/recordsets")
                Http.emptyBody
                (expectJsonWithErrorBody
                    resultToMsg_
                    recordSetsDecoder
                )

        Nothing ->
            Cmd.none


recordSetsDecoder : Decode.Decoder (List OSTypes.RecordSet)
recordSetsDecoder =
    Decode.list
        (Decode.map2 OSTypes.RecordSet
            (Decode.field
                "name"
                Decode.string
            )
            (Decode.maybe (Decode.field "ttl" Decode.string))
        )
