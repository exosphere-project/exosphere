module OpenStack.ServerTags exposing (requestCreateServerTag)

import Helpers.GetterSetters as GetterSetters
import Http
import OpenStack.Types as OSTypes
import Rest.Helpers exposing (expectStringWithErrorBody, openstackCredentialedRequest, resultToMsgErrorBody)
import Types.Error exposing (ErrorContext, ErrorLevel(..))
import Types.HelperTypes exposing (HttpRequestMethod(..))
import Types.Project exposing (Project)
import Types.SharedMsg exposing (SharedMsg(..))


requestCreateServerTag : Project -> OSTypes.ServerUuid -> String -> Cmd SharedMsg
requestCreateServerTag project serverUuid tag =
    let
        errorContext =
            ErrorContext
                ("create a server tag for server with UUID" ++ serverUuid)
                ErrorCrit
                Nothing
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Put
        (Just "compute 2.26")
        (project.endpoints.nova ++ "/servers/" ++ serverUuid ++ "/tags/" ++ tag)
        Http.emptyBody
        (expectStringWithErrorBody
            (resultToMsgErrorBody errorContext (\_ -> NoOp))
        )
