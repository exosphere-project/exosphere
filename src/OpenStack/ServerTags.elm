module OpenStack.ServerTags exposing (requestCreateServerTag)

import Http
import OpenStack.Types as OSTypes
import Rest.Helpers exposing (expectStringWithErrorBody, openstackCredentialedRequest, resultToMsgErrorBody)
import Types.Error exposing (ErrorContext, ErrorLevel(..))
import Types.Msg exposing (Msg(..))
import Types.Types exposing (HttpRequestMethod(..), Project)


requestCreateServerTag : Project -> OSTypes.ServerUuid -> String -> Cmd Msg
requestCreateServerTag project serverUuid tag =
    let
        errorContext =
            ErrorContext
                ("create a server tag for server with UUID" ++ serverUuid)
                ErrorCrit
                Nothing
    in
    openstackCredentialedRequest
        project.auth.project.uuid
        Put
        (Just "compute 2.26")
        (project.endpoints.nova ++ "/servers/" ++ serverUuid ++ "/tags/" ++ tag)
        Http.emptyBody
        (expectStringWithErrorBody
            (resultToMsgErrorBody errorContext (\_ -> NoOp))
        )
