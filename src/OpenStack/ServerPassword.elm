module OpenStack.ServerPassword exposing (requestClearServerPassword, requestServerPassword)

import Http
import Json.Decode as Decode
import OpenStack.Types as OSTypes
import Rest.Helpers
    exposing
        ( expectJsonWithErrorBody
        , expectStringWithErrorBody
        , openstackCredentialedRequest
        , resultToMsgErrorBody
        )
import Types.Error exposing (ErrorContext, ErrorLevel(..))
import Types.HelperTypes exposing (HttpRequestMethod(..))
import Types.Msg exposing (ProjectSpecificMsgConstructor(..), ServerSpecificMsgConstructor(..), SharedMsg(..))
import Types.Project exposing (Project)


requestServerPassword : Project -> OSTypes.ServerUuid -> Cmd SharedMsg
requestServerPassword project serverUuid =
    let
        errorContext =
            ErrorContext
                ("get password for server with UUID" ++ serverUuid)
                ErrorCrit
                Nothing

        resultToMsg_ =
            resultToMsgErrorBody
                errorContext
            <|
                \serverPassword ->
                    ProjectMsg project.auth.project.uuid <|
                        ServerMsg serverUuid <|
                            ReceiveServerPassword serverPassword
    in
    openstackCredentialedRequest
        project.auth.project.uuid
        Get
        Nothing
        (project.endpoints.nova ++ "/servers/" ++ serverUuid ++ "/os-server-password")
        Http.emptyBody
        (expectJsonWithErrorBody resultToMsg_ decodeServerPassword)


requestClearServerPassword : Project -> OSTypes.ServerUuid -> Cmd SharedMsg
requestClearServerPassword project serverUuid =
    let
        errorContext =
            ErrorContext
                ("clear password for server with UUID" ++ serverUuid)
                ErrorCrit
                Nothing
    in
    openstackCredentialedRequest
        project.auth.project.uuid
        Delete
        Nothing
        (project.endpoints.nova ++ "/servers/" ++ serverUuid ++ "/os-server-password")
        Http.emptyBody
        (expectStringWithErrorBody
            (resultToMsgErrorBody errorContext (\_ -> NoOp))
        )


decodeServerPassword : Decode.Decoder OSTypes.ServerPassword
decodeServerPassword =
    Decode.field "password" Decode.string
