module OpenStack.ServerPassword exposing (requestClearServerPassword, requestServerPassword)

import Helpers.Error exposing (ErrorContext, ErrorLevel(..))
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
import Types.Types exposing (HttpRequestMethod(..), Msg(..), Project, ProjectSpecificMsgConstructor(..))


requestServerPassword : Project -> OSTypes.ServerUuid -> Cmd Msg
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
                    ProjectMsg
                        project.auth.project.uuid
                        (ReceiveServerPassword serverUuid serverPassword)
    in
    openstackCredentialedRequest
        project
        Get
        Nothing
        (project.endpoints.nova ++ "/servers/" ++ serverUuid ++ "/os-server-password")
        Http.emptyBody
        (expectJsonWithErrorBody resultToMsg_ decodeServerPassword)


requestClearServerPassword : Project -> OSTypes.ServerUuid -> Cmd Msg
requestClearServerPassword project serverUuid =
    let
        errorContext =
            ErrorContext
                ("clear password for server with UUID" ++ serverUuid)
                ErrorCrit
                Nothing
    in
    openstackCredentialedRequest
        project
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
