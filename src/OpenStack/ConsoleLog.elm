module OpenStack.ConsoleLog exposing (requestConsoleLog)

import Http
import Json.Decode
import Json.Encode
import Rest.Helpers exposing (expectJsonWithErrorBody, openstackCredentialedRequest)
import Types.Error exposing (ErrorContext, ErrorLevel(..))
import Types.Msg exposing (Msg(..), ProjectSpecificMsgConstructor(..), ServerSpecificMsgConstructor(..))
import Types.Project exposing (Project)
import Types.Server exposing (Server)
import Types.Types exposing (HttpRequestMethod(..))


requestConsoleLog : Project -> Server -> Maybe Int -> Cmd Msg
requestConsoleLog project server maybeLength =
    let
        lengthJson =
            case maybeLength of
                Nothing ->
                    []

                Just length ->
                    [ ( "length"
                      , Json.Encode.int length
                      )
                    ]

        body =
            Json.Encode.object
                [ ( "os-getConsoleOutput"
                  , Json.Encode.object
                        lengthJson
                  )
                ]

        errorContext =
            ErrorContext
                ("request console log for server " ++ server.osProps.uuid)
                ErrorDebug
                Nothing

        resultToMsg result =
            ProjectMsg project.auth.project.uuid <|
                ServerMsg server.osProps.uuid <|
                    ReceiveConsoleLog errorContext result
    in
    openstackCredentialedRequest
        project.auth.project.uuid
        Post
        Nothing
        (project.endpoints.nova ++ "/servers/" ++ server.osProps.uuid ++ "/action")
        (Http.jsonBody body)
        (expectJsonWithErrorBody
            resultToMsg
            decodeConsoleLog
        )


decodeConsoleLog : Json.Decode.Decoder String
decodeConsoleLog =
    Json.Decode.field "output" Json.Decode.string
