module OpenStack.ConsoleLog exposing (requestConsoleLog, requestUserConsoleLog)

import Helpers.GetterSetters as GetterSetters
import Http
import Json.Decode
import Json.Encode
import Rest.Helpers exposing (expectJsonWithErrorBody, openstackCredentialedRequest)
import Types.Error exposing (ErrorContext, ErrorLevel(..))
import Types.HelperTypes exposing (HttpRequestMethod(..))
import Types.Project exposing (Project)
import Types.Server exposing (Server)
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), ServerSpecificMsgConstructor(..), SharedMsg(..))


requestConsoleLog : Project -> Server -> Maybe Int -> Cmd SharedMsg
requestConsoleLog project server maybeLength =
    let
        errorContext =
            ErrorContext
                ("request console log for server " ++ server.osProps.uuid)
                ErrorDebug
                Nothing

        resultToMsg result =
            ProjectMsg (GetterSetters.projectIdentifier project) <|
                ServerMsg server.osProps.uuid <|
                    ReceiveConsoleLog errorContext result
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Post
        Nothing
        []
        ( project.endpoints.nova, [ "servers", server.osProps.uuid, "action" ], [] )
        (Http.jsonBody <| consoleLogRequestBody maybeLength)
        (expectJsonWithErrorBody
            resultToMsg
            consoleLogDecoder
        )


requestUserConsoleLog : Project -> String -> Maybe Int -> Cmd SharedMsg
requestUserConsoleLog project serverUuid maybeLength =
    let
        errorContext =
            ErrorContext
                ("get console log for server " ++ serverUuid)
                ErrorDebug
                Nothing

        resultToMsg result =
            ProjectMsg (GetterSetters.projectIdentifier project) <|
                ReceiveUserRequestedConsoleLog serverUuid errorContext result
    in
    openstackCredentialedRequest
        (GetterSetters.projectIdentifier project)
        Post
        Nothing
        []
        ( project.endpoints.nova, [ "servers", serverUuid, "action" ], [] )
        (Http.jsonBody <| consoleLogRequestBody maybeLength)
        (expectJsonWithErrorBody
            resultToMsg
            consoleLogDecoder
        )


consoleLogRequestBody : Maybe Int -> Json.Encode.Value
consoleLogRequestBody maybeLength =
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
    in
    Json.Encode.object
        [ ( "os-getConsoleOutput"
          , Json.Encode.object
                lengthJson
          )
        ]


consoleLogDecoder : Json.Decode.Decoder String
consoleLogDecoder =
    Json.Decode.field "output" Json.Decode.string
