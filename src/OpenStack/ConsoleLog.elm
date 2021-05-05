module OpenStack.ConsoleLog exposing (requestConsoleLog)

import Http
import Json.Decode
import Json.Encode
import Rest.Helpers exposing (expectJsonWithErrorBody, openstackCredentialedRequest)
import Types.Error exposing (ErrorContext, ErrorLevel(..))
import Types.Types
    exposing
        ( HttpRequestMethod(..)
        , Msg(..)
        , Project
        , ProjectSpecificMsgConstructor(..)
        , ProjectViewConstructor(..)
        , Server
        , ServerSpecificMsgConstructor(..)
        )


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
                ErrorCrit
                Nothing

        resultToMsg result =
            ProjectMsg project.auth.project.uuid <|
                ServerMsg server.osProps.uuid <|
                    ReceiveConsoleLog errorContext result
    in
    openstackCredentialedRequest
        project
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
