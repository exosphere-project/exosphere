module State.Error exposing (processStringError, processSynchronousApiError)

import Http
import Json.Decode as Decode
import OpenStack.Error as OSError
import Style.Toast exposing (toastConfig)
import Task
import Time
import Toasty
import Types.Error exposing (ErrorContext, ErrorLevel(..), HttpErrorWithBody)
import Types.Types
    exposing
        ( LogMessage
        , Model
        , Msg(..)
        , Toast
        )


processStringError : Model -> ErrorContext -> String -> ( Model, Cmd Msg )
processStringError model errorContext error =
    let
        logMessageProto =
            LogMessage
                error
                errorContext

        toast =
            Toast
                errorContext
                error

        cmd =
            Task.perform
                (\posix -> NewLogMessage (logMessageProto posix))
                Time.now
    in
    Toasty.addToastIfUnique toastConfig ToastyMsg toast ( model, cmd )


processSynchronousApiError : Model -> ErrorContext -> HttpErrorWithBody -> ( Model, Cmd Msg )
processSynchronousApiError model errorContext httpError =
    let
        apiErrorDecodeResult =
            Decode.decodeString
                OSError.decodeSynchronousErrorJson
                httpError.body

        formattedError =
            case httpError.error of
                Http.BadStatus code ->
                    case apiErrorDecodeResult of
                        Ok syncApiError ->
                            syncApiError.message
                                ++ " (response code: "
                                ++ String.fromInt syncApiError.code
                                ++ ")"

                        Err _ ->
                            httpError.body
                                ++ " (response code: "
                                ++ String.fromInt code
                                ++ ")"

                _ ->
                    Debug.toString httpError
    in
    processStringError model errorContext formattedError
