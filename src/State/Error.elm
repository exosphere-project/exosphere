module State.Error exposing (processStringError, processSynchronousApiError)

import Helpers.Helpers as Helpers
import Http
import Json.Decode as Decode
import OpenStack.Error as OSError
import Style.Toast exposing (toastConfig)
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
        logMessage =
            LogMessage
                error
                errorContext
                model.clientCurrentTime

        newLogMessages =
            logMessage :: model.logMessages

        newModel =
            { model | logMessages = newLogMessages }
    in
    case errorContext.level of
        ErrorDebug ->
            ( newModel, Cmd.none )

        _ ->
            let
                toast =
                    Toast
                        errorContext
                        error
            in
            Toasty.addToastIfUnique toastConfig ToastyMsg toast ( newModel, Cmd.none )


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
                    Helpers.httpErrorToString httpError.error
    in
    processStringError model errorContext formattedError
