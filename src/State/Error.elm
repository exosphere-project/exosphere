module State.Error exposing (processStringError, processSynchronousApiError)

import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers as Helpers
import Http
import Json.Decode as Decode
import OpenStack.Error as OSError
import Parser exposing ((|.), (|=))
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

        suppressErrorBecauseinstanceDeleted : String -> Bool
        suppressErrorBecauseinstanceDeleted message =
            -- Determine if the error should be suppressed because it says an instance could not be found, and we are trying to delete that instance (or it is absent from the model)
            let
                missingInstanceUuidParser =
                    Parser.succeed identity
                        |. Parser.token "Instance"
                        |. Parser.spaces
                        |= Helpers.naiveUuidParser
                        |. Parser.spaces
                        |. Parser.token "could not be found."
                        |. Parser.end
            in
            case Parser.run missingInstanceUuidParser message of
                Err _ ->
                    -- Error message doesn't match pattern
                    False

                Ok errInstanceUuid ->
                    not <| GetterSetters.serverPresentNotDeleting model errInstanceUuid

        newErrorContext =
            -- Suppress error if it's about a nonexistent instance
            case apiErrorDecodeResult of
                Ok syncApiError ->
                    if suppressErrorBecauseinstanceDeleted syncApiError.message then
                        { errorContext | level = ErrorDebug }

                    else
                        errorContext

                Err _ ->
                    errorContext

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
    processStringError model newErrorContext formattedError
