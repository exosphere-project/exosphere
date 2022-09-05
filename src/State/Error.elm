module State.Error exposing (processConnectivityError, processStringError, processSynchronousApiError)

import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers as Helpers
import Http
import Json.Decode as Decode
import OpenStack.Error as OSError
import Parser exposing ((|.), (|=))
import Rest.Sentry
import Style.Widgets.Toast as Toast
import Types.Error exposing (ErrorContext, ErrorLevel(..), HttpErrorWithBody, Toast)
import Types.SharedModel
    exposing
        ( LogMessage
        , SharedModel
        )
import Types.SharedMsg exposing (SharedMsg(..))


processConnectivityError : SharedModel -> Bool -> ( SharedModel, Cmd SharedMsg )
processConnectivityError model online =
    let
        errorLevel =
            if online then
                ErrorInfo

            else
                ErrorCrit

        error =
            if online then
                "Your internet connection is back online now."

            else
                "Your internet connection appears to be offline."
    in
    processStringError model
        (ErrorContext
            Helpers.specialActionContexts.networkConnectivity
            errorLevel
            Nothing
        )
        error


processStringError : SharedModel -> ErrorContext -> String -> ( SharedModel, Cmd SharedMsg )
processStringError model errorContext error =
    let
        silenceNetworkErrors =
            case model.networkConnectivity of
                Nothing ->
                    False

                Just online ->
                    not online

        isNetworkError =
            error == "NetworkError"

        newErrorContext =
            { actionContext = errorContext.actionContext
            , level =
                -- if we know the network is offline, don't treat each network error as critical
                if silenceNetworkErrors && isNetworkError then
                    ErrorDebug

                else
                    errorContext.level
            , recoveryHint = errorContext.recoveryHint
            }

        logMessage =
            LogMessage
                error
                newErrorContext
                model.clientCurrentTime

        newLogMessages =
            logMessage :: model.logMessages

        newModel =
            { model | logMessages = newLogMessages }

        sentryCmd =
            Rest.Sentry.sendErrorToSentry model newErrorContext error
    in
    case newErrorContext.level of
        ErrorDebug ->
            ( newModel, sentryCmd )

        _ ->
            let
                toast =
                    Toast
                        newErrorContext
                        error
            in
            Toast.showToast toast ToastMsg ( newModel, sentryCmd )


processSynchronousApiError : SharedModel -> ErrorContext -> HttpErrorWithBody -> ( SharedModel, Cmd SharedMsg )
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
