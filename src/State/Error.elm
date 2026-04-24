module State.Error exposing (formattedError, isNetworkError, processConnectivityError, processProjectStringError, processProjectSynchronousApiError, processStringError, processSynchronousApiError)

import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers as Helpers
import Http
import Json.Decode as Decode
import OpenStack.Error as OSError
import OpenStack.Types exposing (SynchronousAPIError)
import Parser exposing ((|.), (|=))
import Rest.Sentry
import String
import Style.Widgets.Toast as Toast
import Types.Error exposing (ErrorContext, ErrorLevel(..), HttpErrorWithBody, Toast)
import Types.Project exposing (Project)
import Types.SharedModel
    exposing
        ( LogMessage
        , LogMessageProject
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


projectActionContext : Project -> ErrorContext -> ErrorContext
projectActionContext project errorContext =
    let
        projectDescriptor =
            case project.region of
                Just region ->
                    project.auth.project.name ++ " - " ++ region.id

                Nothing ->
                    project.auth.project.name
    in
    if String.contains projectDescriptor errorContext.actionContext then
        errorContext

    else
        { errorContext
            | actionContext =
                errorContext.actionContext
                    ++ " in "
                    ++ projectDescriptor
        }


logMessageProject : Project -> LogMessageProject
logMessageProject project =
    { name = project.auth.project.name
    , region = project.region |> Maybe.map .id
    , uuid = project.auth.project.uuid
    }


isNetworkError : String -> Bool
isNetworkError error =
    error == "NetworkError" || String.contains "Network error" error


processStringError : SharedModel -> ErrorContext -> String -> ( SharedModel, Cmd SharedMsg )
processStringError model errorContext error =
    processStringError_ Nothing model errorContext error


processProjectStringError : SharedModel -> Project -> ErrorContext -> String -> ( SharedModel, Cmd SharedMsg )
processProjectStringError model project errorContext error =
    processStringError_
        (Just <| logMessageProject project)
        model
        (projectActionContext project errorContext)
        error


processStringError_ : Maybe LogMessageProject -> SharedModel -> ErrorContext -> String -> ( SharedModel, Cmd SharedMsg )
processStringError_ maybeProject model errorContext error =
    let
        silenceNetworkErrors =
            case model.networkConnectivity of
                Nothing ->
                    False

                Just online ->
                    not online

        newErrorContext =
            { actionContext = errorContext.actionContext
            , level =
                -- if we know the network is offline, don't treat each network error as critical
                if silenceNetworkErrors && isNetworkError error then
                    ErrorDebug

                else
                    errorContext.level
            , recoveryHint = errorContext.recoveryHint
            }

        logMessage =
            LogMessage
                error
                newErrorContext
                maybeProject
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


suppressErrorBecauseInstanceDeleted : SharedModel -> SynchronousAPIError -> Bool
suppressErrorBecauseInstanceDeleted model syncApiError =
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
    case Parser.run missingInstanceUuidParser syncApiError.message of
        Err _ ->
            -- Error message doesn't match pattern
            False

        Ok errInstanceUuid ->
            not <| GetterSetters.serverPresentNotDeleting model errInstanceUuid


suppressError : SharedModel -> SynchronousAPIError -> Bool
suppressError model syncApiError =
    List.any
        (\checker -> checker model syncApiError)
        [ -- Suppress error if it's about a nonexistent instance.
          suppressErrorBecauseInstanceDeleted
        ]


decodeApiError : { a | body : String } -> Result Decode.Error SynchronousAPIError
decodeApiError { body } =
    Decode.decodeString
        OSError.synchronousErrorJsonDecoder
        body


formattedError : HttpErrorWithBody -> String
formattedError httpError =
    case httpError.error of
        Http.BadStatus code ->
            let
                apiErrorDecodeResult =
                    decodeApiError httpError
            in
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

        Http.NetworkError ->
            "Network error: Unable to submit request."

        _ ->
            Helpers.httpErrorToString httpError.error


processSynchronousApiError : SharedModel -> ErrorContext -> HttpErrorWithBody -> ( SharedModel, Cmd SharedMsg )
processSynchronousApiError model errorContext httpError =
    processSynchronousApiError_ Nothing model errorContext httpError


processProjectSynchronousApiError : SharedModel -> Project -> ErrorContext -> HttpErrorWithBody -> ( SharedModel, Cmd SharedMsg )
processProjectSynchronousApiError model project errorContext httpError =
    processSynchronousApiError_
        (Just <| logMessageProject project)
        model
        (projectActionContext project errorContext)
        httpError


processSynchronousApiError_ : Maybe LogMessageProject -> SharedModel -> ErrorContext -> HttpErrorWithBody -> ( SharedModel, Cmd SharedMsg )
processSynchronousApiError_ maybeProject model errorContext httpError =
    let
        apiErrorDecodeResult =
            decodeApiError httpError

        newErrorContext =
            case apiErrorDecodeResult of
                Ok syncApiError ->
                    if suppressError model syncApiError then
                        { errorContext | level = ErrorDebug }

                    else
                        errorContext

                Err _ ->
                    errorContext
    in
    processStringError_ maybeProject model newErrorContext <| formattedError httpError
