module Rest.Sentry exposing (sendErrorToSentry)

import Dict
import Http
import Sentry
import Task
import Types.Error as Error
import Types.HelperTypes as HelperTypes
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg as SharedMsg
import UUID


sendErrorToSentry : SharedModel -> Error.ErrorContext -> String -> Cmd SharedMsg.SharedMsg
sendErrorToSentry sharedModel errorContext errorStr =
    case sharedModel.sentryConfig of
        Nothing ->
            -- app is not configured to report errors to Sentry, so do nothing
            Cmd.none

        Just sentryConfig ->
            sendErrorToSentry_ sentryConfig errorContext errorStr
                |> Task.attempt (\_ -> SharedMsg.NoOp)


sendErrorToSentry_ : HelperTypes.SentryConfig -> Error.ErrorContext -> String -> Task.Task Http.Error UUID.UUID
sendErrorToSentry_ sentryConfig errorContext errorStr =
    let
        config =
            Sentry.config
                { publicKey = sentryConfig.dsnPublicKey
                , host = sentryConfig.dsnHost
                , projectId = sentryConfig.dsnProjectId
                }

        sentryLevel =
            exoLevelToSentryLevel errorContext.level

        sentryContext =
            errorContext.actionContext
    in
    Sentry.send
        config
        sentryLevel
        (Sentry.releaseVersion sentryConfig.releaseVersion)
        (Sentry.environment sentryConfig.environmentName)
        (Sentry.context sentryContext)
        errorStr
        Dict.empty


exoLevelToSentryLevel : Error.ErrorLevel -> Sentry.Level
exoLevelToSentryLevel exoLevel =
    case exoLevel of
        Error.ErrorDebug ->
            Sentry.Debug

        Error.ErrorInfo ->
            Sentry.Info

        Error.ErrorWarn ->
            Sentry.Warning

        Error.ErrorCrit ->
            Sentry.Error
