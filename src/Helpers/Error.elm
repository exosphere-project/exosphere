module Helpers.Error exposing
    ( ErrorContext
    , ErrorLevel(..)
    , HttpErrorWithBody
    , toFriendlyErrorLevel
    )

import Http


type ErrorLevel
    = ErrorDebug
    | ErrorInfo
    | ErrorWarn
    | ErrorCrit


type alias ErrorContext =
    { -- actionContext is a concise description of error-prone action written in imperative tense, first letter not capitalized
      -- e.g. "log into OpenStack"
      actionContext : String
    , level : ErrorLevel

    -- recoveryHint is optional guidance for user to recover from error, e.g. "Make sure your credentials and password are correct."
    , recoveryHint : Maybe String
    }


type alias HttpErrorWithBody =
    { error : Http.Error
    , body : String
    }


toFriendlyErrorLevel : ErrorLevel -> String
toFriendlyErrorLevel level =
    case level of
        ErrorDebug ->
            "debug"

        ErrorInfo ->
            "info"

        ErrorWarn ->
            "warn"

        ErrorCrit ->
            "critical"
