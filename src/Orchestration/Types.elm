module Orchestration.Types exposing (PollInterval(..))


type PollInterval
    = -- Something expected to change momentarily / the user is likely waiting for the UI to update.
      Rapid
    | -- The user may not notice a change immediately, but expects the UI to keep up with reality on back-end.
      Regular
    | -- Something that may change on back-end, but the user is unlikely to notice if the UI lags behind
      Seldom
