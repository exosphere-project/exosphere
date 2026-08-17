module Tests.CloudShield.Fixtures exposing (cardViewConfig)

{-| Shared fixtures for the CloudShield card tests.

`CloudShield.Card.ViewConfig` is the whole host-to-card interface, so it is wide by nature and grows
whenever the host learns to project something new. One definition of its quiet default lives here so
that growth is a one-line change rather than an edit to every test module, and so each test can name
only the fields it is actually about.

-}

import CloudShield.Card as Card
import Time


{-| An approved card in its quiet state: history loaded and empty, no run, no session, nothing in
flight. Override with record update, e.g. `{ cardViewConfig | requestBusy = True }`.
-}
cardViewConfig : Card.ViewConfig
cardViewConfig =
    { approved = True
    , sourceName = "cloudshield-vm"
    , manifest = Card.ManifestLoading
    , transportLabel = Nothing
    , scanTimer = Nothing
    , transportWarning = Nothing
    , runOutcome = Nothing
    , statusOverride = Nothing
    , queuedTargets = []
    , results = Nothing
    , history = { rows = [], loading = False, loaded = True }
    , activeResultId = Nothing
    , pendingResultId = Nothing
    , erroredResultId = Nothing
    , expiredResultId = Nothing
    , removingResultId = Nothing
    , removeError = Nothing
    , requestBusy = False
    , scanBusy = False
    , cancellableRun = Nothing
    , stoppingTargetId = Nothing
    , sessionOpen = False
    , allowedIframeOrigins = []
    , embedUrl = ""
    , embedState = Card.EmbedIdle
    , demoIframeUrl = Nothing
    , health = Nothing
    , publisherRunning = True
    , publisherChecking = False
    , now = Time.millisToPosix 0
    }
