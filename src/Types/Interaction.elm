module Types.Interaction exposing (Interaction(..), InteractionStatus(..))

import Types.HelperTypes as HelperTypes


type Interaction
    = GuacTerminal
    | GuacDesktop
    | CockpitDashboard
    | CockpitTerminal
    | NativeSSH
    | Console


type InteractionStatus
    = Unavailable InteractionStatusReason
    | Loading
    | Ready InteractionUrl
    | Error InteractionStatusReason
    | Hidden


type alias InteractionStatusReason =
    String


type alias InteractionUrl =
    HelperTypes.Url
