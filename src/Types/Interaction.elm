module Types.Interaction exposing
    ( Interaction(..)
    , InteractionDetails
    , InteractionStatus(..)
    , InteractionType(..)
    )

import Element
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
    | Ready String
    | Error InteractionStatusReason
    | Hidden


type alias InteractionStatusReason =
    String


type InteractionType
    = TextInteraction
    | UrlInteraction


type alias InteractionDetails =
    { name : String
    , description : String
    , icon : Element.Color -> Int -> Element.Element Never
    , type_ : InteractionType
    }
