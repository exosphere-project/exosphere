module Types.Interaction exposing
    ( Interaction(..)
    , InteractionDetails
    , InteractionStatus(..)
    , InteractionType(..)
    )

import Element


type Interaction
    = GuacTerminal
    | GuacDesktop
    | NativeSSH
    | Console
    | CustomWorkflow


type InteractionStatus
    = Unavailable InteractionStatusReason
    | Loading
    | Ready String
    | Warn String InteractionStatusReason
    | Error InteractionStatusReason
    | Hidden


type alias InteractionStatusReason =
    String


type InteractionType
    = TextInteraction
    | UrlInteraction


type alias InteractionDetails msg =
    { name : String
    , description : String
    , icon : Element.Color -> Int -> Element.Element msg
    , type_ : InteractionType
    }
