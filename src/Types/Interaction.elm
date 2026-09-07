module Types.Interaction exposing
    ( Interaction(..)
    , InteractionDetails
    , InteractionFix
    , InteractionStatus(..)
    , InteractionStatusReason
    , InteractionType(..)
    )

import Element


type Interaction
    = GuacTerminal
    | GuacDesktop
    | NativeSSH
    | Console
    | ConsoleLog
    | CustomWorkflow


type InteractionStatus
    = Unavailable InteractionStatusReason
    | Loading
    | Ready String
    | Warn String InteractionStatusReason
    | WarnWithFix InteractionStatusReason InteractionFix
    | Error InteractionStatusReason
    | Hidden


type alias InteractionStatusReason =
    String


{-| Something the user can do to make a warned interaction work: what the button says, and where in
Exosphere it takes them.
-}
type alias InteractionFix =
    { label : String
    , url : String
    }


type InteractionType
    = TextInteraction
    | UrlInteraction
    | NavigateInteraction


type alias InteractionDetails msg =
    { name : String
    , description : String
    , icon : Int -> Element.Element msg
    , type_ : InteractionType
    }
