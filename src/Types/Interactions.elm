module Types.Interactions exposing (Interaction(..), InteractionStatus(..), interactionNameDescription)

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


interactionNameDescription : Interaction -> ( String, String )
interactionNameDescription interaction =
    -- TODO provide an icon as well
    case interaction of
        GuacTerminal ->
            ( "Web Terminal"
            , "Get a command-line session to your server"
            )

        GuacDesktop ->
            ( "Streaming Desktop", "Interact with your server's desktop environment" )

        CockpitDashboard ->
            ( "Server Dashboard", "Deprecated feature" )

        CockpitTerminal ->
            ( "Web Terminal", "Deprecated feature" )

        NativeSSH ->
            ( "Native SSH"
            , "Advanced feature: use your computer's native SSH client to get a command-line session with extra capabilities"
            )

        Console ->
            ( "Console"
            , "Advanced feature: Launching the console is like connecting a screen, mouse, and keyboard to your server (useful for troubleshooting if the Web Terminal isn't working)"
            )
