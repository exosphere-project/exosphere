module View.HelpAbout exposing (helpAbout)

import Element
import Types.Types exposing (..)
import View.Helpers as VH
import View.Types


helpAbout : Model -> Element.Element Msg
helpAbout model =
    Element.column (List.append VH.exoColumnAttributes [ Element.spacing 30 ])
        [ Element.el VH.heading2 <| Element.text "About Exosphere"
        , Element.paragraph []
            [ Element.text "Exosphere is a user-friendly, extensible client for cloud computing. Check out our "
            , VH.browserLink model.isElectron "https://gitlab.com/exosphere/exosphere/blob/master/README.md" <|
                View.Types.BrowserLinkTextLabel "README on GitLab"
            , Element.text "."
            ]
        , Element.el VH.heading2 <| Element.text "App Config Info"
        , Element.paragraph [] <|
            case model.proxyUrl of
                Nothing ->
                    [ Element.text "You are not using a proxy server." ]

                Just proxyUrl ->
                    [ Element.text ("You are using a proxy server at " ++ proxyUrl ++ ". All communication between Exosphere and OpenStack is passing through this server.") ]
        , Element.el VH.heading2 <| Element.text "Getting Help"
        , Element.paragraph []
            [ Element.text "To ask for help, report a bug, or request a new feature, "
            , VH.browserLink
                model.isElectron
                "https://gitlab.com/exosphere/exosphere/issues"
              <|
                View.Types.BrowserLinkTextLabel "create an issue"
            , Element.text " on Exosphere's GitLab project. Someone will respond within a day or so. For real-time assistance, see if anyone is on "
            , VH.browserLink
                model.isElectron
                "https://c-mart.sandcats.io/shared/ak1ymBWynN1MZe0ot1yEBOh6RF6fZ9G2ZOo2xhnmVC5"
              <|
                View.Types.BrowserLinkTextLabel "Exosphere Chat"
            , Element.text ". Sign in there with your email or GitHub account."
            ]
        ]
