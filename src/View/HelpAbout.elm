module View.HelpAbout exposing (helpAbout)

import Element
import Types.Types exposing (Model, Msg(..))
import UUID
import View.Helpers as VH
import View.Types


helpAbout : Model -> View.Types.Context -> Element.Element Msg
helpAbout model context =
    Element.column (List.append VH.exoColumnAttributes [ Element.spacing 30 ])
        [ Element.el (VH.heading2 context.palette) <| Element.text <| "About " ++ model.style.appTitle
        , case model.style.aboutAppMarkdown of
            Just aboutAppMarkdown ->
                Element.column [] <|
                    VH.renderMarkdown context aboutAppMarkdown

            Nothing ->
                defaultHelpAboutText context
        , Element.el (VH.heading2 context.palette) <| Element.text "App Config Info"
        , Element.paragraph [] <|
            case model.cloudCorsProxyUrl of
                Nothing ->
                    [ Element.text "You are not using a proxy server." ]

                Just proxyUrl ->
                    [ Element.text ("You are using a cloud CORS proxy server at " ++ proxyUrl ++ ". All communication between Exosphere and OpenStack APIs pass through this server.") ]
        , Element.paragraph [] [ Element.text ("Exosphere client UUID: " ++ UUID.toString model.clientUuid) ]
        ]


defaultHelpAboutText : View.Types.Context -> Element.Element Msg
defaultHelpAboutText context =
    Element.column [ Element.spacing 20 ]
        [ Element.paragraph []
            [ Element.text "Exosphere is a user-friendly, extensible client for cloud computing. Check out our "
            , VH.browserLink
                context
                "https://gitlab.com/exosphere/exosphere/blob/master/README.md"
              <|
                View.Types.BrowserLinkTextLabel "README on GitLab"
            , Element.text "."
            ]
        , Element.paragraph []
            [ Element.text "To ask for help, report a bug, or request a new feature, "
            , VH.browserLink
                context
                "https://gitlab.com/exosphere/exosphere/issues"
              <|
                View.Types.BrowserLinkTextLabel "create an issue"
            , Element.text " on Exosphere's GitLab project. Someone will respond within a day or so. For real-time assistance, try Exosphere chat. Our chat is on "
            , VH.browserLink
                context
                "https://gitter.im/exosphere-app/community"
              <|
                View.Types.BrowserLinkTextLabel "gitter"
            , Element.text " and "
            , VH.browserLink
                context
                "https://riot.im/app/#/room/#exosphere:matrix.org"
              <|
                View.Types.BrowserLinkTextLabel "Matrix via Element"
            , Element.text ". The chat is bridged across both platforms, so join whichever you prefer."
            ]
        ]
