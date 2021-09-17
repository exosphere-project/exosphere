module Page.HelpAbout exposing (view)

import Element
import FeatherIcons
import Types.SharedModel exposing (SharedModel)
import UUID
import View.Helpers as VH
import View.Types



-- No state or Msgs to keep track of, so there is no Model, Msg, or update here


view : SharedModel -> View.Types.Context -> Element.Element msg
view model context =
    Element.column (List.append VH.exoColumnAttributes [ Element.spacing 30, Element.width Element.fill ])
        [ Element.row (VH.heading2 context.palette ++ [ Element.spacing 12 ])
            [ FeatherIcons.info
                |> FeatherIcons.toHtml []
                |> Element.html
                |> Element.el []
            , Element.text <| "About " ++ model.style.appTitle
            ]
        , case model.style.aboutAppMarkdown of
            Just aboutAppMarkdown ->
                Element.column VH.contentContainer <|
                    VH.renderMarkdown context aboutAppMarkdown

            Nothing ->
                defaultHelpAboutText context
        , Element.el (VH.heading2 context.palette) <| Element.text "App Config Info"
        , Element.column VH.contentContainer
            [ Element.paragraph [] <|
                case model.cloudCorsProxyUrl of
                    Nothing ->
                        [ Element.text "You are not using a proxy server." ]

                    Just proxyUrl ->
                        [ Element.text ("You are using a cloud CORS proxy server at " ++ proxyUrl ++ ". All communication between Exosphere and OpenStack APIs pass through this server.") ]
            , Element.paragraph [] [ Element.text ("Exosphere client UUID: " ++ UUID.toString model.clientUuid) ]
            ]
        ]


defaultHelpAboutText : View.Types.Context -> Element.Element msg
defaultHelpAboutText context =
    Element.column VH.contentContainer
        [ Element.paragraph []
            [ Element.text "Exosphere is a user-friendly, extensible client for cloud computing. Check out our "
            , VH.externalLink
                context
                "https://gitlab.com/exosphere/exosphere/blob/master/README.md"
                "README on GitLab"
            , Element.text "."
            ]
        , Element.paragraph []
            [ Element.text "To ask for help, report a bug, or request a new feature, "
            , VH.externalLink
                context
                "https://gitlab.com/exosphere/exosphere/issues"
                "create an issue"
            , Element.text " on Exosphere's GitLab project. Someone will respond within a day or so. For real-time assistance, try Exosphere chat. Our chat is on "
            , VH.externalLink
                context
                "https://gitter.im/exosphere-app/community"
                "gitter"
            , Element.text " and "
            , VH.externalLink
                context
                "https://riot.im/app/#/room/#exosphere:matrix.org"
                "Matrix via Element"
            , Element.text ". The chat is bridged across both platforms, so join whichever you prefer."
            ]
        ]
