module Page.HelpAbout exposing (view)

import Element
import Element.Font as Font
import FeatherIcons
import Style.Widgets.Link as Link
import Style.Widgets.Text as Text
import Types.SharedModel exposing (SharedModel)
import UUID
import View.Helpers as VH
import View.Types



-- No state or Msgs to keep track of, so there is no Model, Msg, or update here


view : SharedModel -> View.Types.Context -> Element.Element msg
view model context =
    Element.column (List.append VH.exoColumnAttributes [ Element.spacing 16, Element.width Element.fill ])
        [ Text.heading context.palette
            []
            (FeatherIcons.info
                |> FeatherIcons.toHtml []
                |> Element.html
                |> Element.el []
            )
            ("About " ++ model.style.appTitle)
        , case model.style.aboutAppMarkdown of
            Just aboutAppMarkdown ->
                Element.column VH.contentContainer <|
                    VH.renderMarkdown context aboutAppMarkdown

            Nothing ->
                defaultHelpAboutText context
        , Text.heading context.palette [] Element.none "App Config Info"
        , Element.column VH.contentContainer
            [ Text.p [] <|
                case model.cloudCorsProxyUrl of
                    Nothing ->
                        [ Element.text "You are not using a proxy server." ]

                    Just proxyUrl ->
                        [ Element.text ("You are using a cloud CORS proxy server at " ++ proxyUrl ++ ". All communication between Exosphere and OpenStack APIs pass through this server.") ]
            , Text.p [] [ Element.text ("Exosphere client UUID: " ++ UUID.toString model.clientUuid) ]
            ]
        ]


defaultHelpAboutText : View.Types.Context -> Element.Element msg
defaultHelpAboutText context =
    Element.textColumn (VH.contentContainer ++ [ Font.size 16, Element.spacing 16 ])
        [ Text.p []
            [ Element.text "Exosphere is a user-friendly, extensible client for cloud computing. Check out our "
            , Link.externalLink
                context.palette
                "https://gitlab.com/exosphere/exosphere/blob/master/README.md"
                "README on GitLab"
            , Element.text "."
            ]
        , Text.p []
            [ Element.text "To ask for help, report a bug, or request a new feature, "
            , Link.externalLink
                context.palette
                "https://gitlab.com/exosphere/exosphere/issues"
                "create an issue"
            , Element.text " on Exosphere's GitLab project. Someone will respond within a day or so. For real-time assistance, try Exosphere chat. Our chat is on "
            , Link.externalLink
                context.palette
                "https://gitter.im/exosphere-app/community"
                "gitter"
            , Element.text " and "
            , Link.externalLink
                context.palette
                "https://riot.im/app/#/room/#exosphere:matrix.org"
                "Matrix via Element"
            , Element.text ". The chat is bridged across both platforms, so join whichever you prefer."
            ]
        ]
