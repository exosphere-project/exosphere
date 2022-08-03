module Page.HelpAbout exposing (headerView, view)

import Element
import FeatherIcons
import Style.Widgets.Link as Link
import Style.Widgets.Text as Text
import Types.SharedModel exposing (SharedModel)
import UUID
import View.Helpers as VH exposing (edges)
import View.Types



-- No state or Msgs to keep track of, so there is no Model, Msg, or update here


headerView : SharedModel -> View.Types.Context -> Element.Element msg
headerView model context =
    Text.heading context.palette
        VH.headerHeadingAttributes
        (FeatherIcons.info
            |> FeatherIcons.toHtml []
            |> Element.html
            |> Element.el []
        )
        ("About " ++ model.style.appTitle)


view : SharedModel -> View.Types.Context -> Element.Element msg
view model context =
    Element.column
        (VH.contentContainer ++ [ Element.spacing 16 ])
        [ case model.style.aboutAppMarkdown of
            Just aboutAppMarkdown ->
                Element.column [ Element.spacing 16 ] <|
                    VH.renderMarkdown context aboutAppMarkdown

            Nothing ->
                defaultHelpAboutText context
        , Text.heading context.palette
            [ Element.paddingEach { edges | top = 16, bottom = 8 } ]
            Element.none
            "App Config Info"
        , Element.column [ Element.spacing 16 ]
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
    Element.column [ Element.spacing 16 ]
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
