module Page.HelpAbout exposing (Msg, headerView, update, view)

import Browser.Navigation
import Element
import Element.Font as Font
import Helpers.Helpers exposing (currentAppVersion, isAppUpdateAvailable, latestAppVersion)
import Style.Helpers as SH
import Style.Widgets.Alert as Alert
import Style.Widgets.Button as Button
import Style.Widgets.Code as Code
import Style.Widgets.CopyableText exposing (copyableText)
import Style.Widgets.Link as Link
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg as SharedMsg
import UUID
import View.Helpers as VH exposing (edges)
import View.Types


type Msg
    = GotRefresh


update : Msg -> SharedModel -> ( Cmd Msg, SharedMsg.SharedMsg )
update msg _ =
    case msg of
        GotRefresh ->
            ( Browser.Navigation.reloadAndSkipCache, SharedMsg.NoOp )


headerView : SharedModel -> View.Types.Context -> Element.Element msg
headerView model context =
    Text.heading context.palette
        VH.headerHeadingAttributes
        Element.none
        ("About " ++ model.style.appTitle)


view : SharedModel -> View.Types.Context -> Element.Element Msg
view model context =
    Element.column
        (VH.contentContainer ++ [ Element.spacing spacer.px16 ])
        [ case model.style.aboutAppMarkdown of
            Just aboutAppMarkdown ->
                Element.column [ Element.spacing spacer.px16 ] <|
                    VH.renderMarkdown context.palette aboutAppMarkdown

            Nothing ->
                defaultHelpAboutText context
        , Text.heading context.palette
            [ Element.paddingEach { edges | top = spacer.px16, bottom = spacer.px8 } ]
            Element.none
            "App Config Info"
        , Element.column [ Element.spacing spacer.px16 ]
            [ Text.p [] <|
                case model.cloudCorsProxyUrl of
                    Nothing ->
                        [ Element.text "You are not using a proxy server." ]

                    Just proxyUrl ->
                        [ Text.body {- @nonlocalized -} "You are using a cloud CORS proxy server at "
                        , Code.codeSpan context.palette proxyUrl
                        , Text.body ". All communication between Exosphere and OpenStack APIs pass through this server."
                        ]
            , Element.row []
                [ Text.body "Exosphere client UUID:"
                , Element.el
                    [ Text.fontSize Text.Small
                    , Font.color (SH.toElementColor context.palette.neutral.text.subdued)
                    , Element.paddingEach { bottom = 0, left = spacer.px16, right = 0, top = 0 }
                    ]
                    (copyableText context.palette
                        [ Element.width (Element.shrink |> Element.minimum 280) ]
                        (UUID.toString model.clientUuid)
                    )
                ]
            , Element.row []
                [ Text.body "Exosphere version:"
                , Element.el
                    [ Text.fontSize Text.Small
                    , Font.color (SH.toElementColor context.palette.neutral.text.subdued)
                    , Element.paddingEach { bottom = 0, left = spacer.px16, right = 0, top = 0 }
                    ]
                    (copyableText context.palette [] (currentAppVersion model))
                ]
            , if isAppUpdateAvailable model then
                Alert.alert []
                    context.palette
                    { state = Alert.Info
                    , showIcon = False
                    , showContainer = True
                    , content =
                        Element.row [ Element.width Element.fill ]
                            [ Text.p [ Element.paddingEach { top = 0, bottom = spacer.px4, left = 0, right = spacer.px16 } ]
                                [ Text.text Text.Small [] "A new version of Exosphere is available ("
                                , Text.text Text.Small [ Text.fontFamily Text.Mono ] (latestAppVersion model)
                                , Text.text Text.Small [] ")."
                                ]
                            , Button.button Button.Text
                                context.palette
                                { text = "Refresh to Update"
                                , onPress = Just GotRefresh
                                }
                            ]
                    }

              else
                Element.none
            ]
        ]


defaultHelpAboutText : View.Types.Context -> Element.Element msg
defaultHelpAboutText context =
    Element.column [ Element.spacing spacer.px16 ]
        [ Text.p []
            [ Element.text {- @nonlocalized -} "Exosphere is a user-friendly, extensible client for cloud computing. Check out our "
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
            , Element.text {- @nonlocalized -} " on Exosphere's GitLab project. Someone will respond within a day or so. For real-time assistance, try Exosphere chat. Our chat is on "
            , Link.externalLink
                context.palette
                "https://gitter.im/exosphere-app/community"
                "gitter"
            , Element.text " and "
            , Link.externalLink
                context.palette
                "https://matrix.to/#/#exosphere:matrix.org"
                "Matrix"
            , Element.text ". The chat is bridged across both platforms, so join whichever you prefer."
            ]
        ]
