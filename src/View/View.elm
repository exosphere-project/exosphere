module View.View exposing (view, viewElectron)

import Browser
import Element
import Element.Background as Background
import Element.Font as Font
import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers as Helpers
import Helpers.String
import Html
import Style.Helpers as SH
import Style.Toast
import Toasty
import Types.Types
    exposing
        ( LoginView(..)
        , Model
        , Msg(..)
        , NonProjectViewConstructor(..)
        , ViewState(..)
        , WindowSize
        )
import View.GetSupport
import View.HelpAbout
import View.Helpers as VH
import View.Login
import View.Messages
import View.Nav
import View.PageTitle
import View.Project
import View.SelectProjects
import View.Settings
import View.Toast
import View.Types


view : Model -> Browser.Document Msg
view model =
    let
        viewContext =
            VH.toViewContext model
    in
    { title =
        View.PageTitle.pageTitle model viewContext
    , body =
        [ view_ model viewContext ]
    }


viewElectron : Model -> View.Types.Context -> Html.Html Msg
viewElectron =
    view_


view_ : Model -> View.Types.Context -> Html.Html Msg
view_ model viewContext =
    Element.layout
        [ Font.size 17
        , Font.family
            [ Font.typeface "Open Sans"
            , Font.sansSerif
            ]
        , Font.color <| SH.toElementColor <| viewContext.palette.on.background
        , Background.color <| SH.toElementColor <| viewContext.palette.background
        ]
        (elementView model.maybeWindowSize model viewContext)


elementView : Maybe WindowSize -> Model -> View.Types.Context -> Element.Element Msg
elementView maybeWindowSize model context =
    let
        mainContentContainerView =
            Element.column
                [ Element.padding 10
                , Element.alignTop
                , Element.width <|
                    case maybeWindowSize of
                        Just windowSize ->
                            Element.px (windowSize.width - View.Nav.navMenuWidth)

                        Nothing ->
                            Element.fill
                , Element.height Element.fill
                , Element.scrollbars
                ]
                [ case model.viewState of
                    NonProjectView viewConstructor ->
                        case viewConstructor of
                            LoginPicker ->
                                View.Login.viewLoginPicker context model.openIdConnectLoginConfig

                            Login loginView ->
                                case loginView of
                                    LoginOpenstack openstackCreds ->
                                        View.Login.viewLoginOpenstack context openstackCreds

                                    LoginJetstream jetstreamCreds ->
                                        View.Login.viewLoginJetstream context jetstreamCreds

                            LoadingUnscopedProjects _ ->
                                -- TODO put a fidget spinner here
                                Element.text <|
                                    String.join " "
                                        [ "Loading"
                                        , context.localization.unitOfTenancy
                                            |> Helpers.String.pluralizeWord
                                            |> Helpers.String.capitalizeString
                                        ]

                            SelectProjects authUrl selectedProjects ->
                                View.SelectProjects.selectProjects model context authUrl selectedProjects

                            MessageLog ->
                                View.Messages.messageLog context model.logMessages

                            Settings ->
                                View.Settings.settings model.style.styleMode

                            GetSupport maybeSupportableItem requestDescription isSubmitted ->
                                View.GetSupport.getSupport
                                    model
                                    context
                                    maybeSupportableItem
                                    requestDescription
                                    isSubmitted

                            HelpAbout ->
                                View.HelpAbout.helpAbout model context

                            PageNotFound ->
                                Element.text "Error: page not found. Perhaps you are trying to reach an invalid URL."

                    ProjectView projectName projectViewParams viewConstructor ->
                        case GetterSetters.projectLookup model projectName of
                            Nothing ->
                                Element.text <|
                                    String.join " "
                                        [ "Oops!"
                                        , context.localization.unitOfTenancy
                                            |> Helpers.String.capitalizeString
                                        , "not found"
                                        ]

                            Just project ->
                                View.Project.project
                                    model
                                    context
                                    project
                                    projectViewParams
                                    viewConstructor
                , Element.html (Toasty.view Style.Toast.toastConfig (View.Toast.toast context model.showDebugMsgs) ToastyMsg model.toasties)
                ]
    in
    Element.row
        [ Element.padding 0
        , Element.spacing 0
        , Element.width Element.fill
        , Element.height <|
            case maybeWindowSize of
                Just windowSize ->
                    Element.px windowSize.height

                Nothing ->
                    Element.fill
        ]
        [ Element.column
            [ Element.padding 0
            , Element.spacing 0
            , Element.width Element.fill
            , Element.height <|
                case maybeWindowSize of
                    Just windowSize ->
                        Element.px windowSize.height

                    Nothing ->
                        Element.fill
            ]
            [ View.Nav.navBar model context
            , if Helpers.appIsElectron model then
                electronDeprecationWarning context

              else
                Element.none
            , Element.row
                [ Element.padding 0
                , Element.spacing 0
                , Element.width Element.fill
                , Element.height <|
                    case maybeWindowSize of
                        Just windowSize ->
                            Element.px (windowSize.height - View.Nav.navBarHeight)

                        Nothing ->
                            Element.fill
                ]
                [ View.Nav.navMenu model context
                , mainContentContainerView
                ]
            ]
        ]


electronDeprecationWarning : View.Types.Context -> Element.Element Msg
electronDeprecationWarning context =
    -- Electron deprecation warning per Phase 1 of https://gitlab.com/exosphere/exosphere/-/merge_requests/381
    let
        warningMD =
            """**Deprecation Notice:** this Electron-based desktop application will stop working starting March 31, 2021.

Please start using Exosphere in your browser at [try.exosphere.app](https://try.exosphere.app). If you are a Jetstream user, please use [exosphere.jetstream-cloud.org](https://exosphere.jetstream-cloud.org).

Both of these sites support installation to your desktop or home screen ([more info](https://gitlab.com/exosphere/exosphere/-/blob/master/docs/pwa-install.md))."""
    in
    Element.column
        (VH.exoElementAttributes
            ++ [ Element.width Element.fill
               , Font.center
               , Background.color <| SH.toElementColor <| context.palette.warn
               , Font.color <| SH.toElementColor <| context.palette.on.warn
               ]
        )
    <|
        VH.renderMarkdown
            context
            warningMD
