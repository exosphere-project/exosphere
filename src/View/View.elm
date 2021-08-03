module View.View exposing (view)

import Browser
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Helpers.GetterSetters as GetterSetters
import Helpers.String
import Html
import Style.Helpers as SH
import Style.Toast
import Toasty
import Types.Msg exposing (Msg(..))
import Types.Types exposing (Model, WindowSize)
import Types.View exposing (LoginView(..), NonProjectViewConstructor(..), ViewState(..))
import View.GetSupport
import View.HelpAbout
import View.Helpers as VH
import View.Login
import View.LoginPicker
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
        context =
            VH.toViewContext model
    in
    { title =
        View.PageTitle.pageTitle model context
    , body =
        [ view_ model context ]
    }


view_ : Model -> View.Types.Context -> Html.Html Msg
view_ model context =
    Element.layout
        [ Font.size 17
        , Font.family
            [ Font.typeface "Open Sans"
            , Font.sansSerif
            ]
        , Font.color <| SH.toElementColor <| context.palette.on.background
        , Background.color <| SH.toElementColor <| context.palette.background
        ]
        (elementView model.windowSize model context)


elementView : WindowSize -> Model -> View.Types.Context -> Element.Element Msg
elementView windowSize model context =
    let
        mainContentContainerView =
            Element.column
                [ Element.padding 10
                , Element.alignTop
                , Element.width <|
                    Element.px (windowSize.width - View.Nav.navMenuWidth)
                , Element.height Element.fill
                , Element.scrollbars
                ]
                [ case model.viewState of
                    NonProjectView viewConstructor ->
                        case viewConstructor of
                            LoginPicker ->
                                View.LoginPicker.loginPicker context model.openIdConnectLoginConfig

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
                                            |> Helpers.String.pluralize
                                            |> Helpers.String.toTitleCase
                                        ]

                            SelectProjects authUrl selectedProjects ->
                                View.SelectProjects.selectProjects model context authUrl selectedProjects

                            MessageLog showDebugMsgs ->
                                View.Messages.messageLog context model.logMessages showDebugMsgs

                            Settings ->
                                View.Settings.settings context model.style.styleMode

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
                                            |> Helpers.String.toTitleCase
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
            Element.px windowSize.height
        ]
        [ Element.column
            [ Element.padding 0
            , Element.spacing 0
            , Element.width Element.fill
            , Element.height <|
                Element.px windowSize.height
            ]
            [ Element.el
                [ Border.shadow { offset = ( 0, 0 ), size = 1, blur = 5, color = Element.rgb 0.1 0.1 0.1 }
                , Element.width Element.fill
                ]
                (View.Nav.navBar model context)
            , Element.row
                [ Element.padding 0
                , Element.spacing 0
                , Element.width Element.fill
                , Element.height <|
                    Element.px (windowSize.height - View.Nav.navBarHeight)
                ]
                [ View.Nav.navMenu model context
                , mainContentContainerView
                ]
            ]
        ]
