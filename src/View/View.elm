module View.View exposing (view, viewElectron)

import Browser
import Element
import Element.Background as Background
import Element.Font as Font
import Helpers.GetterSetters as GetterSetters
import Html
import Style.Helpers as SH
import Style.Toast
import Style.Types
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


view : Model -> Browser.Document Msg
view model =
    { title =
        View.PageTitle.pageTitle model
    , body =
        [ view_ model ]
    }


viewElectron : Model -> Html.Html Msg
viewElectron =
    view_


view_ : Model -> Html.Html Msg
view_ model =
    let
        palette =
            VH.toExoPalette model.style
    in
    Element.layout
        [ Font.size 17
        , Font.family
            [ Font.typeface "Open Sans"
            , Font.sansSerif
            ]
        , Font.color <| SH.toElementColor <| palette.on.background
        , Background.color <| SH.toElementColor <| palette.background
        ]
        (elementView model.maybeWindowSize model palette)


elementView : Maybe WindowSize -> Model -> Style.Types.ExoPalette -> Element.Element Msg
elementView maybeWindowSize model palette =
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
                                View.Login.viewLoginPicker palette model.openIdConnectLoginConfig

                            Login loginView ->
                                case loginView of
                                    LoginOpenstack openstackCreds ->
                                        View.Login.viewLoginOpenstack model palette openstackCreds

                                    LoginJetstream jetstreamCreds ->
                                        View.Login.viewLoginJetstream model palette jetstreamCreds

                            SelectProjects authUrl selectedProjects ->
                                View.SelectProjects.selectProjects model palette authUrl selectedProjects

                            MessageLog ->
                                View.Messages.messageLog model

                            Settings ->
                                View.Settings.settings model

                            GetSupport maybeSupportableItem requestDescription isSubmitted ->
                                View.GetSupport.getSupport
                                    model
                                    palette
                                    maybeSupportableItem
                                    requestDescription
                                    isSubmitted

                            HelpAbout ->
                                View.HelpAbout.helpAbout model palette

                            PageNotFound ->
                                Element.text "Error: page not found. Perhaps you are trying to reach an invalid URL."

                    ProjectView projectName projectViewParams viewConstructor ->
                        case GetterSetters.projectLookup model projectName of
                            Nothing ->
                                Element.text "Oops! Project not found"

                            Just project ->
                                View.Project.project
                                    model
                                    palette
                                    project
                                    projectViewParams
                                    viewConstructor
                , Element.html (Toasty.view Style.Toast.toastConfig (View.Toast.toast palette model.showDebugMsgs) ToastyMsg model.toasties)
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
            [ View.Nav.navBar model palette
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
                [ View.Nav.navMenu model palette
                , mainContentContainerView
                ]
            ]
        ]
