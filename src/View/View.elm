module View.View exposing (view)

import Browser
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Helpers.GetterSetters as GetterSetters
import Helpers.String
import Html
import Page.Example
import Page.LoginOpenstack
import Style.Helpers as SH
import Style.Toast
import Toasty
import Types.HelperTypes exposing (WindowSize)
import Types.OuterModel exposing (OuterModel)
import Types.OuterMsg exposing (OuterMsg(..))
import Types.SharedMsg exposing (SharedMsg(..))
import Types.View exposing (LoginView(..), NonProjectViewConstructor(..), ViewState(..))
import View.GetSupport
import View.HelpAbout
import View.Helpers as VH
import View.LoginJetstream
import View.LoginPicker
import View.Messages
import View.Nav
import View.PageTitle
import View.Project
import View.SelectProjects
import View.Settings
import View.Toast
import View.Types


view : OuterModel -> Browser.Document OuterMsg
view outerModel =
    let
        context =
            VH.toViewContext outerModel.sharedModel
    in
    { title =
        View.PageTitle.pageTitle outerModel context
    , body =
        [ view_ outerModel context ]
    }


view_ : OuterModel -> View.Types.Context -> Html.Html OuterMsg
view_ outerModel context =
    Element.layout
        [ Font.size 17
        , Font.family
            [ Font.typeface "Open Sans"
            , Font.sansSerif
            ]
        , Font.color <| SH.toElementColor <| context.palette.on.background
        , Background.color <| SH.toElementColor <| context.palette.background
        ]
        (elementView outerModel.sharedModel.windowSize outerModel context)


elementView : WindowSize -> OuterModel -> View.Types.Context -> Element.Element OuterMsg
elementView windowSize outerModel context =
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
                [ case outerModel.viewState of
                    NonProjectView viewConstructor ->
                        case viewConstructor of
                            LoginPicker ->
                                View.LoginPicker.loginPicker context outerModel.sharedModel.openIdConnectLoginConfig

                            Login loginView ->
                                case loginView of
                                    LoginOpenstack model ->
                                        Page.LoginOpenstack.view context model
                                            |> Element.map LoginOpenstackMsg

                                    LoginJetstream jetstreamCreds ->
                                        View.LoginJetstream.viewLoginJetstream context jetstreamCreds

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
                                View.SelectProjects.selectProjects outerModel.sharedModel context authUrl selectedProjects

                            MessageLog showDebugMsgs ->
                                View.Messages.messageLog context outerModel.sharedModel.logMessages showDebugMsgs

                            Settings ->
                                View.Settings.settings context outerModel.sharedModel.style.styleMode

                            GetSupport maybeSupportableItem requestDescription isSubmitted ->
                                View.GetSupport.getSupport
                                    outerModel.sharedModel
                                    context
                                    maybeSupportableItem
                                    requestDescription
                                    isSubmitted

                            HelpAbout ->
                                View.HelpAbout.helpAbout outerModel.sharedModel context

                            ExamplePage model ->
                                Page.Example.view model
                                    |> Element.map (\msg -> ExamplePageMsg msg)

                            PageNotFound ->
                                Element.text "Error: page not found. Perhaps you are trying to reach an invalid URL."

                    ProjectView projectName projectViewParams viewConstructor ->
                        case GetterSetters.projectLookup outerModel.sharedModel projectName of
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
                                    outerModel.sharedModel
                                    context
                                    project
                                    projectViewParams
                                    viewConstructor
                , Element.html
                    (Toasty.view Style.Toast.toastConfig
                        (View.Toast.toast context outerModel.sharedModel.showDebugMsgs)
                        (\m -> SharedMsg <| ToastyMsg m)
                        outerModel.sharedModel.toasties
                    )
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
                (View.Nav.navBar outerModel context)
            , Element.row
                [ Element.padding 0
                , Element.spacing 0
                , Element.width Element.fill
                , Element.height <|
                    Element.px (windowSize.height - View.Nav.navBarHeight)
                ]
                [ View.Nav.navMenu outerModel context
                , mainContentContainerView
                ]
            ]
        ]
