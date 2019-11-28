module View.View exposing (view)

import Element
import Element.Font as Font
import Helpers.Helpers as Helpers
import Html exposing (Html)
import Toasty
import Types.Types
    exposing
        ( Model
        , Msg(..)
        , NonProjectViewConstructor(..)
        , ViewState(..)
        , WindowSize
        )
import View.HelpAbout
import View.Login
import View.Messages
import View.Nav
import View.Project
import View.SelectProjects
import View.Toast


view : Model -> Html Msg
view model =
    Element.layout
        [ Font.size 17
        , Font.family
            [ Font.typeface "Open Sans"
            , Font.sansSerif
            ]
        ]
        (elementView model.maybeWindowSize model)


elementView : Maybe WindowSize -> Model -> Element.Element Msg
elementView maybeWindowSize model =
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
                                View.Login.viewLoginPicker

                            LoginOpenstack openstackCreds ->
                                View.Login.viewLoginOpenstack model openstackCreds

                            LoginJetstream jetstreamCreds ->
                                View.Login.viewLoginJetstream model jetstreamCreds

                            SelectProjects authUrl selectedProjects ->
                                View.SelectProjects.selectProjects model authUrl selectedProjects

                            MessageLog ->
                                View.Messages.messageLog model

                            HelpAbout ->
                                View.HelpAbout.helpAbout model

                    ProjectView projectName viewConstructor ->
                        case Helpers.projectLookup model projectName of
                            Nothing ->
                                Element.text "Oops! Project not found"

                            Just project ->
                                View.Project.project model project viewConstructor
                , Element.html (Toasty.view Helpers.toastConfig View.Toast.toast ToastyMsg model.toasties)
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
            [ View.Nav.navBar model
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
                [ View.Nav.navMenu model
                , mainContentContainerView
                ]
            ]
        ]
