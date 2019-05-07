module View.View exposing (view)

import Base64
import Element
import Element.Font as Font
import Helpers.Helpers as Helpers
import Html exposing (Html)
import Maybe
import Toasty
import Types.Types exposing (..)
import View.Login
import View.Messages
import View.Nav
import View.Project
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
                            Login ->
                                View.Login.viewLogin model

                            MessageLog ->
                                View.Messages.viewMessageLog model

                    ProjectView projectName viewConstructor ->
                        case Helpers.projectLookup model projectName of
                            Nothing ->
                                Element.text "Oops! Project not found"

                            Just project ->
                                View.Project.projectView model project viewConstructor
                , Element.html (Toasty.view Helpers.toastConfig View.Toast.toastView ToastyMsg model.toasties)
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
            [ View.Nav.navBarView model
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
                [ View.Nav.navMenuView model
                , mainContentContainerView
                ]
            ]
        ]



{- View Helpers -}


getEffectiveUserDataSize : CreateServerRequest -> String
getEffectiveUserDataSize createServerRequest =
    let
        rawLength =
            String.length createServerRequest.userData

        base64Value =
            Base64.encode createServerRequest.userData

        base64Length =
            String.length base64Value
    in
    String.fromInt rawLength
        ++ " characters,  "
        ++ String.fromInt base64Length
        ++ "/16384 allowed bytes (Base64 encoded)"
