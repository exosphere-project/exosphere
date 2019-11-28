module View.SelectProjects exposing (selectProjects)

import Element
import Element.Input as Input
import Framework.Button as Button
import Framework.Modifier as Modifier
import Helpers.Helpers as Helpers
import OpenStack.Types as OSTypes
import RemoteData
import Types.HelperTypes as HelperTypes
import Types.Types
    exposing
        ( Model
        , Msg(..)
        , NonProjectViewConstructor(..)
        , UnscopedProviderProject
        )
import View.Helpers as VH


selectProjects : Model -> OSTypes.KeystoneUrl -> HelperTypes.Password -> List UnscopedProviderProject -> Element.Element Msg
selectProjects model keystoneUrl password selectedProjects =
    case Helpers.providerLookup model keystoneUrl of
        Just provider ->
            let
                urlLabel =
                    Helpers.hostnameFromUrl keystoneUrl
            in
            Element.column VH.exoColumnAttributes
                [ Element.el VH.heading2
                    (Element.text <| "Choose Projects for " ++ urlLabel)
                , case provider.projectsAvailable of
                    RemoteData.Success projectsAvailable ->
                        Element.column VH.exoColumnAttributes <|
                            List.append
                                (List.map
                                    (renderProject keystoneUrl password selectedProjects)
                                    projectsAvailable
                                )
                                [ Button.button
                                    [ Modifier.Primary ]
                                    (Just <|
                                        RequestProjectLoginFromProvider keystoneUrl password selectedProjects
                                    )
                                    "Choose"
                                ]

                    RemoteData.Loading ->
                        Element.text "Loading list of projects"

                    RemoteData.Failure e ->
                        Element.text ("Error loading list of projects: " ++ Debug.toString e)

                    RemoteData.NotAsked ->
                        -- This state should be impossible because when we create an unscoped Provider we always immediately ask for a list of projects
                        Element.none
                ]

        Nothing ->
            Element.text "Provider not found"


renderProject : OSTypes.KeystoneUrl -> HelperTypes.Password -> List UnscopedProviderProject -> UnscopedProviderProject -> Element.Element Msg
renderProject keystoneUrl password selectedProjects project =
    let
        onChange : Bool -> Msg
        onChange bool =
            case bool of
                True ->
                    SetNonProjectView <|
                        SelectProjects
                            keystoneUrl
                            password
                        <|
                            (project :: selectedProjects)

                False ->
                    SetNonProjectView <|
                        SelectProjects
                            keystoneUrl
                            password
                        <|
                            List.filter
                                (\p -> p.name /= project.name)
                                selectedProjects

        renderProjectLabel : UnscopedProviderProject -> Element.Element Msg
        renderProjectLabel p =
            let
                labelStr =
                    case p.description of
                        "" ->
                            p.name

                        _ ->
                            p.name ++ " -- " ++ p.description
            in
            Element.text labelStr
    in
    Input.checkbox []
        { checked = List.member project.name (List.map .name selectedProjects)
        , onChange = onChange
        , icon = Input.defaultCheckbox
        , label = Input.labelRight [] (renderProjectLabel project)
        }
