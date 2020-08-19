module View.SelectProjects exposing (selectProjects)

import Element
import Element.Input as Input
import Helpers.Helpers as Helpers
import OpenStack.Types as OSTypes
import RemoteData
import Style.Theme
import Types.Types
    exposing
        ( Model
        , Msg(..)
        , NonProjectViewConstructor(..)
        , UnscopedProviderProject
        )
import View.Helpers as VH
import Widget
import Widget.Style.Material


selectProjects : Model -> OSTypes.KeystoneUrl -> List UnscopedProviderProject -> Element.Element Msg
selectProjects model keystoneUrl selectedProjects =
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
                                    (renderProject keystoneUrl selectedProjects)
                                    projectsAvailable
                                )
                                [ Widget.textButton
                                    (Widget.Style.Material.containedButton Style.Theme.exoPalette)
                                    { text = "Choose"
                                    , onPress =
                                        Just <|
                                            RequestProjectLoginFromProvider keystoneUrl provider.keystonePassword selectedProjects
                                    }
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


renderProject : OSTypes.KeystoneUrl -> List UnscopedProviderProject -> UnscopedProviderProject -> Element.Element Msg
renderProject keystoneUrl selectedProjects project =
    let
        onChange : Bool -> Bool -> Msg
        onChange projectEnabled enableDisable =
            if projectEnabled then
                if enableDisable then
                    SetNonProjectView <|
                        SelectProjects
                            keystoneUrl
                        <|
                            (project :: selectedProjects)

                else
                    SetNonProjectView <|
                        SelectProjects
                            keystoneUrl
                        <|
                            List.filter
                                (\p -> p.name /= project.name)
                                selectedProjects

            else
                NoOp

        renderProjectLabel : UnscopedProviderProject -> Element.Element Msg
        renderProjectLabel p =
            let
                disabledMsg =
                    if p.enabled then
                        ""

                    else
                        " (disabled)"

                labelStr =
                    case p.description of
                        "" ->
                            p.name ++ disabledMsg

                        _ ->
                            p.name ++ " -- " ++ p.description ++ disabledMsg
            in
            Element.text labelStr
    in
    Input.checkbox []
        { checked = List.member project.name (List.map .name selectedProjects)
        , onChange = onChange project.enabled
        , icon =
            if project.enabled then
                Input.defaultCheckbox

            else
                \_ -> nullCheckbox
        , label = Input.labelRight [] (renderProjectLabel project)
        }


nullCheckbox : Element.Element msg
nullCheckbox =
    Element.el
        [ Element.width (Element.px 14)
        , Element.height (Element.px 14)
        ]
        Element.none
