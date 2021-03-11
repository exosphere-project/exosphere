module View.SelectProjects exposing (selectProjects)

import Element
import Element.Input as Input
import Helpers.GetterSetters as GetterSetters
import Helpers.String
import Helpers.Url as UrlHelpers
import OpenStack.Types as OSTypes
import RemoteData
import Style.Helpers as SH
import Types.Types
    exposing
        ( Model
        , Msg(..)
        , NonProjectViewConstructor(..)
        , UnscopedProviderProject
        )
import View.Helpers as VH
import View.Types
import Widget
import Widget.Style.Material


selectProjects :
    Model
    -> View.Types.Context
    -> OSTypes.KeystoneUrl
    -> List UnscopedProviderProject
    -> Element.Element Msg
selectProjects model context keystoneUrl selectedProjects =
    case GetterSetters.providerLookup model keystoneUrl of
        Just provider ->
            let
                urlLabel =
                    UrlHelpers.hostnameFromUrl keystoneUrl
            in
            Element.column VH.exoColumnAttributes
                [ Element.el VH.heading2
                    (Element.text <|
                        String.join " "
                            [ "Choose"
                            , context.localization.unitOfTenancy
                                |> Helpers.String.pluralizeWord
                                |> Helpers.String.stringToTitleCase
                            , "for"
                            , urlLabel
                            ]
                    )
                , case provider.projectsAvailable of
                    RemoteData.Success projectsAvailable ->
                        Element.column VH.exoColumnAttributes <|
                            List.append
                                (List.map
                                    (renderProject keystoneUrl selectedProjects)
                                    projectsAvailable
                                )
                                [ Widget.textButton
                                    (Widget.Style.Material.containedButton (SH.toMaterialPalette context.palette))
                                    { text = "Choose"
                                    , onPress =
                                        Just <|
                                            RequestProjectLoginFromProvider keystoneUrl selectedProjects
                                    }
                                ]

                    RemoteData.Loading ->
                        Element.row [ Element.spacing 15 ]
                            [ Widget.circularProgressIndicator
                                (SH.materialStyle context.palette).progressIndicator
                                Nothing
                            , Element.text <|
                                String.join " "
                                    [ "Loading list of"
                                    , Helpers.String.pluralizeWord context.localization.unitOfTenancy
                                    ]
                            ]

                    RemoteData.Failure e ->
                        Element.text <|
                            String.join " "
                                [ "Error loading list of"
                                , Helpers.String.pluralizeWord context.localization.unitOfTenancy
                                , "--"
                                , Debug.toString e
                                ]

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
                                (\p -> p.project.name /= project.project.name)
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
                            p.project.name ++ disabledMsg

                        _ ->
                            p.project.name ++ " -- " ++ p.description ++ disabledMsg
            in
            Element.text labelStr
    in
    Input.checkbox []
        { checked = List.member project.project.name (List.map (\p -> p.project.name) selectedProjects)
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
