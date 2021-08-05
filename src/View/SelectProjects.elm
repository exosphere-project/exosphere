module View.SelectProjects exposing (selectProjects)

import Element
import Element.Input as Input
import Helpers.GetterSetters as GetterSetters
import Helpers.String
import Helpers.Url as UrlHelpers
import OpenStack.Types as OSTypes
import Style.Helpers as SH
import Types.HelperTypes exposing (UnscopedProviderProject)
import Types.Msg exposing (SharedMsg(..))
import Types.Types exposing (SharedModel)
import Types.View exposing (NonProjectViewConstructor(..))
import View.Helpers as VH
import View.Types
import Widget


selectProjects :
    SharedModel
    -> View.Types.Context
    -> OSTypes.KeystoneUrl
    -> List UnscopedProviderProject
    -> Element.Element SharedMsg
selectProjects model context keystoneUrl selectedProjects =
    case GetterSetters.providerLookup model keystoneUrl of
        Just provider ->
            let
                urlLabel =
                    UrlHelpers.hostnameFromUrl keystoneUrl

                renderSuccessCase : List UnscopedProviderProject -> Element.Element SharedMsg
                renderSuccessCase projects =
                    Element.column VH.formContainer <|
                        List.append
                            (List.map
                                (renderProject keystoneUrl selectedProjects)
                                (VH.sortProjects projects)
                            )
                            [ Widget.textButton
                                (SH.materialStyle context.palette).primaryButton
                                { text = "Choose"
                                , onPress =
                                    Just <|
                                        RequestProjectLoginFromProvider keystoneUrl selectedProjects
                                }
                            ]
            in
            Element.column (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
                [ Element.el (VH.heading2 context.palette)
                    (Element.text <|
                        String.join " "
                            [ "Choose"
                            , context.localization.unitOfTenancy
                                |> Helpers.String.pluralize
                                |> Helpers.String.toTitleCase
                            , "for"
                            , urlLabel
                            ]
                    )
                , VH.renderWebData
                    context
                    provider.projectsAvailable
                    (context.localization.unitOfTenancy
                        |> Helpers.String.pluralize
                    )
                    renderSuccessCase
                ]

        Nothing ->
            Element.text "Provider not found"


renderProject : OSTypes.KeystoneUrl -> List UnscopedProviderProject -> UnscopedProviderProject -> Element.Element SharedMsg
renderProject keystoneUrl selectedProjects project =
    let
        onChange : Bool -> Bool -> SharedMsg
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

        renderProjectLabel : UnscopedProviderProject -> Element.Element SharedMsg
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
