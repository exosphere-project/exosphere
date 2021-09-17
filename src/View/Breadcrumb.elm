module View.Breadcrumb exposing (breadcrumb)

import Element
import Helpers.GetterSetters as GetterSetters
import Helpers.String
import Route
import Types.OuterModel
import Types.SharedMsg exposing (SharedMsg)
import Types.View exposing (LoginView(..), NonProjectViewConstructor(..), ProjectViewConstructor(..), ViewState(..))
import View.Helpers
import View.PageTitle
import View.Types


breadcrumb : Types.OuterModel.OuterModel -> View.Types.Context -> Element.Element SharedMsg
breadcrumb outerModel context =
    let
        path =
            case outerModel.viewState of
                NonProjectView _ ->
                    [ Element.text <| View.PageTitle.pageTitle outerModel context ]

                ProjectView projectId _ constructor ->
                    let
                        renderProject =
                            case GetterSetters.projectLookup outerModel.sharedModel projectId of
                                Just project ->
                                    Element.link (View.Helpers.linkAttribs context)
                                        { url = Route.toUrl context.urlPathPrefix (Route.ProjectRoute projectId <| Route.AllResourcesList)
                                        , label = Element.text ("Project " ++ project.auth.project.name)
                                        }

                                Nothing ->
                                    Element.text "Unknown Project"

                        renderProjectPage =
                            Element.text <|
                                case constructor of
                                    -- TODO a lot of this duplicates what is in View.PageTitle, consider factoring things out
                                    AllResourcesList _ ->
                                        "All Resources"

                                    FloatingIpAssign _ ->
                                        String.join " "
                                            [ "Assign"
                                            , context.localization.floatingIpAddress
                                                |> Helpers.String.toTitleCase
                                            ]

                                    FloatingIpList _ ->
                                        String.join " "
                                            [ context.localization.floatingIpAddress
                                                |> Helpers.String.pluralize
                                                |> Helpers.String.toTitleCase
                                            ]

                                    InstanceSourcePicker _ ->
                                        String.join " "
                                            [ context.localization.virtualComputer
                                                |> Helpers.String.toTitleCase
                                            , "sources"
                                            ]

                                    KeypairCreate _ ->
                                        String.join " "
                                            [ "Upload"
                                            , context.localization.pkiPublicKeyForSsh
                                            ]

                                    KeypairList _ ->
                                        String.join " "
                                            [ context.localization.pkiPublicKeyForSsh
                                                |> Helpers.String.pluralize
                                                |> Helpers.String.toTitleCase
                                            ]

                                    ServerCreate _ ->
                                        String.join " "
                                            [ "Create"
                                            , context.localization.virtualComputer
                                                |> Helpers.String.toTitleCase
                                            ]

                                    ServerCreateImage _ ->
                                        String.join " "
                                            [ "Create"
                                            , context.localization.staticRepresentationOfBlockDeviceContents
                                                |> Helpers.String.toTitleCase
                                            ]

                                    ServerDetail pageModel ->
                                        -- TODO this should render as "Home > Project whatever > Instances > instance-name-here"
                                        -- not "Home > Project whatever > Instance instance-name-here"
                                        String.join " "
                                            [ context.localization.virtualComputer
                                                |> Helpers.String.toTitleCase
                                            , View.PageTitle.serverName (GetterSetters.projectLookup outerModel.sharedModel projectId) pageModel.serverUuid
                                            ]

                                    ServerList _ ->
                                        String.join " "
                                            [ context.localization.virtualComputer
                                                |> Helpers.String.pluralize
                                                |> Helpers.String.toTitleCase
                                            ]

                                    VolumeAttach _ ->
                                        String.join " "
                                            [ "Attach"
                                            , context.localization.blockDevice
                                                |> Helpers.String.toTitleCase
                                            ]

                                    VolumeCreate _ ->
                                        String.join " "
                                            [ "Create"
                                            , context.localization.blockDevice
                                                |> Helpers.String.toTitleCase
                                            ]

                                    VolumeDetail pageModel ->
                                        String.join " "
                                            [ context.localization.blockDevice
                                                |> Helpers.String.toTitleCase
                                            , View.PageTitle.volumeName (GetterSetters.projectLookup outerModel.sharedModel projectId) pageModel.volumeUuid
                                            ]

                                    VolumeList _ ->
                                        String.join " "
                                            [ context.localization.blockDevice
                                                |> Helpers.String.pluralize
                                                |> Helpers.String.toTitleCase
                                            ]

                                    VolumeMountInstructions _ ->
                                        String.join " "
                                            [ "Mount"
                                            , context.localization.blockDevice
                                                |> Helpers.String.toTitleCase
                                            ]
                    in
                    [ renderProject
                    , renderProjectPage
                    ]
    in
    Element.row
        [ Element.paddingXY 10 4, Element.spacing 10 ]
    <|
        List.intersperse (Element.text ">") <|
            List.concat
                [ [ Element.text "Home" ], path ]
