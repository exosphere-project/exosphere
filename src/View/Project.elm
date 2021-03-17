module View.Project exposing (project)

import Element
import FeatherIcons
import Helpers.String
import Helpers.Url as UrlHelpers
import Set
import Style.Helpers as SH
import Style.Widgets.NumericTextInput.Types exposing (NumericTextInput(..))
import Types.Defaults as Defaults
import Types.Types exposing (Model, Msg(..), NonProjectViewConstructor(..), Project, ProjectIdentifier, ProjectSpecificMsgConstructor(..), ProjectViewConstructor(..), ProjectViewParams, ViewState(..))
import View.AttachVolume
import View.CreateServer
import View.CreateServerImage
import View.Helpers as VH
import View.Images
import View.Keypairs
import View.QuotaUsage
import View.ServerDetail
import View.ServerList
import View.Types
import View.Volumes
import Widget
import Widget.Style.Material


project : Model -> View.Types.Context -> Project -> ProjectViewParams -> ProjectViewConstructor -> Element.Element Msg
project model context p viewParams viewConstructor =
    let
        v =
            case viewConstructor of
                ListImages imageFilter sortTableParams ->
                    View.Images.imagesIfLoaded context p imageFilter sortTableParams

                ListProjectServers serverListViewParams ->
                    View.ServerList.serverList context p serverListViewParams

                ServerDetail serverUuid serverDetailViewParams ->
                    View.ServerDetail.serverDetail context p ( model.clientCurrentTime, model.timeZone ) serverDetailViewParams serverUuid

                CreateServer createServerViewParams ->
                    View.CreateServer.createServer context p createServerViewParams

                ListProjectVolumes deleteVolumeConfirmations ->
                    View.Volumes.volumes context p deleteVolumeConfirmations

                VolumeDetail volumeUuid deleteVolumeConfirmations ->
                    View.Volumes.volumeDetailView context p deleteVolumeConfirmations volumeUuid

                CreateVolume volName volSizeInput ->
                    View.Volumes.createVolume context p volName volSizeInput

                AttachVolumeModal maybeServerUuid maybeVolumeUuid ->
                    View.AttachVolume.attachVolume context p maybeServerUuid maybeVolumeUuid

                MountVolInstructions attachment ->
                    View.AttachVolume.mountVolInstructions context p attachment

                ListKeypairs deleteConfirmations ->
                    View.Keypairs.listKeypairs context p deleteConfirmations

                CreateKeypair keypairName publicKey ->
                    View.Keypairs.createKeypair context p keypairName publicKey

                CreateServerImage serverUuid imageName ->
                    View.CreateServerImage.createServerImage context p serverUuid imageName

                ListQuotaUsage ->
                    View.QuotaUsage.dashboard context p
    in
    Element.column
        (Element.width Element.fill
            :: VH.exoColumnAttributes
        )
        [ projectNav context p viewParams
        , v
        ]


projectNav : View.Types.Context -> Project -> ProjectViewParams -> Element.Element Msg
projectNav context p viewParams =
    Element.column [ Element.width Element.fill, Element.spacing 10 ]
        [ Element.el
            VH.heading2
          <|
            Element.text <|
                UrlHelpers.hostnameFromUrl p.endpoints.keystone
                    ++ " - "
                    ++ p.auth.project.name
        , Element.row [ Element.width Element.fill, Element.spacing 10 ]
            [ Element.el
                []
              <|
                Widget.textButton
                    (Widget.Style.Material.outlinedButton (SH.toMaterialPalette context.palette))
                    { text =
                        context.localization.virtualComputer
                            |> Helpers.String.pluralize
                            |> Helpers.String.toTitleCase
                    , onPress =
                        Just <|
                            ProjectMsg p.auth.project.uuid <|
                                SetProjectView <|
                                    ListProjectServers Defaults.serverListViewParams
                    }
            , Element.el
                []
              <|
                Widget.textButton
                    (Widget.Style.Material.outlinedButton (SH.toMaterialPalette context.palette))
                    { text =
                        context.localization.blockDevice
                            |> Helpers.String.pluralize
                            |> Helpers.String.toTitleCase
                    , onPress =
                        Just <| ProjectMsg p.auth.project.uuid <| SetProjectView <| ListProjectVolumes []
                    }
            , Element.el
                []
              <|
                Widget.textButton
                    (Widget.Style.Material.outlinedButton (SH.toMaterialPalette context.palette))
                    { text =
                        context.localization.pkiPublicKeyForSsh
                            |> Helpers.String.pluralize
                            |> Helpers.String.toTitleCase
                    , onPress =
                        Just <| ProjectMsg p.auth.project.uuid <| SetProjectView <| ListKeypairs []
                    }
            , Element.el [] <|
                Widget.textButton
                    (Widget.Style.Material.outlinedButton (SH.toMaterialPalette context.palette))
                    { text =
                        String.join " "
                            [ context.localization.maxResourcesPerProject
                                |> Helpers.String.toTitleCase
                            , "Usage"
                            ]
                    , onPress =
                        SetProjectView ListQuotaUsage
                            |> ProjectMsg p.auth.project.uuid
                            |> Just
                    }
            , Element.el
                -- TODO replace these
                [ Element.alignRight ]
              <|
                Widget.textButton
                    (Widget.Style.Material.textButton (SH.toMaterialPalette context.palette))
                    { text =
                        String.join " "
                            [ "Remove"
                            , Helpers.String.toTitleCase context.localization.unitOfTenancy
                            ]
                    , onPress =
                        Just <| ProjectMsg p.auth.project.uuid RemoveProject
                    }
            , Element.el
                [ Element.alignRight ]
                (createButton context p.auth.project.uuid viewParams.createPopup)
            ]
        ]


createButton : View.Types.Context -> ProjectIdentifier -> Bool -> Element.Element Msg
createButton context projectId expanded =
    let
        ( attribs, icon ) =
            if expanded then
                ( [ Element.below <|
                        Element.column
                            [ Element.spacing 5
                            , Element.paddingEach
                                { top = 5
                                , bottom = 0
                                , right = 0
                                , left = 0
                                }
                            ]
                            [ Widget.textButton
                                (Widget.Style.Material.outlinedButton (SH.toMaterialPalette context.palette))
                                { text =
                                    context.localization.virtualComputer
                                        |> Helpers.String.toTitleCase
                                , onPress =
                                    Just <|
                                        ProjectMsg projectId <|
                                            SetProjectView <|
                                                ListImages
                                                    { searchText = ""
                                                    , tags = Set.empty
                                                    , onlyOwnImages = False
                                                    , expandImageDetails = Set.empty
                                                    }
                                                    { title = "Name"
                                                    , asc = True
                                                    }
                                }

                            {- TODO store default values of CreateVolumeRequest (name and size) somewhere else, like global defaults imported by State.elm -}
                            , Widget.textButton
                                (Widget.Style.Material.outlinedButton (SH.toMaterialPalette context.palette))
                                { text = Helpers.String.toTitleCase context.localization.blockDevice
                                , onPress =
                                    Just <|
                                        ProjectMsg projectId <|
                                            SetProjectView <|
                                                CreateVolume "" (ValidNumericTextInput 10)
                                }
                            , Widget.textButton
                                (Widget.Style.Material.outlinedButton (SH.toMaterialPalette context.palette))
                                { text = Helpers.String.toTitleCase context.localization.pkiPublicKeyForSsh
                                , onPress =
                                    Just <|
                                        ProjectMsg projectId <|
                                            SetProjectView <|
                                                CreateKeypair "" ""
                                }
                            ]
                  ]
                , FeatherIcons.chevronUp
                )

            else
                ( []
                , FeatherIcons.chevronDown
                )
    in
    Element.column
        attribs
        [ Widget.iconButton
            (Widget.Style.Material.containedButton (SH.toMaterialPalette context.palette))
            { text = "Create"
            , icon =
                Element.row
                    [ Element.spacing 5 ]
                    [ Element.text "Create"
                    , Element.el []
                        (icon
                            |> FeatherIcons.toHtml []
                            |> Element.html
                        )
                    ]
            , onPress =
                Just <|
                    ProjectMsg projectId <|
                        ToggleCreatePopup
            }
        ]
