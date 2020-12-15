module View.Project exposing (project)

import Element
import FeatherIcons
import Helpers.Helpers as Helpers
import Helpers.Url as UrlHelpers
import Set
import Style.Helpers as SH
import Style.Types
import Style.Widgets.NumericTextInput.Types exposing (NumericTextInput(..))
import Types.Defaults as Defaults
import Types.Types
    exposing
        ( Model
        , Msg(..)
        , NonProjectViewConstructor(..)
        , Project
        , ProjectIdentifier
        , ProjectSpecificMsgConstructor(..)
        , ProjectViewConstructor(..)
        , ProjectViewParams
        , ViewState(..)
        )
import View.AttachVolume
import View.CreateServer
import View.CreateServerImage
import View.Helpers as VH
import View.Images
import View.QuotaUsage
import View.ServerDetail
import View.ServerList
import View.Volumes
import Widget
import Widget.Style.Material


project : Model -> Style.Types.ExoPalette -> Project -> ProjectViewParams -> ProjectViewConstructor -> Element.Element Msg
project model palette p viewParams viewConstructor =
    let
        v =
            case viewConstructor of
                ListImages imageFilter sortTableParams ->
                    View.Images.imagesIfLoaded palette p imageFilter sortTableParams

                ListProjectServers serverListViewParams ->
                    View.ServerList.serverList palette p serverListViewParams

                ServerDetail serverUuid serverDetailViewParams ->
                    View.ServerDetail.serverDetail palette p (Helpers.appIsElectron model) ( model.clientCurrentTime, model.timeZone ) serverDetailViewParams serverUuid

                CreateServer createServerViewParams ->
                    View.CreateServer.createServer palette p createServerViewParams

                ListProjectVolumes deleteVolumeConfirmations ->
                    View.Volumes.volumes palette p deleteVolumeConfirmations

                VolumeDetail volumeUuid deleteVolumeConfirmations ->
                    View.Volumes.volumeDetailView palette p deleteVolumeConfirmations volumeUuid

                CreateVolume volName volSizeInput ->
                    View.Volumes.createVolume palette p volName volSizeInput

                AttachVolumeModal maybeServerUuid maybeVolumeUuid ->
                    View.AttachVolume.attachVolume palette p maybeServerUuid maybeVolumeUuid

                MountVolInstructions attachment ->
                    View.AttachVolume.mountVolInstructions palette p attachment

                CreateServerImage serverUuid imageName ->
                    View.CreateServerImage.createServerImage palette p serverUuid imageName

                ListQuotaUsage ->
                    View.QuotaUsage.dashboard palette p
    in
    Element.column
        (Element.width Element.fill
            :: VH.exoColumnAttributes
        )
        [ projectNav palette p viewParams
        , v
        ]


projectNav : Style.Types.ExoPalette -> Project -> ProjectViewParams -> Element.Element Msg
projectNav palette p viewParams =
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
                    (Widget.Style.Material.outlinedButton (SH.toMaterialPalette palette))
                    { text = "Servers"
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
                    (Widget.Style.Material.outlinedButton (SH.toMaterialPalette palette))
                    { text = "Volumes"
                    , onPress =
                        Just <| ProjectMsg p.auth.project.uuid <| SetProjectView <| ListProjectVolumes []
                    }
            , Element.el [] <|
                Widget.textButton
                    (Widget.Style.Material.outlinedButton (SH.toMaterialPalette palette))
                    { text = "Quota/Usage"
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
                    (Widget.Style.Material.textButton (SH.toMaterialPalette palette))
                    { text = "Remove Project"
                    , onPress =
                        Just <| ProjectMsg p.auth.project.uuid RemoveProject
                    }
            , Element.el
                [ Element.alignRight ]
                (createButton palette p.auth.project.uuid viewParams.createPopup)
            ]
        ]


createButton : Style.Types.ExoPalette -> ProjectIdentifier -> Bool -> Element.Element Msg
createButton palette projectId expanded =
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
                                (Widget.Style.Material.outlinedButton (SH.toMaterialPalette palette))
                                { text = "Server"
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
                                (Widget.Style.Material.outlinedButton (SH.toMaterialPalette palette))
                                { text = "Volume"
                                , onPress =
                                    Just <|
                                        ProjectMsg projectId <|
                                            SetProjectView <|
                                                CreateVolume "" (ValidNumericTextInput 10)
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
            (Widget.Style.Material.containedButton (SH.toMaterialPalette palette))
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
