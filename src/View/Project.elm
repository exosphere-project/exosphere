module View.Project exposing (project)

import Element
import Helpers.Helpers as Helpers
import Set
import Style.Theme
import Style.Widgets.Icon exposing (downArrow, upArrow)
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
import View.ServerDetail
import View.ServerList
import View.Volumes
import Widget
import Widget.Style.Material


project : Model -> Project -> ProjectViewParams -> ProjectViewConstructor -> Element.Element Msg
project model p viewParams viewConstructor =
    let
        v =
            case viewConstructor of
                ListImages imageFilter sortTableParams ->
                    View.Images.imagesIfLoaded model.globalDefaults p imageFilter sortTableParams

                ListProjectServers serverListViewParams ->
                    View.ServerList.serverList p serverListViewParams

                ServerDetail serverUuid serverDetailViewParams ->
                    View.ServerDetail.serverDetail p model.isElectron serverDetailViewParams serverUuid

                CreateServer createServerRequest ->
                    View.CreateServer.createServer p createServerRequest

                ListProjectVolumes deleteVolumeConfirmations ->
                    View.Volumes.volumes p deleteVolumeConfirmations

                VolumeDetail volumeUuid deleteVolumeConfirmations ->
                    View.Volumes.volumeDetailView p deleteVolumeConfirmations volumeUuid

                CreateVolume volName volSizeStr ->
                    View.Volumes.createVolume p volName volSizeStr

                AttachVolumeModal maybeServerUuid maybeVolumeUuid ->
                    View.AttachVolume.attachVolume p maybeServerUuid maybeVolumeUuid

                MountVolInstructions attachment ->
                    View.AttachVolume.mountVolInstructions p attachment

                CreateServerImage serverUuid imageName ->
                    View.CreateServerImage.createServerImage p serverUuid imageName
    in
    Element.column
        (Element.width Element.fill
            :: VH.exoColumnAttributes
        )
        [ projectNav p viewParams
        , v
        ]


projectNav : Project -> ProjectViewParams -> Element.Element Msg
projectNav p viewParams =
    Element.column [ Element.width Element.fill, Element.spacing 10 ]
        [ Element.el
            VH.heading2
          <|
            Element.text <|
                Helpers.hostnameFromUrl p.endpoints.keystone
                    ++ " - "
                    ++ p.auth.project.name
        , Element.row [ Element.width Element.fill, Element.spacing 10 ]
            [ Element.el
                []
              <|
                Widget.textButton
                    (Widget.Style.Material.outlinedButton Style.Theme.exoPalette)
                    { text = "My Servers"
                    , onPress =
                        Just <|
                            ProjectMsg (Helpers.getProjectId p) <|
                                SetProjectView <|
                                    ListProjectServers { onlyOwnServers = True, deleteConfirmations = [] }
                    }
            , Element.el
                []
              <|
                Widget.textButton
                    (Widget.Style.Material.outlinedButton Style.Theme.exoPalette)
                    { text = "My Volumes"
                    , onPress =
                        Just <| ProjectMsg (Helpers.getProjectId p) <| SetProjectView <| ListProjectVolumes []
                    }
            , Element.el
                -- TODO replace these
                [ Element.alignRight ]
              <|
                Widget.textButton
                    (Widget.Style.Material.textButton Style.Theme.exoPalette)
                    { text = "Remove Project"
                    , onPress =
                        Just <| ProjectMsg (Helpers.getProjectId p) RemoveProject
                    }
            , Element.el
                [ Element.alignRight ]
                (createButton (Helpers.getProjectId p) viewParams.createPopup)
            ]
        ]


createButton : ProjectIdentifier -> Bool -> Element.Element Msg
createButton projectId expanded =
    if expanded then
        let
            belowStuff =
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
                        (Widget.Style.Material.outlinedButton Style.Theme.exoPalette)
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
                        (Widget.Style.Material.outlinedButton Style.Theme.exoPalette)
                        { text = "Volume"
                        , onPress =
                            Just <|
                                ProjectMsg projectId <|
                                    SetProjectView <|
                                        CreateVolume "" "10"
                        }
                    ]
        in
        Element.column
            [ Element.below belowStuff ]
            [ Widget.iconButton
                (Widget.Style.Material.containedButton Style.Theme.exoPalette)
                { text = "Create"
                , icon =
                    Element.row
                        [ Element.spacing 5 ]
                        [ Element.text "Create"
                        , upArrow (Element.rgb255 255 255 255) 15
                        ]
                , onPress =
                    Just <|
                        ProjectMsg projectId <|
                            ToggleCreatePopup
                }
            ]

    else
        Element.column
            []
            [ Widget.iconButton
                (Widget.Style.Material.containedButton Style.Theme.exoPalette)
                { text = "Create"
                , icon =
                    Element.row
                        [ Element.spacing 5 ]
                        [ Element.text "Create"
                        , downArrow (Element.rgb255 255 255 255) 15
                        ]
                , onPress =
                    Just <|
                        ProjectMsg projectId <|
                            ToggleCreatePopup
                }
            ]
