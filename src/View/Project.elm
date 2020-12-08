module View.Project exposing (project)

import Element
import Helpers.Helpers as Helpers
import Helpers.Url as UrlHelpers
import Set
import Style.Theme
import Style.Widgets.Icon exposing (downArrow, upArrow)
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
        , Style
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


project : Model -> Project -> ProjectViewParams -> ProjectViewConstructor -> Element.Element Msg
project model p viewParams viewConstructor =
    let
        v =
            case viewConstructor of
                ListImages imageFilter sortTableParams ->
                    View.Images.imagesIfLoaded model.style p imageFilter sortTableParams

                ListProjectServers serverListViewParams ->
                    View.ServerList.serverList model.style p serverListViewParams

                ServerDetail serverUuid serverDetailViewParams ->
                    View.ServerDetail.serverDetail model.style p (Helpers.appIsElectron model) ( model.clientCurrentTime, model.timeZone ) serverDetailViewParams serverUuid

                CreateServer createServerViewParams ->
                    View.CreateServer.createServer model.style p createServerViewParams

                ListProjectVolumes deleteVolumeConfirmations ->
                    View.Volumes.volumes model.style p deleteVolumeConfirmations

                VolumeDetail volumeUuid deleteVolumeConfirmations ->
                    View.Volumes.volumeDetailView model.style p deleteVolumeConfirmations volumeUuid

                CreateVolume volName volSizeInput ->
                    View.Volumes.createVolume model.style p volName volSizeInput

                AttachVolumeModal maybeServerUuid maybeVolumeUuid ->
                    View.AttachVolume.attachVolume model.style p maybeServerUuid maybeVolumeUuid

                MountVolInstructions attachment ->
                    View.AttachVolume.mountVolInstructions model.style p attachment

                CreateServerImage serverUuid imageName ->
                    View.CreateServerImage.createServerImage model.style p serverUuid imageName

                ListQuotaUsage ->
                    View.QuotaUsage.dashboard model.style p
    in
    Element.column
        (Element.width Element.fill
            :: VH.exoColumnAttributes
        )
        [ projectNav model.style p viewParams
        , v
        ]


projectNav : Style -> Project -> ProjectViewParams -> Element.Element Msg
projectNav style p viewParams =
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
                    (Widget.Style.Material.outlinedButton (Style.Theme.toMaterialPalette style.palette))
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
                    (Widget.Style.Material.outlinedButton (Style.Theme.toMaterialPalette style.palette))
                    { text = "Volumes"
                    , onPress =
                        Just <| ProjectMsg p.auth.project.uuid <| SetProjectView <| ListProjectVolumes []
                    }
            , Element.el [] <|
                Widget.textButton
                    (Widget.Style.Material.outlinedButton (Style.Theme.toMaterialPalette style.palette))
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
                    (Widget.Style.Material.textButton (Style.Theme.toMaterialPalette style.palette))
                    { text = "Remove Project"
                    , onPress =
                        Just <| ProjectMsg p.auth.project.uuid RemoveProject
                    }
            , Element.el
                [ Element.alignRight ]
                (createButton style p.auth.project.uuid viewParams.createPopup)
            ]
        ]


createButton : Style -> ProjectIdentifier -> Bool -> Element.Element Msg
createButton style projectId expanded =
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
                        (Widget.Style.Material.outlinedButton (Style.Theme.toMaterialPalette style.palette))
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
                        (Widget.Style.Material.outlinedButton (Style.Theme.toMaterialPalette style.palette))
                        { text = "Volume"
                        , onPress =
                            Just <|
                                ProjectMsg projectId <|
                                    SetProjectView <|
                                        CreateVolume "" (ValidNumericTextInput 10)
                        }
                    ]
        in
        Element.column
            [ Element.below belowStuff ]
            [ Widget.iconButton
                (Widget.Style.Material.containedButton (Style.Theme.toMaterialPalette style.palette))
                { text = "Create"
                , icon =
                    Element.row
                        [ Element.spacing 5 ]
                        [ Element.text "Create"
                        , upArrow (VH.toElementColor style.palette.on.primary) 15
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
                (Widget.Style.Material.containedButton (Style.Theme.toMaterialPalette style.palette))
                { text = "Create"
                , icon =
                    Element.row
                        [ Element.spacing 5 ]
                        [ Element.text "Create"
                        , downArrow (VH.toElementColor style.palette.on.primary) 15
                        ]
                , onPress =
                    Just <|
                        ProjectMsg projectId <|
                            ToggleCreatePopup
                }
            ]
