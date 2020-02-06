module View.Project exposing (project)

import Color
import Element
import Framework.Button as Button
import Framework.Modifier as Modifier
import Helpers.Helpers as Helpers
import Style.Widgets.Icon exposing (downArrow, upArrow)
import Style.Widgets.IconButton exposing (iconButton)
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
import View.Servers
import View.Volumes


project : Model -> Project -> ProjectViewParams -> ProjectViewConstructor -> Element.Element Msg
project model p viewParams viewConstructor =
    let
        v =
            case viewConstructor of
                ListImages imageFilter ->
                    View.Images.imagesIfLoaded model.globalDefaults p imageFilter

                ListProjectServers serverFilter deleteConfirmations ->
                    View.Servers.servers p serverFilter deleteConfirmations

                ServerDetail serverUuid serverDetailViewParams ->
                    View.Servers.serverDetail model.isElectron p serverUuid serverDetailViewParams

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

        {- TODO nest these somehow, perhaps put the "create server" and "create volume" buttons as a dropdown under a big "Create" button -}
        , Element.row [ Element.width Element.fill, Element.spacing 10 ]
            [ Element.el
                []
                (Button.button
                    []
                    (Just <|
                        ProjectMsg (Helpers.getProjectId p) <|
                            SetProjectView <|
                                ListProjectServers { onlyOwnServers = False } []
                    )
                    "My Servers"
                )
            , Element.el []
                (Button.button
                    []
                    (Just <| ProjectMsg (Helpers.getProjectId p) <| SetProjectView <| ListProjectVolumes [])
                    "My Volumes"
                )
            , Element.el
                [ Element.alignRight ]
                (Button.button [ Modifier.Muted ] (Just <| ProjectMsg (Helpers.getProjectId p) RemoveProject) "Remove Project")
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
                    [ Button.button
                        []
                        (Just <|
                            ProjectMsg projectId <|
                                SetProjectView <|
                                    ListImages { searchText = "", tag = "", onlyOwnImages = False }
                        )
                        "Server"

                    {- TODO store default values of CreateVolumeRequest (name and size) somewhere else, like global defaults imported by State.elm -}
                    , Button.button
                        []
                        (Just <|
                            ProjectMsg projectId <|
                                SetProjectView <|
                                    CreateVolume "" "10"
                        )
                        "Volume"
                    ]
        in
        Element.column
            [ Element.below belowStuff ]
            [ iconButton
                [ Modifier.Primary ]
                (Just <|
                    ProjectMsg projectId <|
                        ToggleCreatePopup
                )
                (Element.row
                    [ Element.spacing 5 ]
                    [ Element.text "Create"
                    , upArrow Color.white 15
                    ]
                )
            ]

    else
        Element.column
            []
            [ iconButton
                [ Modifier.Primary ]
                (Just <|
                    ProjectMsg projectId <|
                        ToggleCreatePopup
                )
                (Element.row
                    [ Element.spacing 5 ]
                    [ Element.text "Create"
                    , downArrow Color.white 15
                    ]
                )
            ]
