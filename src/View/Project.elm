module View.Project exposing (project)

import Element
import Element.Background as Background
import Element.Border as Border
import FeatherIcons
import Helpers.String
import Helpers.Url as UrlHelpers
import Style.Helpers as SH
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
import View.AllResources
import View.AttachVolume
import View.CreateServer
import View.CreateServerImage
import View.FloatingIps
import View.Helpers as VH
import View.Images
import View.Keypairs
import View.ServerDetail
import View.ServerList
import View.Types
import View.Volumes
import Widget


project : Model -> View.Types.Context -> Project -> ProjectViewParams -> ProjectViewConstructor -> Element.Element Msg
project model context p viewParams viewConstructor =
    let
        v =
            case viewConstructor of
                AllResources allResourcesViewParams ->
                    View.AllResources.allResources
                        context
                        p
                        allResourcesViewParams

                ListImages imageFilter sortTableParams ->
                    View.Images.imagesIfLoaded context p imageFilter sortTableParams

                ListProjectServers serverListViewParams ->
                    View.ServerList.serverList context
                        True
                        p
                        serverListViewParams
                        (\newParams ->
                            ProjectMsg p.auth.project.uuid <|
                                SetProjectView <|
                                    ListProjectServers newParams
                        )

                ServerDetail serverUuid serverDetailViewParams ->
                    View.ServerDetail.serverDetail context p ( model.clientCurrentTime, model.timeZone ) serverDetailViewParams serverUuid

                CreateServer createServerViewParams ->
                    View.CreateServer.createServer context p createServerViewParams

                ListProjectVolumes volumeListViewParams ->
                    View.Volumes.volumes context
                        True
                        p
                        volumeListViewParams
                        (\newParams ->
                            ProjectMsg p.auth.project.uuid <|
                                SetProjectView <|
                                    ListProjectVolumes newParams
                        )

                VolumeDetail volumeUuid deleteVolumeConfirmations ->
                    View.Volumes.volumeDetailView context
                        p
                        deleteVolumeConfirmations
                        (\newParams ->
                            ProjectMsg p.auth.project.uuid <|
                                SetProjectView <|
                                    VolumeDetail volumeUuid newParams
                        )
                        volumeUuid

                CreateVolume volName volSizeInput ->
                    View.Volumes.createVolume context p volName volSizeInput

                AttachVolumeModal maybeServerUuid maybeVolumeUuid ->
                    View.AttachVolume.attachVolume context p maybeServerUuid maybeVolumeUuid

                MountVolInstructions attachment ->
                    View.AttachVolume.mountVolInstructions context p attachment

                ListFloatingIps floatingIpListViewParams ->
                    View.FloatingIps.floatingIps context
                        True
                        p
                        floatingIpListViewParams
                        (\newParams ->
                            ProjectMsg p.auth.project.uuid <|
                                SetProjectView <|
                                    ListFloatingIps newParams
                        )

                AssignFloatingIp assignFloatingIpViewParams ->
                    View.FloatingIps.assignFloatingIp
                        context
                        p
                        assignFloatingIpViewParams

                ListKeypairs keypairListViewParams ->
                    View.Keypairs.listKeypairs context
                        True
                        p
                        keypairListViewParams
                        (\newParams ->
                            ProjectMsg p.auth.project.uuid <|
                                SetProjectView <|
                                    ListKeypairs newParams
                        )

                CreateKeypair keypairName publicKey ->
                    View.Keypairs.createKeypair context p keypairName publicKey

                CreateServerImage serverUuid imageName ->
                    View.CreateServerImage.createServerImage context p serverUuid imageName
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
    let
        edges =
            VH.edges

        removeText =
            String.join " "
                [ "Remove"
                , Helpers.String.toTitleCase context.localization.unitOfTenancy
                ]
    in
    Element.row [ Element.width Element.fill, Element.spacing 10, Element.paddingEach { edges | bottom = 10 } ]
        [ Element.el
            (VH.heading2 context.palette
                -- Removing bottom border from this heading because it runs into buttons to the right and looks weird
                ++ [ Border.width 0
                   ]
            )
          <|
            Element.text <|
                UrlHelpers.hostnameFromUrl p.endpoints.keystone
                    ++ " - "
                    ++ p.auth.project.name
        , Element.el
            [ Element.alignRight ]
          <|
            Widget.iconButton
                (SH.materialStyle context.palette).button
                { icon =
                    Element.row [ Element.spacing 10 ]
                        [ Element.text removeText
                        , FeatherIcons.logOut |> FeatherIcons.toHtml [] |> Element.html |> Element.el []
                        ]
                , text = removeText
                , onPress =
                    Just <| ProjectMsg p.auth.project.uuid RemoveProject
                }
        , Element.el
            [ Element.alignRight
            , Element.paddingEach
                { top = 0
                , right = 15
                , bottom = 0
                , left = 0
                }
            ]
            (createButton context p.auth.project.uuid viewParams.createPopup)
        ]


createButton : View.Types.Context -> ProjectIdentifier -> Bool -> Element.Element Msg
createButton context projectId expanded =
    let
        materialStyle =
            (SH.materialStyle context.palette).button

        buttonStyle =
            { materialStyle
                | container = Element.width Element.fill :: materialStyle.container
            }

        renderButton : Element.Element Never -> String -> Maybe Msg -> Element.Element Msg
        renderButton icon_ text onPress =
            Widget.iconButton
                buttonStyle
                { icon =
                    Element.row
                        [ Element.spacing 10
                        , Element.width Element.fill
                        ]
                        [ Element.el [] icon_
                        , Element.text text
                        ]
                , text =
                    text
                , onPress =
                    onPress
                }

        dropdown =
            Element.column
                [ Element.alignRight
                , Element.moveDown 5
                , Element.spacing 5
                , Element.paddingEach
                    { top = 5
                    , right = 6
                    , bottom = 5
                    , left = 6
                    }
                , Background.color <| SH.toElementColor context.palette.background
                , Border.shadow
                    { blur = 10
                    , color = SH.toElementColorWithOpacity context.palette.muted 0.2
                    , offset = ( 0, 2 )
                    , size = 1
                    }
                , Border.width 1
                , Border.color <| SH.toElementColor context.palette.muted
                , Border.rounded 4
                ]
                [ renderButton
                    (FeatherIcons.server |> FeatherIcons.toHtml [] |> Element.html)
                    (context.localization.virtualComputer
                        |> Helpers.String.toTitleCase
                    )
                    (Just <|
                        ProjectMsg projectId <|
                            SetProjectView <|
                                ListImages
                                    Defaults.imageListViewParams
                                    { title = "Name"
                                    , asc = True
                                    }
                    )
                , renderButton
                    (FeatherIcons.hardDrive |> FeatherIcons.toHtml [] |> Element.html)
                    (context.localization.blockDevice
                        |> Helpers.String.toTitleCase
                    )
                    (Just <|
                        ProjectMsg projectId <|
                            SetProjectView <|
                                -- TODO store default values of CreateVolumeRequest (name and size) somewhere else, like global defaults imported by State.elm
                                CreateVolume "" (ValidNumericTextInput 10)
                    )
                , renderButton
                    (FeatherIcons.key |> FeatherIcons.toHtml [] |> Element.html)
                    (context.localization.pkiPublicKeyForSsh
                        |> Helpers.String.toTitleCase
                    )
                    (Just <|
                        ProjectMsg projectId <|
                            SetProjectView <|
                                CreateKeypair "" ""
                    )
                ]

        ( attribs, icon ) =
            if expanded then
                ( [ Element.below dropdown ]
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
            (SH.materialStyle context.palette).primaryButton
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
