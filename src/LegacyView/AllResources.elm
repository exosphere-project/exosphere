module LegacyView.AllResources exposing (allResources)

import Element
import Element.Events as Events
import Element.Font as Font
import FeatherIcons
import Helpers.String
import LegacyView.ServerList
import Page.FloatingIpList
import Page.KeypairList
import Page.VolumeList
import Style.Helpers as SH
import Style.Widgets.Icon as Icon
import Types.Defaults as Defaults
import Types.OuterMsg exposing (OuterMsg(..))
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg
import Types.View
    exposing
        ( AllResourcesListViewParams
        , LoginView(..)
        , NonProjectViewConstructor(..)
        , ProjectViewConstructor(..)
        , ViewState(..)
        )
import View.Helpers as VH
import View.Types


allResources :
    View.Types.Context
    -> Project
    -> AllResourcesListViewParams
    -> Element.Element OuterMsg
allResources context p viewParams =
    let
        renderHeaderLink : Element.Element OuterMsg -> String -> OuterMsg -> Element.Element OuterMsg
        renderHeaderLink icon str msg =
            Element.row
                (VH.heading3 context.palette
                    ++ [ Element.spacing 12
                       , Events.onClick msg
                       , Element.mouseOver
                            [ Font.color
                                (context.palette.primary
                                    |> SH.toElementColor
                                )
                            ]
                       , Element.pointer
                       ]
                )
                [ icon
                , Element.text str
                ]
    in
    Element.column
        [ Element.spacing 25, Element.width Element.fill ]
        [ Element.column
            [ Element.width Element.fill ]
            [ renderHeaderLink
                (FeatherIcons.server
                    |> FeatherIcons.toHtml []
                    |> Element.html
                    |> Element.el []
                )
                (context.localization.virtualComputer
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                )
                (SetProjectView p.auth.project.uuid <|
                    ListProjectServers
                        Defaults.serverListViewParams
                )
            , LegacyView.ServerList.serverList context
                False
                p
                viewParams.serverListViewParams
                (\newParams ->
                    SetProjectView p.auth.project.uuid <|
                        AllResources { viewParams | serverListViewParams = newParams }
                )
            ]
        , Element.column
            [ Element.width Element.fill ]
            [ renderHeaderLink
                (FeatherIcons.hardDrive
                    |> FeatherIcons.toHtml []
                    |> Element.html
                    |> Element.el []
                )
                (context.localization.blockDevice
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                )
                (SetProjectView p.auth.project.uuid <|
                    VolumeList
                        Page.VolumeList.init
                )
            , Page.VolumeList.view context
                False
                p
                viewParams.volumeListViewParams
                |> Element.map VolumeListMsg
            ]
        , Element.column
            [ Element.width Element.fill ]
            [ renderHeaderLink
                (Icon.ipAddress (SH.toElementColor context.palette.on.background) 24)
                (context.localization.floatingIpAddress
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                )
                (SharedMsg <| SharedMsg.NavigateToView <| SharedMsg.FloatingIpList <| p.auth.project.uuid)
            , Page.FloatingIpList.view context
                p
                viewParams.floatingIpListViewParams
                False
                |> Element.map FloatingIpListMsg
            ]
        , Element.column
            [ Element.width Element.fill
            , Element.spacingXY 0 15 -- Because no quota view taking up space
            ]
            [ renderHeaderLink
                (FeatherIcons.key
                    |> FeatherIcons.toHtml []
                    |> Element.html
                    |> Element.el []
                )
                (context.localization.pkiPublicKeyForSsh
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                )
                (SharedMsg <| SharedMsg.NavigateToView <| SharedMsg.KeypairList <| p.auth.project.uuid)
            , Page.KeypairList.view context
                p
                viewParams.keypairListViewParams
                False
                |> Element.map KeypairListMsg
            ]
        ]
