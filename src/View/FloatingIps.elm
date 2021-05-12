module View.FloatingIps exposing (floatingIps)

import Element
import Element.Font as Font
import FeatherIcons
import Helpers.GetterSetters as GetterSetters
import Helpers.String
import OpenStack.Types as OSTypes
import RemoteData
import Style.Helpers as SH
import Style.Widgets.Button
import Style.Widgets.Card
import Style.Widgets.CopyableText
import Style.Widgets.IconButton
import Types.Defaults as Defaults
import Types.Types exposing (FloatingIpListViewParams, Msg(..), Project, ProjectSpecificMsgConstructor(..), ProjectViewConstructor(..))
import View.Helpers as VH
import View.QuotaUsage
import View.Types
import Widget
import Widget.Style.Material


floatingIps : View.Types.Context -> Bool -> Project -> FloatingIpListViewParams -> (FloatingIpListViewParams -> Msg) -> Element.Element Msg
floatingIps context showHeading project viewParams toMsg =
    let
        renderFloatingIps : List OSTypes.IpAddress -> Element.Element Msg
        renderFloatingIps ips =
            let
                ipAssignedToServersWeKnowAbout ip =
                    case GetterSetters.getFloatingIpServer project ip.address of
                        Just _ ->
                            True

                        Nothing ->
                            False

                ( ipsAssignedToServers, ipsNotAssignedToServers ) =
                    List.partition ipAssignedToServersWeKnowAbout ips
            in
            if List.isEmpty ips then
                Element.column
                    (VH.exoColumnAttributes ++ [ Element.paddingXY 10 0 ])
                    [ Element.text <|
                        String.concat
                            [ "You don't have any "
                            , context.localization.floatingIpAddress
                                |> Helpers.String.pluralize
                            , " yet. They will be created when you launch "
                            , context.localization.virtualComputer
                                |> Helpers.String.indefiniteArticle
                            , " "
                            , context.localization.virtualComputer
                            , "."
                            ]
                    ]

            else
                Element.column
                    (VH.exoColumnAttributes
                        ++ [ Element.paddingXY 10 0, Element.width Element.fill ]
                    )
                <|
                    List.concat
                        [ List.map
                            (renderFloatingIpCard context project viewParams toMsg)
                            ipsNotAssignedToServers
                        , [ ipsAssignedToServersExpander context viewParams toMsg ipsAssignedToServers ]
                        , if viewParams.hideAssignedIps then
                            []

                          else
                            List.map (renderFloatingIpCard context project viewParams toMsg) ipsAssignedToServers
                        ]

        floatingIpsUsedCount =
            project.floatingIps
                -- Defaulting to 0 if not loaded yet, not the greatest factoring
                |> RemoteData.withDefault []
                |> List.length
    in
    Element.column
        [ Element.spacing 20, Element.width Element.fill ]
        [ if showHeading then
            Element.el VH.heading2 <|
                Element.text
                    (context.localization.floatingIpAddress
                        |> Helpers.String.pluralize
                        |> Helpers.String.toTitleCase
                    )

          else
            Element.none
        , View.QuotaUsage.floatingIpQuotaDetails context project.computeQuota floatingIpsUsedCount
        , VH.renderWebData
            context
            project.floatingIps
            (Helpers.String.pluralize context.localization.floatingIpAddress)
            renderFloatingIps
        ]


renderFloatingIpCard :
    View.Types.Context
    -> Project
    -> FloatingIpListViewParams
    -> (FloatingIpListViewParams -> Msg)
    -> OSTypes.IpAddress
    -> Element.Element Msg
renderFloatingIpCard context project viewParams toMsg ip =
    let
        subtitle =
            actionButtons context project toMsg viewParams ip

        cardBody =
            case GetterSetters.getFloatingIpServer project ip.address of
                Just server ->
                    Element.row [ Element.spacing 5 ]
                        [ Element.text <|
                            String.join " "
                                [ "Assigned to"
                                , context.localization.virtualComputer
                                , server.osProps.name
                                ]
                        , Style.Widgets.IconButton.goToButton
                            context.palette
                            (Just
                                (ProjectMsg project.auth.project.uuid <|
                                    SetProjectView <|
                                        ServerDetail server.osProps.uuid Defaults.serverDetailViewParams
                                )
                            )
                        ]

                Nothing ->
                    case ip.status of
                        Just OSTypes.IpAddressActive ->
                            Element.text "Active"

                        Just OSTypes.IpAddressDown ->
                            Element.text "Unassigned"

                        Just OSTypes.IpAddressError ->
                            Element.text "Status: Error"

                        Nothing ->
                            Element.none
    in
    Style.Widgets.Card.exoCard
        context.palette
        (Style.Widgets.CopyableText.copyableText
            context.palette
            [ Font.family [ Font.monospace ] ]
            ip.address
        )
        subtitle
        cardBody


actionButtons : View.Types.Context -> Project -> (FloatingIpListViewParams -> Msg) -> FloatingIpListViewParams -> OSTypes.IpAddress -> Element.Element Msg
actionButtons context project toMsg viewParams ip =
    let
        confirmationNeeded =
            List.member ip.address viewParams.deleteConfirmations

        deleteButton =
            if confirmationNeeded then
                Element.row [ Element.spacing 10 ]
                    [ Element.text "Confirm delete?"
                    , Widget.textButton
                        (Style.Widgets.Button.dangerButton context.palette)
                        { text = "Delete"
                        , onPress =
                            ip.uuid
                                |> Maybe.map
                                    (\uuid ->
                                        ProjectMsg
                                            project.auth.project.uuid
                                            (RequestDeleteFloatingIp uuid)
                                    )
                        }
                    , Widget.textButton
                        (Widget.Style.Material.outlinedButton (SH.toMaterialPalette context.palette))
                        { text = "Cancel"
                        , onPress =
                            Just <|
                                toMsg
                                    { viewParams
                                        | deleteConfirmations =
                                            List.filter
                                                ((/=) ip.address)
                                                viewParams.deleteConfirmations
                                    }
                        }
                    ]

            else
                Widget.textButton
                    (Style.Widgets.Button.dangerButton context.palette)
                    { text = "Delete"
                    , onPress =
                        Just <|
                            toMsg
                                { viewParams
                                    | deleteConfirmations =
                                        ip.address
                                            :: viewParams.deleteConfirmations
                                }
                    }
    in
    Element.row
        [ Element.width Element.fill ]
        [ Element.el [ Element.alignRight ] deleteButton ]



-- TODO factor this out with onlyOwnExpander in ServerList.elm


ipsAssignedToServersExpander :
    View.Types.Context
    -> FloatingIpListViewParams
    -> (FloatingIpListViewParams -> Msg)
    -> List OSTypes.IpAddress
    -> Element.Element Msg
ipsAssignedToServersExpander context viewParams toMsg ipsAssignedToServers =
    let
        numIpsAssignedToServers =
            List.length ipsAssignedToServers

        statusText =
            let
                ( ipsPluralization, serversPluralization ) =
                    if numIpsAssignedToServers == 1 then
                        ( context.localization.floatingIpAddress
                        , String.join " "
                            [ context.localization.virtualComputer
                                |> Helpers.String.indefiniteArticle
                            , context.localization.virtualComputer
                            ]
                        )

                    else
                        ( Helpers.String.pluralize context.localization.floatingIpAddress
                        , Helpers.String.pluralize context.localization.virtualComputer
                        )
            in
            if viewParams.hideAssignedIps then
                String.concat
                    [ "Hiding "
                    , String.fromInt numIpsAssignedToServers
                    , " "
                    , ipsPluralization
                    , " assigned to "
                    , serversPluralization
                    ]

            else
                String.join " "
                    [ context.localization.floatingIpAddress
                        |> Helpers.String.pluralize
                        |> Helpers.String.toTitleCase
                    , "assigned to"
                    , context.localization.virtualComputer
                        |> Helpers.String.pluralize
                    ]

        ( ( changeActionVerb, changeActionIcon ), newServerListViewParams ) =
            if viewParams.hideAssignedIps then
                ( ( "Show", FeatherIcons.chevronDown )
                , { viewParams
                    | hideAssignedIps = False
                  }
                )

            else
                ( ( "Hide", FeatherIcons.chevronUp )
                , { viewParams
                    | hideAssignedIps = True
                  }
                )

        changeOnlyOwnMsg : Msg
        changeOnlyOwnMsg =
            toMsg newServerListViewParams

        changeButton =
            Widget.button
                (Widget.Style.Material.textButton (SH.toMaterialPalette context.palette))
                { onPress = Just changeOnlyOwnMsg
                , icon =
                    changeActionIcon
                        |> FeatherIcons.toHtml []
                        |> Element.html
                        |> Element.el []
                , text = changeActionVerb
                }
    in
    if numIpsAssignedToServers == 0 then
        Element.none

    else
        Element.column [ Element.spacing 3, Element.padding 0, Element.width Element.fill ]
            [ Element.el
                [ Element.centerX, Font.size 14 ]
                (Element.text statusText)
            , Element.el
                [ Element.centerX ]
                changeButton
            ]
