module View.FloatingIps exposing (floatingIps)

import Element exposing (Element)
import Element.Font as Font
import Helpers.GetterSetters as GetterSetters
import Helpers.String
import OpenStack.Types as OSTypes
import RemoteData
import Style.Helpers as SH
import Style.Widgets.Button
import Style.Widgets.Card
import Style.Widgets.CopyableText
import Types.Types exposing (FloatingIpListViewParams, Msg(..), Project, ProjectSpecificMsgConstructor(..))
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
                    (List.map
                        (renderFloatingIpCard context project viewParams toMsg)
                        ips
                    )

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
                    Element.text <|
                        String.join " "
                            [ "Assigned to"
                            , context.localization.virtualComputer
                            , server.osProps.name
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
