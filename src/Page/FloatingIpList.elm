module Page.FloatingIpList exposing (Model, Msg(..), init, update, view)

import Element
import Element.Background as Background
import Element.Font as Font
import FeatherIcons
import Helpers.Formatting exposing (humanCount)
import Helpers.GetterSetters as GetterSetters
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.String
import OpenStack.Types as OSTypes
import Page.QuotaUsage
import Route
import Set
import Style.Helpers as SH
import Style.Widgets.Card
import Style.Widgets.CopyableText
import Style.Widgets.Icon as Icon
import Style.Widgets.IconButton
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types
import Widget


type alias Model =
    { showHeading : Bool
    , deleteConfirmations : Set.Set OSTypes.IpAddressUuid
    , hideAssignedIps : Bool
    }


type Msg
    = GotHideAssignedIps Bool
    | GotUnassign OSTypes.IpAddressUuid
    | GotDeleteNeedsConfirm OSTypes.IpAddressUuid
    | GotDeleteConfirm OSTypes.IpAddressUuid
    | GotDeleteCancel OSTypes.IpAddressUuid
    | SharedMsg SharedMsg.SharedMsg
    | NoOp


init : Bool -> Model
init showHeading =
    { showHeading = showHeading
    , deleteConfirmations = Set.empty
    , hideAssignedIps = True
    }


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg project model =
    case msg of
        GotHideAssignedIps hidden ->
            let
                newModel =
                    { model | hideAssignedIps = hidden }
            in
            ( newModel, Cmd.none, SharedMsg.NoOp )

        GotUnassign ipUuid ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg project.auth.project.uuid <| SharedMsg.RequestUnassignFloatingIp ipUuid
            )

        GotDeleteNeedsConfirm ipUuid ->
            let
                newModel =
                    { model | deleteConfirmations = Set.insert ipUuid model.deleteConfirmations }
            in
            ( newModel, Cmd.none, SharedMsg.NoOp )

        GotDeleteConfirm ipUuid ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg project.auth.project.uuid (SharedMsg.RequestDeleteFloatingIp ipUuid)
            )

        GotDeleteCancel ipUuid ->
            let
                newModel =
                    { model | deleteConfirmations = Set.remove ipUuid model.deleteConfirmations }
            in
            ( newModel, Cmd.none, SharedMsg.NoOp )

        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )

        NoOp ->
            ( model, Cmd.none, SharedMsg.NoOp )


view : View.Types.Context -> Project -> Model -> Element.Element Msg
view context project model =
    let
        renderFloatingIps : List OSTypes.FloatingIp -> Element.Element Msg
        renderFloatingIps ips =
            let
                -- Warn the user when their project has at least this many unassigned floating IPs.
                -- Perhaps in the future this behavior becomes configurable at runtime.
                ipScarcityWarningThreshold =
                    2

                ipsSorted =
                    List.sortBy .address ips

                ipAssignedToAResource ip =
                    case ip.portUuid of
                        Just _ ->
                            True

                        Nothing ->
                            False

                ( ipsAssignedToResources, ipsNotAssignedToResources ) =
                    List.partition ipAssignedToAResource ipsSorted
            in
            if List.isEmpty ipsSorted then
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
                        [ if List.length ipsNotAssignedToResources >= ipScarcityWarningThreshold then
                            [ ipScarcityWarning context ]

                          else
                            []
                        , List.map
                            (renderFloatingIpCard context project model)
                            ipsNotAssignedToResources
                        , [ ipsAssignedToResourcesExpander context model ipsAssignedToResources ]
                        , if model.hideAssignedIps then
                            []

                          else
                            List.map (renderFloatingIpCard context project model) ipsAssignedToResources
                        ]

        floatingIpsUsedCount =
            project.floatingIps
                -- Defaulting to 0 if not loaded yet, not the greatest factoring
                |> RDPP.withDefault []
                |> List.length
    in
    Element.column
        [ Element.spacing 15, Element.width Element.fill ]
        [ if model.showHeading then
            Element.row (VH.heading2 context.palette ++ [ Element.spacing 15 ])
                [ Icon.ipAddress (SH.toElementColor context.palette.on.background) 24
                , Element.text
                    (context.localization.floatingIpAddress
                        |> Helpers.String.pluralize
                        |> Helpers.String.toTitleCase
                    )
                ]

          else
            Element.none
        , Element.column VH.contentContainer
            [ Page.QuotaUsage.view context (Page.QuotaUsage.FloatingIp project.computeQuota floatingIpsUsedCount)
            , VH.renderRDPP
                context
                project.floatingIps
                (Helpers.String.pluralize context.localization.floatingIpAddress)
                renderFloatingIps
            ]
        ]


ipScarcityWarning : View.Types.Context -> Element.Element Msg
ipScarcityWarning context =
    Element.paragraph
        [ Element.padding 10
        , Background.color (context.palette.warn |> SH.toElementColor)
        , Font.color (context.palette.on.warn |> SH.toElementColor)
        ]
        [ Element.text <|
            String.join " "
                [ context.localization.floatingIpAddress
                    |> Helpers.String.toTitleCase
                    |> Helpers.String.pluralize
                , "are a scarce resource. Please delete your unassigned"
                , context.localization.floatingIpAddress
                    |> Helpers.String.pluralize
                , "to free them up for other cloud users, unless you are saving them for a specific purpose."
                ]
        ]


renderFloatingIpCard :
    View.Types.Context
    -> Project
    -> Model
    -> OSTypes.FloatingIp
    -> Element.Element Msg
renderFloatingIpCard context project model ip =
    let
        subtitle =
            actionButtons context project model ip

        cardBody =
            case ip.portUuid of
                Just _ ->
                    case GetterSetters.getFloatingIpServer project ip of
                        Just server ->
                            Element.row [ Element.spacing 5 ]
                                [ Element.text <|
                                    String.join " "
                                        [ "Assigned to"
                                        , context.localization.virtualComputer
                                        , server.osProps.name
                                        ]
                                , Element.link []
                                    { url =
                                        Route.toUrl context.urlPathPrefix
                                            (Route.ProjectRoute project.auth.project.uuid <|
                                                Route.ServerDetail server.osProps.uuid
                                            )
                                    , label =
                                        Style.Widgets.IconButton.goToButton
                                            context.palette
                                            (Just <| SharedMsg <| SharedMsg.NoOp)
                                    }
                                ]

                        Nothing ->
                            Element.text "Assigned to a resource that Exosphere cannot represent"

                Nothing ->
                    Element.text "Unassigned"
    in
    Style.Widgets.Card.exoCardWithTitleAndSubtitle
        context.palette
        (Style.Widgets.CopyableText.copyableText
            context.palette
            [ Font.family [ Font.monospace ] ]
            ip.address
        )
        subtitle
        cardBody


actionButtons : View.Types.Context -> Project -> Model -> OSTypes.FloatingIp -> Element.Element Msg
actionButtons context project model ip =
    let
        assignUnassignButton =
            case ip.portUuid of
                Nothing ->
                    Element.link []
                        { url =
                            Route.toUrl context.urlPathPrefix
                                (Route.ProjectRoute project.auth.project.uuid <|
                                    Route.FloatingIpAssign (Just ip.uuid) Nothing
                                )
                        , label =
                            Widget.textButton
                                (SH.materialStyle context.palette).button
                                { text = "Assign"
                                , onPress = Just NoOp
                                }
                        }

                Just _ ->
                    Widget.textButton
                        (SH.materialStyle context.palette).button
                        { text = "Unassign"
                        , onPress = Just <| GotUnassign ip.uuid
                        }

        confirmationNeeded =
            Set.member ip.uuid model.deleteConfirmations

        deleteButton =
            if confirmationNeeded then
                Element.row [ Element.spacing 10 ]
                    [ Element.text "Confirm delete?"
                    , Widget.iconButton
                        (SH.materialStyle context.palette).dangerButton
                        { icon = Icon.remove (SH.toElementColor context.palette.on.error) 16
                        , text = "Delete"
                        , onPress =
                            Just <| GotDeleteConfirm ip.uuid
                        }
                    , Widget.textButton
                        (SH.materialStyle context.palette).button
                        { text = "Cancel"
                        , onPress =
                            Just <| GotDeleteCancel ip.uuid
                        }
                    ]

            else
                Widget.iconButton
                    (SH.materialStyle context.palette).dangerButton
                    { icon = Icon.remove (SH.toElementColor context.palette.on.error) 16
                    , text = "Delete"
                    , onPress =
                        Just <| GotDeleteNeedsConfirm ip.uuid
                    }
    in
    Element.row
        [ Element.width Element.fill ]
        [ Element.row [ Element.alignRight, Element.spacing 10 ] [ assignUnassignButton, deleteButton ] ]



-- TODO factor this out with onlyOwnExpander in ServerList.elm


ipsAssignedToResourcesExpander : View.Types.Context -> Model -> List OSTypes.FloatingIp -> Element.Element Msg
ipsAssignedToResourcesExpander context model ipsAssignedToResources =
    let
        numIpsAssignedToResources =
            List.length ipsAssignedToResources

        statusText =
            let
                ( ipsPluralization, resourcesPluralization ) =
                    if numIpsAssignedToResources == 1 then
                        ( context.localization.floatingIpAddress
                        , "a resource"
                        )

                    else
                        ( Helpers.String.pluralize context.localization.floatingIpAddress
                        , "resources"
                        )
            in
            if model.hideAssignedIps then
                String.join " "
                    [ "Hiding"
                    , humanCount context.locale numIpsAssignedToResources
                    , ipsPluralization
                    , "assigned to"
                    , resourcesPluralization
                    ]

            else
                String.join " "
                    [ context.localization.floatingIpAddress
                        |> Helpers.String.pluralize
                        |> Helpers.String.toTitleCase
                    , "assigned to resources"
                    ]

        ( changeActionVerb, changeActionIcon ) =
            if model.hideAssignedIps then
                ( "Show", FeatherIcons.chevronDown )

            else
                ( "Hide", FeatherIcons.chevronUp )

        changeOnlyOwnMsg : Msg
        changeOnlyOwnMsg =
            GotHideAssignedIps (not model.hideAssignedIps)

        changeButton =
            Widget.iconButton
                (SH.materialStyle context.palette).button
                { onPress = Just changeOnlyOwnMsg
                , icon =
                    Element.row [ Element.spacing 5 ]
                        [ Element.text changeActionVerb
                        , changeActionIcon
                            |> FeatherIcons.withSize 18
                            |> FeatherIcons.toHtml []
                            |> Element.html
                            |> Element.el []
                        ]
                , text = changeActionVerb
                }
    in
    if numIpsAssignedToResources == 0 then
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
