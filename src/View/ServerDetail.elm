module View.ServerDetail exposing (serverDetail)

import Dict
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers as Helpers
import Helpers.Interaction as IHelpers
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.String
import OpenStack.ServerActions as ServerActions
import OpenStack.ServerNameValidator exposing (serverNameValidator)
import OpenStack.Types as OSTypes
import RemoteData
import Style.Helpers as SH
import Style.Widgets.Button
import Style.Widgets.CopyableText exposing (copyableText)
import Style.Widgets.Icon as Icon
import Time
import Types.Interaction as ITypes
import Types.Types
    exposing
        ( CockpitLoginStatus(..)
        , IPInfoLevel(..)
        , Msg(..)
        , NonProjectViewConstructor(..)
        , PasswordVisibility(..)
        , Project
        , ProjectIdentifier
        , ProjectSpecificMsgConstructor(..)
        , ProjectViewConstructor(..)
        , Server
        , ServerDetailActiveTooltip(..)
        , ServerDetailViewParams
        , ServerOrigin(..)
        , UserAppProxyHostname
        , ViewState(..)
        )
import View.Helpers as VH exposing (edges)
import View.ResourceUsageCharts
import View.Types
import Widget
import Widget.Style.Material


updateServerDetail : Project -> ServerDetailViewParams -> Server -> Msg
updateServerDetail project serverDetailViewParams server =
    ProjectMsg project.auth.project.uuid <|
        SetProjectView <|
            ServerDetail server.osProps.uuid serverDetailViewParams


serverDetail : View.Types.Context -> Project -> ( Time.Posix, Time.Zone ) -> ServerDetailViewParams -> OSTypes.ServerUuid -> Element.Element Msg
serverDetail context project currentTimeAndZone serverDetailViewParams serverUuid =
    {- Attempt to look up a given server UUID; if a Server type is found, call rendering function serverDetail_ -}
    case GetterSetters.serverLookup project serverUuid of
        Just server ->
            serverDetail_ context project currentTimeAndZone serverDetailViewParams server

        Nothing ->
            Element.text <|
                String.join " "
                    [ "No"
                    , context.localization.virtualComputer
                    , "found"
                    ]


serverDetail_ : View.Types.Context -> Project -> ( Time.Posix, Time.Zone ) -> ServerDetailViewParams -> Server -> Element.Element Msg
serverDetail_ context project currentTimeAndZone serverDetailViewParams server =
    {- Render details of a server type and associated resources (e.g. volumes) -}
    let
        details =
            server.osProps.details

        creatorNameView =
            case server.exoProps.serverOrigin of
                ServerFromExo exoOriginProps ->
                    case exoOriginProps.exoCreatorUsername of
                        Just creatorUsername ->
                            VH.compactKVRow "Created by" (Element.text creatorUsername)

                        _ ->
                            Element.none

                _ ->
                    Element.none

        flavorText =
            GetterSetters.flavorLookup project details.flavorUuid
                |> Maybe.map .name
                |> Maybe.withDefault ("Unknown" ++ context.localization.virtualComputerHardwareConfig)

        imageText =
            let
                maybeImageName =
                    GetterSetters.imageLookup
                        project
                        details.imageUuid
                        |> Maybe.map .name

                maybeVolBackedImageName =
                    let
                        vols =
                            RemoteData.withDefault [] project.volumes
                    in
                    Helpers.getBootVol vols server.osProps.uuid
                        |> Maybe.andThen .imageMetadata
                        |> Maybe.map .name
            in
            case maybeImageName of
                Just name ->
                    name

                Nothing ->
                    case maybeVolBackedImageName of
                        Just name_ ->
                            name_

                        Nothing ->
                            "N/A"

        serverNameViewPlain =
            Element.row
                [ Element.spacing 10 ]
                [ Element.text server.osProps.name
                , Widget.iconButton
                    (Widget.Style.Material.outlinedButton (SH.toMaterialPalette context.palette))
                    { text = "Edit"
                    , icon =
                        FeatherIcons.edit3
                            |> FeatherIcons.withSize 16
                            |> FeatherIcons.toHtml []
                            |> Element.html
                            |> Element.el []
                    , onPress =
                        Just
                            (updateServerDetail project
                                { serverDetailViewParams
                                    | serverNamePendingConfirmation = Just server.osProps.name
                                }
                                server
                            )
                    }
                ]

        serverNameViewEdit =
            let
                invalidNameReasons =
                    serverNameValidator
                        (Just context.localization.virtualComputer)
                        (serverDetailViewParams.serverNamePendingConfirmation
                            |> Maybe.withDefault ""
                        )

                renderInvalidNameReasons =
                    case invalidNameReasons of
                        Just reasons ->
                            List.map Element.text reasons
                                |> List.map List.singleton
                                |> List.map (Element.paragraph [])
                                |> Element.column
                                    [ Font.color (SH.toElementColor context.palette.error)
                                    , Font.size 14
                                    , Element.alignRight
                                    , Element.moveDown 6
                                    , Background.color (SH.toElementColorWithOpacity context.palette.surface 0.9)
                                    , Element.spacing 10
                                    , Element.padding 10
                                    , Border.rounded 4
                                    , Border.shadow
                                        { blur = 10
                                        , color = SH.toElementColorWithOpacity context.palette.muted 0.2
                                        , offset = ( 0, 2 )
                                        , size = 1
                                        }
                                    ]

                        Nothing ->
                            Element.none

                rowStyle =
                    { containerRow =
                        [ Element.spacing 8
                        , Element.width Element.fill
                        ]
                    , element = []
                    , ifFirst = [ Element.width <| Element.minimum 200 <| Element.fill ]
                    , ifLast = []
                    , otherwise = []
                    }

                saveOnPress =
                    case ( invalidNameReasons, serverDetailViewParams.serverNamePendingConfirmation ) of
                        ( Nothing, Just validName ) ->
                            Just
                                (ProjectMsg project.auth.project.uuid
                                    (RequestSetServerName server.osProps.uuid validName)
                                )

                        ( _, _ ) ->
                            Nothing
            in
            Widget.row
                rowStyle
                [ Element.el
                    [ Element.below renderInvalidNameReasons
                    ]
                    (Widget.textInput (Widget.Style.Material.textInput (SH.toMaterialPalette context.palette))
                        { chips = []
                        , text = serverDetailViewParams.serverNamePendingConfirmation |> Maybe.withDefault ""
                        , placeholder =
                            Just
                                (Input.placeholder
                                    []
                                    (Element.text <|
                                        String.join " "
                                            [ "My"
                                            , context.localization.virtualComputer
                                                |> Helpers.String.stringToTitleCase
                                            ]
                                    )
                                )
                        , label = "Name"
                        , onChange =
                            \n ->
                                updateServerDetail project
                                    { serverDetailViewParams
                                        | serverNamePendingConfirmation = Just n
                                    }
                                    server
                        }
                    )
                , Widget.iconButton
                    (Widget.Style.Material.outlinedButton (SH.toMaterialPalette context.palette))
                    { text = "Save"
                    , icon =
                        FeatherIcons.save
                            |> FeatherIcons.withSize 16
                            |> FeatherIcons.toHtml []
                            |> Element.html
                            |> Element.el []
                    , onPress =
                        saveOnPress
                    }
                , Widget.iconButton
                    (Widget.Style.Material.textButton (SH.toMaterialPalette context.palette))
                    { text = "Cancel"
                    , icon =
                        FeatherIcons.xCircle
                            |> FeatherIcons.withSize 16
                            |> FeatherIcons.toHtml []
                            |> Element.html
                            |> Element.el []
                    , onPress =
                        Just
                            (updateServerDetail project
                                { serverDetailViewParams
                                    | serverNamePendingConfirmation = Nothing
                                }
                                server
                            )
                    }
                ]

        serverNameView =
            case serverDetailViewParams.serverNamePendingConfirmation of
                Just _ ->
                    serverNameViewEdit

                Nothing ->
                    serverNameViewPlain
    in
    Element.wrappedRow []
        [ Element.column
            (Element.alignTop
                :: Element.width (Element.px 585)
                :: VH.exoColumnAttributes
            )
            [ Element.el
                VH.heading2
                (Element.text <|
                    String.join " "
                        [ context.localization.virtualComputer
                            |> Helpers.String.stringToTitleCase
                        , "Details"
                        ]
                )
            , passwordVulnWarning context server
            , VH.compactKVRow "Name" serverNameView
            , VH.compactKVRow "Status" (serverStatus context project.auth.project.uuid serverDetailViewParams server)
            , VH.compactKVRow "UUID" <| copyableText context.palette server.osProps.uuid
            , VH.compactKVRow "Created on" (Element.text details.created)
            , creatorNameView
            , VH.compactKVRow
                (Helpers.String.stringToTitleCase context.localization.staticRepresentationOfBlockDeviceContents)
                (Element.text imageText)
            , VH.compactKVRow
                (Helpers.String.stringToTitleCase context.localization.virtualComputerHardwareConfig)
                (Element.text flavorText)
            , VH.compactKVRow
                (String.join " "
                    [ context.localization.pkiPublicKeyForSsh
                        |> Helpers.String.stringToTitleCase
                    , "Name"
                    ]
                )
                (Element.text (Maybe.withDefault "(none)" details.keypairName))
            , VH.compactKVRow "IP addresses"
                (renderIpAddresses
                    context
                    project.auth.project.uuid
                    server.osProps.uuid
                    serverDetailViewParams
                    details.ipAddresses
                )
            , Element.el VH.heading3
                (Element.text <|
                    String.concat
                        [ context.localization.blockDevice
                            |> Helpers.String.pluralizeWord
                            |> Helpers.String.stringToTitleCase
                        , " Attached"
                        ]
                )
            , serverVolumes context project server
            , case GetterSetters.getVolsAttachedToServer project server of
                [] ->
                    Element.none

                _ ->
                    Element.paragraph [ Font.size 11 ] <|
                        [ Element.text <|
                            String.join
                                " "
                                [ "* "
                                , context.localization.blockDevice
                                    |> Helpers.String.stringToTitleCase
                                , "will only be automatically formatted/mounted on operating systems which use systemd 236 or newer (e.g. Ubuntu 18.04, CentOS 8)."
                                ]
                        ]
            , if
                not <|
                    List.member
                        server.osProps.details.openstackStatus
                        [ OSTypes.ServerShelved
                        , OSTypes.ServerShelvedOffloaded
                        , OSTypes.ServerError
                        , OSTypes.ServerSoftDeleted
                        , OSTypes.ServerBuilding
                        ]
              then
                Widget.textButton
                    (Widget.Style.Material.textButton (SH.toMaterialPalette context.palette))
                    { text = "Attach volume"
                    , onPress =
                        Just <|
                            ProjectMsg project.auth.project.uuid <|
                                SetProjectView <|
                                    AttachVolumeModal
                                        (Just server.osProps.uuid)
                                        Nothing
                    }

              else
                Element.none
            , Element.el VH.heading2 (Element.text "Interactions")
            , interactions
                context
                server
                project.auth.project.uuid
                (Tuple.first currentTimeAndZone)
                project.userAppProxyHostname
                serverDetailViewParams
            , Element.el VH.heading3 (Element.text "Password")
            , serverPassword context project.auth.project.uuid serverDetailViewParams server
            ]
        , Element.column (Element.alignTop :: Element.width (Element.px 585) :: VH.exoColumnAttributes)
            [ Element.el VH.heading3 (Element.text "Actions")
            , viewServerActions context project.auth.project.uuid serverDetailViewParams server
            , Element.el VH.heading3 (Element.text "System Resource Usage")
            , resourceUsageCharts context currentTimeAndZone server
            ]
        ]


passwordVulnWarning : View.Types.Context -> Server -> Element.Element Msg
passwordVulnWarning context server =
    case server.exoProps.serverOrigin of
        ServerNotFromExo ->
            Element.none

        ServerFromExo serverFromExoProps ->
            if serverFromExoProps.exoServerVersion < 1 then
                Element.paragraph
                    [ Font.color (SH.toElementColor context.palette.error) ]
                    [ Element.text <|
                        String.join " "
                            [ "Warning: this"
                            , context.localization.virtualComputer
                            , "was created with an older version of Exosphere which left the opportunity for unprivileged processes running on the"
                            , context.localization.virtualComputer
                            , "to query the instance metadata service and determine the password for exouser (who is a sudoer). This represents a "
                            ]
                    , VH.browserLink
                        context
                        "https://en.wikipedia.org/wiki/Privilege_escalation"
                        (View.Types.BrowserLinkTextLabel "privilege escalation vulnerability")
                    , Element.text <|
                        String.join " "
                            [ ". If you have used this"
                            , context.localization.virtualComputer
                            , "for anything important or sensitive, consider rotating the password for exouser, or building a new"
                            , context.localization.virtualComputer
                            , "and moving to that one instead of this one. For more information, see "
                            ]
                    , VH.browserLink
                        context
                        "https://gitlab.com/exosphere/exosphere/issues/284"
                        (View.Types.BrowserLinkTextLabel "issue #284")
                    , Element.text " on the Exosphere GitLab project."
                    ]

            else
                Element.none


serverStatus : View.Types.Context -> ProjectIdentifier -> ServerDetailViewParams -> Server -> Element.Element Msg
serverStatus context projectId serverDetailViewParams server =
    let
        details =
            server.osProps.details

        friendlyOpenstackStatus =
            Debug.toString details.openstackStatus
                |> String.dropLeft 6

        friendlyPowerState =
            Debug.toString details.powerState
                |> String.dropLeft 5

        statusGraphic =
            let
                spinner =
                    Widget.circularProgressIndicator
                        (SH.materialStyle context.palette).progressIndicator
                        Nothing

                g =
                    case ( details.openstackStatus, server.exoProps.targetOpenstackStatus ) of
                        ( OSTypes.ServerBuilding, _ ) ->
                            spinner

                        ( _, Just _ ) ->
                            spinner

                        ( _, Nothing ) ->
                            Icon.roundRect
                                (server |> VH.getServerUiStatus |> VH.getServerUiStatusColor context.palette)
                                28
            in
            Element.el
                [ Element.paddingEach { edges | right = 15 } ]
                g

        lockStatus : OSTypes.ServerLockStatus -> Element.Element Msg
        lockStatus lockStatus_ =
            case lockStatus_ of
                OSTypes.ServerLocked ->
                    Element.row
                        []
                        [ Element.el
                            [ Element.paddingEach { edges | right = 15 } ]
                          <|
                            Icon.lock (SH.toElementColor context.palette.on.background) 28
                        , Element.text "Locked"
                        ]

                OSTypes.ServerUnlocked ->
                    Element.row
                        []
                        [ Element.el
                            [ Element.paddingEach
                                { edges | right = 15 }
                            ]
                          <|
                            Icon.lockOpen (SH.toElementColor context.palette.on.background) 28
                        , Element.text "Unlocked"
                        ]

        verboseStatus =
            if serverDetailViewParams.verboseStatus then
                [ Element.text "Detailed status"
                , VH.compactKVSubRow "OpenStack status" (Element.text friendlyOpenstackStatus)
                , VH.compactKVSubRow "Power state" (Element.text friendlyPowerState)
                , VH.compactKVSubRow
                    (String.join " "
                        [ context.localization.virtualComputer
                            |> Helpers.String.stringToTitleCase
                        , "Dashboard and Terminal readiness"
                        ]
                    )
                    (Element.paragraph [] [ Element.text (friendlyCockpitReadiness server.exoProps.serverOrigin) ])
                ]

            else
                [ Widget.textButton
                    (Widget.Style.Material.textButton (SH.toMaterialPalette context.palette))
                    { text = "See detail"
                    , onPress =
                        Just <|
                            ProjectMsg projectId <|
                                SetProjectView <|
                                    ServerDetail
                                        server.osProps.uuid
                                        { serverDetailViewParams | verboseStatus = True }
                    }
                ]

        statusString =
            Element.text
                (server
                    |> VH.getServerUiStatus
                    |> VH.getServerUiStatusStr
                )
    in
    Element.column
        (VH.exoColumnAttributes ++ [ Element.padding 0, Element.spacing 5 ])
    <|
        List.concat
            [ [ Element.row [ Font.bold ]
                    [ statusGraphic
                    , statusString
                    ]
              ]
            , [ lockStatus server.osProps.details.lockStatus ]
            , verboseStatus
            ]


interactions : View.Types.Context -> Server -> ProjectIdentifier -> Time.Posix -> Maybe UserAppProxyHostname -> ServerDetailViewParams -> Element.Element Msg
interactions context server projectId currentTime tlsReverseProxyHostname serverDetailViewParams =
    let
        renderInteraction interaction =
            let
                interactionDetails =
                    IHelpers.interactionDetails interaction context

                interactionStatus =
                    IHelpers.interactionStatus
                        server
                        interaction
                        context
                        currentTime
                        tlsReverseProxyHostname

                ( statusWord, statusColor ) =
                    IHelpers.interactionStatusWordColor context.palette interactionStatus

                statusTooltip =
                    -- TODO deduplicate with below function?
                    case serverDetailViewParams.activeTooltip of
                        Just (InteractionStatusTooltip interaction_) ->
                            if interaction == interaction_ then
                                Element.el
                                    [ Element.paddingEach { top = 10, right = 0, left = 0, bottom = 0 } ]
                                <|
                                    Element.column
                                        [ Element.padding 5

                                        -- TODO this should use the same border/shadow as the server name change tooltip, turn it into a widget
                                        , Background.color <| SH.toElementColor <| context.palette.surface
                                        , Font.color <| SH.toElementColor <| context.palette.on.surface
                                        ]
                                        [ Element.text statusWord
                                        , case interactionStatus of
                                            ITypes.Unavailable reason ->
                                                Element.text reason

                                            ITypes.Error reason ->
                                                Element.text reason

                                            ITypes.Warn _ reason ->
                                                Element.text reason

                                            _ ->
                                                Element.none
                                        ]

                            else
                                Element.none

                        _ ->
                            Element.none

                interactionTooltip =
                    -- TODO deduplicate with above function?
                    case serverDetailViewParams.activeTooltip of
                        Just (InteractionTooltip interaction_) ->
                            if interaction == interaction_ then
                                Element.el
                                    [ Element.paddingEach { top = 0, right = 0, left = 10, bottom = 0 } ]
                                <|
                                    Element.column
                                        [ Element.padding 5

                                        -- TODO this should use the same border/shadow as the server name change tooltip, turn it into a widget
                                        , Background.color <| SH.toElementColor <| context.palette.surface
                                        , Font.color <| SH.toElementColor <| context.palette.on.surface
                                        , Element.width (Element.maximum 300 Element.shrink)
                                        ]
                                        [ Element.paragraph
                                            -- Ugh? https://github.com/mdgriffith/elm-ui/issues/157
                                            [ Element.width (Element.minimum 200 Element.fill) ]
                                            [ Element.text interactionDetails.description ]
                                        ]

                            else
                                Element.none

                        _ ->
                            Element.none

                showHideTooltipMsg : ServerDetailActiveTooltip -> Msg
                showHideTooltipMsg tooltip =
                    let
                        newValue =
                            case serverDetailViewParams.activeTooltip of
                                Just _ ->
                                    Nothing

                                Nothing ->
                                    Just <| tooltip
                    in
                    ProjectMsg projectId <|
                        SetProjectView <|
                            ServerDetail server.osProps.uuid
                                { serverDetailViewParams | activeTooltip = newValue }
            in
            case interactionStatus of
                ITypes.Hidden ->
                    Element.none

                _ ->
                    Element.row
                        VH.exoRowAttributes
                        [ Element.el
                            [ Element.below statusTooltip
                            , Events.onClick <| showHideTooltipMsg (InteractionStatusTooltip interaction)
                            ]
                            (Icon.roundRect statusColor 14)
                        , case interactionDetails.type_ of
                            ITypes.UrlInteraction ->
                                Widget.button
                                    (Widget.Style.Material.outlinedButton (SH.toMaterialPalette context.palette))
                                    { text = interactionDetails.name
                                    , icon =
                                        Element.el
                                            [ Element.paddingEach
                                                { top = 0
                                                , right = 5
                                                , left = 0
                                                , bottom = 0
                                                }
                                            ]
                                            (interactionDetails.icon (SH.toElementColor context.palette.primary) 22)
                                    , onPress =
                                        case interactionStatus of
                                            ITypes.Ready url ->
                                                Just <| OpenNewWindow url

                                            ITypes.Warn url _ ->
                                                Just <| OpenNewWindow url

                                            _ ->
                                                Nothing
                                    }

                            ITypes.TextInteraction ->
                                let
                                    ( iconColor, fontColor ) =
                                        case interactionStatus of
                                            ITypes.Ready _ ->
                                                ( SH.toElementColor context.palette.primary
                                                , SH.toElementColor context.palette.on.surface
                                                )

                                            _ ->
                                                ( SH.toElementColor context.palette.muted
                                                , SH.toElementColor context.palette.muted
                                                )
                                in
                                Element.row
                                    [ Font.color fontColor
                                    ]
                                    [ Element.el
                                        [ Font.color iconColor
                                        , Element.paddingEach
                                            { top = 0
                                            , right = 5
                                            , left = 0
                                            , bottom = 0
                                            }
                                        ]
                                        (interactionDetails.icon iconColor 22)
                                    , Element.text interactionDetails.name
                                    , case interactionStatus of
                                        ITypes.Ready text ->
                                            Element.row
                                                []
                                                [ Element.text ": "
                                                , copyableText context.palette text
                                                ]

                                        _ ->
                                            Element.none
                                    ]
                        , Element.el
                            [ Element.onRight interactionTooltip
                            , Events.onClick <| showHideTooltipMsg (InteractionTooltip interaction)
                            ]
                            (FeatherIcons.helpCircle |> FeatherIcons.toHtml [] |> Element.html)
                        ]
    in
    [ ITypes.GuacTerminal
    , ITypes.CockpitDashboard
    , ITypes.CockpitTerminal
    , ITypes.NativeSSH
    , ITypes.Console
    ]
        |> List.map renderInteraction
        |> Element.column []


serverPassword : View.Types.Context -> ProjectIdentifier -> ServerDetailViewParams -> Server -> Element.Element Msg
serverPassword context projectId serverDetailViewParams server =
    let
        passwordShower password =
            Element.column
                [ Element.spacing 10 ]
                [ case serverDetailViewParams.passwordVisibility of
                    PasswordShown ->
                        copyableText context.palette password

                    PasswordHidden ->
                        Element.none
                , let
                    changeMsg newValue =
                        ProjectMsg projectId <|
                            SetProjectView <|
                                ServerDetail server.osProps.uuid
                                    { serverDetailViewParams | passwordVisibility = newValue }

                    ( buttonText, onPressMsg ) =
                        case serverDetailViewParams.passwordVisibility of
                            PasswordShown ->
                                ( "Hide password"
                                , changeMsg PasswordHidden
                                )

                            PasswordHidden ->
                                ( "Show password"
                                , changeMsg PasswordShown
                                )
                  in
                  Widget.textButton
                    (Widget.Style.Material.textButton (SH.toMaterialPalette context.palette))
                    { text = buttonText
                    , onPress = Just onPressMsg
                    }
                ]

        passwordHint =
            GetterSetters.getServerExouserPassword server.osProps.details
                |> Maybe.withDefault (Element.text "Not available yet, check back in a few minutes.")
                << Maybe.map
                    (\password ->
                        Element.column
                            [ Element.spacing 10 ]
                            [ Element.text "Try logging in with username \"exouser\" and the following password:"
                            , passwordShower password
                            ]
                    )
    in
    Element.column
        VH.exoColumnAttributes
        [ passwordHint
        ]


viewServerActions : View.Types.Context -> ProjectIdentifier -> ServerDetailViewParams -> Server -> Element.Element Msg
viewServerActions context projectId serverDetailViewParams server =
    Element.column
        (VH.exoColumnAttributes ++ [ Element.spacingXY 0 10 ])
    <|
        case server.exoProps.targetOpenstackStatus of
            Nothing ->
                List.map
                    (renderServerActionButton context projectId serverDetailViewParams server)
                    (ServerActions.getAllowed
                        (Just context.localization.virtualComputer)
                        (Just context.localization.staticRepresentationOfBlockDeviceContents)
                        server.osProps.details.openstackStatus
                        server.osProps.details.lockStatus
                    )

            Just _ ->
                []


renderServerActionButton : View.Types.Context -> ProjectIdentifier -> ServerDetailViewParams -> Server -> ServerActions.ServerAction -> Element.Element Msg
renderServerActionButton context projectId serverDetailViewParams server serverAction =
    let
        displayConfirmation =
            case serverDetailViewParams.serverActionNamePendingConfirmation of
                Nothing ->
                    False

                Just actionName ->
                    actionName == serverAction.name
    in
    case ( serverAction.action, serverAction.confirmable, displayConfirmation ) of
        ( ServerActions.CmdAction _, True, False ) ->
            let
                updateAction =
                    ProjectMsg
                        projectId
                        (SetProjectView
                            (ServerDetail
                                server.osProps.uuid
                                { serverDetailViewParams | serverActionNamePendingConfirmation = Just serverAction.name }
                            )
                        )
            in
            renderActionButton context serverAction (Just updateAction) serverAction.name

        ( ServerActions.CmdAction cmdAction, True, True ) ->
            let
                actionMsg =
                    Just <|
                        ProjectMsg projectId <|
                            RequestServerAction
                                server
                                cmdAction
                                serverAction.targetStatus

                cancelMsg =
                    Just <|
                        ProjectMsg
                            projectId
                            (SetProjectView
                                (ServerDetail
                                    server.osProps.uuid
                                    { serverDetailViewParams | serverActionNamePendingConfirmation = Nothing }
                                )
                            )

                title =
                    confirmationMessage serverAction
            in
            renderConfirmationButton context serverAction actionMsg cancelMsg title

        ( ServerActions.CmdAction cmdAction, False, _ ) ->
            let
                actionMsg =
                    Just <|
                        ProjectMsg projectId <|
                            RequestServerAction
                                server
                                cmdAction
                                serverAction.targetStatus

                title =
                    serverAction.name
            in
            renderActionButton context serverAction actionMsg title

        ( ServerActions.UpdateAction updateAction, _, _ ) ->
            let
                actionMsg =
                    Just <| updateAction projectId server

                title =
                    serverAction.name
            in
            renderActionButton context serverAction actionMsg title


confirmationMessage : ServerActions.ServerAction -> String
confirmationMessage serverAction =
    "Are you sure you want to " ++ (serverAction.name |> String.toLower) ++ "?"


serverActionSelectModButton : View.Types.Context -> ServerActions.SelectMod -> (Widget.TextButton Msg -> Element.Element Msg)
serverActionSelectModButton context selectMod =
    case selectMod of
        ServerActions.NoMod ->
            Widget.textButton (Widget.Style.Material.outlinedButton (SH.toMaterialPalette context.palette))

        ServerActions.Primary ->
            Widget.textButton (Widget.Style.Material.containedButton (SH.toMaterialPalette context.palette))

        ServerActions.Warning ->
            Widget.textButton (Style.Widgets.Button.warningButton context.palette)

        ServerActions.Danger ->
            Widget.textButton (Style.Widgets.Button.dangerButton context.palette)


renderActionButton : View.Types.Context -> ServerActions.ServerAction -> Maybe Msg -> String -> Element.Element Msg
renderActionButton context serverAction actionMsg title =
    Element.row
        [ Element.spacing 10 ]
        [ Element.el
            [ Element.width <| Element.px 120 ]
          <|
            serverActionSelectModButton context
                serverAction.selectMod
                { text = title
                , onPress = actionMsg
                }
        , Element.text serverAction.description

        -- TODO hover text with description
        ]


renderConfirmationButton : View.Types.Context -> ServerActions.ServerAction -> Maybe Msg -> Maybe Msg -> String -> Element.Element Msg
renderConfirmationButton context serverAction actionMsg cancelMsg title =
    Element.row
        [ Element.spacing 10 ]
        [ Element.text title
        , Element.el
            []
          <|
            serverActionSelectModButton context
                serverAction.selectMod
                { text = "Yes"
                , onPress = actionMsg
                }
        , Element.el
            []
          <|
            Widget.textButton (Widget.Style.Material.outlinedButton (SH.toMaterialPalette context.palette))
                { text = "No"
                , onPress = cancelMsg
                }

        -- TODO hover text with description
        ]


resourceUsageCharts : View.Types.Context -> ( Time.Posix, Time.Zone ) -> Server -> Element.Element Msg
resourceUsageCharts context currentTimeAndZone server =
    let
        thirtyMinMillis =
            1000 * 60 * 30
    in
    case server.exoProps.serverOrigin of
        ServerNotFromExo ->
            Element.text <|
                String.join " "
                    [ "Charts not available because"
                    , context.localization.virtualComputer
                    , "was not created by Exosphere."
                    ]

        ServerFromExo exoOriginProps ->
            case exoOriginProps.resourceUsage.data of
                RDPP.DoHave history _ ->
                    if Dict.isEmpty history.timeSeries then
                        if Helpers.serverLessThanThisOld server (Tuple.first currentTimeAndZone) thirtyMinMillis then
                            Element.text <|
                                String.join " "
                                    [ "No chart data yet. This"
                                    , context.localization.virtualComputer
                                    , "is new and may take a few minutes to start reporting data."
                                    ]

                        else
                            Element.text "No chart data to show."

                    else
                        View.ResourceUsageCharts.charts context currentTimeAndZone history.timeSeries

                _ ->
                    if exoOriginProps.exoServerVersion < 2 then
                        Element.text <|
                            String.join " "
                                [ "Charts not available because"
                                , context.localization.virtualComputer
                                , "was not created using a new enough build of Exosphere."
                                ]

                    else
                        Element.text <|
                            String.join " "
                                [ "Could not access the"
                                , context.localization.virtualComputer
                                , "console log, charts not available."
                                ]


renderIpAddresses : View.Types.Context -> ProjectIdentifier -> OSTypes.ServerUuid -> ServerDetailViewParams -> List OSTypes.IpAddress -> Element.Element Msg
renderIpAddresses context projectId serverUuid serverDetailViewParams ipAddresses =
    let
        ipAddressesOfType : OSTypes.IpAddressType -> List OSTypes.IpAddress
        ipAddressesOfType ipAddressType =
            ipAddresses
                |> List.filter
                    (\ipAddress ->
                        ipAddress.openstackType == ipAddressType
                    )

        fixedIpAddressRows =
            ipAddressesOfType OSTypes.IpAddressFixed
                |> List.map
                    (\ipAddress ->
                        VH.compactKVSubRow "Fixed IP" (Element.text ipAddress.address)
                    )

        floatingIpAddressRows =
            ipAddressesOfType OSTypes.IpAddressFloating
                |> List.map
                    (\ipAddress ->
                        VH.compactKVSubRow "Floating IP" <| copyableText context.palette ipAddress.address
                    )

        ipButton : Element.Element Msg -> String -> IPInfoLevel -> Element.Element Msg
        ipButton label displayLabel ipMsg =
            Element.row
                [ Element.spacing 3 ]
                [ Input.button
                    [ Font.size 10
                    , Border.width 1
                    , Border.rounded 20
                    , Border.color (SH.toElementColor context.palette.muted)
                    , Element.padding 3
                    ]
                    { onPress =
                        Just <|
                            ProjectMsg projectId <|
                                SetProjectView <|
                                    ServerDetail
                                        serverUuid
                                        { serverDetailViewParams | ipInfoLevel = ipMsg }
                    , label = label
                    }
                , Element.el [ Font.size 10 ] (Element.text displayLabel)
                ]
    in
    case serverDetailViewParams.ipInfoLevel of
        IPDetails ->
            let
                icon =
                    FeatherIcons.chevronDown
                        |> FeatherIcons.withSize 12
                        |> FeatherIcons.toHtml []
                        |> Element.html
            in
            Element.column
                (VH.exoColumnAttributes ++ [ Element.padding 0 ])
                (floatingIpAddressRows
                    ++ ipButton icon "IP Details" IPSummary
                    :: fixedIpAddressRows
                )

        IPSummary ->
            let
                icon =
                    FeatherIcons.chevronRight
                        |> FeatherIcons.withSize 12
                        |> FeatherIcons.toHtml []
                        |> Element.html
            in
            Element.column
                (VH.exoColumnAttributes ++ [ Element.padding 0 ])
                (floatingIpAddressRows ++ [ ipButton icon "IP Details" IPDetails ])


friendlyCockpitReadiness : ServerOrigin -> String
friendlyCockpitReadiness serverOrigin =
    case serverOrigin of
        ServerNotFromExo ->
            "N/A"

        ServerFromExo serverFromExoProps ->
            case serverFromExoProps.cockpitStatus of
                NotChecked ->
                    "Not checked yet"

                CheckedNotReady ->
                    "Checked, not ready yet"

                Ready ->
                    "Ready"

                ReadyButRecheck ->
                    "Ready"


serverVolumes : View.Types.Context -> Project -> Server -> Element.Element Msg
serverVolumes context project server =
    let
        vols =
            GetterSetters.getVolsAttachedToServer project server

        deviceRawName vol =
            vol.attachments
                |> List.filter (\a -> a.serverUuid == server.osProps.uuid)
                |> List.head
                |> Maybe.map .device

        isBootVol vol =
            deviceRawName vol
                |> Maybe.map (\d -> List.member d [ "/dev/vda", "/dev/sda" ])
                |> Maybe.withDefault False
    in
    case List.length vols of
        0 ->
            Element.text "(none)"

        _ ->
            let
                volDetailsButton v =
                    Input.button
                        [ Border.width 1
                        , Border.rounded 6
                        , Border.color <| SH.toElementColor context.palette.muted
                        , Element.padding 3
                        ]
                        { onPress =
                            Just
                                (ProjectMsg
                                    project.auth.project.uuid
                                    (SetProjectView <| VolumeDetail v.uuid [])
                                )
                        , label =
                            FeatherIcons.chevronRight
                                |> FeatherIcons.withSize 14
                                |> FeatherIcons.toHtml []
                                |> Element.html
                                |> Element.el []
                        }

                volumeRow v =
                    let
                        ( device, mountpoint ) =
                            if isBootVol v then
                                ( String.join " "
                                    [ "Boot"
                                    , context.localization.blockDevice
                                    ]
                                , ""
                                )

                            else
                                case deviceRawName v of
                                    Just device_ ->
                                        ( device_
                                        , case Helpers.volDeviceToMountpoint device_ of
                                            Just mountpoint_ ->
                                                mountpoint_

                                            Nothing ->
                                                "Could not determine"
                                        )

                                    Nothing ->
                                        ( "Could not determine", "" )
                    in
                    { name = VH.possiblyUntitledResource v.name "volume"
                    , device = device
                    , mountpoint = mountpoint
                    , toButton = volDetailsButton v
                    }
            in
            Element.table
                []
                { data =
                    vols
                        |> List.map volumeRow
                        |> List.sortBy .device
                , columns =
                    [ { header = Element.el [ Font.heavy ] <| Element.text "Name"
                      , width = Element.fill
                      , view = \v -> Element.text v.name
                      }
                    , { header = Element.el [ Font.heavy ] <| Element.text "Device"
                      , width = Element.fill
                      , view = \v -> Element.text v.device
                      }
                    , { header = Element.el [ Font.heavy ] <| Element.text "Mount point *"
                      , width = Element.fill
                      , view = \v -> Element.text v.mountpoint
                      }
                    , { header = Element.none
                      , width = Element.px 22
                      , view = \v -> v.toButton
                      }
                    ]
                }
