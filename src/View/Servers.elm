module View.Servers exposing (serverDetail, servers)

import Color
import Element
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Framework.Card as Card
import Helpers.Helpers as Helpers
import Helpers.RemoteDataPlusPlus as RDPP
import Html
import Html.Attributes
import OpenStack.ServerActions as ServerActions
import OpenStack.Types as OSTypes
import RemoteData
import Style.Theme
import Style.Widgets.Button
import Style.Widgets.Card as ExoCard
import Style.Widgets.CopyableText exposing (copyableText)
import Style.Widgets.Icon as Icon
import Types.Types
    exposing
        ( CockpitLoginStatus(..)
        , DeleteConfirmation
        , IPInfoLevel(..)
        , Msg(..)
        , NonProjectViewConstructor(..)
        , PasswordVisibility(..)
        , Project
        , ProjectIdentifier
        , ProjectSpecificMsgConstructor(..)
        , ProjectViewConstructor(..)
        , Server
        , ServerDetailViewParams
        , ServerFilter
        , ServerOrigin(..)
        , ViewState(..)
        )
import View.Helpers as VH exposing (edges)
import View.Types
import Widget
import Widget.Style.Material


servers : Project -> ServerFilter -> List DeleteConfirmation -> Element.Element Msg
servers project serverFilter deleteConfirmations =
    case ( project.servers.data, project.servers.refreshStatus ) of
        ( RDPP.DontHave, RDPP.NotLoading Nothing ) ->
            Element.paragraph [] [ Element.text "Please wait..." ]

        ( RDPP.DontHave, RDPP.NotLoading (Just _) ) ->
            Element.paragraph [] [ Element.text ("Cannot display servers. Error message: " ++ Debug.toString e) ]

        ( RDPP.DontHave, RDPP.Loading _ ) ->
            Element.paragraph [] [ Element.text "Loading..." ]

        ( RDPP.DoHave allServers _, _ ) ->
            if List.isEmpty allServers then
                Element.paragraph [] [ Element.text "You don't have any servers yet, go create one!" ]

            else
                let
                    userUuid =
                        project.auth.user.uuid

                    someServers =
                        if serverFilter.onlyOwnServers == True then
                            List.filter (\s -> s.osProps.details.userUuid == userUuid) allServers

                        else
                            allServers

                    noServersSelected =
                        List.any (\s -> s.exoProps.selected) someServers |> not

                    allServersSelected =
                        someServers
                            |> List.filter (\s -> s.osProps.details.lockStatus == OSTypes.ServerUnlocked)
                            |> List.all (\s -> s.exoProps.selected)

                    selectedServers =
                        List.filter (\s -> s.exoProps.selected) someServers

                    deleteButtonOnPress =
                        if noServersSelected == True then
                            Nothing

                        else
                            let
                                uuidsToDelete =
                                    List.map (\s -> s.osProps.uuid) selectedServers
                            in
                            Just (ProjectMsg (Helpers.getProjectId project) (RequestDeleteServers uuidsToDelete))
                in
                Element.column (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
                    [ Element.el VH.heading2 (Element.text "My Servers")
                    , Element.column (VH.exoColumnAttributes ++ [ Element.padding 5, Border.width 1 ])
                        [ Element.text "Bulk Actions"
                        , Input.checkbox []
                            { checked = allServersSelected
                            , onChange = \new -> ProjectMsg (Helpers.getProjectId project) (SelectAllServers new)
                            , icon = Input.defaultCheckbox
                            , label = Input.labelRight [] (Element.text "Select All")
                            }
                        , Widget.textButton
                            (Style.Widgets.Button.dangerButton Style.Theme.exoPalette)
                            { text = "Delete"
                            , onPress = deleteButtonOnPress
                            }
                        ]
                    , Input.checkbox []
                        { checked = serverFilter.onlyOwnServers
                        , onChange = \new -> ProjectMsg (Helpers.getProjectId project) <| SetProjectView <| ListProjectServers { serverFilter | onlyOwnServers = new } []
                        , icon = Input.defaultCheckbox
                        , label = Input.labelRight [] (Element.text "Show only servers created by me")
                        }
                    , Element.column (VH.exoColumnAttributes ++ [ Element.width (Element.fill |> Element.maximum 960) ])
                        (List.map (renderServer project serverFilter deleteConfirmations) someServers)
                    ]


serverDetail : Bool -> Project -> OSTypes.ServerUuid -> ServerDetailViewParams -> Element.Element Msg
serverDetail appIsElectron project serverUuid serverDetailViewParams =
    Helpers.serverLookup project serverUuid
        |> Maybe.withDefault (Element.text "No server found")
        << Maybe.map
            (\server ->
                let
                    details =
                        server.osProps.details

                    flavorText =
                        Helpers.flavorLookup project details.flavorUuid
                            |> Maybe.map .name
                            |> Maybe.withDefault "Unknown flavor"

                    imageText =
                        let
                            maybeImageName =
                                Helpers.imageLookup
                                    project
                                    details.imageUuid
                                    |> Maybe.map .name

                            maybeVolBackedImageName =
                                let
                                    vols =
                                        RemoteData.withDefault [] project.volumes
                                in
                                Helpers.getBootVol vols serverUuid
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

                    maybeFloatingIp =
                        Helpers.getServerFloatingIp details.ipAddresses

                    projectId =
                        Helpers.getProjectId project
                in
                Element.wrappedRow []
                    [ Element.column
                        (Element.alignTop
                            :: Element.width (Element.px 585)
                            :: VH.exoColumnAttributes
                        )
                        [ Element.el
                            VH.heading2
                            (Element.text "Server Details")
                        , passwordVulnWarning appIsElectron server
                        , VH.compactKVRow "Name" (Element.text server.osProps.name)
                        , VH.compactKVRow "Status" (serverStatus projectId server serverDetailViewParams)
                        , VH.compactKVRow "UUID" <| copyableText server.osProps.uuid
                        , VH.compactKVRow "Created on" (Element.text details.created)
                        , VH.compactKVRow "Image" (Element.text imageText)
                        , VH.compactKVRow "Flavor" (Element.text flavorText)
                        , VH.compactKVRow "SSH Key Name" (Element.text (Maybe.withDefault "(none)" details.keypairName))
                        , VH.compactKVRow "IP addresses"
                            (renderIpAddresses
                                details.ipAddresses
                                projectId
                                server.osProps.uuid
                                serverDetailViewParams
                            )
                        , Element.el VH.heading3 (Element.text "Volumes Attached")
                        , serverVolumes project server
                        , case Helpers.getVolsAttachedToServer project server of
                            [] ->
                                Element.none

                            _ ->
                                Element.paragraph [ Font.size 11 ] <|
                                    [ Element.text "* Volume will only be automatically formatted/mounted on operating systems which use systemd 236 or newer (e.g. Ubuntu 18.04, CentOS 8)." ]
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
                                (Widget.Style.Material.textButton Style.Theme.exoPalette)
                                { text = "Attach volume"
                                , onPress =
                                    Just <|
                                        ProjectMsg projectId <|
                                            SetProjectView <|
                                                AttachVolumeModal
                                                    (Just serverUuid)
                                                    Nothing
                                }

                          else
                            Element.none
                        , Element.el VH.heading2 (Element.text "Interact with server")
                        , Element.el VH.heading3 (Element.text "SSH")
                        , sshInstructions maybeFloatingIp
                        , Element.el VH.heading3 (Element.text "Console")
                        , consoleLink appIsElectron project server serverUuid serverDetailViewParams
                        , Element.el VH.heading3 (Element.text "Terminal / Dashboard")
                        , cockpitInteraction server.exoProps.serverOrigin maybeFloatingIp
                        ]
                    , Element.column (Element.alignTop :: Element.width (Element.px 585) :: VH.exoColumnAttributes)
                        [ Element.el VH.heading3 (Element.text "Server Actions")
                        , viewServerActions projectId server serverDetailViewParams
                        , Element.el VH.heading3 (Element.text "System Resource Usage")
                        , resourceUsageGraphs server.exoProps.serverOrigin maybeFloatingIp
                        ]
                    ]
            )


passwordVulnWarning : Bool -> Server -> Element.Element Msg
passwordVulnWarning appIsElectron server =
    case server.exoProps.serverOrigin of
        ServerNotFromExo ->
            Element.none

        ServerFromExo serverFromExoProps ->
            if serverFromExoProps.exoServerVersion < 1 then
                Element.paragraph
                    [ Font.color (Element.rgb 255 0 0) ]
                    [ Element.text "Warning: this server was created with an older version of Exosphere which left the opportunity for unprivileged processes running on the server to query the instance metadata service and determine the password for exouser (who is a sudoer). This represents a "
                    , VH.browserLink
                        appIsElectron
                        "https://en.wikipedia.org/wiki/Privilege_escalation"
                        (View.Types.BrowserLinkTextLabel "privilege escalation vulnerability")
                    , Element.text ". If you have used this server for anything important or sensitive, consider rotating the password for exouser, or building a new server and moving to that one instead of this one. For more information, see "
                    , VH.browserLink
                        appIsElectron
                        "https://gitlab.com/exosphere/exosphere/issues/284"
                        (View.Types.BrowserLinkTextLabel "issue #284")
                    , Element.text " on the Exosphere GitLab project."
                    ]

            else
                Element.none


serverStatus : ProjectIdentifier -> Server -> ServerDetailViewParams -> Element.Element Msg
serverStatus projectId server serverDetailViewParams =
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
                    Widget.circularProgressIndicator Style.Theme.materialStyle.progressIndicator Nothing

                g =
                    case ( details.openstackStatus, server.exoProps.targetOpenstackStatus ) of
                        ( OSTypes.ServerBuilding, _ ) ->
                            spinner

                        ( _, Just _ ) ->
                            spinner

                        ( _, Nothing ) ->
                            Icon.roundRect (server |> Helpers.getServerUiStatus |> Helpers.getServerUiStatusColor) 28
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
                        [ Element.el [ Element.paddingEach { edges | right = 15 } ] <| Icon.lock (Element.rgb255 10 10 10) 28
                        , Element.text "Locked"
                        ]

                OSTypes.ServerUnlocked ->
                    Element.row
                        []
                        [ Element.el [ Element.paddingEach { edges | right = 15 } ] <| Icon.lockOpen (Element.rgb255 10 10 10) 28
                        , Element.text "Unlocked"
                        ]

        verboseStatus =
            if serverDetailViewParams.verboseStatus then
                [ Element.text "Detailed status"
                , VH.compactKVSubRow "OpenStack status" (Element.text friendlyOpenstackStatus)
                , VH.compactKVSubRow "Power state" (Element.text friendlyPowerState)
                , VH.compactKVSubRow "Server Dashboard and Terminal readiness" (Element.paragraph [] [ Element.text (friendlyCockpitReadiness server.exoProps.serverOrigin) ])
                ]

            else
                [ Widget.textButton
                    (Widget.Style.Material.textButton Style.Theme.exoPalette)
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
                    |> Helpers.getServerUiStatus
                    |> Helpers.getServerUiStatusStr
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


sshInstructions : Maybe String -> Element.Element Msg
sshInstructions maybeFloatingIp =
    case maybeFloatingIp of
        Nothing ->
            Element.none

        Just floatingIp ->
            copyableText ("exouser@" ++ floatingIp)


consoleLink : Bool -> Project -> Server -> OSTypes.ServerUuid -> ServerDetailViewParams -> Element.Element Msg
consoleLink appIsElectron project server serverUuid serverDetailViewParams =
    let
        details =
            server.osProps.details
    in
    case details.openstackStatus of
        OSTypes.ServerActive ->
            case server.osProps.consoleUrl of
                RemoteData.NotAsked ->
                    Element.text "Console not available yet"

                RemoteData.Loading ->
                    Element.text "Requesting console link..."

                RemoteData.Failure error ->
                    Element.column VH.exoColumnAttributes
                        [ Element.text "Console not available. The following error was returned when Exosphere asked for a console:"
                        , Element.paragraph [] [ Element.text (Debug.toString error) ]
                        ]

                RemoteData.Success consoleUrl ->
                    let
                        flippyCardContents : PasswordVisibility -> Element.Element Msg -> Element.Element Msg
                        flippyCardContents pwVizOnClick contents =
                            Element.el
                                [ Events.onClick
                                    (ProjectMsg (Helpers.getProjectId project) <|
                                        SetProjectView <|
                                            ServerDetail serverUuid
                                                { serverDetailViewParams | passwordVisibility = pwVizOnClick }
                                    )
                                , Element.centerX
                                , Element.centerY
                                , Element.height Element.fill
                                , Element.width Element.fill
                                ]
                                (Element.el
                                    [ Element.centerX ]
                                    contents
                                )

                        passwordFlippyCard password =
                            Card.flipping
                                { width = 550
                                , height = 30
                                , activeFront =
                                    case serverDetailViewParams.passwordVisibility of
                                        PasswordShown ->
                                            False

                                        PasswordHidden ->
                                            True
                                , front = flippyCardContents PasswordShown <| Element.text "(click to view password)"
                                , back = flippyCardContents PasswordHidden <| copyableText password
                                }

                        passwordHint =
                            Helpers.getServerExouserPassword details
                                |> Maybe.withDefault Element.none
                                << Maybe.map
                                    (\password ->
                                        Element.column
                                            [ Element.spacing 10 ]
                                            [ Element.text "Try logging in with username \"exouser\" and the following password:"
                                            , passwordFlippyCard password
                                            ]
                                    )
                    in
                    Element.column
                        VH.exoColumnAttributes
                        [ VH.browserLink
                            appIsElectron
                            consoleUrl
                            (View.Types.BrowserLinkFancyLabel
                                (Widget.textButton
                                    (Widget.Style.Material.outlinedButton Style.Theme.exoPalette)
                                    { text = "Console", onPress = Just NoOp }
                                )
                            )
                        , Element.paragraph []
                            [ Element.text <|
                                "Launching the console is like connecting a screen, mouse, and keyboard to your server. "
                                    ++ "If your server has a desktop environment then you can interact with it here."
                            , passwordHint
                            ]
                        ]

        OSTypes.ServerBuilding ->
            Element.text "Server building, console not available yet."

        _ ->
            Element.text "Console not available with server in this state."


cockpitInteraction : ServerOrigin -> Maybe String -> Element.Element Msg
cockpitInteraction serverOrigin maybeFloatingIp =
    maybeFloatingIp
        |> Maybe.withDefault (Element.text "Server Dashboard and Terminal not ready yet.")
        << Maybe.map
            (\floatingIp ->
                case serverOrigin of
                    ServerNotFromExo ->
                        Element.text "Not available (server launched outside of Exosphere)."

                    ServerFromExo serverFromExoProps ->
                        let
                            interaction =
                                Element.column VH.exoColumnAttributes
                                    [ Element.text "Server Dashboard and Terminal are ready..."
                                    , Element.row VH.exoRowAttributes
                                        [ Widget.textButton
                                            (Widget.Style.Material.outlinedButton Style.Theme.exoPalette)
                                            { text = "Type commands in a shell!"
                                            , onPress =
                                                Just <|
                                                    OpenNewWindow <|
                                                        "https://"
                                                            ++ floatingIp
                                                            ++ ":9090/cockpit/@localhost/system/terminal.html"
                                            }
                                        ]
                                    , Element.row
                                        VH.exoRowAttributes
                                        [ Widget.textButton
                                            (Widget.Style.Material.outlinedButton Style.Theme.exoPalette)
                                            { text = "Server Dashboard"
                                            , onPress =
                                                Just <|
                                                    OpenNewWindow <|
                                                        "https://"
                                                            ++ floatingIp
                                                            ++ ":9090/cockpit/@localhost/system/terminal.html"
                                            }
                                        ]
                                    ]
                        in
                        case serverFromExoProps.cockpitStatus of
                            NotChecked ->
                                Element.text "Status of server dashboard and terminal not available yet."

                            CheckedNotReady ->
                                Element.text "Server Dashboard and Terminal not ready yet."

                            Ready ->
                                interaction

                            ReadyButRecheck ->
                                interaction
            )


viewServerActions : ProjectIdentifier -> Server -> ServerDetailViewParams -> Element.Element Msg
viewServerActions projectId server serverDetailViewParams =
    Element.column
        [ Element.spacingXY 0 10 ]
    <|
        case server.exoProps.targetOpenstackStatus of
            Nothing ->
                List.map
                    (renderServerActionButton projectId server serverDetailViewParams)
                    (ServerActions.getAllowed server.osProps.details.openstackStatus server.osProps.details.lockStatus)

            Just _ ->
                []


renderServerActionButton : ProjectIdentifier -> Server -> ServerDetailViewParams -> ServerActions.ServerAction -> Element.Element Msg
renderServerActionButton projectId server serverDetailViewParams serverAction =
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
            renderActionButton serverAction (Just updateAction) serverAction.name

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
            renderConfirmationButton serverAction actionMsg cancelMsg title

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
            renderActionButton serverAction actionMsg title

        ( ServerActions.UpdateAction updateAction, _, _ ) ->
            let
                actionMsg =
                    Just <| updateAction projectId server

                title =
                    serverAction.name
            in
            renderActionButton serverAction actionMsg title


confirmationMessage : ServerActions.ServerAction -> String
confirmationMessage serverAction =
    "Are you sure you want to " ++ (serverAction.name |> String.toLower) ++ "?"


serverActionSelectModButton : ServerActions.SelectMod -> (Widget.TextButton Msg -> Element.Element Msg)
serverActionSelectModButton selectMod =
    case selectMod of
        ServerActions.NoMod ->
            Widget.textButton (Widget.Style.Material.outlinedButton Style.Theme.exoPalette)

        ServerActions.Primary ->
            Widget.textButton (Widget.Style.Material.containedButton Style.Theme.exoPalette)

        ServerActions.Warning ->
            Widget.textButton (Style.Widgets.Button.warningButton Style.Theme.exoPalette (Color.rgb255 255 221 87))

        ServerActions.Danger ->
            Widget.textButton (Style.Widgets.Button.dangerButton Style.Theme.exoPalette)


renderActionButton : ServerActions.ServerAction -> Maybe Msg -> String -> Element.Element Msg
renderActionButton serverAction actionMsg title =
    Element.row
        [ Element.spacing 10 ]
        [ Element.el
            [ Element.width <| Element.px 120 ]
          <|
            serverActionSelectModButton serverAction.selectMod
                { text = title
                , onPress = actionMsg
                }
        , Element.text serverAction.description

        -- TODO hover text with description
        ]


renderConfirmationButton : ServerActions.ServerAction -> Maybe Msg -> Maybe Msg -> String -> Element.Element Msg
renderConfirmationButton serverAction actionMsg cancelMsg title =
    Element.row
        [ Element.spacing 10 ]
        [ Element.text title
        , Element.el
            []
          <|
            serverActionSelectModButton serverAction.selectMod
                { text = "Yes"
                , onPress = actionMsg
                }
        , Element.el
            []
          <|
            serverActionSelectModButton serverAction.selectMod
                { text = "No"
                , onPress = cancelMsg
                }

        -- TODO hover text with description
        ]


resourceUsageGraphs : ServerOrigin -> Maybe String -> Element.Element Msg
resourceUsageGraphs serverOrigin maybeFloatingIp =
    maybeFloatingIp
        |> Maybe.withDefault (Element.text "Graphs not ready yet.")
        << Maybe.map
            (\floatingIp ->
                case serverOrigin of
                    ServerNotFromExo ->
                        Element.text "Not available (server launched outside of Exosphere)."

                    ServerFromExo serverFromExoProps ->
                        let
                            graphs =
                                let
                                    graphsUrl =
                                        "https://" ++ floatingIp ++ ":9090/cockpit/@localhost/system/index.html"
                                in
                                -- I am so sorry
                                Element.html
                                    (Html.div
                                        [ Html.Attributes.style "position" "relative"
                                        , Html.Attributes.style "overflow" "hidden"
                                        , Html.Attributes.style "width" "550px"
                                        , Html.Attributes.style "height" "650px"
                                        ]
                                        [ Html.iframe
                                            [ Html.Attributes.style "position" "absolute"
                                            , Html.Attributes.style "top" "-320px"
                                            , Html.Attributes.style "left" "-30px"
                                            , Html.Attributes.style "width" "600px"
                                            , Html.Attributes.style "height" "1000px"

                                            -- https://stackoverflow.com/questions/15494568/html-iframe-disable-scroll
                                            -- This is not compliant HTML5 but still works
                                            , Html.Attributes.attribute "scrolling" "no"
                                            , Html.Attributes.src graphsUrl
                                            ]
                                            []
                                        ]
                                    )
                        in
                        case serverFromExoProps.cockpitStatus of
                            Ready ->
                                graphs

                            ReadyButRecheck ->
                                graphs

                            NotChecked ->
                                Element.text "Graphs not ready yet."

                            CheckedNotReady ->
                                Element.text "Graphs not ready yet."
            )


renderServer : Project -> ServerFilter -> List DeleteConfirmation -> Server -> Element.Element Msg
renderServer project serverFilter deleteConfirmations server =
    let
        userUuid =
            project.auth.user.uuid

        statusIcon =
            Element.el [ Element.paddingEach { edges | right = 15 } ] (Icon.roundRect (server |> Helpers.getServerUiStatus |> Helpers.getServerUiStatusColor) 16)

        checkbox =
            case server.osProps.details.lockStatus of
                OSTypes.ServerUnlocked ->
                    Input.checkbox [ Element.width Element.shrink ]
                        { checked = server.exoProps.selected
                        , onChange = \new -> ProjectMsg (Helpers.getProjectId project) (SelectServer server new)
                        , icon = Input.defaultCheckbox
                        , label = Input.labelHidden server.osProps.name
                        }

                OSTypes.ServerLocked ->
                    Input.checkbox [ Element.width Element.shrink ]
                        { checked = server.exoProps.selected
                        , onChange = \_ -> NoOp
                        , icon = \_ -> Icon.lock (Element.rgb255 10 10 10) 14
                        , label = Input.labelHidden server.osProps.name
                        }

        serverLabelName : Server -> Element.Element Msg
        serverLabelName aServer =
            Element.row [ Element.width Element.fill ] <|
                [ statusIcon ]
                    ++ (if aServer.osProps.details.userUuid == userUuid then
                            [ Element.el
                                [ Font.bold
                                , Element.paddingEach { edges | right = 15 }
                                ]
                                (Element.text aServer.osProps.name)
                            , ExoCard.badge "created by you"
                            ]

                        else
                            [ Element.el [ Font.bold ] (Element.text aServer.osProps.name)
                            ]
                       )

        serverNameClickEvent : Msg
        serverNameClickEvent =
            ProjectMsg (Helpers.getProjectId project) <|
                SetProjectView <|
                    ServerDetail
                        server.osProps.uuid
                        { verboseStatus = False
                        , passwordVisibility = PasswordHidden
                        , ipInfoLevel = IPSummary
                        , serverActionNamePendingConfirmation = Nothing
                        }

        serverLabel : Server -> Element.Element Msg
        serverLabel aServer =
            Element.row
                [ Element.width Element.fill
                , Events.onClick serverNameClickEvent
                , Element.pointer
                ]
                [ serverLabelName aServer
                , Element.el [ Font.size 15 ] (Element.text (server |> Helpers.getServerUiStatus |> Helpers.getServerUiStatusStr))
                ]

        deletionAttempted =
            server.exoProps.deletionAttempted

        confirmationNeeded =
            List.member server.osProps.uuid deleteConfirmations

        deleteWidget =
            case ( deletionAttempted, server.osProps.details.lockStatus, confirmationNeeded ) of
                ( True, _, _ ) ->
                    [ Element.text "Deleting..." ]

                ( False, OSTypes.ServerUnlocked, True ) ->
                    [ Element.text "Confirm delete?"
                    , Widget.iconButton
                        (Style.Widgets.Button.dangerButton Style.Theme.exoPalette)
                        { icon = Icon.remove (Element.rgb255 255 255 255) 16
                        , text = "Delete"
                        , onPress =
                            Just
                                (ProjectMsg (Helpers.getProjectId project) (RequestDeleteServer server.osProps.uuid))
                        }
                    , Widget.iconButton
                        (Widget.Style.Material.outlinedButton Style.Theme.exoPalette)
                        { icon = Icon.windowClose (Element.rgb255 0 0 0) 16
                        , text = "Cancel"
                        , onPress =
                            Just
                                (ProjectMsg
                                    (Helpers.getProjectId project)
                                    (SetProjectView <|
                                        ListProjectServers
                                            serverFilter
                                            (deleteConfirmations |> List.filter ((/=) server.osProps.uuid))
                                    )
                                )
                        }
                    ]

                ( False, OSTypes.ServerUnlocked, False ) ->
                    [ Widget.iconButton
                        (Style.Widgets.Button.dangerButton Style.Theme.exoPalette)
                        { icon = Icon.remove (Element.rgb255 255 255 255) 16
                        , text = "Delete"
                        , onPress =
                            Just
                                (ProjectMsg (Helpers.getProjectId project)
                                    (SetProjectView <| ListProjectServers serverFilter [ server.osProps.uuid ])
                                )
                        }
                    ]

                ( False, OSTypes.ServerLocked, _ ) ->
                    [ Widget.iconButton
                        (Style.Widgets.Button.dangerButton Style.Theme.exoPalette)
                        { icon = Icon.remove (Element.rgb255 255 255 255) 16
                        , text = "Delete"
                        , onPress = Nothing
                        }
                    ]
    in
    Element.row (VH.exoRowAttributes ++ [ Element.width Element.fill ])
        ([ checkbox
         , serverLabel server
         ]
            ++ deleteWidget
        )


renderIpAddresses : List OSTypes.IpAddress -> ProjectIdentifier -> OSTypes.ServerUuid -> ServerDetailViewParams -> Element.Element Msg
renderIpAddresses ipAddresses projectId serverUuid serverDetailViewParams =
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
                        VH.compactKVSubRow "Floating IP" <| copyableText ipAddress.address
                    )

        gray : Element.Color
        gray =
            Element.rgb255 219 219 219

        ipButton : String -> String -> IPInfoLevel -> Element.Element Msg
        ipButton displayButtonString displayLabel ipMsg =
            Element.row
                [ Element.spacing 3 ]
                [ Input.button
                    [ Font.size 10
                    , Border.width 1
                    , Border.rounded 20
                    , Border.color gray
                    , Element.padding 3
                    ]
                    { onPress =
                        Just <|
                            ProjectMsg projectId <|
                                SetProjectView <|
                                    ServerDetail
                                        serverUuid
                                        { serverDetailViewParams | ipInfoLevel = ipMsg }
                    , label = Element.text displayButtonString
                    }
                , Element.el [ Font.size 10 ] (Element.text displayLabel)
                ]
    in
    case serverDetailViewParams.ipInfoLevel of
        IPDetails ->
            Element.column
                (VH.exoColumnAttributes ++ [ Element.padding 0 ])
                (floatingIpAddressRows
                    ++ fixedIpAddressRows
                    ++ [ ipButton "^" "IP summary" IPSummary ]
                )

        IPSummary ->
            Element.column
                (VH.exoColumnAttributes ++ [ Element.padding 0 ])
                (floatingIpAddressRows ++ [ ipButton ">" "IP details" IPDetails ])


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


serverVolumes : Project -> Server -> Element.Element Msg
serverVolumes project server =
    let
        vols =
            Helpers.getVolsAttachedToServer project server

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
                        , Border.color <| Element.rgb255 122 122 122
                        , Element.padding 3
                        ]
                        { onPress =
                            Just
                                (ProjectMsg
                                    (Helpers.getProjectId project)
                                    (SetProjectView <| VolumeDetail v.uuid [])
                                )
                        , label = Icon.rightArrow (Element.rgb255 122 122 122) 16
                        }

                volumeRow v =
                    let
                        ( device, mountpoint ) =
                            if isBootVol v then
                                ( "Boot volume", "" )

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
