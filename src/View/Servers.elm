module View.Servers exposing (serverDetail, servers)

import Color
import Element
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Framework.Button as Button
import Framework.Card as Card
import Framework.Color
import Framework.Modifier as Modifier
import Framework.Spinner as Spinner
import Helpers.Helpers as Helpers
import Html
import Html.Attributes
import OpenStack.ServerActions as ServerActions
import OpenStack.Types as OSTypes
import RemoteData
import Style.Widgets.Icon as Icon
import Style.Widgets.IconButton as IconButton
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
        , ViewState(..)
        , ViewStateParams
        )
import View.Helpers as VH exposing (edges)
import View.Types


servers : Project -> Element.Element Msg
servers project =
    case project.servers of
        RemoteData.NotAsked ->
            Element.paragraph [] [ Element.text "Please wait..." ]

        RemoteData.Loading ->
            Element.paragraph [] [ Element.text "Loading..." ]

        RemoteData.Failure e ->
            Element.paragraph [] [ Element.text ("Cannot display servers. Error message: " ++ Debug.toString e) ]

        RemoteData.Success someServers ->
            if List.isEmpty someServers then
                Element.paragraph [] [ Element.text "You don't have any servers yet, go create one!" ]

            else
                let
                    noServersSelected =
                        List.any (\s -> s.exoProps.selected) someServers |> not

                    allServersSelected =
                        List.all (\s -> s.exoProps.selected) someServers

                    selectedServers =
                        List.filter (\s -> s.exoProps.selected) someServers

                    deleteButtonOnPress =
                        if noServersSelected == True then
                            Nothing

                        else
                            Just (ProjectMsg (Helpers.getProjectId project) (RequestDeleteServers selectedServers))

                    deleteButtonModifiers =
                        if noServersSelected == True then
                            [ Modifier.Danger, Modifier.Disabled ]

                        else
                            [ Modifier.Danger ]
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
                        , Button.button deleteButtonModifiers deleteButtonOnPress "Delete"
                        ]
                    , Element.column (VH.exoColumnAttributes ++ [ Element.width (Element.fill |> Element.maximum 960) ])
                        (List.map (renderServer project) someServers)
                    ]


serverDetail : Bool -> Project -> OSTypes.ServerUuid -> ViewStateParams -> Element.Element Msg
serverDetail appIsElectron project serverUuid viewStateParams =
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
                        Helpers.imageLookup project details.imageUuid
                            |> Maybe.map .name
                            |> Maybe.withDefault "N/A"

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
                        , VH.compactKVRow "Name" (Element.text server.osProps.name)
                        , VH.compactKVRow "Status" (serverStatus projectId server viewStateParams)
                        , VH.compactKVRow "UUID" (Element.text server.osProps.uuid)
                        , VH.compactKVRow "Created on" (Element.text details.created)
                        , VH.compactKVRow "Image" (Element.text imageText)
                        , VH.compactKVRow "Flavor" (Element.text flavorText)
                        , VH.compactKVRow "SSH Key Name" (Element.text (Maybe.withDefault "(none)" details.keypairName))
                        , VH.compactKVRow "IP addresses"
                            (renderIpAddresses
                                details.ipAddresses
                                projectId
                                server.osProps.uuid
                                viewStateParams
                            )
                        , VH.compactKVRow "Volumes Attached" (serverVolumes project server)
                        , Element.el VH.heading2 (Element.text "Interact with server")
                        , VH.compactKVRow "SSH" <| sshInstructions maybeFloatingIp
                        , VH.compactKVRow "Console" <| consoleLink appIsElectron project server serverUuid viewStateParams
                        , VH.compactKVRow "Terminal / Dashboard" <| cockpitInteraction server.exoProps.cockpitStatus maybeFloatingIp
                        ]
                    , Element.column (Element.alignTop :: Element.width (Element.px 585) :: VH.exoColumnAttributes)
                        [ Element.el VH.heading3 (Element.text "Server Actions")
                        , actions projectId server
                        , Element.el VH.heading3 (Element.text "System Resource Usage")
                        , resourceUsageGraphs server.exoProps.cockpitStatus maybeFloatingIp
                        ]
                    ]
            )


serverStatus : ProjectIdentifier -> Server -> ViewStateParams -> Element.Element Msg
serverStatus projectId server viewStateParams =
    let
        details =
            server.osProps.details

        friendlyOpenstackStatus =
            Debug.toString details.openstackStatus
                |> String.dropLeft 6

        friendlyPowerState =
            Debug.toString details.powerState
                |> String.dropLeft 5

        graphic =
            let
                g =
                    case ( details.openstackStatus, server.exoProps.targetOpenstackStatus ) of
                        ( OSTypes.ServerBuilding, _ ) ->
                            Spinner.spinner Spinner.ThreeCircles 30 Framework.Color.yellow

                        ( _, Just _ ) ->
                            Spinner.spinner Spinner.ThreeCircles 30 Framework.Color.grey_darker

                        ( _, Nothing ) ->
                            Icon.roundRect (server |> Helpers.getServerUiStatus |> Helpers.getServerUiStatusColor) 32
            in
            Element.el
                [ Element.paddingEach { edges | right = 15 } ]
                g

        verboseStatus =
            if viewStateParams.verboseStatus then
                [ Element.text "Detailed status"
                , VH.compactKVSubRow "OpenStack status" (Element.text friendlyOpenstackStatus)
                , VH.compactKVSubRow "Power state" (Element.text friendlyPowerState)
                , VH.compactKVSubRow "Server Dashboard and Terminal readiness" (Element.paragraph [] [ Element.text (friendlyCockpitReadiness server.exoProps.cockpitStatus) ])
                ]

            else
                [ Button.button
                    []
                    (Just <|
                        ProjectMsg projectId <|
                            SetProjectView <|
                                ServerDetail
                                    server.osProps.uuid
                                    { viewStateParams | verboseStatus = True }
                    )
                    "See detail"
                ]

        statusString =
            Element.text
                (server
                    |> Helpers.getServerUiStatus
                    |> Helpers.getServerUiStatusStr
                )
    in
    Element.column
        (VH.exoColumnAttributes ++ [ Element.padding 0 ])
        (Element.row [ Font.bold ]
            [ graphic
            , statusString
            ]
            :: verboseStatus
        )


sshInstructions : Maybe String -> Element.Element Msg
sshInstructions maybeFloatingIp =
    case maybeFloatingIp of
        Nothing ->
            Element.none

        Just floatingIp ->
            Element.paragraph
                []
                [ Element.text "exouser@"
                , Element.text floatingIp
                ]


consoleLink : Bool -> Project -> Server -> OSTypes.ServerUuid -> ViewStateParams -> Element.Element Msg
consoleLink appIsElectron project server serverUuid viewStateParams =
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
                        flippyCardContents : PasswordVisibility -> String -> Element.Element Msg
                        flippyCardContents pwVizOnClick text =
                            Element.el
                                [ Events.onClick
                                    (ProjectMsg (Helpers.getProjectId project) <|
                                        SetProjectView <|
                                            ServerDetail serverUuid
                                                { viewStateParams | passwordVisibility = pwVizOnClick }
                                    )
                                , Element.centerX
                                , Element.centerY
                                ]
                                (Element.el
                                    [ Element.centerX ]
                                    (Element.text text)
                                )

                        passwordFlippyCard password =
                            Card.flipping
                                { width = 550
                                , height = 30
                                , activeFront =
                                    case viewStateParams.passwordVisibility of
                                        PasswordShown ->
                                            False

                                        PasswordHidden ->
                                            True
                                , front = flippyCardContents PasswordShown "(click to view password)"
                                , back = flippyCardContents PasswordHidden password
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
                            (View.Types.BrowserLinkFancyLabel (Button.button [] Nothing "Console"))
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


cockpitInteraction : CockpitLoginStatus -> Maybe String -> Element.Element Msg
cockpitInteraction cockpitStatus maybeFloatingIp =
    maybeFloatingIp
        |> Maybe.withDefault (Element.text "Server Dashboard and Terminal not ready yet.")
        << Maybe.map
            (\floatingIp ->
                case cockpitStatus of
                    NotChecked ->
                        Element.text "Status of server dashboard and terminal not available yet."

                    CheckedNotReady ->
                        Element.text "Server Dashboard and Terminal not ready yet."

                    Ready ->
                        Element.column VH.exoColumnAttributes
                            [ Element.text "Server Dashboard and Terminal are ready..."
                            , Element.row VH.exoRowAttributes
                                [ Button.button
                                    []
                                    (Just <|
                                        OpenNewWindow <|
                                            "https://"
                                                ++ floatingIp
                                                ++ ":9090/cockpit/@localhost/system/terminal.html"
                                    )
                                    "Type commands in a shell!"
                                , Element.text "Type commands in a shell!"
                                ]
                            , Element.row
                                VH.exoRowAttributes
                                [ Button.button
                                    []
                                    (Just <|
                                        OpenNewWindow <|
                                            "https://"
                                                ++ floatingIp
                                                ++ ":9090"
                                    )
                                    "Server Dashboard"
                                , Element.text "Manage your server with an interactive dashboard!"
                                ]
                            ]
            )


actions : ProjectIdentifier -> Server -> Element.Element Msg
actions projectId server =
    let
        details =
            server.osProps.details
    in
    case server.exoProps.targetOpenstackStatus of
        Nothing ->
            let
                allowedActions =
                    ServerActions.getAllowed details.openstackStatus

                renderActionButton action =
                    Element.row
                        [ Element.spacing 10 ]
                        [ Element.el
                            [ Element.width <| Element.px 100 ]
                          <|
                            Button.button
                                action.selectMods
                                (Just <| ProjectMsg projectId <| RequestServerAction server action.action action.targetStatus)
                                action.name
                        , Element.text action.description
                        ]

                -- TODO hover text with description
            in
            Element.column
                [ Element.spacingXY 0 10 ]
            <|
                List.map renderActionButton allowedActions

        Just _ ->
            Element.el
                [ Element.padding 10 ]
                Element.none


resourceUsageGraphs : CockpitLoginStatus -> Maybe String -> Element.Element Msg
resourceUsageGraphs cockpitStatus maybeFloatingIp =
    maybeFloatingIp
        |> Maybe.withDefault (Element.text "Graphs not ready yet.")
        << Maybe.map
            (\floatingIp ->
                case cockpitStatus of
                    Ready ->
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

                    NotChecked ->
                        Element.text "Graphs not ready yet."

                    CheckedNotReady ->
                        Element.text "Graphs not ready yet."
            )


renderServer : Project -> Server -> Element.Element Msg
renderServer project server =
    let
        statusIcon =
            Element.el [ Element.paddingEach { edges | right = 15 } ] (Icon.roundRect (server |> Helpers.getServerUiStatus |> Helpers.getServerUiStatusColor) 16)

        checkBoxLabel : Server -> Element.Element Msg
        checkBoxLabel aServer =
            Element.row []
                [ statusIcon
                , Element.el [ Font.bold ] (Element.text aServer.osProps.name)
                ]
    in
    Element.row (VH.exoRowAttributes ++ [ Element.width Element.fill ])
        [ Input.checkbox []
            { checked = server.exoProps.selected
            , onChange = \new -> ProjectMsg (Helpers.getProjectId project) (SelectServer server new)
            , icon = Input.defaultCheckbox
            , label = Input.labelRight [] (checkBoxLabel server)
            }
        , Element.el [ Font.size 15 ] (Element.text (server |> Helpers.getServerUiStatus |> Helpers.getServerUiStatusStr))
        , Button.button
            []
            (Just <|
                ProjectMsg (Helpers.getProjectId project) <|
                    SetProjectView <|
                        ServerDetail
                            server.osProps.uuid
                            { verboseStatus = False
                            , passwordVisibility = PasswordHidden
                            , ipInfoLevel = IPSummary
                            }
            )
            "Details"
        , if server.exoProps.deletionAttempted == True then
            Element.text "Deleting..."

          else
            IconButton.iconButton [ Modifier.Danger, Modifier.Small ] (Just (ProjectMsg (Helpers.getProjectId project) (RequestDeleteServer server))) (Icon.remove Framework.Color.white 16)
        ]


renderIpAddresses : List OSTypes.IpAddress -> ProjectIdentifier -> OSTypes.ServerUuid -> ViewStateParams -> Element.Element Msg
renderIpAddresses ipAddresses projectId serverUuid viewStateParams =
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
                        VH.compactKVSubRow "Floating IP" (Element.text ipAddress.address)
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
                                        { viewStateParams | ipInfoLevel = ipMsg }
                    , label = Element.text displayButtonString
                    }
                , Element.el [ Font.size 10 ] (Element.text displayLabel)
                ]
    in
    case viewStateParams.ipInfoLevel of
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


friendlyCockpitReadiness : CockpitLoginStatus -> String
friendlyCockpitReadiness cockpitLoginStatus =
    case cockpitLoginStatus of
        NotChecked ->
            "Not checked yet"

        CheckedNotReady ->
            "Checked but not ready yet (May become ready soon)"

        Ready ->
            "Ready"


serverVolumes : Project -> Server -> Element.Element Msg
serverVolumes project server =
    let
        vols =
            Helpers.getVolsAttachedToServer project server

        isBootVol v =
            v.attachments
                |> List.map .device
                |> List.member "/dev/vda"
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
                        , Border.color <| Color.toElementColor <| Framework.Color.grey
                        , Element.padding 3
                        ]
                        { onPress =
                            Just
                                (ProjectMsg
                                    (Helpers.getProjectId project)
                                    (SetProjectView <| VolumeDetail v.uuid)
                                )
                        , label = Icon.rightArrow Framework.Color.grey 16
                        }

                renderVolume v =
                    Element.row VH.exoRowAttributes
                        [ Element.text <| VH.possiblyUntitledResource v.name "volume"
                        , if isBootVol v then
                            Element.el
                                [ Font.color <| Color.toElementColor <| Framework.Color.grey ]
                            <|
                                Element.text "Boot volume"

                          else
                            Element.none
                        , volDetailsButton v
                        ]
            in
            Element.column [ Element.spacing 3, Font.size 14 ]
                (vols |> List.map renderVolume)
