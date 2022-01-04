module Page.ServerDetail exposing (Model, Msg(..), init, update, view)

import DateFormat.Relative
import Dict
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons
import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers as Helpers
import Helpers.Interaction as IHelpers
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.String
import Helpers.Time
import OpenStack.ServerActions as ServerActions
import OpenStack.ServerNameValidator exposing (serverNameValidator)
import OpenStack.Types as OSTypes
import Page.ServerResourceUsageAlerts
import Page.ServerResourceUsageCharts
import RemoteData
import Route
import Style.Helpers as SH exposing (shadowDefaults)
import Style.Widgets.Card
import Style.Widgets.CopyableText exposing (copyableText)
import Style.Widgets.Icon as Icon
import Style.Widgets.IconButton
import Style.Widgets.ToggleTip
import Time
import Types.HelperTypes exposing (FloatingIpOption(..), ServerResourceQtys, UserAppProxyHostname)
import Types.Interaction as ITypes exposing (Interaction)
import Types.Project exposing (Project)
import Types.Server exposing (Server, ServerOrigin(..))
import Types.ServerResourceUsage
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types
import Widget
import Widget.Style.Material


type alias Model =
    { serverUuid : OSTypes.ServerUuid
    , showCreatedTimeToggleTip : Bool
    , showFlavorToggleTip : Bool
    , verboseStatus : VerboseStatus
    , passwordVisibility : PasswordVisibility
    , ipInfoLevel : IpInfoLevel
    , serverActionNamePendingConfirmation : Maybe String
    , serverNamePendingConfirmation : Maybe String
    , activeInteractionToggleTip : Maybe Interaction
    , activeEventHistoryAbsoluteTimeToggleTip : Maybe Time.Posix
    , retainFloatingIpsWhenDeleting : Bool
    , showActionsDropdown : Bool
    }


type IpInfoLevel
    = IpDetails
    | IpSummary


type alias VerboseStatus =
    Bool


type PasswordVisibility
    = PasswordShown
    | PasswordHidden


type Msg
    = GotShowCreatedTimeToggleTip Bool
    | GotShowFlavorToggleTip Bool
    | GotShowVerboseStatus Bool
    | GotPasswordVisibility PasswordVisibility
    | GotIpInfoLevel IpInfoLevel
    | GotServerActionNamePendingConfirmation (Maybe String)
    | GotServerNamePendingConfirmation (Maybe String)
    | GotActiveInteractionToggleTip (Maybe Interaction)
    | GotActiveEventHistoryAbsoluteTimeToggleTip (Maybe Time.Posix)
    | GotRetainFloatingIpsWhenDeleting Bool
    | GotShowActionsDropdown Bool
    | GotSetServerName String
    | SharedMsg SharedMsg.SharedMsg
    | NoOp


init : OSTypes.ServerUuid -> Model
init serverUuid =
    { serverUuid = serverUuid
    , showCreatedTimeToggleTip = False
    , showFlavorToggleTip = False
    , verboseStatus = False
    , passwordVisibility = PasswordHidden
    , ipInfoLevel = IpSummary
    , serverActionNamePendingConfirmation = Nothing
    , serverNamePendingConfirmation = Nothing
    , activeInteractionToggleTip = Nothing
    , activeEventHistoryAbsoluteTimeToggleTip = Nothing
    , retainFloatingIpsWhenDeleting = False
    , showActionsDropdown = False
    }


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg project model =
    case msg of
        GotShowCreatedTimeToggleTip shown ->
            ( { model | showCreatedTimeToggleTip = shown }, Cmd.none, SharedMsg.NoOp )

        GotShowFlavorToggleTip shown ->
            ( { model | showFlavorToggleTip = shown }, Cmd.none, SharedMsg.NoOp )

        GotShowVerboseStatus shown ->
            ( { model | verboseStatus = shown }, Cmd.none, SharedMsg.NoOp )

        GotPasswordVisibility visibility ->
            ( { model | passwordVisibility = visibility }, Cmd.none, SharedMsg.NoOp )

        GotIpInfoLevel level ->
            ( { model | ipInfoLevel = level }, Cmd.none, SharedMsg.NoOp )

        GotServerActionNamePendingConfirmation maybeAction ->
            ( { model | serverActionNamePendingConfirmation = maybeAction }, Cmd.none, SharedMsg.NoOp )

        GotServerNamePendingConfirmation maybeName ->
            ( { model | serverNamePendingConfirmation = maybeName }, Cmd.none, SharedMsg.NoOp )

        GotActiveInteractionToggleTip maybeInteraction ->
            ( { model | activeInteractionToggleTip = maybeInteraction }, Cmd.none, SharedMsg.NoOp )

        GotActiveEventHistoryAbsoluteTimeToggleTip maybeTime ->
            ( { model | activeEventHistoryAbsoluteTimeToggleTip = maybeTime }, Cmd.none, SharedMsg.NoOp )

        GotRetainFloatingIpsWhenDeleting retain ->
            ( { model | retainFloatingIpsWhenDeleting = retain }, Cmd.none, SharedMsg.NoOp )

        GotShowActionsDropdown shown ->
            ( { model | showActionsDropdown = shown }, Cmd.none, SharedMsg.NoOp )

        GotSetServerName validName ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg project.auth.project.uuid <|
                SharedMsg.ServerMsg model.serverUuid <|
                    SharedMsg.RequestSetServerName validName
            )

        SharedMsg msg_ ->
            -- TODO convert other pages to use this style
            ( { model | showActionsDropdown = False }, Cmd.none, msg_ )

        NoOp ->
            ( model, Cmd.none, SharedMsg.NoOp )


view : View.Types.Context -> Project -> ( Time.Posix, Time.Zone ) -> Model -> Element.Element Msg
view context project currentTimeAndZone model =
    {- Attempt to look up a given server UUID; if a Server type is found, call rendering function serverDetail_ -}
    case GetterSetters.serverLookup project model.serverUuid of
        Just server ->
            serverDetail_ context project currentTimeAndZone model server

        Nothing ->
            Element.text <|
                String.join " "
                    [ "No"
                    , context.localization.virtualComputer
                    , "found"
                    ]


serverDetail_ : View.Types.Context -> Project -> ( Time.Posix, Time.Zone ) -> Model -> Server -> Element.Element Msg
serverDetail_ context project currentTimeAndZone model server =
    {- Render details of a server type and associated resources (e.g. volumes) -}
    let
        details =
            server.osProps.details

        creatorName =
            case server.exoProps.serverOrigin of
                ServerFromExo exoOriginProps ->
                    case exoOriginProps.exoCreatorUsername of
                        Just creatorName_ ->
                            creatorName_

                        Nothing ->
                            "unknown user"

                _ ->
                    "unknown user"

        maybeFlavor =
            GetterSetters.flavorLookup project details.flavorId

        flavorContents =
            case maybeFlavor of
                Just flavor ->
                    let
                        serverResourceQtys =
                            Helpers.serverResourceQtys project flavor server

                        toggleTipContents =
                            Element.column
                                []
                                [ Element.text (String.fromInt serverResourceQtys.cores ++ " CPU cores")
                                , case serverResourceQtys.vgpus of
                                    Just vgpuQty ->
                                        let
                                            desc =
                                                if vgpuQty == 1 then
                                                    "virtual GPU"

                                                else
                                                    "virtual GPUs"
                                        in
                                        Element.text
                                            (String.fromInt vgpuQty ++ " " ++ desc)

                                    Nothing ->
                                        Element.none
                                , Element.text
                                    (String.fromInt serverResourceQtys.ramGb ++ " GB RAM")
                                , case serverResourceQtys.rootDiskGb of
                                    Just rootDiskGb ->
                                        Element.text
                                            (String.fromInt rootDiskGb
                                                ++ " GB root disk"
                                            )

                                    Nothing ->
                                        Element.text "unknown root disk size"
                                ]

                        toggleTip =
                            Style.Widgets.ToggleTip.toggleTip
                                context.palette
                                toggleTipContents
                                model.showFlavorToggleTip
                                (GotShowFlavorToggleTip (not model.showFlavorToggleTip))
                    in
                    Element.row
                        [ Element.spacing 5 ]
                        [ Element.text flavor.name
                        , toggleTip
                        ]

                Nothing ->
                    Element.text ("Unknown " ++ context.localization.virtualComputerHardwareConfig)

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
                    GetterSetters.getBootVolume vols server.osProps.uuid
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

        tile : List (Element.Element Msg) -> List (Element.Element Msg) -> Element.Element Msg
        tile headerContents contents =
            Style.Widgets.Card.exoCard context.palette
                (Element.column (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
                    (List.concat
                        [ [ Element.row
                                (VH.heading3 context.palette
                                    ++ [ Element.spacing 10
                                       , Border.width 0
                                       ]
                                )
                                headerContents
                          ]
                        , contents
                        ]
                    )
                )

        firstColumnContents : List (Element.Element Msg)
        firstColumnContents =
            [ tile
                [ FeatherIcons.monitor
                    |> FeatherIcons.toHtml []
                    |> Element.html
                    |> Element.el []
                , Element.text "Interactions"
                ]
                [ interactions
                    context
                    project
                    server
                    (Tuple.first currentTimeAndZone)
                    (VH.userAppProxyLookup context project)
                    model
                ]
            , tile
                [ FeatherIcons.hash
                    |> FeatherIcons.toHtml []
                    |> Element.html
                    |> Element.el []
                , Element.text "Credentials"
                ]
                [ renderIpAddresses
                    context
                    project
                    server
                    model
                , VH.compactKVSubRow "Username" (Element.text "exouser")
                , VH.compactKVSubRow "Passphrase"
                    (serverPassword context model server)
                , VH.compactKVSubRow
                    (String.join " "
                        [ context.localization.pkiPublicKeyForSsh
                            |> Helpers.String.toTitleCase
                        , "Name"
                        ]
                    )
                    (Element.text (Maybe.withDefault "(none)" details.keypairName))
                ]
            ]

        secondColumnContents : List (Element.Element Msg)
        secondColumnContents =
            let
                attachButton =
                    if
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
                        Element.link []
                            { url =
                                Route.toUrl context.urlPathPrefix
                                    (Route.ProjectRoute project.auth.project.uuid <|
                                        Route.VolumeAttach (Just server.osProps.uuid) Nothing
                                    )
                            , label =
                                Widget.textButton
                                    (SH.materialStyle context.palette).button
                                    { text = "Attach " ++ context.localization.blockDevice
                                    , onPress = Just NoOp
                                    }
                            }

                    else
                        Element.none
            in
            [ tile
                [ FeatherIcons.hardDrive
                    |> FeatherIcons.toHtml []
                    |> Element.html
                    |> Element.el []
                , context.localization.blockDevice
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                    |> Element.text
                ]
                [ serverVolumes context project server
                , Element.el [ Element.centerX ] attachButton
                ]
            , tile
                [ Icon.history (SH.toElementColor context.palette.on.background) 20
                , Element.text "Action History"
                ]
                [ serverEventHistory
                    context
                    model
                    (Tuple.first currentTimeAndZone)
                    server.events
                ]
            ]

        ( dualColumn, columnWidth ) =
            if context.windowSize.width < 1150 then
                ( False, Element.fill )

            else
                let
                    colWidthPx =
                        (context.windowSize.width - 100) // 2
                in
                ( True, colWidthPx |> Element.px )
    in
    Element.column (VH.exoColumnAttributes ++ [ Element.spacing 15 ])
        [ Element.column [ Element.width Element.fill ]
            [ Element.row
                (VH.heading2 context.palette ++ [ Element.spacing 10 ])
                [ FeatherIcons.server |> FeatherIcons.toHtml [] |> Element.html |> Element.el []
                , Element.column [ Element.spacing 5 ]
                    [ Element.row [ Element.spacing 10 ]
                        [ Element.text
                            (context.localization.virtualComputer
                                |> Helpers.String.toTitleCase
                            )
                        , serverNameView context model server
                        ]
                    , Element.el
                        [ Font.size 12, Font.color (SH.toElementColor context.palette.muted) ]
                        (copyableText context.palette [] server.osProps.uuid)
                    ]
                , Element.el
                    [ Element.alignRight, Font.size 18, Font.regular ]
                    (serverStatus context model server)
                , Element.el
                    [ Element.alignRight, Font.size 16, Font.regular ]
                    (serverActionsDropdown context project model server)
                ]
            , passwordVulnWarning context server
            , VH.createdAgoByFromSize
                context
                (Tuple.first currentTimeAndZone)
                details.created
                (Just ( "user", creatorName ))
                (Just ( context.localization.staticRepresentationOfBlockDeviceContents, imageText ))
                (Just ( context.localization.virtualComputerHardwareConfig, flavorContents ))
                model.showCreatedTimeToggleTip
                (GotShowCreatedTimeToggleTip (not model.showCreatedTimeToggleTip))
            ]
        , if details.openstackStatus == OSTypes.ServerActive then
            resourceUsageCharts context
                currentTimeAndZone
                server
                (maybeFlavor |> Maybe.map (\flavor -> Helpers.serverResourceQtys project flavor server))

          else
            Element.none
        , if dualColumn then
            let
                columnAttributes =
                    VH.exoColumnAttributes
                        ++ [ Element.alignTop
                           , Element.centerX
                           , Element.width columnWidth
                           , Element.spacing 25
                           ]
            in
            Element.row
                [ Element.width Element.fill
                , Element.spacing 5
                , Element.paddingEach { top = 10, bottom = 0, left = 0, right = 0 }
                ]
                [ Element.column columnAttributes firstColumnContents
                , Element.column columnAttributes secondColumnContents
                ]

          else
            Element.column
                (VH.exoColumnAttributes ++ [ Element.width (Element.maximum 700 Element.fill), Element.centerX, Element.spacing 25 ])
                (List.append firstColumnContents secondColumnContents)
        ]


serverNameView : View.Types.Context -> Model -> Server -> Element.Element Msg
serverNameView context model server =
    let
        serverNameViewPlain =
            Element.row
                [ Element.spacing 10 ]
                [ Element.text server.osProps.name
                , Widget.iconButton
                    (SH.materialStyle context.palette).button
                    { text = "Edit"
                    , icon =
                        FeatherIcons.edit3
                            |> FeatherIcons.withSize 16
                            |> FeatherIcons.toHtml []
                            |> Element.html
                            |> Element.el []
                    , onPress =
                        Just <| GotServerNamePendingConfirmation (Just server.osProps.name)
                    }
                ]

        serverNameViewEdit =
            let
                invalidNameReasons =
                    serverNameValidator
                        (Just context.localization.virtualComputer)
                        (model.serverNamePendingConfirmation
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
                                        { shadowDefaults
                                            | color = SH.toElementColorWithOpacity context.palette.muted 0.2
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
                    case ( invalidNameReasons, model.serverNamePendingConfirmation ) of
                        ( Nothing, Just validName ) ->
                            Just <|
                                GotSetServerName validName

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
                        , text = model.serverNamePendingConfirmation |> Maybe.withDefault ""
                        , placeholder =
                            Just
                                (Input.placeholder
                                    []
                                    (Element.text <|
                                        String.join " "
                                            [ "My"
                                            , context.localization.virtualComputer
                                                |> Helpers.String.toTitleCase
                                            ]
                                    )
                                )
                        , label = "Name"
                        , onChange = \name -> GotServerNamePendingConfirmation <| Just name
                        }
                    )
                , Widget.iconButton
                    (SH.materialStyle context.palette).button
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
                    (SH.materialStyle context.palette).button
                    { text = "Cancel"
                    , icon =
                        FeatherIcons.xCircle
                            |> FeatherIcons.withSize 16
                            |> FeatherIcons.toHtml []
                            |> Element.html
                            |> Element.el []
                    , onPress =
                        Just <| GotServerNamePendingConfirmation Nothing
                    }
                ]
    in
    case model.serverNamePendingConfirmation of
        Just _ ->
            serverNameViewEdit

        Nothing ->
            serverNameViewPlain


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
                    , VH.externalLink
                        context
                        "https://en.wikipedia.org/wiki/Privilege_escalation"
                        "privilege escalation vulnerability"
                    , Element.text <|
                        String.join " "
                            [ ". If you have used this"
                            , context.localization.virtualComputer
                            , "for anything important or sensitive, consider rotating the password for exouser, or building a new"
                            , context.localization.virtualComputer
                            , "and moving to that one instead of this one. For more information, see "
                            ]
                    , VH.externalLink
                        context
                        "https://gitlab.com/exosphere/exosphere/issues/284"
                        "issue #284"
                    , Element.text " on the Exosphere GitLab project."
                    ]

            else
                Element.none


serverStatus : View.Types.Context -> Model -> Server -> Element.Element Msg
serverStatus context model server =
    let
        details =
            server.osProps.details

        statusBadge =
            VH.serverStatusBadge context.palette server

        lockStatus : OSTypes.ServerLockStatus -> Element.Element Msg
        lockStatus lockStatus_ =
            case lockStatus_ of
                OSTypes.ServerLocked ->
                    Icon.lock (SH.toElementColor context.palette.on.background) 28

                OSTypes.ServerUnlocked ->
                    Icon.lockOpen (SH.toElementColor context.palette.on.background) 28

        verboseStatusToggleTip =
            let
                friendlyOpenstackStatus : OSTypes.ServerStatus -> String
                friendlyOpenstackStatus osStatus =
                    OSTypes.serverStatusToString osStatus
                        |> String.dropLeft 6

                friendlyPowerState =
                    OSTypes.serverPowerStateToString details.powerState
                        |> String.dropLeft 5

                contents =
                    -- TODO nicer layout here?
                    Element.column []
                        [ Element.text ("OpenStack Status: " ++ friendlyOpenstackStatus details.openstackStatus)
                        , case server.exoProps.targetOpenstackStatus of
                            Just expectedStatusList ->
                                let
                                    listStr =
                                        expectedStatusList
                                            |> List.map friendlyOpenstackStatus
                                            |> String.join ", "
                                in
                                Element.text ("Transitioning to: " ++ listStr)

                            Nothing ->
                                Element.none
                        , Element.text ("Power State: " ++ friendlyPowerState)
                        , Element.text
                            ("Lock Status: "
                                ++ (case details.lockStatus of
                                        OSTypes.ServerLocked ->
                                            "Locked"

                                        OSTypes.ServerUnlocked ->
                                            "Unlocked"
                                   )
                            )
                        , case VH.getExoSetupStatusStr server of
                            Just setupStatusStr ->
                                Element.text ("Exosphere Setup Status: " ++ setupStatusStr)

                            Nothing ->
                                Element.none
                        ]
            in
            Style.Widgets.ToggleTip.toggleTip context.palette
                contents
                model.verboseStatus
                (GotShowVerboseStatus (not model.verboseStatus))
    in
    Element.row [ Element.spacing 15 ]
        [ verboseStatusToggleTip
        , statusBadge
        , lockStatus details.lockStatus
        ]


interactions : View.Types.Context -> Project -> Server -> Time.Posix -> Maybe UserAppProxyHostname -> Model -> Element.Element Msg
interactions context project server currentTime tlsReverseProxyHostname model =
    let
        renderInteraction interaction =
            let
                interactionStatus =
                    IHelpers.interactionStatus
                        project
                        server
                        interaction
                        context
                        currentTime
                        tlsReverseProxyHostname

                ( statusWord, statusColor ) =
                    IHelpers.interactionStatusWordColor context.palette interactionStatus

                interactionDetails =
                    IHelpers.interactionDetails interaction context

                interactionToggleTip =
                    let
                        status =
                            Element.row []
                                [ Element.el [ Font.bold ] <| Element.text "Status: "
                                , Element.text statusWord
                                ]

                        statusReason =
                            let
                                renderReason reason =
                                    Element.text <| "(" ++ reason ++ ")"
                            in
                            case interactionStatus of
                                ITypes.Unavailable reason ->
                                    renderReason reason

                                ITypes.Error reason ->
                                    renderReason reason

                                ITypes.Warn _ reason ->
                                    renderReason reason

                                _ ->
                                    Element.none

                        description =
                            Element.paragraph []
                                [ Element.el [ Font.bold ] <| Element.text "Description: "
                                , Element.text interactionDetails.description
                                ]

                        contents =
                            Element.column
                                [ Element.width (Element.shrink |> Element.minimum 200)
                                , Element.spacing 10
                                , Element.padding 5
                                ]
                                [ status
                                , statusReason
                                , description
                                ]

                        shown =
                            case model.activeInteractionToggleTip of
                                Just interaction_ ->
                                    interaction == interaction_

                                _ ->
                                    False

                        showHideMsg : ITypes.Interaction -> Msg
                        showHideMsg interaction_ =
                            let
                                newValue =
                                    case model.activeInteractionToggleTip of
                                        Just _ ->
                                            Nothing

                                        Nothing ->
                                            Just <| interaction_
                            in
                            GotActiveInteractionToggleTip newValue
                    in
                    Style.Widgets.ToggleTip.toggleTip
                        context.palette
                        contents
                        shown
                        (showHideMsg interaction)
            in
            case interactionStatus of
                ITypes.Hidden ->
                    Element.none

                _ ->
                    Element.row
                        VH.exoRowAttributes
                        [ Icon.roundRect statusColor 14
                        , case interactionDetails.type_ of
                            ITypes.UrlInteraction ->
                                Widget.button
                                    (SH.materialStyle context.palette).button
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
                                            (interactionDetails.icon (SH.toElementColor context.palette.primary) 18)
                                    , onPress =
                                        case interactionStatus of
                                            ITypes.Ready url ->
                                                Just <| SharedMsg <| SharedMsg.OpenNewWindow url

                                            ITypes.Warn url _ ->
                                                Just <| SharedMsg <| SharedMsg.OpenNewWindow url

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
                                                , copyableText context.palette [] text
                                                ]

                                        _ ->
                                            Element.none
                                    ]
                        , interactionToggleTip
                        ]
    in
    [ ITypes.GuacTerminal
    , ITypes.GuacDesktop
    , ITypes.NativeSSH
    , ITypes.Console
    , ITypes.CustomWorkflow
    ]
        |> List.map renderInteraction
        |> Element.column []


serverPassword : View.Types.Context -> Model -> Server -> Element.Element Msg
serverPassword context model server =
    let
        passwordShower password =
            Element.column
                [ Element.spacing 10 ]
                [ case model.passwordVisibility of
                    PasswordShown ->
                        copyableText context.palette [] password

                    PasswordHidden ->
                        Element.none
                , let
                    changeMsg newValue =
                        GotPasswordVisibility newValue

                    ( buttonText, onPressMsg ) =
                        case model.passwordVisibility of
                            PasswordShown ->
                                ( "Hide passphrase"
                                , changeMsg PasswordHidden
                                )

                            PasswordHidden ->
                                ( "Show"
                                , changeMsg PasswordShown
                                )
                  in
                  Widget.textButton
                    (SH.materialStyle context.palette).button
                    { text = buttonText
                    , onPress = Just onPressMsg
                    }
                ]

        passwordHint =
            GetterSetters.getServerExouserPassword server.osProps.details
                |> Maybe.withDefault (Element.text "Not available yet, check back in a few minutes.")
                << Maybe.map
                    (\password ->
                        passwordShower password
                    )
    in
    passwordHint


serverActionsDropdown : View.Types.Context -> Project -> Model -> Server -> Element.Element Msg
serverActionsDropdown context project model server =
    let
        contents =
            Element.column
                (VH.dropdownAttributes context ++ [ Element.padding 10 ])
            <|
                List.map
                    (renderServerActionButton context project model server)
                    (ServerActions.getAllowed
                        (Just context.localization.virtualComputer)
                        (Just context.localization.staticRepresentationOfBlockDeviceContents)
                        server.osProps.details.openstackStatus
                        server.osProps.details.lockStatus
                    )

        ( attribs, icon ) =
            if model.showActionsDropdown then
                ( [ Element.below contents ], FeatherIcons.chevronUp )

            else
                ( [], FeatherIcons.chevronDown )
    in
    case server.exoProps.targetOpenstackStatus of
        Nothing ->
            Element.column
                attribs
                [ Widget.iconButton
                    (SH.materialStyle context.palette).button
                    { text = "Actions"
                    , icon =
                        Element.row
                            [ Element.spacing 5 ]
                            [ Element.text "Actions"
                            , Element.el []
                                (icon
                                    |> FeatherIcons.withSize 18
                                    |> FeatherIcons.toHtml []
                                    |> Element.html
                                )
                            ]
                    , onPress = Just (GotShowActionsDropdown (not model.showActionsDropdown))
                    }
                ]

        Just _ ->
            Element.none


serverEventHistory :
    View.Types.Context
    -> Model
    -> Time.Posix
    -> RemoteData.WebData (List OSTypes.ServerEvent)
    -> Element.Element Msg
serverEventHistory context model currentTime serverEventsWebData =
    case serverEventsWebData of
        RemoteData.Success serverEvents ->
            let
                renderTableHeader : String -> Element.Element Msg
                renderTableHeader headerText =
                    Element.el [ Font.bold ] <| Element.text headerText

                columns : List (Element.Column OSTypes.ServerEvent Msg)
                columns =
                    [ { header = renderTableHeader "Action"
                      , width = Element.px 180
                      , view =
                            \event ->
                                let
                                    actionStr =
                                        event.action
                                            |> String.replace "_" " "
                                in
                                Element.paragraph [] [ Element.text actionStr ]
                      }
                    , { header = renderTableHeader "Time"
                      , width = Element.px 180
                      , view =
                            \event ->
                                let
                                    relativeTime =
                                        DateFormat.Relative.relativeTime currentTime event.startTime

                                    absoluteTime =
                                        let
                                            shown =
                                                model.activeEventHistoryAbsoluteTimeToggleTip
                                                    == Just event.startTime

                                            showHideMsg =
                                                if shown then
                                                    GotActiveEventHistoryAbsoluteTimeToggleTip Nothing

                                                else
                                                    GotActiveEventHistoryAbsoluteTimeToggleTip
                                                        (Just event.startTime)
                                        in
                                        Style.Widgets.ToggleTip.toggleTip context.palette
                                            (Element.text (Helpers.Time.humanReadableTime event.startTime))
                                            shown
                                            showHideMsg
                                in
                                Element.row []
                                    [ Element.text relativeTime
                                    , absoluteTime
                                    ]
                      }
                    ]
            in
            Element.table
                (VH.formContainer
                    ++ [ Element.spacingXY 0 7
                       , Element.width Element.fill
                       , Border.color (context.palette.muted |> SH.toElementColor)
                       ]
                )
                { data = serverEvents, columns = columns }

        _ ->
            Element.none


renderServerActionButton : View.Types.Context -> Project -> Model -> Server -> ServerActions.ServerAction -> Element.Element Msg
renderServerActionButton context project model server serverAction =
    let
        displayConfirmation =
            case model.serverActionNamePendingConfirmation of
                Nothing ->
                    False

                Just actionName ->
                    actionName == serverAction.name
    in
    case ( serverAction.confirmable, displayConfirmation ) of
        ( True, False ) ->
            let
                updateAction =
                    GotServerActionNamePendingConfirmation <| Just serverAction.name
            in
            renderActionButton context serverAction (Just updateAction) serverAction.name

        ( True, True ) ->
            let
                renderKeepFloatingIpCheckbox : List (Element.Element Msg)
                renderKeepFloatingIpCheckbox =
                    if
                        serverAction.name
                            == "Delete"
                            && (not <| List.isEmpty <| GetterSetters.getServerFloatingIps project server.osProps.uuid)
                    then
                        [ Input.checkbox
                            []
                            { onChange = GotRetainFloatingIpsWhenDeleting
                            , icon = Input.defaultCheckbox
                            , checked = model.retainFloatingIpsWhenDeleting
                            , label =
                                Input.labelRight []
                                    (Element.text <|
                                        String.join " "
                                            [ "Keep the"
                                            , context.localization.floatingIpAddress
                                            , "of this"
                                            , context.localization.virtualComputer
                                            , "for future use"
                                            ]
                                    )
                            }
                        ]

                    else
                        []

                actionMsg =
                    Just <| serverAction.action project.auth.project.uuid server model.retainFloatingIpsWhenDeleting

                cancelMsg =
                    Just <| GotServerActionNamePendingConfirmation Nothing

                title =
                    confirmationMessage serverAction
            in
            Element.column
                [ Element.spacing 5 ]
            <|
                List.concat
                    [ [ renderConfirmationButton context serverAction actionMsg cancelMsg title ]
                    , renderKeepFloatingIpCheckbox
                    ]

        ( _, _ ) ->
            -- This is ugly, we should have an explicit custom type for server actions and match on that
            if String.toLower serverAction.name == String.toLower context.localization.staticRepresentationOfBlockDeviceContents then
                -- Overriding button for image, because we just want to navigate to another page
                Element.link [ Element.width Element.fill ]
                    { url =
                        Route.toUrl context.urlPathPrefix
                            (Route.ProjectRoute project.auth.project.uuid <|
                                Route.ServerCreateImage server.osProps.uuid <|
                                    Just <|
                                        server.osProps.name
                                            ++ "-image"
                            )
                    , label =
                        renderActionButton
                            context
                            serverAction
                            (Just NoOp)
                            (Helpers.String.toTitleCase context.localization.staticRepresentationOfBlockDeviceContents)
                    }

            else
                let
                    actionMsg =
                        Just <| SharedMsg <| serverAction.action project.auth.project.uuid server model.retainFloatingIpsWhenDeleting

                    title =
                        serverAction.name
                in
                renderActionButton context serverAction actionMsg title


confirmationMessage : ServerActions.ServerAction -> String
confirmationMessage serverAction =
    "Are you sure you want to " ++ (serverAction.name |> String.toLower) ++ "?"


serverActionSelectModButton : View.Types.Context -> ServerActions.SelectMod -> (Widget.TextButton Msg -> Element.Element Msg)
serverActionSelectModButton context selectMod =
    let
        buttonPalette =
            case selectMod of
                ServerActions.NoMod ->
                    (SH.materialStyle context.palette).button

                ServerActions.Primary ->
                    (SH.materialStyle context.palette).primaryButton

                ServerActions.Warning ->
                    (SH.materialStyle context.palette).warningButton

                ServerActions.Danger ->
                    (SH.materialStyle context.palette).dangerButton
    in
    Widget.textButton
        { buttonPalette
            | container =
                buttonPalette.container
                    ++ [ Element.width Element.fill ]
            , labelRow =
                buttonPalette.labelRow
                    ++ [ Element.centerX ]
            , text =
                buttonPalette.text
                    ++ [ Element.centerX ]
        }


renderActionButton : View.Types.Context -> ServerActions.ServerAction -> Maybe Msg -> String -> Element.Element Msg
renderActionButton context serverAction actionMsg title =
    Element.row
        [ Element.spacing 10, Element.width Element.fill ]
        [ Element.text serverAction.description
        , Element.el
            [ Element.width <| Element.px 100, Element.alignRight ]
          <|
            serverActionSelectModButton context
                serverAction.selectMod
                { text = title
                , onPress = actionMsg
                }
        ]


renderConfirmationButton : View.Types.Context -> ServerActions.ServerAction -> Maybe SharedMsg.SharedMsg -> Maybe Msg -> String -> Element.Element Msg
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
                , onPress = Maybe.map SharedMsg actionMsg
                }
        , Element.el
            []
          <|
            Widget.textButton (SH.materialStyle context.palette).button
                { text = "No"
                , onPress = cancelMsg
                }

        -- TODO hover text with description
        ]


resourceUsageCharts : View.Types.Context -> ( Time.Posix, Time.Zone ) -> Server -> Maybe ServerResourceQtys -> Element.Element Msg
resourceUsageCharts context currentTimeAndZone server maybeServerResourceQtys =
    let
        containerWidth =
            context.windowSize.width - 76

        chartsWidth =
            max 1075 containerWidth

        thirtyMinMillis =
            1000 * 60 * 30

        toChartHeading : String -> String -> Element.Element Msg
        toChartHeading title subtitle =
            Element.row
                [ Element.width Element.fill, Element.paddingEach { top = 0, bottom = 0, left = 0, right = 25 } ]
                [ Element.el [ Font.bold ] (Element.text title)
                , Element.el
                    [ Font.color (context.palette.muted |> SH.toElementColor)
                    , Element.alignRight
                    ]
                    (Element.text subtitle)
                ]

        cpuHeading : Element.Element Msg
        cpuHeading =
            toChartHeading
                "CPU"
                (maybeServerResourceQtys
                    |> Maybe.map .cores
                    |> Maybe.map
                        (\x ->
                            String.join " "
                                [ "of"
                                , String.fromInt x
                                , "total"
                                , if x == 1 then
                                    "core"

                                  else
                                    "cores"
                                ]
                        )
                    |> Maybe.withDefault ""
                )

        memHeading : Element.Element Msg
        memHeading =
            toChartHeading
                "RAM"
                (maybeServerResourceQtys
                    |> Maybe.map .ramGb
                    |> Maybe.map
                        (\x ->
                            String.join " "
                                [ "of"
                                , String.fromInt x
                                , "total GB"
                                ]
                        )
                    |> Maybe.withDefault ""
                )

        diskHeading : Element.Element Msg
        diskHeading =
            toChartHeading
                "Disk"
                (maybeServerResourceQtys
                    |> Maybe.andThen .rootDiskGb
                    |> Maybe.map
                        (\x ->
                            String.join " "
                                [ "of"
                                , String.fromInt x
                                , "total GB"
                                ]
                        )
                    |> Maybe.withDefault ""
                )

        charts_ : Types.ServerResourceUsage.TimeSeries -> Element.Element Msg
        charts_ timeSeries =
            Element.column
                [ Element.width (Element.px containerWidth) ]
                [ Page.ServerResourceUsageAlerts.view context (Tuple.first currentTimeAndZone) timeSeries
                , Page.ServerResourceUsageCharts.view
                    context
                    chartsWidth
                    currentTimeAndZone
                    timeSeries
                    cpuHeading
                    memHeading
                    diskHeading
                ]

        charts =
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
                                charts_ history.timeSeries

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
    in
    charts


renderIpAddresses : View.Types.Context -> Project -> Server -> Model -> Element.Element Msg
renderIpAddresses context project server model =
    let
        fixedIpAddressRows =
            GetterSetters.getServerFixedIps project server.osProps.uuid
                |> List.map
                    (\ipAddress ->
                        VH.compactKVSubRow
                            (Helpers.String.toTitleCase context.localization.nonFloatingIpAddress)
                            (Element.text ipAddress)
                    )

        floatingIpAddressRows =
            if List.isEmpty (GetterSetters.getServerFloatingIps project server.osProps.uuid) then
                if server.exoProps.floatingIpCreationOption == DoNotUseFloatingIp then
                    -- The server doesn't have a floating IP and we aren't waiting to create one, so give user option to assign one
                    [ Element.text <|
                        String.join " "
                            [ "No"
                            , context.localization.floatingIpAddress
                            , "assigned."
                            ]
                    , Element.link []
                        { url =
                            Route.toUrl context.urlPathPrefix <|
                                Route.ProjectRoute project.auth.project.uuid <|
                                    Route.FloatingIpAssign Nothing (Just server.osProps.uuid)
                        , label =
                            Widget.textButton
                                (SH.materialStyle context.palette).button
                                { text =
                                    String.join " "
                                        [ "Assign a", context.localization.floatingIpAddress ]
                                , onPress = Just NoOp
                                }
                        }
                    ]

                else
                    -- Floating IP is not yet created as part of server launch, but it might be.
                    [ Element.text <|
                        String.join " "
                            [ "No"
                            , context.localization.floatingIpAddress
                            , "yet, please wait"
                            ]
                    ]

            else
                GetterSetters.getServerFloatingIps project server.osProps.uuid
                    |> List.map
                        (\ipAddress ->
                            VH.compactKVSubRow
                                (Helpers.String.toTitleCase context.localization.floatingIpAddress)
                                (Element.row [ Element.spacing 15 ]
                                    [ copyableText context.palette [] ipAddress.address
                                    , Widget.textButton
                                        (SH.materialStyle context.palette).button
                                        { text =
                                            "Unassign"
                                        , onPress =
                                            Just <|
                                                SharedMsg <|
                                                    SharedMsg.ProjectMsg project.auth.project.uuid <|
                                                        SharedMsg.RequestUnassignFloatingIp ipAddress.uuid
                                        }
                                    ]
                                )
                        )

        ipButton : Element.Element Msg -> String -> IpInfoLevel -> Element.Element Msg
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
                    { onPress = Just <| GotIpInfoLevel ipMsg
                    , label = label
                    }
                , Element.el [ Font.size 10 ] (Element.text displayLabel)
                ]
    in
    case model.ipInfoLevel of
        IpDetails ->
            let
                icon =
                    FeatherIcons.chevronDown
                        |> FeatherIcons.withSize 12
                        |> FeatherIcons.toHtml []
                        |> Element.html
            in
            Element.column
                []
                (floatingIpAddressRows
                    ++ ipButton icon "IP Details" IpSummary
                    :: fixedIpAddressRows
                )

        IpSummary ->
            let
                icon =
                    FeatherIcons.chevronRight
                        |> FeatherIcons.withSize 12
                        |> FeatherIcons.toHtml []
                        |> Element.html
            in
            Element.column
                []
                (floatingIpAddressRows ++ [ ipButton icon "IP Details" IpDetails ])


serverVolumes : View.Types.Context -> Project -> Server -> Element.Element Msg
serverVolumes context project server =
    let
        vols =
            GetterSetters.getVolsAttachedToServer project server
    in
    case List.length vols of
        0 ->
            Element.text "(none)"

        _ ->
            let
                volDetailsButton v =
                    Element.link []
                        { url =
                            Route.toUrl context.urlPathPrefix <|
                                Route.ProjectRoute project.auth.project.uuid <|
                                    Route.VolumeDetail v.uuid
                        , label =
                            Style.Widgets.IconButton.goToButton context.palette (Just NoOp)
                        }

                volumeRow v =
                    let
                        ( device, mountpoint ) =
                            if GetterSetters.isBootVolume (Just server.osProps.uuid) v then
                                ( String.join " "
                                    [ "Boot"
                                    , context.localization.blockDevice
                                    ]
                                , ""
                                )

                            else
                                case GetterSetters.volumeDeviceRawName server v of
                                    Just device_ ->
                                        ( device_
                                        , case GetterSetters.volDeviceToMountpoint device_ of
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
                    , { header = Element.el [ Font.heavy ] <| Element.text "Mount point"
                      , width = Element.fill
                      , view = \v -> Element.text v.mountpoint
                      }
                    , { header = Element.none
                      , width = Element.px 22
                      , view = \v -> v.toButton
                      }
                    ]
                }
