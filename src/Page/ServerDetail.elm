module Page.ServerDetail exposing (Model, Msg(..), init, update, view)

import DateFormat.Relative
import Dict
import Element
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
import Helpers.Validation as Validation
import OpenStack.DnsRecordSet
import OpenStack.ServerActions as ServerActions
import OpenStack.ServerNameValidator exposing (serverNameValidator)
import OpenStack.ServerVolumes exposing (serverCanHaveVolumeAttached)
import OpenStack.Types as OSTypes
import Page.ServerResourceUsageAlerts
import Page.ServerResourceUsageCharts
import RemoteData
import Route
import Style.Helpers as SH
import Style.Types as ST
import Style.Widgets.Alert as Alert
import Style.Widgets.Button
import Style.Widgets.Card
import Style.Widgets.CopyableText exposing (copyableText)
import Style.Widgets.Icon as Icon
import Style.Widgets.IconButton
import Style.Widgets.Link as Link
import Style.Widgets.Popover.Popover exposing (popover, popoverStyleDefaults)
import Style.Widgets.Popover.Types exposing (PopoverId)
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import Style.Widgets.ToggleTip
import Time
import Types.HelperTypes exposing (FloatingIpOption(..), ServerResourceQtys, UserAppProxyHostname)
import Types.Interaction as ITypes
import Types.Project exposing (Project)
import Types.Server exposing (ExoSetupStatus(..), Server, ServerOrigin(..))
import Types.ServerResourceUsage
import Types.SharedMsg as SharedMsg
import View.Helpers as VH exposing (edges)
import View.Types
import Widget


type alias Model =
    { serverUuid : OSTypes.ServerUuid
    , verboseStatus : VerboseStatus
    , passphraseVisibility : PassphraseVisibility
    , ipInfoLevel : IpInfoLevel
    , serverActionNamePendingConfirmation : Maybe String
    , serverNamePendingConfirmation : Maybe String
    , retainFloatingIpsWhenDeleting : Bool
    }


type IpInfoLevel
    = IpDetails
    | IpSummary


type alias VerboseStatus =
    Bool


type PassphraseVisibility
    = PassphraseShown
    | PassphraseHidden


type Msg
    = GotShowVerboseStatus Bool
    | GotPassphraseVisibility PassphraseVisibility
    | GotIpInfoLevel IpInfoLevel
    | GotServerActionNamePendingConfirmation (Maybe String)
    | GotServerNamePendingConfirmation (Maybe String)
    | GotRetainFloatingIpsWhenDeleting Bool
    | GotSetServerName String
    | SharedMsg SharedMsg.SharedMsg
    | NoOp


init : OSTypes.ServerUuid -> Model
init serverUuid =
    { serverUuid = serverUuid
    , verboseStatus = False
    , passphraseVisibility = PassphraseHidden
    , ipInfoLevel = IpSummary
    , serverActionNamePendingConfirmation = Nothing
    , serverNamePendingConfirmation = Nothing
    , retainFloatingIpsWhenDeleting = False
    }


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg project model =
    case msg of
        GotShowVerboseStatus shown ->
            ( { model | verboseStatus = shown }, Cmd.none, SharedMsg.NoOp )

        GotPassphraseVisibility visibility ->
            ( { model | passphraseVisibility = visibility }, Cmd.none, SharedMsg.NoOp )

        GotIpInfoLevel level ->
            ( { model | ipInfoLevel = level }, Cmd.none, SharedMsg.NoOp )

        GotServerActionNamePendingConfirmation maybeAction ->
            ( { model | serverActionNamePendingConfirmation = maybeAction }, Cmd.none, SharedMsg.NoOp )

        GotServerNamePendingConfirmation maybeName ->
            ( { model | serverNamePendingConfirmation = maybeName }, Cmd.none, SharedMsg.NoOp )

        GotRetainFloatingIpsWhenDeleting retain ->
            ( { model | retainFloatingIpsWhenDeleting = retain }, Cmd.none, SharedMsg.NoOp )

        GotSetServerName validName ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                SharedMsg.ServerMsg model.serverUuid <|
                    SharedMsg.RequestSetServerName validName
            )

        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )

        NoOp ->
            ( model, Cmd.none, SharedMsg.NoOp )


popoverMsgMapper : PopoverId -> Msg
popoverMsgMapper popoverId =
    SharedMsg <| SharedMsg.TogglePopover popoverId


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
serverDetail_ context project ( currentTime, timeZone ) model server =
    {- Render details of a server type and associated resources (e.g. volumes) -}
    let
        details =
            server.osProps.details

        whenCreated =
            let
                timeDistanceStr =
                    DateFormat.Relative.relativeTime currentTime details.created

                createdTimeText =
                    let
                        createdTimeFormatted =
                            Helpers.Time.humanReadableDateAndTime details.created
                    in
                    Element.text ("Created on: " ++ createdTimeFormatted)

                setupTimeText =
                    case server.exoProps.serverOrigin of
                        ServerFromExo exoOriginProps ->
                            case exoOriginProps.exoSetupStatus.data of
                                RDPP.DoHave ( ExoSetupComplete, maybeSetupCompleteTime ) _ ->
                                    let
                                        setupTimeStr =
                                            case maybeSetupCompleteTime of
                                                Nothing ->
                                                    "Unknown"

                                                Just setupCompleteTime ->
                                                    Helpers.Time.relativeTimeNoAffixes details.created setupCompleteTime
                                    in
                                    Element.text ("Setup time: " ++ setupTimeStr)

                                _ ->
                                    Element.none

                        _ ->
                            Element.none

                toggleTipContents =
                    Element.column [] [ createdTimeText, setupTimeText ]
            in
            Element.row
                [ Element.spacing spacer.px4 ]
                [ Element.text timeDistanceStr
                , Style.Widgets.ToggleTip.toggleTip
                    context
                    popoverMsgMapper
                    (Helpers.String.hyphenate
                        [ "createdTimeTip"
                        , project.auth.project.uuid
                        , server.osProps.uuid
                        ]
                    )
                    toggleTipContents
                    ST.PositionBottomLeft
                ]

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
                                context
                                popoverMsgMapper
                                (Helpers.String.hyphenate [ "flavorToggleTip", project.auth.project.uuid, server.osProps.uuid ])
                                toggleTipContents
                                ST.PositionBottomRight
                    in
                    Element.row
                        [ Element.spacing spacer.px4 ]
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
                        |> (\maybeName -> VH.resourceName maybeName details.imageUuid)
                        |> Just

                maybeVolBackedImageName =
                    let
                        vols =
                            RemoteData.withDefault [] project.volumes
                    in
                    GetterSetters.getBootVolume vols server.osProps.uuid
                        |> Maybe.andThen .imageMetadata
                        |> Maybe.map (\data -> VH.resourceName (Just data.name) data.uuid)
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
                (Element.column
                    [ Element.width Element.fill
                    , Element.padding spacer.px16
                    , Element.spacing spacer.px16
                    ]
                    (List.concat
                        [ [ Element.row
                                (Text.subheadingStyleAttrs context.palette
                                    ++ Text.typographyAttrs Text.Large
                                    ++ [ Border.width 0 ]
                                )
                                headerContents
                          ]
                        , contents
                        ]
                    )
                )

        serverDetailTiles =
            let
                usernameView =
                    case server.exoProps.serverOrigin of
                        ServerFromExo _ ->
                            Element.text "exouser"

                        _ ->
                            Element.el
                                [ context.palette.neutral.text.subdued
                                    |> SH.toElementColor
                                    |> Font.color
                                ]
                                (Element.text "unknown")

                buttonLabel onPress =
                    Widget.textButton
                        (SH.materialStyle context.palette).button
                        { text = "Attach " ++ context.localization.blockDevice
                        , onPress = onPress
                        }

                attachButton =
                    if serverCanHaveVolumeAttached server then
                        Element.link []
                            { url =
                                Route.toUrl context.urlPathPrefix
                                    (Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                                        Route.VolumeAttach (Just server.osProps.uuid) Nothing
                                    )
                            , label = buttonLabel <| Just NoOp
                            }

                    else
                        Element.none
            in
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
                    currentTime
                    (GetterSetters.getUserAppProxyFromContext project context)
                ]
            , tile
                [ FeatherIcons.key
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
                , VH.compactKVSubRow "Username" usernameView
                , VH.compactKVSubRow "Passphrase"
                    (serverPassphrase context model server)
                , VH.compactKVSubRow
                    (String.join " "
                        [ context.localization.pkiPublicKeyForSsh
                            |> Helpers.String.toTitleCase
                        , "Name"
                        ]
                    )
                    (Element.text (Maybe.withDefault "(none)" details.keypairName))
                ]
            , tile
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
                [ Icon.history (SH.toElementColor context.palette.neutral.text.default) 20
                , Element.text "Action History"
                ]
                [ serverEventHistory
                    context
                    project
                    server
                    currentTime
                ]
            ]

        serverFaultView =
            case details.fault of
                Just serverFault ->
                    Alert.alert [ Element.width Element.fill ]
                        context.palette
                        { state = Alert.Danger
                        , showIcon = True
                        , showContainer = True
                        , content =
                            Element.paragraph []
                                [ Text.strong serverFault.message ]
                        }

                Nothing ->
                    Element.none
    in
    Element.column [ Element.spacing spacer.px24, Element.width Element.fill ]
        [ Element.row (Text.headingStyleAttrs context.palette)
            [ FeatherIcons.server |> FeatherIcons.toHtml [] |> Element.html |> Element.el []
            , Text.text Text.ExtraLarge
                []
                (context.localization.virtualComputer
                    |> Helpers.String.toTitleCase
                )
            , serverNameView context project currentTime model server
            , Element.row [ Element.alignRight, Text.fontSize Text.Body, Font.regular, Element.spacing spacer.px16 ]
                [ serverStatus context project server
                , serverActionsDropdown context project model server
                ]
            ]
        , tile
            [ FeatherIcons.cpu |> FeatherIcons.toHtml [] |> Element.html |> Element.el []
            , Element.text "Info"
            , Element.el
                [ Text.fontSize Text.Tiny
                , Font.color (SH.toElementColor context.palette.neutral.text.subdued)
                , Element.alignBottom
                ]
                (copyableText context.palette
                    [ Element.width (Element.shrink |> Element.minimum 240) ]
                    server.osProps.uuid
                )
            ]
            [ VH.createdAgoByFromSize
                context
                ( "created", whenCreated )
                (Just ( "user", creatorName ))
                (Just ( context.localization.staticRepresentationOfBlockDeviceContents, imageText ))
                (Just ( context.localization.virtualComputerHardwareConfig, flavorContents ))
                server.osProps
                project
            , passphraseVulnWarning context server
            ]
        , serverFaultView
        , if List.member details.openstackStatus [ OSTypes.ServerActive, OSTypes.ServerVerifyResize ] then
            tile
                [ FeatherIcons.activity |> FeatherIcons.toHtml [] |> Element.html |> Element.el []
                , Element.text "Resource Usage"
                ]
                [ resourceUsageCharts context
                    ( currentTime, timeZone )
                    server
                    (maybeFlavor |> Maybe.map (\flavor -> Helpers.serverResourceQtys project flavor server))
                ]

          else
            Element.none
        , Element.wrappedRow [ Element.spacing spacer.px24 ] serverDetailTiles
        ]


serverNameView : View.Types.Context -> Project -> Time.Posix -> Model -> Server -> Element.Element Msg
serverNameView context project currentTime model server =
    let
        name_ =
            VH.resourceName (Just server.osProps.name) server.osProps.uuid

        serverNameViewPlain =
            Element.row
                [ Element.spacing spacer.px8 ]
                [ Text.text Text.ExtraLarge [] name_
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
                        Just <| GotServerNamePendingConfirmation (Just name_)
                    }
                ]

        serverNameViewEdit =
            let
                serverNamePendingConfirmation =
                    model.serverNamePendingConfirmation
                        |> Maybe.withDefault ""

                invalidNameReasons =
                    serverNameValidator
                        (Just context.localization.virtualComputer)
                        serverNamePendingConfirmation

                renderInvalidNameReasons =
                    case invalidNameReasons of
                        Just reasons ->
                            List.map Element.text reasons
                                |> List.map List.singleton
                                |> List.map (Element.paragraph [])
                                |> Element.column
                                    (popoverStyleDefaults context.palette
                                        ++ [ Font.color (SH.toElementColor context.palette.danger.textOnNeutralBG)
                                           , Text.fontSize Text.Small
                                           , Element.alignRight
                                           , Element.moveDown 6
                                           , Element.spacing spacer.px12
                                           , Element.padding spacer.px16
                                           ]
                                    )

                        Nothing ->
                            Element.none

                renderServerNameExists =
                    if
                        Validation.serverNameExists project serverNamePendingConfirmation
                            -- the server's own name currently exists, of course:
                            && server.osProps.name
                            /= Maybe.withDefault "" model.serverNamePendingConfirmation
                    then
                        let
                            message =
                                Element.row []
                                    [ Element.paragraph
                                        [ Element.width (Element.fill |> Element.minimum 300)
                                        , Element.spacing spacer.px8
                                        , Font.regular
                                        , Font.color <| SH.toElementColor <| context.palette.warning.textOnNeutralBG
                                        ]
                                        [ Element.text <|
                                            Validation.resourceNameExistsMessage context.localization.virtualComputer context.localization.unitOfTenancy
                                        ]
                                    ]

                            suggestedNames =
                                Validation.resourceNameSuggestions currentTime project serverNamePendingConfirmation
                                    |> List.filter (\n -> not (Validation.serverNameExists project n))

                            content =
                                Element.column []
                                    (message
                                        :: List.map
                                            (\name ->
                                                Element.row [ Element.paddingEach { edges | top = spacer.px12 } ]
                                                    [ Style.Widgets.Button.default
                                                        context.palette
                                                        { text = name
                                                        , onPress = Just <| GotServerNamePendingConfirmation (Just name)
                                                        }
                                                    ]
                                            )
                                            suggestedNames
                                    )
                        in
                        Style.Widgets.ToggleTip.warningToggleTip
                            context
                            (\serverRenameAlreadyExistsToggleTipId -> SharedMsg <| SharedMsg.TogglePopover serverRenameAlreadyExistsToggleTipId)
                            "serverRenameAlreadyExistsToggleTip"
                            content
                            ST.PositionRightTop

                    else
                        Element.none

                rowStyle =
                    { containerRow =
                        [ Element.spacing spacer.px8
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
                    (Input.text
                        (VH.inputItemAttributes context.palette
                            ++ [ Element.width <| Element.minimum 300 Element.fill ]
                        )
                        { text = model.serverNamePendingConfirmation |> Maybe.withDefault ""
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
                        , onChange = \name -> GotServerNamePendingConfirmation <| Just name
                        , label = Input.labelHidden "Name"
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
                , renderServerNameExists
                ]
    in
    case model.serverNamePendingConfirmation of
        Just _ ->
            serverNameViewEdit

        Nothing ->
            serverNameViewPlain


passphraseVulnWarning : View.Types.Context -> Server -> Element.Element Msg
passphraseVulnWarning context server =
    case server.exoProps.serverOrigin of
        ServerNotFromExo ->
            Element.none

        ServerFromExo serverFromExoProps ->
            if serverFromExoProps.exoServerVersion < 1 then
                Element.el [ Element.paddingXY 0 spacer.px16 ] <|
                    Alert.alert []
                        context.palette
                        { state = Alert.Danger
                        , showIcon = False
                        , showContainer = True
                        , content =
                            Element.paragraph []
                                [ Element.text <|
                                    String.join " "
                                        [ "This"
                                        , context.localization.virtualComputer
                                        , "was created with an older version of Exosphere which left the opportunity for unprivileged processes running on the"
                                        , context.localization.virtualComputer
                                        , "to query the instance metadata service and determine the passphrase for exouser (who is a sudoer). This represents a "
                                        ]
                                , Link.externalLink
                                    context.palette
                                    "https://en.wikipedia.org/wiki/Privilege_escalation"
                                    "privilege escalation vulnerability"
                                , Element.text <|
                                    String.join " "
                                        [ ". If you have used this"
                                        , context.localization.virtualComputer
                                        , "for anything important or sensitive, consider rotating the passphrase for exouser, or building a new"
                                        , context.localization.virtualComputer
                                        , "and moving to that one instead of this one. For more information, see "
                                        ]
                                , Link.externalLink
                                    context.palette
                                    "https://gitlab.com/exosphere/exosphere/issues/284"
                                    "issue #284"
                                , Element.text " on the Exosphere GitLab project."
                                ]
                        }

            else
                Element.none


serverStatus : View.Types.Context -> Project -> Server -> Element.Element Msg
serverStatus context project server =
    let
        details =
            server.osProps.details

        statusBadge =
            VH.serverStatusBadge context.palette server

        lockStatus : OSTypes.ServerLockStatus -> Element.Element Msg
        lockStatus lockStatus_ =
            case lockStatus_ of
                OSTypes.ServerLocked ->
                    Icon.lock (SH.toElementColor context.palette.neutral.icon) 28

                OSTypes.ServerUnlocked ->
                    Icon.lockOpen (SH.toElementColor context.palette.neutral.icon) 28

        verboseStatusToggleTip =
            let
                friendlyOpenstackStatus : OSTypes.ServerStatus -> String
                friendlyOpenstackStatus osStatus =
                    OSTypes.serverStatusToString osStatus

                friendlyPowerState =
                    OSTypes.serverPowerStateToString details.powerState
                        |> String.dropLeft 5

                contents =
                    -- TODO nicer layout here?
                    Element.column [ Element.spacing spacer.px8, Element.padding spacer.px4 ]
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

                toggleTipId =
                    Helpers.String.hyphenate
                        [ "verboseStatusTip"
                        , project.auth.project.uuid
                        , server.osProps.uuid
                        , friendlyOpenstackStatus details.openstackStatus
                        ]
            in
            Style.Widgets.ToggleTip.toggleTip
                context
                popoverMsgMapper
                toggleTipId
                contents
                ST.PositionLeft
    in
    Element.row [ Element.spacing spacer.px16 ]
        [ verboseStatusToggleTip
        , statusBadge
        , lockStatus details.lockStatus
        ]


interactions : View.Types.Context -> Project -> Server -> Time.Posix -> Maybe UserAppProxyHostname -> Element.Element Msg
interactions context project server currentTime tlsReverseProxyHostname =
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
                                [ Text.strong "Status: "
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
                                [ Text.strong "Description: "
                                , Element.text interactionDetails.description
                                ]

                        contents =
                            Element.column
                                [ Element.width (Element.shrink |> Element.minimum 200)
                                , Element.spacing spacer.px12
                                , Element.padding spacer.px4
                                ]
                                [ status
                                , statusReason
                                , description
                                ]

                        toggleTipId =
                            Helpers.String.hyphenate
                                [ "interactionToggleTip"
                                , project.auth.project.uuid
                                , server.osProps.uuid
                                , interactionDetails.name
                                ]
                    in
                    Style.Widgets.ToggleTip.toggleTip
                        context
                        popoverMsgMapper
                        toggleTipId
                        contents
                        ST.PositionRightBottom
            in
            case interactionStatus of
                ITypes.Hidden ->
                    Element.none

                _ ->
                    Element.row
                        [ Element.spacing spacer.px12 ]
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
                                                , right = spacer.px4
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
                                                , SH.toElementColor context.palette.neutral.text.default
                                                )

                                            _ ->
                                                ( SH.toElementColor context.palette.neutral.icon
                                                , SH.toElementColor context.palette.neutral.text.subdued
                                                )
                                in
                                Element.row
                                    [ Font.color fontColor
                                    , Element.spacing spacer.px8
                                    ]
                                    [ Element.el
                                        [ Font.color iconColor ]
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
        |> Element.column [ Element.spacing spacer.px16 ]


serverPassphrase : View.Types.Context -> Model -> Server -> Element.Element Msg
serverPassphrase context model server =
    let
        passphraseShower passphrase =
            Element.column
                [ Element.spacing spacer.px12 ]
                [ case model.passphraseVisibility of
                    PassphraseShown ->
                        copyableText context.palette [] passphrase

                    PassphraseHidden ->
                        Element.none
                , let
                    changeMsg newValue =
                        GotPassphraseVisibility newValue

                    ( buttonText, onPressMsg ) =
                        case model.passphraseVisibility of
                            PassphraseShown ->
                                ( "Hide passphrase"
                                , changeMsg PassphraseHidden
                                )

                            PassphraseHidden ->
                                ( "Show"
                                , changeMsg PassphraseShown
                                )
                  in
                  Widget.textButton
                    (SH.materialStyle context.palette).button
                    { text = buttonText
                    , onPress = Just onPressMsg
                    }
                ]

        passphraseHint =
            case GetterSetters.getServerExouserPassphrase server.osProps.details of
                Just passphrase ->
                    passphraseShower passphrase

                Nothing ->
                    -- TODO factor out this logic used to determine whether to display the charts as well
                    case server.exoProps.serverOrigin of
                        ServerFromExo originProps ->
                            case originProps.exoSetupStatus.data of
                                RDPP.DoHave ( ExoSetupWaiting, _ ) _ ->
                                    Element.text "Not available yet, check in a few minutes."

                                RDPP.DoHave ( ExoSetupRunning, _ ) _ ->
                                    Element.text "Not available yet, check in a few minutes."

                                _ ->
                                    Element.text "Not available"

                        _ ->
                            Element.el
                                [ context.palette.neutral.text.subdued
                                    |> SH.toElementColor
                                    |> Font.color
                                ]
                                (Element.text <|
                                    String.concat
                                        [ "Not available because "
                                        , context.localization.virtualComputer
                                        , " was not created by Exosphere"
                                        ]
                                )
    in
    passphraseHint


serverActionsDropdown : View.Types.Context -> Project -> Model -> Server -> Element.Element Msg
serverActionsDropdown context project model server =
    let
        dropdownId =
            [ "serverActionsDropdown", project.auth.project.uuid, server.osProps.uuid ]
                |> List.intersperse "-"
                |> String.concat

        dropdownContent closeDropdown =
            Element.column [ Element.spacing spacer.px8 ] <|
                List.map
                    (renderServerActionButton context project model server closeDropdown)
                    (ServerActions.getAllowed
                        (Just context.localization.virtualComputer)
                        (Just context.localization.staticRepresentationOfBlockDeviceContents)
                        (Just context.localization.virtualComputerHardwareConfig)
                        server.osProps.details.openstackStatus
                        server.osProps.details.lockStatus
                    )

        dropdownTarget toggleDropdownMsg dropdownIsShown =
            Widget.iconButton
                (SH.materialStyle context.palette).button
                { text = "Actions"
                , icon =
                    Element.row
                        [ Element.spacing spacer.px4 ]
                        [ Element.text "Actions"
                        , Element.el []
                            ((if dropdownIsShown then
                                FeatherIcons.chevronUp

                              else
                                FeatherIcons.chevronDown
                             )
                                |> FeatherIcons.withSize 18
                                |> FeatherIcons.toHtml []
                                |> Element.html
                            )
                        ]
                , onPress = Just toggleDropdownMsg
                }
    in
    case server.exoProps.targetOpenstackStatus of
        Nothing ->
            popover context
                popoverMsgMapper
                { id = dropdownId
                , content = dropdownContent
                , contentStyleAttrs = [ Element.padding spacer.px24 ]
                , position = ST.PositionBottomRight
                , distanceToTarget = Nothing
                , target = dropdownTarget
                , targetStyleAttrs = []
                }

        Just _ ->
            Element.none


serverEventHistory :
    View.Types.Context
    -> Project
    -> Server
    -> Time.Posix
    -> Element.Element Msg
serverEventHistory context project server currentTime =
    case server.events.data of
        RDPP.DoHave serverEvents _ ->
            let
                serverSetupStatus : Maybe ( String, Maybe Time.Posix )
                serverSetupStatus =
                    case server.exoProps.serverOrigin of
                        ServerNotFromExo ->
                            Nothing

                        ServerFromExo exoOriginProps ->
                            case exoOriginProps.exoSetupStatus.data of
                                RDPP.DoHave ( exoSetupStatus, timestamp ) _ ->
                                    Just
                                        ( Types.Server.exoSetupStatusToString exoSetupStatus
                                        , timestamp
                                        )

                                RDPP.DontHave ->
                                    Nothing

                columns : List (Element.Column { action : String, startTime : Time.Posix } Msg)
                columns =
                    [ { header = Text.strong "Action"
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
                    , { header = Text.strong "Time"
                      , width = Element.px 180
                      , view =
                            \event ->
                                let
                                    relativeTime =
                                        DateFormat.Relative.relativeTime currentTime event.startTime

                                    absoluteTime =
                                        let
                                            toggleTipId =
                                                Helpers.String.hyphenate
                                                    [ "serverEventTimeTip"
                                                    , project.auth.project.uuid
                                                    , server.osProps.uuid
                                                    , event.startTime |> Time.posixToMillis |> String.fromInt
                                                    ]
                                        in
                                        Style.Widgets.ToggleTip.toggleTip
                                            context
                                            popoverMsgMapper
                                            toggleTipId
                                            (Element.text (Helpers.Time.humanReadableDateAndTime event.startTime))
                                            ST.PositionBottomRight
                                in
                                Element.row []
                                    [ Element.text relativeTime
                                    , absoluteTime
                                    ]
                      }
                    ]

                serverEventsWithActionAndStartTime =
                    serverEvents
                        |> List.map (\{ action, startTime } -> { action = action, startTime = startTime })

                serverSetupStatusInfo =
                    case serverSetupStatus of
                        Just ( status, Just timestamp ) ->
                            [ { action = "Setup " ++ status
                              , startTime = timestamp
                              }
                            ]

                        Just ( _, Nothing ) ->
                            []

                        Nothing ->
                            []
            in
            Element.table
                [ Element.spacingXY 0 spacer.px8
                , Element.width Element.fill
                ]
                { data =
                    (serverEventsWithActionAndStartTime ++ serverSetupStatusInfo)
                        |> List.sortBy (\{ startTime } -> startTime |> Time.posixToMillis)
                        |> List.reverse
                , columns = columns
                }

        _ ->
            Element.none


renderServerActionButton :
    View.Types.Context
    -> Project
    -> Model
    -> Server
    -> Element.Attribute Msg
    -> ServerActions.ServerAction
    -> Element.Element Msg
renderServerActionButton context project model server closeActionsDropdown serverAction =
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
            renderActionButton context serverAction (Just updateAction) serverAction.name Nothing

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
                    Just <| serverAction.action (GetterSetters.projectIdentifier project) server model.retainFloatingIpsWhenDeleting

                cancelMsg =
                    Just <| GotServerActionNamePendingConfirmation Nothing

                title =
                    confirmationMessage serverAction
            in
            Element.column
                [ Element.spacing spacer.px8 ]
            <|
                List.concat
                    [ [ renderConfirmationButton context serverAction actionMsg cancelMsg title closeActionsDropdown ]
                    , renderKeepFloatingIpCheckbox
                    ]

        ( _, _ ) ->
            -- This is ugly, we should have an explicit custom type for server actions and match on that
            if String.toLower serverAction.name == String.toLower context.localization.staticRepresentationOfBlockDeviceContents then
                -- Overriding button for image, because we just want to navigate to another page
                Element.link [ Element.width Element.fill ]
                    { url =
                        Route.toUrl context.urlPathPrefix
                            (Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
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
                            (Just closeActionsDropdown)
                    }
                -- This is similarly ugly

            else if serverAction.name == "Resize" then
                -- Overriding button for resize, because we just want to navigate to another page
                Element.link [ Element.width Element.fill ]
                    { url =
                        Route.toUrl context.urlPathPrefix
                            (Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                                Route.ServerResize server.osProps.uuid
                            )
                    , label =
                        renderActionButton
                            context
                            serverAction
                            (Just NoOp)
                            (Helpers.String.toTitleCase "Resize")
                            (Just closeActionsDropdown)
                    }

            else
                let
                    actionMsg =
                        Just <| SharedMsg <| serverAction.action (GetterSetters.projectIdentifier project) server model.retainFloatingIpsWhenDeleting

                    title =
                        serverAction.name
                in
                renderActionButton context serverAction actionMsg title (Just closeActionsDropdown)


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


renderActionButton : View.Types.Context -> ServerActions.ServerAction -> Maybe Msg -> String -> Maybe (Element.Attribute Msg) -> Element.Element Msg
renderActionButton context serverAction actionMsg title closeActionsDropdown =
    let
        additionalBtnAttribs =
            case closeActionsDropdown of
                Just closeActionsDropdown_ ->
                    [ closeActionsDropdown_ ]

                Nothing ->
                    []
    in
    Element.row
        [ Element.spacing spacer.px12, Element.width Element.fill ]
        [ Element.text serverAction.description
        , Element.el
            ([ Element.width <| Element.px 100, Element.alignRight ] ++ additionalBtnAttribs)
          <|
            serverActionSelectModButton context
                serverAction.selectMod
                { text = title
                , onPress = actionMsg
                }
        ]


renderConfirmationButton : View.Types.Context -> ServerActions.ServerAction -> Maybe SharedMsg.SharedMsg -> Maybe Msg -> String -> Element.Attribute Msg -> Element.Element Msg
renderConfirmationButton context serverAction actionMsg cancelMsg title closeActionsDropdown =
    Element.row
        [ Element.spacing spacer.px12 ]
        [ Element.text title
        , Element.el
            [ closeActionsDropdown ]
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
            context.windowSize.width - 120

        chartsWidth =
            max 1075 containerWidth

        thirtyMinMillis =
            1000 * 60 * 30

        charts_ : Types.ServerResourceUsage.TimeSeries -> Element.Element Msg
        charts_ timeSeries =
            Element.column
                [ Element.width Element.fill
                , Element.spacing spacer.px8
                ]
                [ Page.ServerResourceUsageAlerts.view context (Tuple.first currentTimeAndZone) timeSeries
                , Page.ServerResourceUsageCharts.view
                    context
                    chartsWidth
                    currentTimeAndZone
                    maybeServerResourceQtys
                    timeSeries
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
                                case exoOriginProps.exoSetupStatus.data of
                                    RDPP.DoHave ( ExoSetupError, _ ) _ ->
                                        Element.none

                                    RDPP.DoHave ( ExoSetupTimeout, _ ) _ ->
                                        Element.none

                                    RDPP.DoHave ( ExoSetupWaiting, _ ) _ ->
                                        Element.none

                                    _ ->
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
                                Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
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
                            Element.column []
                                [ case OpenStack.DnsRecordSet.addressToRecord (project.dnsRecordSets |> RDPP.withDefault []) ipAddress.address of
                                    Just { name } ->
                                        VH.compactKVSubRow
                                            (Helpers.String.toTitleCase context.localization.hostname)
                                            (Element.row [ Element.spacing spacer.px16 ]
                                                [ copyableText context.palette [] name
                                                ]
                                            )

                                    Nothing ->
                                        Element.none
                                , VH.compactKVSubRow
                                    (Helpers.String.toTitleCase context.localization.floatingIpAddress)
                                    (Element.row [ Element.spacing spacer.px16 ]
                                        [ copyableText context.palette [] ipAddress.address
                                        , Widget.textButton
                                            (SH.materialStyle context.palette).button
                                            { text =
                                                "Unassign"
                                            , onPress =
                                                Just <|
                                                    SharedMsg <|
                                                        SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                                                            SharedMsg.RequestUnassignFloatingIp ipAddress.uuid
                                            }
                                        ]
                                    )
                                ]
                        )

        ipButton : Element.Element Msg -> String -> IpInfoLevel -> Element.Element Msg
        ipButton label displayLabel ipMsg =
            Element.row
                [ Element.spacing spacer.px8, Text.fontSize Text.Tiny ]
                [ Input.button
                    [ Border.width 1
                    , Border.rounded 20
                    , Border.color (SH.toElementColor context.palette.neutral.border)
                    , Element.padding spacer.px4
                    ]
                    { onPress = Just <| GotIpInfoLevel ipMsg
                    , label = label
                    }
                , Element.text displayLabel
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
                [ Element.spacing spacer.px8 ]
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
                [ Element.spacing spacer.px8 ]
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
                vdevice : { a | device : String } -> Element.Element msg
                vdevice =
                    \v -> Element.text v.device

                volDetailsButton v =
                    Element.link []
                        { url =
                            Route.toUrl context.urlPathPrefix <|
                                Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
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
                    { name = VH.resourceName v.name v.uuid
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
                      , view = vdevice
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
