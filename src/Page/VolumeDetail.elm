module Page.VolumeDetail exposing (Model, Msg(..), init, update, view)

import Element
import Element.Font as Font
import FeatherIcons
import FormatNumber.Locales exposing (Decimals(..))
import Helpers.Formatting exposing (Unit(..), humanNumber)
import Helpers.GetterSetters as GetterSetters
import Helpers.String exposing (removeEmptiness)
import OpenStack.HelperTypes exposing (Uuid)
import OpenStack.Types as OSTypes exposing (Volume)
import OpenStack.VolumeSnapshots exposing (VolumeSnapshot)
import Route
import Style.Helpers as SH
import Style.Types as ST
import Style.Widgets.Button as Button
import Style.Widgets.CopyableText exposing (copyableText)
import Style.Widgets.Grid exposing (scrollableCell)
import Style.Widgets.Icon as Icon
import Style.Widgets.Link as Link
import Style.Widgets.Popover.Popover exposing (popover)
import Style.Widgets.Popover.Types exposing (PopoverId)
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.StatusBadge as StatusBadge
import Style.Widgets.Tag exposing (tag)
import Style.Widgets.Text as Text
import Style.Widgets.ToggleTip
import Time
import Types.Project exposing (Project)
import Types.Server exposing (ExoFeature(..))
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types
import Widget


type alias Model =
    { volumeUuid : OSTypes.VolumeUuid
    , deletePendingConfirmation : Maybe OSTypes.VolumeUuid
    }


type Msg
    = GotDeleteNeedsConfirm (Maybe OSTypes.VolumeUuid)
    | GotDeleteConfirm OSTypes.VolumeUuid
    | GotDeleteSnapshotConfirm Uuid
    | GotDetachVolumeConfirm OSTypes.VolumeUuid
    | SharedMsg SharedMsg.SharedMsg
    | NoOp


init : OSTypes.VolumeUuid -> Model
init volumeId =
    Model volumeId Nothing


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg project model =
    let
        projectId =
            GetterSetters.projectIdentifier project
    in
    case msg of
        GotDeleteNeedsConfirm volumeUuid ->
            ( { model | deletePendingConfirmation = volumeUuid }, Cmd.none, SharedMsg.NoOp )

        GotDeleteConfirm volumeUuid ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <| SharedMsg.RequestDeleteVolume volumeUuid
            )

        GotDeleteSnapshotConfirm snapshotUuid ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg projectId <|
                SharedMsg.RequestDeleteVolumeSnapshot snapshotUuid
            )

        GotDetachVolumeConfirm volumeUuid ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg projectId <| SharedMsg.RequestDetachVolume volumeUuid
            )

        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )

        NoOp ->
            ( model, Cmd.none, SharedMsg.NoOp )


view : View.Types.Context -> Project -> ( Time.Posix, Time.Zone ) -> Model -> Element.Element Msg
view context project currentTimeAndZone model =
    VH.renderRDPP context
        project.volumes
        context.localization.blockDevice
        (\_ ->
            -- Attempt to look up the resource; if found, call render.
            case GetterSetters.volumeLookup project model.volumeUuid of
                Just volume ->
                    render context project currentTimeAndZone model volume

                Nothing ->
                    Element.text <|
                        String.join " "
                            [ "No"
                            , context.localization.blockDevice
                            , "found"
                            ]
        )


volumeNameView : Volume -> Element.Element Msg
volumeNameView volume =
    let
        name_ =
            VH.resourceName volume.name volume.uuid
    in
    Element.row
        [ Element.spacing spacer.px8 ]
        [ Text.text Text.ExtraLarge [] name_ ]


volumeStatus : View.Types.Context -> Volume -> Element.Element Msg
volumeStatus context volume =
    let
        statusBadge =
            VH.volumeStatusBadge context.palette StatusBadge.Normal volume
    in
    Element.row [ Element.spacing spacer.px16 ]
        [ statusBadge
        ]


renderDeleteAction : View.Types.Context -> Model -> Volume -> Maybe Msg -> Maybe (Element.Attribute Msg) -> Element.Element Msg
renderDeleteAction context model volume actionMsg closeActionsDropdown =
    case model.deletePendingConfirmation of
        Just _ ->
            let
                additionalBtnAttribs =
                    case closeActionsDropdown of
                        Just closeActionsDropdown_ ->
                            [ closeActionsDropdown_ ]

                        Nothing ->
                            []
            in
            VH.renderConfirmation
                context
                actionMsg
                (Just <|
                    GotDeleteNeedsConfirm Nothing
                )
                "Are you sure?"
                additionalBtnAttribs

        Nothing ->
            let
                isDeleteDisabled =
                    GetterSetters.isVolumeCurrentlyBackingServer Nothing volume

                warning =
                    case VH.deleteVolumeWarning context volume of
                        Just warning_ ->
                            Text.body <| warning_

                        Nothing ->
                            Text.body <| "Destroy " ++ context.localization.blockDevice ++ "?"
            in
            Element.row
                [ Element.spacing spacer.px12, Element.width (Element.fill |> Element.minimum 280) ]
                [ warning
                , Element.el
                    [ Element.alignRight ]
                  <|
                    Button.button
                        Button.Danger
                        context.palette
                        { text = "Delete"
                        , onPress =
                            if isDeleteDisabled then
                                Nothing

                            else
                                Just <| GotDeleteNeedsConfirm <| Just model.volumeUuid
                        }
                ]


popoverMsgMapper : PopoverId -> Msg
popoverMsgMapper popoverId =
    SharedMsg <| SharedMsg.TogglePopover popoverId


volumeActionsDropdown : View.Types.Context -> Project -> Model -> Volume -> Element.Element Msg
volumeActionsDropdown context project model volume =
    let
        dropdownId =
            [ "volumeActionsDropdown", project.auth.project.uuid, volume.uuid ]
                |> List.intersperse "-"
                |> String.concat

        dropdownContent closeDropdown =
            Element.column [ Element.spacing spacer.px8 ] <|
                [ renderDeleteAction context
                    model
                    volume
                    (Just <| GotDeleteConfirm volume.uuid)
                    (Just closeDropdown)
                ]

        dropdownTarget toggleDropdownMsg dropdownIsShown =
            Widget.iconButton
                (SH.materialStyle context.palette).button
                { text = "Actions"
                , icon =
                    Element.row
                        [ Element.spacing spacer.px4 ]
                        [ Element.text "Actions"
                        , Icon.sizedFeatherIcon 18 <|
                            if dropdownIsShown then
                                FeatherIcons.chevronUp

                            else
                                FeatherIcons.chevronDown
                        ]
                , onPress = Just toggleDropdownMsg
                }
    in
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


createdAgoByWhomEtc :
    View.Types.Context
    ->
        { ago : ( String, Element.Element msg )
        , creator : String
        , size : String
        , image : Maybe String
        }
    -> Element.Element msg
createdAgoByWhomEtc context { ago, creator, size, image } =
    let
        ( agoWord, agoContents ) =
            ago

        subduedText =
            Font.color (context.palette.neutral.text.subdued |> SH.toElementColor)
    in
    Element.wrappedRow
        [ Element.width Element.fill, Element.spaceEvenly ]
    <|
        [ Element.row [ Element.padding spacer.px8 ]
            [ Element.el [ subduedText ] (Element.text <| agoWord ++ " ")
            , agoContents
            , Element.el [ subduedText ] (Element.text <| " by ")
            , Element.text creator
            ]
        , Element.row [ Element.padding spacer.px8 ]
            [ Element.el [ subduedText ] (Element.text <| "size ")
            , Element.text size
            ]
        , case image of
            Just img ->
                Element.row [ Element.padding spacer.px8 ]
                    [ Element.el [ subduedText ] (Element.text <| "created from " ++ context.localization.staticRepresentationOfBlockDeviceContents ++ " ")
                    , Element.text <| img
                    ]

            Nothing ->
                Element.none
        ]


serverNameNotFound : View.Types.Context -> String
serverNameNotFound context =
    String.join " "
        [ "(Could not resolve"
        , context.localization.virtualComputer
        , "name)"
        ]


centerRow : Element.Element msg -> Element.Element msg
centerRow =
    Element.el [ Element.centerY ]


detachButton : View.Types.Context -> Project -> Volume -> Element.Element Msg
detachButton context project volume =
    let
        isBootVolume =
            GetterSetters.isVolumeCurrentlyBackingServer Nothing volume

        bootVolumeTag =
            if isBootVolume then
                tag context.palette <| "boot " ++ context.localization.blockDevice

            else
                Element.none

        controls =
            Element.row [ Element.spacing spacer.px12 ]
                [ bootVolumeTag
                , VH.detachVolumeButton
                    context
                    project
                    (SharedMsg << SharedMsg.TogglePopover)
                    "volumeDetailDetachPopconfirm"
                    volume
                    (Just <| GotDetachVolumeConfirm volume.uuid)
                    (Just NoOp)
                ]
    in
    case volume.status of
        OSTypes.Detaching ->
            centerRow <| Text.body <| "Detaching..."

        OSTypes.InUse ->
            controls

        OSTypes.Reserved ->
            controls

        _ ->
            Element.none


attachmentsTable : View.Types.Context -> Project -> Volume -> Element.Element Msg
attachmentsTable context project volume =
    case List.length volume.attachments of
        0 ->
            case volume.status of
                OSTypes.Reserved ->
                    let
                        maybeServerUuid =
                            -- Reserved volumes don't necessarily know their attachments but servers do.
                            List.head <| GetterSetters.getServerUuidsByVolume project volume.uuid
                    in
                    case maybeServerUuid of
                        Just serverUuid ->
                            Element.row [ Element.width Element.fill ]
                                (Element.text "Reserved for "
                                    :: (let
                                            maybeServer =
                                                GetterSetters.serverLookup project serverUuid

                                            serverName =
                                                case maybeServer of
                                                    Just server ->
                                                        VH.resourceName (Just server.osProps.name) server.osProps.uuid

                                                    Nothing ->
                                                        serverNameNotFound context

                                            isServerShelved =
                                                maybeServer
                                                    |> Maybe.map (\s -> s.osProps.details.openstackStatus)
                                                    |> Maybe.map (\status -> [ OSTypes.ServerShelved, OSTypes.ServerShelvedOffloaded ] |> List.member status)
                                                    |> Maybe.withDefault False
                                        in
                                        [ Link.link
                                            context.palette
                                            (Route.toUrl context.urlPathPrefix <|
                                                Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                                                    Route.ServerDetail serverUuid
                                            )
                                            serverName
                                        , if isServerShelved && GetterSetters.isBootableVolume volume then
                                            Style.Widgets.ToggleTip.toggleTip
                                                context
                                                (SharedMsg << SharedMsg.TogglePopover)
                                                ("volumeReservedTip-" ++ volume.uuid)
                                                (Text.body <| "Unshelve the attached " ++ context.localization.virtualComputer ++ " to interact with this " ++ context.localization.blockDevice ++ ".")
                                                ST.PositionBottom

                                          else
                                            -- If the volume was attached when the server was shelved,
                                            -- it would be reserved but detachable.
                                            Element.row
                                                [ Element.width Element.fill ]
                                                [ Element.el [ Element.width Element.fill ] Element.none
                                                , detachButton context project volume
                                                ]
                                        ]
                                       )
                                )

                        Nothing ->
                            Element.text "(none)"

                OSTypes.Available ->
                    Element.row
                        [ Element.width Element.fill
                        , Element.spaceEvenly
                        ]
                        [ Text.body
                            (String.join " "
                                [ "This"
                                , context.localization.blockDevice
                                , "is not attached to any"
                                , context.localization.virtualComputer ++ "."
                                ]
                            )
                        , Element.link []
                            { url =
                                Route.toUrl context.urlPathPrefix
                                    (Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                                        Route.VolumeAttach Nothing (Just volume.uuid)
                                    )
                            , label =
                                Button.primary
                                    context.palette
                                    { text = "Attach"
                                    , onPress = Just NoOp
                                    }
                            }
                        ]

                _ ->
                    Element.text "(none)"

        _ ->
            Element.table
                [ Element.spacing spacer.px16
                ]
                { data = volume.attachments
                , columns =
                    [ { header = VH.tableHeader (context.localization.virtualComputer |> Helpers.String.toTitleCase)
                      , width = Element.shrink
                      , view =
                            \item ->
                                let
                                    maybeServer =
                                        GetterSetters.serverLookup project item.serverUuid

                                    serverName =
                                        case maybeServer of
                                            Just { osProps } ->
                                                VH.resourceName (Just osProps.name) osProps.uuid

                                            Nothing ->
                                                serverNameNotFound context
                                in
                                centerRow <|
                                    Link.link
                                        context.palette
                                        (Route.toUrl context.urlPathPrefix <|
                                            Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                                                Route.ServerDetail item.serverUuid
                                        )
                                        serverName
                      }
                    , { header = VH.tableHeader "Device"
                      , width = Element.shrink
                      , view =
                            \item ->
                                let
                                    device =
                                        item.device |> Maybe.withDefault "-"
                                in
                                centerRow <| Text.mono <| device
                      }
                    , { header =
                            Element.row []
                                [ VH.tableHeader "Mount Point"
                                , Style.Widgets.ToggleTip.toggleTip
                                    context
                                    (SharedMsg << SharedMsg.TogglePopover)
                                    ("mountPointTip-" ++ volume.uuid)
                                    (Text.p [ Text.fontSize Text.Tiny ]
                                        [ Element.text <|
                                            String.join " "
                                                [ context.localization.blockDevice
                                                    |> Helpers.String.pluralize
                                                    |> Helpers.String.toTitleCase
                                                , "will only be automatically formatted/mounted on operating systems which use systemd 236 or newer (e.g. Ubuntu 18.04 or newer, Rocky Linux, or AlmaLinux)."
                                                ]
                                        ]
                                    )
                                    ST.PositionBottom
                                ]
                      , width = Element.fill
                      , view =
                            \item ->
                                let
                                    maybeServer =
                                        GetterSetters.serverLookup project item.serverUuid

                                    mountPoint =
                                        maybeServer
                                            |> Maybe.andThen
                                                (\server ->
                                                    if GetterSetters.serverSupportsFeature NamedMountpoints server then
                                                        volume.name |> Maybe.andThen GetterSetters.volNameToMountpoint

                                                    else
                                                        GetterSetters.volDeviceToMountpoint item.device
                                                )
                                            |> Maybe.withDefault ""
                                in
                                centerRow <| scrollableCell [ Element.width Element.fill ] <| Text.mono <| mountPoint
                      }
                    , { header = VH.tableHeader ""
                      , width = Element.shrink
                      , view =
                            \_ ->
                                detachButton context project volume
                      }
                    ]
                }


snapshotsTable : View.Types.Context -> Project -> Time.Posix -> List VolumeSnapshot -> Element.Element Msg
snapshotsTable context project currentTime snapshots =
    case List.length snapshots of
        0 ->
            Element.text "(none)"

        _ ->
            Element.table
                [ Element.spacing spacer.px16
                ]
                { data = snapshots
                , columns =
                    [ { header = VH.tableHeader "Name"
                      , width = Element.fill |> Element.maximum 360
                      , view =
                            \item ->
                                centerRow <| scrollableCell [ Element.width Element.fill ] <| Text.body <| VH.resourceName item.name item.uuid
                      }
                    , { header = VH.tableHeader "Size"
                      , width = Element.shrink
                      , view =
                            \item ->
                                let
                                    locale =
                                        context.locale

                                    ( sizeDisplay, sizeLabel ) =
                                        humanNumber { locale | decimals = Exact 0 } GibiBytes item.sizeInGiB
                                in
                                centerRow <| Text.body <| sizeDisplay ++ " " ++ sizeLabel
                      }
                    , { header = VH.tableHeader "Created"
                      , width = Element.shrink
                      , view =
                            \item ->
                                centerRow <| VH.whenCreated context project popoverMsgMapper currentTime item
                      }
                    , { header = VH.tableHeader "Description"
                      , width = Element.fill
                      , view =
                            \item ->
                                centerRow <| scrollableCell [ Element.width Element.fill ] <| Text.body <| Maybe.withDefault "-" <| removeEmptiness item.description
                      }
                    , { header = VH.tableHeader ""
                      , width = Element.shrink
                      , view =
                            \item ->
                                centerRow <|
                                    VH.deleteVolumeSnapshotIconButton
                                        context
                                        project
                                        popoverMsgMapper
                                        "volumeSnapshotDeletePopconfirm"
                                        item
                                        (Just <| GotDeleteSnapshotConfirm item.uuid)
                                        (Just NoOp)
                      }
                    ]
                }


render : View.Types.Context -> Project -> ( Time.Posix, Time.Zone ) -> Model -> Volume -> Element.Element Msg
render context project ( currentTime, _ ) model volume =
    let
        creator =
            if volume.userUuid == project.auth.user.uuid then
                "me"

            else
                "another user"

        sizeString =
            let
                locale =
                    context.locale

                ( sizeDisplay, sizeLabel ) =
                    -- Volume size, in GiBs.
                    humanNumber { locale | decimals = Exact 0 } GibiBytes volume.size
            in
            sizeDisplay ++ " " ++ sizeLabel

        imageString =
            volume.imageMetadata
                |> Maybe.map
                    (\imageMetadata ->
                        VH.resourceName (Just imageMetadata.name) imageMetadata.uuid
                    )

        description =
            case removeEmptiness volume.description of
                Just str ->
                    Element.row [ Element.padding spacer.px8 ]
                        [ Element.paragraph [ Element.width Element.fill ] <|
                            [ Element.text <| str ]
                        ]

                Nothing ->
                    Element.none

        attachments =
            attachmentsTable context project volume

        snapshotWord =
            "snapshot"

        snapshots =
            VH.renderRDPP
                context
                project.volumeSnapshots
                (snapshotWord |> Helpers.String.pluralize)
                (List.filter (\snapshot -> GetterSetters.isSnapshotOfVolume volume snapshot)
                    >> snapshotsTable context project currentTime
                )
    in
    Element.column [ Element.spacing spacer.px24, Element.width Element.fill ]
        [ Element.row (Text.headingStyleAttrs context.palette)
            [ FeatherIcons.hardDrive |> FeatherIcons.toHtml [] |> Element.html |> Element.el []
            , Text.text Text.ExtraLarge
                []
                (context.localization.blockDevice
                    |> Helpers.String.toTitleCase
                )
            , volumeNameView volume
            , Element.row [ Element.alignRight, Text.fontSize Text.Body, Font.regular, Element.spacing spacer.px16 ]
                [ volumeStatus context volume
                , volumeActionsDropdown context project model volume
                ]
            ]
        , VH.tile
            context
            [ FeatherIcons.database |> FeatherIcons.toHtml [] |> Element.html |> Element.el []
            , Element.text "Info"
            , Element.el
                [ Text.fontSize Text.Tiny
                , Font.color (SH.toElementColor context.palette.neutral.text.subdued)
                , Element.alignBottom
                ]
                (copyableText context.palette
                    [ Element.width (Element.shrink |> Element.minimum 240) ]
                    volume.uuid
                )
            ]
            [ description
            , createdAgoByWhomEtc
                context
                { ago = ( "created", VH.whenCreated context project popoverMsgMapper currentTime volume )
                , creator = creator
                , size = sizeString
                , image = imageString
                }
            ]
        , VH.tile
            context
            [ FeatherIcons.server
                |> FeatherIcons.toHtml []
                |> Element.html
                |> Element.el []
            , "attachment"
                |> Helpers.String.pluralize
                |> Helpers.String.toTitleCase
                |> Element.text
            ]
            [ attachments
            ]
        , VH.tile
            context
            [ FeatherIcons.archive
                |> FeatherIcons.toHtml []
                |> Element.html
                |> Element.el []
            , snapshotWord
                |> Helpers.String.pluralize
                |> Helpers.String.toTitleCase
                |> Element.text
            ]
            [ snapshots
            ]
        ]
