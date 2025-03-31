module Page.VolumeDetail exposing (Model, Msg(..), init, update, view)

import DateFormat.Relative
import Element
import Element.Border as Border
import Element.Font as Font
import FeatherIcons
import FormatNumber.Locales exposing (Decimals(..))
import Helpers.Formatting exposing (Unit(..), humanNumber)
import Helpers.GetterSetters as GetterSetters
import Helpers.String exposing (removeEmptiness)
import Helpers.Time
import OpenStack.Types as OSTypes exposing (Volume)
import Route
import Style.Helpers as SH
import Style.Types as ST
import Style.Widgets.Button as Button
import Style.Widgets.Card
import Style.Widgets.CopyableText exposing (copyableText)
import Style.Widgets.DeleteButton exposing (deletePopconfirm)
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
    | SharedMsg SharedMsg.SharedMsg
    | NoOp


init : OSTypes.VolumeUuid -> Model
init volumeId =
    Model volumeId Nothing


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg project model =
    case msg of
        GotDeleteNeedsConfirm volumeUuid ->
            ( { model | deletePendingConfirmation = volumeUuid }, Cmd.none, SharedMsg.NoOp )

        GotDeleteConfirm volumeUuid ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <| SharedMsg.RequestDeleteVolume volumeUuid
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


renderConfirmation : View.Types.Context -> Maybe Msg -> Maybe Msg -> String -> List (Element.Attribute Msg) -> Element.Element Msg
renderConfirmation context actionMsg cancelMsg title closeActionsAttributes =
    Element.row
        [ Element.spacing spacer.px12, Element.width (Element.fill |> Element.minimum 280) ]
        [ Element.text title
        , Element.el
            (Element.alignRight :: closeActionsAttributes)
          <|
            Button.button
                Button.Danger
                context.palette
                { text = "Yes"
                , onPress = actionMsg
                }
        , Element.el
            [ Element.alignRight ]
          <|
            Button.button
                Button.Secondary
                context.palette
                { text = "No"
                , onPress = cancelMsg
                }
        ]


renderDeleteAction : View.Types.Context -> Model -> Maybe Msg -> Maybe (Element.Attribute Msg) -> Element.Element Msg
renderDeleteAction context model actionMsg closeActionsDropdown =
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
            renderConfirmation
                context
                actionMsg
                (Just <|
                    GotDeleteNeedsConfirm Nothing
                )
                "Are you sure?"
                additionalBtnAttribs

        Nothing ->
            Element.row
                [ Element.spacing spacer.px12, Element.width (Element.fill |> Element.minimum 280) ]
                [ Element.text ("Destroy " ++ context.localization.blockDevice ++ "?")
                , Element.el
                    [ Element.alignRight ]
                  <|
                    Button.button
                        Button.Danger
                        context.palette
                        { text = "Delete"
                        , onPress = Just <| GotDeleteNeedsConfirm <| Just model.volumeUuid
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


header : String -> Element.Element msg
header text =
    Element.el [ Font.heavy ] <| Element.text text


serverNameNotFound : View.Types.Context -> String
serverNameNotFound context =
    String.join " "
        [ "(Could not resolve"
        , context.localization.virtualComputer
        , "name)"
        ]


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
                            Element.row []
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

                                            maybeServerShelved =
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
                                        , case ( volume.status, maybeServerShelved ) of
                                            ( OSTypes.Reserved, True ) ->
                                                Style.Widgets.ToggleTip.toggleTip
                                                    context
                                                    (SharedMsg << SharedMsg.TogglePopover)
                                                    ("volumeReservedTip-" ++ volume.uuid)
                                                    (Text.body <| "Unshelve the attached " ++ context.localization.virtualComputer ++ " to interact with this " ++ context.localization.blockDevice ++ ".")
                                                    ST.PositionBottom

                                            _ ->
                                                Element.none
                                        ]
                                       )
                                )

                        Nothing ->
                            Element.text "(none)"

                _ ->
                    Element.text "(none)"

        _ ->
            Element.table
                [ Element.spacing spacer.px16
                ]
                { data = volume.attachments
                , columns =
                    [ { header = header (context.localization.virtualComputer |> Helpers.String.toTitleCase)
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
                                Link.link
                                    context.palette
                                    (Route.toUrl context.urlPathPrefix <|
                                        Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                                            Route.ServerDetail item.serverUuid
                                    )
                                    serverName
                      }
                    , { header = header "Device"
                      , width = Element.shrink
                      , view =
                            \item ->
                                let
                                    device =
                                        item.device
                                in
                                Text.mono <| device
                      }
                    , { header =
                            Element.row []
                                [ header "Mount Point"
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
                                scrollableCell [] <| Text.mono <| mountPoint
                      }
                    ]
                }


render : View.Types.Context -> Project -> ( Time.Posix, Time.Zone ) -> Model -> Volume -> Element.Element Msg
render context project ( currentTime, _ ) model volume =
    let
        isBootVolume =
            GetterSetters.isBootVolume Nothing volume

        bootVolumeTag =
            if isBootVolume then
                tag context.palette <| "boot " ++ context.localization.blockDevice

            else
                Element.none

        whenCreated =
            let
                timeDistanceStr =
                    DateFormat.Relative.relativeTime currentTime volume.createdAt

                createdTimeText =
                    let
                        createdTimeFormatted =
                            Helpers.Time.humanReadableDateAndTime volume.createdAt
                    in
                    Element.text ("Created on: " ++ createdTimeFormatted)

                toggleTipContents =
                    Element.column [] [ createdTimeText ]
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
                        , volume.uuid
                        ]
                    )
                    toggleTipContents
                    ST.PositionBottomLeft
                ]

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

        attachments =
            attachmentsTable context project volume
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
            , bootVolumeTag
            , Element.row [ Element.alignRight, Text.fontSize Text.Body, Font.regular, Element.spacing spacer.px16 ]
                [ volumeStatus context volume
                , volumeActionsDropdown context project model volume
                ]
            ]
        , tile
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
                { ago = ( "created", whenCreated )
                , creator = creator
                , size = sizeString
                , image = imageString
                }
            ]
        , tile
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
        ]





volumeActionButtons :
    View.Types.Context
    -> Project
    -> Model
    -> OSTypes.Volume
    -> Element.Element Msg
volumeActionButtons context project model volume =
    let
        volDetachDeleteWarning =
            if GetterSetters.isBootVolume Nothing volume then
                Element.text <|
                    String.join " "
                        [ "This"
                        , context.localization.blockDevice
                        , "backs"
                        , Helpers.String.indefiniteArticle context.localization.virtualComputer
                        , context.localization.virtualComputer ++ ";"
                        , "it cannot be detached or deleted until the"
                        , context.localization.virtualComputer
                        , "is deleted."
                        ]

            else if volume.status == OSTypes.InUse then
                Element.text <|
                    String.join " "
                        [ "This"
                        , context.localization.blockDevice
                        , "must be detached before it can be deleted."
                        ]

            else
                Element.none

        attachDetachButton =
            case volume.status of
                OSTypes.Available ->
                    Element.link []
                        { url =
                            Route.toUrl context.urlPathPrefix
                                (Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                                    Route.VolumeAttach Nothing (Just volume.uuid)
                                )
                        , label =
                            Button.default
                                context.palette
                                { text = "Attach"
                                , onPress = Just NoOp
                                }
                        }

                OSTypes.InUse ->
                    if GetterSetters.isBootVolume Nothing volume then
                        Button.default
                            context.palette
                            { text = "Detach"
                            , onPress = Nothing
                            }

                    else
                        let
                            detachMsg =
                                SharedMsg <|
                                    SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                                        SharedMsg.RequestDetachVolume model.volumeUuid

                            detachButton : msg -> Bool -> Element.Element msg
                            detachButton togglePopconfirm _ =
                                Button.default
                                    context.palette
                                    { text = "Detach"
                                    , onPress = Just togglePopconfirm
                                    }

                            detachPopconfirmId =
                                Helpers.String.hyphenate [ "volumeDetailDetachPopconfirm", project.auth.project.uuid, volume.uuid ]
                        in
                        deletePopconfirm context
                            (SharedMsg << SharedMsg.TogglePopover)
                            detachPopconfirmId
                            { confirmation =
                                Element.column [ Element.spacing spacer.px8 ]
                                    [ Element.text <|
                                        "Detaching "
                                            ++ Helpers.String.indefiniteArticle context.localization.blockDevice
                                            ++ " "
                                            ++ context.localization.blockDevice
                                            ++ " while it is in use may cause data loss."
                                    , Element.text
                                        "Make sure to close any open files before detaching."
                                    ]
                            , buttonText = Just "Detach"
                            , onConfirm = Just detachMsg
                            , onCancel = Just NoOp
                            }
                            ST.PositionBottom
                            detachButton

                _ ->
                    Element.none
    in
    Style.Widgets.Card.exoCard
        context.palette
        (Element.column
            [ Element.padding spacer.px8
            , Element.spacing spacer.px16
            , Element.width Element.fill
            ]
            [ volDetachDeleteWarning
            , Element.row
                [ Element.alignRight
                , Element.spacing spacer.px12
                ]
                [ attachDetachButton
                ]
            ]
        )
