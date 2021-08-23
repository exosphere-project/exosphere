module Page.VolumeDetail exposing (Model, Msg(..), init, update, view)

import Element
import Element.Font as Font
import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers as Helpers
import Helpers.String
import OpenStack.Types as OSTypes
import OpenStack.Volumes
import Set
import Style.Helpers as SH
import Style.Widgets.CopyableText exposing (copyableText)
import Style.Widgets.Icon as Icon
import Style.Widgets.IconButton
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))
import View.Helpers as VH
import View.Types
import Widget


type alias Model =
    { volumeUuid : OSTypes.VolumeUuid
    , deleteConfirmations : Set.Set OSTypes.VolumeUuid
    }


init : OSTypes.VolumeUuid -> Model
init volumeId =
    Model volumeId Set.empty


type Msg
    = GotDeleteNeedsConfirm
    | GotDeleteConfirm
    | GotDeleteCancel
    | SharedMsg SharedMsg.SharedMsg
    | RequestDetachVolume


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg project model =
    case msg of
        GotDeleteNeedsConfirm ->
            ( { model
                | deleteConfirmations =
                    Set.insert
                        model.volumeUuid
                        model.deleteConfirmations
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        GotDeleteConfirm ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg project.auth.project.uuid <| SharedMsg.RequestDeleteVolume model.volumeUuid
            )

        GotDeleteCancel ->
            ( { model
                | deleteConfirmations =
                    Set.remove
                        model.volumeUuid
                        model.deleteConfirmations
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )

        RequestDetachVolume ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg project.auth.project.uuid <| SharedMsg.RequestDetachVolume model.volumeUuid
            )


view :
    View.Types.Context
    -> Project
    -> Model
    -> Bool
    -> Element.Element Msg
view context project model showHeading =
    if showHeading then
        Element.column
            (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
            [ Element.el (VH.heading2 context.palette) <|
                Element.text <|
                    String.join " "
                        [ context.localization.blockDevice
                            |> Helpers.String.toTitleCase
                        , "Detail"
                        ]
            , volumeDetail context project model
            ]

    else
        volumeDetail context project model


volumeDetail :
    View.Types.Context
    -> Project
    -> Model
    -> Element.Element Msg
volumeDetail context project model =
    OpenStack.Volumes.volumeLookup project model.volumeUuid
        |> Maybe.withDefault
            (Element.text <|
                String.join " "
                    [ "No"
                    , context.localization.blockDevice
                    , "found"
                    ]
            )
        << Maybe.map
            (\volume ->
                Element.column
                    VH.contentContainer
                    [ VH.compactKVRow "Name:" <| Element.text <| VH.possiblyUntitledResource volume.name context.localization.blockDevice
                    , VH.compactKVRow "Status:" <| Element.text <| OSTypes.volumeStatusToString volume.status
                    , renderAttachments context project volume
                    , VH.compactKVRow "Description:" <|
                        Element.paragraph [ Element.width Element.fill ] <|
                            [ Element.text <| Maybe.withDefault "" volume.description ]
                    , VH.compactKVRow "UUID:" <| copyableText context.palette [] volume.uuid
                    , case volume.imageMetadata of
                        Nothing ->
                            Element.none

                        Just metadata ->
                            VH.compactKVRow
                                (String.concat
                                    [ "Created from "
                                    , context.localization.staticRepresentationOfBlockDeviceContents
                                    , ":"
                                    ]
                                )
                                (Element.text metadata.name)
                    , volumeActionButtons context project model volume
                    ]
            )


renderAttachment : View.Types.Context -> Project -> OSTypes.VolumeAttachment -> Element.Element Msg
renderAttachment context project attachment =
    let
        serverName serverUuid =
            case GetterSetters.serverLookup project serverUuid of
                Just server ->
                    server.osProps.name

                Nothing ->
                    String.join " "
                        [ "(Could not resolve"
                        , context.localization.virtualComputer
                        , "name)"
                        ]
    in
    Element.column
        (VH.exoColumnAttributes ++ [ Element.padding 0 ])
        [ Element.el [ Font.bold ] <| Element.text "Server:"
        , Element.row [ Element.spacing 5 ]
            [ Element.text (serverName attachment.serverUuid)
            , Style.Widgets.IconButton.goToButton context.palette
                (Just <|
                    SharedMsg <|
                        NavigateToView <|
                            SharedMsg.ProjectPage project.auth.project.uuid <|
                                SharedMsg.ServerDetail attachment.serverUuid
                )
            ]
        , Element.el [ Font.bold ] <| Element.text "Device:"
        , Element.text attachment.device
        , Element.el [ Font.bold ] <| Element.text "Mount point*:"
        , Helpers.volDeviceToMountpoint attachment.device |> Maybe.withDefault "" |> Element.text
        , Element.el [ Font.size 11 ] <|
            Element.text <|
                String.join " "
                    [ "*"
                    , context.localization.blockDevice
                        |> Helpers.String.toTitleCase
                    , "will only be automatically formatted/mounted on operating"
                    ]
        , Element.el [ Font.size 11 ] <| Element.text "systems which use systemd 236 or newer (e.g. Ubuntu 18.04 and CentOS 8)"
        ]


renderAttachments : View.Types.Context -> Project -> OSTypes.Volume -> Element.Element Msg
renderAttachments context project volume =
    case List.length volume.attachments of
        0 ->
            Element.none

        _ ->
            VH.compactKVRow "Attached to:" <|
                Element.column
                    (VH.exoColumnAttributes ++ [ Element.padding 0 ])
                <|
                    List.map (renderAttachment context project) volume.attachments


volumeActionButtons :
    View.Types.Context
    -> Project
    -> Model
    -> OSTypes.Volume
    -> Element.Element Msg
volumeActionButtons context project model volume =
    let
        volDetachDeleteWarning =
            if Helpers.isBootVol Nothing volume then
                Element.text <|
                    String.concat
                        [ "This "
                        , context.localization.blockDevice
                        , " backs a "
                        , context.localization.virtualComputer
                        , "; it cannot be detached or deleted until the "
                        , context.localization.virtualComputer
                        , " is deleted."
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
                    Widget.textButton
                        (SH.materialStyle context.palette).button
                        { text = "Attach"
                        , onPress =
                            Just <|
                                SharedMsg <|
                                    NavigateToView <|
                                        SharedMsg.ProjectPage project.auth.project.uuid <|
                                            SharedMsg.VolumeAttach Nothing (Just volume.uuid)
                        }

                OSTypes.InUse ->
                    if Helpers.isBootVol Nothing volume then
                        Widget.textButton
                            (SH.materialStyle context.palette).button
                            { text = "Detach"
                            , onPress = Nothing
                            }

                    else
                        Widget.textButton
                            (SH.materialStyle context.palette).button
                            { text = "Detach"
                            , onPress =
                                Just RequestDetachVolume
                            }

                _ ->
                    Element.none

        confirmationNeeded =
            Set.member volume.uuid model.deleteConfirmations

        deleteButton =
            case ( volume.status, confirmationNeeded ) of
                ( OSTypes.Deleting, _ ) ->
                    Widget.circularProgressIndicator (SH.materialStyle context.palette).progressIndicator Nothing

                ( _, True ) ->
                    Element.row [ Element.spacing 10 ]
                        [ Element.text "Confirm delete?"
                        , Widget.iconButton
                            (SH.materialStyle context.palette).dangerButton
                            { icon = Icon.remove (SH.toElementColor context.palette.on.error) 16
                            , text = "Delete"
                            , onPress =
                                Just <| GotDeleteConfirm
                            }
                        , Widget.textButton
                            (SH.materialStyle context.palette).button
                            { text = "Cancel"
                            , onPress =
                                Just <| GotDeleteCancel
                            }
                        ]

                ( _, False ) ->
                    if volume.status == OSTypes.InUse then
                        Widget.iconButton
                            (SH.materialStyle context.palette).button
                            { icon = Icon.remove (SH.toElementColor context.palette.on.error) 16
                            , text = "Delete"
                            , onPress = Nothing
                            }

                    else
                        Widget.iconButton
                            (SH.materialStyle context.palette).dangerButton
                            { icon = Icon.remove (SH.toElementColor context.palette.on.error) 16
                            , text = "Delete"
                            , onPress =
                                Just <| GotDeleteNeedsConfirm
                            }
    in
    Element.column (Element.width Element.fill :: VH.exoColumnAttributes)
        [ volDetachDeleteWarning
        , Element.row [ Element.width Element.fill, Element.spacing 10 ]
            [ attachDetachButton
            , Element.el [ Element.alignRight ] deleteButton
            ]
        ]
