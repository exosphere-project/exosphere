module Page.VolumeList exposing (Model, Msg, init, update, view)

import Element
import Element.Font as Font
import FeatherIcons as Icons
import FormatNumber.Locales exposing (Decimals(..))
import Helpers.Formatting exposing (Unit(..), humanNumber)
import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers exposing (lookupUsername)
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.ResourceList exposing (creationTimeFilterOptions, creatorFilterOptions, listItemColumnAttribs, onCreationTimeFilter)
import Helpers.String
import OpenStack.Types as OSTypes
import OpenStack.VolumeSnapshots as VS
import Page.QuotaUsage
import Route
import Set
import Style.Helpers as SH
import Style.Types as ST
import Style.Widgets.Button as Button
import Style.Widgets.DataList as DataList
import Style.Widgets.DeleteButton exposing (deleteIconButton, deletePopconfirm)
import Style.Widgets.HumanTime exposing (relativeTimeElement)
import Style.Widgets.Icon exposing (featherIcon)
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import Time
import Types.HelperTypes exposing (Uuid)
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types


type alias Model =
    { showHeading : Bool
    , dataListModel : DataList.Model
    }


type Msg
    = DetachVolume OSTypes.VolumeUuid
    | GotDeleteSnapshotConfirm Uuid
    | GotDeleteVolumeConfirm OSTypes.VolumeUuid
    | SharedMsg SharedMsg.SharedMsg
    | DataListMsg DataList.Msg
    | NoOp


init : Project -> Bool -> Model
init project showHeading =
    Model showHeading
        (DataList.init <| DataList.getDefaultFilterOptions (filters project (Time.millisToPosix 0)))


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg project model =
    let
        projectId =
            GetterSetters.projectIdentifier project
    in
    case msg of
        DetachVolume volumeUuid ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg projectId <|
                SharedMsg.RequestDetachVolume volumeUuid
            )

        GotDeleteSnapshotConfirm snapshotUuid ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg projectId <|
                SharedMsg.RequestDeleteVolumeSnapshot snapshotUuid
            )

        GotDeleteVolumeConfirm volumeUuid ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg projectId <|
                SharedMsg.RequestDeleteVolume volumeUuid
            )

        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )

        DataListMsg dataListMsg ->
            ( { model
                | dataListModel =
                    DataList.update dataListMsg model.dataListModel
              }
            , Cmd.none
            , SharedMsg.NoOp
            )

        NoOp ->
            ( model
            , Cmd.none
            , SharedMsg.NoOp
            )


view : View.Types.Context -> Project -> Time.Posix -> Model -> Element.Element Msg
view context project currentTime model =
    let
        renderSuccessCase : ( List OSTypes.Volume, List VS.VolumeSnapshot ) -> Element.Element Msg
        renderSuccessCase ( volumes_, snapshots ) =
            DataList.view
                context.localization.blockDevice
                model.dataListModel
                DataListMsg
                context
                []
                (volumeView context project currentTime)
                (volumeRecords project ( volumes_, snapshots ))
                []
                (Just
                    { filters = filters project currentTime
                    , dropdownMsgMapper =
                        \dropdownId ->
                            SharedMsg <| SharedMsg.TogglePopover dropdownId
                    }
                )
                Nothing
    in
    Element.column
        (VH.contentContainer ++ [ Element.spacing spacer.px32 ])
        [ if model.showHeading then
            Text.heading context.palette
                []
                (featherIcon [] Icons.hardDrive)
                (context.localization.blockDevice
                    |> Helpers.String.pluralize
                    |> Helpers.String.toTitleCase
                )

          else
            Element.none
        , Page.QuotaUsage.view context Page.QuotaUsage.Full (Page.QuotaUsage.Volume ( project.volumeQuota, project.volumeSnapshots ))
        , VH.renderRDPP
            context
            (RDPP.map2 Tuple.pair project.volumes project.volumeSnapshots)
            (Helpers.String.pluralize context.localization.blockDevice)
            renderSuccessCase
        ]


type alias VolumeRecord =
    DataList.DataRecord
        { volume : OSTypes.Volume
        , snapshots : List VS.VolumeSnapshot
        , creator : String
        }


volumeRecords : Project -> ( List OSTypes.Volume, List VS.VolumeSnapshot ) -> List VolumeRecord
volumeRecords project ( volumes, snapshots ) =
    let
        creator volume =
            lookupUsername project volume.userUuid
                |> Maybe.withDefault "unknown user"

        isSnapshotOfVolume : OSTypes.Volume -> VS.VolumeSnapshot -> Bool
        isSnapshotOfVolume { uuid } { volumeId } =
            uuid == volumeId

        volumeSnapshots volume =
            List.filter (\snapshot -> isSnapshotOfVolume volume snapshot) snapshots
    in
    List.map
        (\volume ->
            { id = volume.uuid
            , selectable = False
            , volume = volume
            , snapshots = volumeSnapshots volume
            , creator = creator volume
            }
        )
        volumes


volumeView :
    View.Types.Context
    -> Project
    -> Time.Posix
    -> VolumeRecord
    -> Element.Element Msg
volumeView context project currentTime volumeRecord =
    let
        volumeLink =
            Element.link []
                { url =
                    Route.toUrl context.urlPathPrefix
                        (Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                            Route.VolumeDetail volumeRecord.id
                        )
                , label =
                    Element.el
                        (Text.typographyAttrs Text.Emphasized ++ [ Font.color (SH.toElementColor context.palette.primary) ])
                        (Element.text <|
                            VH.extendedResourceName
                                volumeRecord.volume.name
                                volumeRecord.volume.uuid
                                context.localization.blockDevice
                        )
                }

        volumeAttachment =
            let
                serverName serverUuid =
                    case GetterSetters.serverLookup project serverUuid of
                        Just server ->
                            VH.resourceName (Just server.osProps.name) server.osProps.uuid

                        Nothing ->
                            "unresolvable " ++ context.localization.virtualComputer ++ " name"
            in
            Element.row [ Element.alignRight ]
                [ Element.text "Attached to "
                , case List.head volumeRecord.volume.attachments of
                    Just volumeAttachment_ ->
                        Element.link []
                            { url =
                                Route.toUrl context.urlPathPrefix
                                    (Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                                        Route.ServerDetail volumeAttachment_.serverUuid
                                    )
                            , label =
                                Element.el
                                    [ Font.color (SH.toElementColor context.palette.primary)
                                    ]
                                    (Element.text <| serverName volumeAttachment_.serverUuid)
                            }

                    Nothing ->
                        Element.el
                            [ Font.color
                                (SH.toElementColor context.palette.neutral.text.default)
                            ]
                            (Element.text <| "no " ++ context.localization.virtualComputer)
                ]

        volumeActions =
            case volumeRecord.volume.status of
                OSTypes.Detaching ->
                    Element.el [ Font.italic ] (Element.text "Detaching ...")

                OSTypes.Deleting ->
                    Element.el [ Font.italic ] (Element.text "Deleting ...")

                OSTypes.InUse ->
                    if GetterSetters.isBootVolume Nothing volumeRecord.volume then
                        -- Volume cannot be deleted or detached
                        Element.row [ Element.height <| Element.minimum 32 Element.fill ]
                            [ Element.text "as "
                            , Element.el
                                [ Font.color (SH.toElementColor context.palette.neutral.text.default) ]
                                (Element.text <| "boot " ++ context.localization.blockDevice)
                            ]

                    else
                        -- Volume can only be detached
                        Button.default
                            context.palette
                            { text = "Detach"
                            , onPress =
                                Just <| DetachVolume volumeRecord.id
                            }

                OSTypes.Available ->
                    -- Volume can be either deleted or attached
                    let
                        deleteVolumeBtn togglePopconfirmMsg _ =
                            deleteIconButton
                                context.palette
                                False
                                ("Delete " ++ context.localization.blockDevice)
                                (Just togglePopconfirmMsg)

                        deletePopconfirmId =
                            Helpers.String.hyphenate
                                [ "volumeListDeletePopconfirm"
                                , project.auth.project.uuid
                                , volumeRecord.id
                                ]
                    in
                    Element.row [ Element.spacing spacer.px12 ]
                        [ deletePopconfirm context
                            (SharedMsg << SharedMsg.TogglePopover)
                            deletePopconfirmId
                            { confirmation =
                                Element.text <|
                                    "Are you sure you want to delete this "
                                        ++ context.localization.blockDevice
                                        ++ "?"
                            , buttonText = Nothing
                            , onCancel = Just NoOp
                            , onConfirm = Just <| GotDeleteVolumeConfirm volumeRecord.id
                            }
                            ST.PositionBottomRight
                            deleteVolumeBtn
                        , Element.link []
                            { url =
                                Route.toUrl context.urlPathPrefix
                                    (Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                                        Route.VolumeAttach Nothing (Just volumeRecord.id)
                                    )
                            , label =
                                Button.default
                                    context.palette
                                    { text = "Attach"
                                    , onPress = Just NoOp
                                    }
                            }
                        ]

                _ ->
                    Element.none

        sizeString bytes =
            let
                locale =
                    context.locale

                ( sizeDisplay, sizeLabel ) =
                    humanNumber { locale | decimals = Exact 0 } GibiBytes bytes
            in
            sizeDisplay ++ " " ++ sizeLabel

        snapshotRows =
            case volumeRecord.snapshots of
                [] ->
                    []

                snapshots ->
                    let
                        neutralColor =
                            SH.toElementColor context.palette.neutral.text.default

                        renderCreationTime : VS.VolumeSnapshot -> Element.Element msg
                        renderCreationTime { createdAt } =
                            Element.row []
                                [ Element.text "created "
                                , relativeTimeElement currentTime createdAt
                                ]
                    in
                    [ Element.row [ Element.spacing spacer.px8, Font.color neutralColor ] [ Element.text "Snapshots" ]
                    , Element.table
                        [ Element.spacing spacer.px12 ]
                        { data = snapshots
                        , columns =
                            [ { header = Element.none
                              , width = Element.shrink
                              , view = \snapshot -> Element.text (sizeString snapshot.sizeInGiB)
                              }
                            , { header = Element.none
                              , width = Element.shrink
                              , view =
                                    \{ name, description, uuid } ->
                                        let
                                            renderedName =
                                                VH.resourceName name uuid
                                        in
                                        Element.column
                                            [ Element.spacing spacer.px4 ]
                                            [ Element.el [ Font.color neutralColor ] (Element.text renderedName)
                                            , description
                                                |> Maybe.map Element.text
                                                |> Maybe.withDefault Element.none
                                            ]
                              }
                            , { header = Element.none
                              , width = Element.fill
                              , view = renderCreationTime
                              }
                            , { header = Element.none
                              , width = Element.shrink
                              , view =
                                    \snapshot ->
                                        if not <| List.member snapshot.status [ VS.Deleted, VS.Deleting ] then
                                            let
                                                deviceLabel =
                                                    context.localization.blockDevice ++ " snapshot"

                                                deletePopconfirmId =
                                                    Helpers.String.hyphenate
                                                        [ "volumeListDeleteSnapshotPopconfirm"
                                                        , project.auth.project.uuid
                                                        , snapshot.uuid
                                                        ]

                                                deleteButton =
                                                    deletePopconfirm context
                                                        (SharedMsg << SharedMsg.TogglePopover)
                                                        deletePopconfirmId
                                                        { confirmation =
                                                            Element.text <|
                                                                "Are you sure you want to delete this "
                                                                    ++ deviceLabel
                                                                    ++ "?"
                                                        , buttonText = Nothing
                                                        , onCancel = Just NoOp
                                                        , onConfirm = Just <| GotDeleteSnapshotConfirm snapshot.uuid
                                                        }
                                                        ST.PositionBottomRight
                                                        (\msg _ ->
                                                            deleteIconButton context.palette
                                                                False
                                                                ("Delete " ++ deviceLabel)
                                                                (Just msg)
                                                        )
                                            in
                                            Element.row
                                                [ Element.spacing spacer.px12 ]
                                                [ deleteButton ]

                                        else
                                            let
                                                label =
                                                    if VS.isTransitioning snapshot then
                                                        VS.statusToString snapshot.status ++ " ..."

                                                    else
                                                        VS.statusToString snapshot.status
                                            in
                                            Element.el [ Font.italic ] (Element.text label)
                              }
                            ]
                        }
                    ]
    in
    Element.column
        (listItemColumnAttribs context.palette)
        (Element.column
            (listItemColumnAttribs context.palette)
            [ Element.row [ Element.spacing spacer.px12, Element.width Element.fill ]
                [ volumeLink
                , volumeAttachment
                ]
            , Element.row
                [ Element.spacing spacer.px8, Element.width Element.fill ]
                [ Element.el [] (Element.text (sizeString volumeRecord.volume.size))
                , Element.text "·"
                , let
                    accentColor =
                        context.palette.neutral.text.default |> SH.toElementColor

                    accented =
                        Element.el [ Font.color accentColor ]
                  in
                  Element.paragraph []
                    [ Element.text "created "
                    , accented (relativeTimeElement currentTime volumeRecord.volume.createdAt)
                    , Element.text " by "
                    , accented (Element.text volumeRecord.creator)
                    ]
                , Element.el [ Element.alignRight ]
                    volumeActions
                ]
            ]
            :: snapshotRows
        )


filters :
    Project
    -> Time.Posix
    -> List (DataList.Filter { record | volume : OSTypes.Volume, creator : String })
filters project currentTime =
    [ { id = "creator"
      , label = "Creator"
      , chipPrefix = "Created by "
      , filterOptions =
            \records -> creatorFilterOptions project (List.map .creator records)
      , filterTypeAndDefaultValue =
            DataList.MultiselectOption <| Set.singleton project.auth.user.name
      , onFilter =
            \optionValue volume ->
                volume.creator == optionValue
      }
    , { id = "creationTime"
      , label = "Created within"
      , chipPrefix = "Created within "
      , filterOptions =
            \_ -> creationTimeFilterOptions
      , filterTypeAndDefaultValue =
            DataList.UniselectOption DataList.UniselectNoChoice
      , onFilter =
            \optionValue volume ->
                onCreationTimeFilter optionValue volume.volume.createdAt currentTime
      }
    ]
