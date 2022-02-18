module Page.VolumeList exposing (Model, Msg, init, update, view)

import Element
import Element.Font as Font
import FeatherIcons
import Helpers.GetterSetters as GetterSetters
import Helpers.String
import OpenStack.Types as OSTypes
import Page.QuotaUsage
import Page.VolumeDetail
import Route
import Set
import Style.Helpers as SH
import Style.Widgets.Card as ExoCard
import Style.Widgets.DataList as DataList
import Time
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))
import View.Helpers as VH
import View.Types
import Widget


type alias Model =
    { showHeading : Bool
    , expandedVols : Set.Set OSTypes.VolumeUuid
    , deleteConfirmations : Set.Set OSTypes.VolumeUuid
    , dataListModel : DataList.Model
    }


type Msg
    = GotExpandCard OSTypes.VolumeUuid Bool
    | VolumeDetailMsg OSTypes.VolumeUuid Page.VolumeDetail.Msg
    | DataListMsg DataList.Msg


init : Bool -> Model
init showHeading =
    Model showHeading Set.empty Set.empty (DataList.init <| DataList.getDefaultFilterOptions [])


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg project model =
    case msg of
        GotExpandCard uuid bool ->
            ( { model
                | expandedVols =
                    if bool then
                        Set.insert uuid model.expandedVols

                    else
                        Set.remove uuid model.expandedVols
              }
            , Cmd.none
            , NoOp
            )

        VolumeDetailMsg uuid subMsg ->
            -- This is an experiment
            case subMsg of
                Page.VolumeDetail.GotDeleteNeedsConfirm ->
                    ( { model
                        | deleteConfirmations =
                            Set.insert
                                uuid
                                model.deleteConfirmations
                      }
                    , Cmd.none
                    , SharedMsg.NoOp
                    )

                Page.VolumeDetail.GotDeleteConfirm ->
                    ( model
                    , Cmd.none
                    , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <| SharedMsg.RequestDeleteVolume uuid
                    )

                Page.VolumeDetail.GotDeleteCancel ->
                    ( { model
                        | deleteConfirmations =
                            Set.remove
                                uuid
                                model.deleteConfirmations
                      }
                    , Cmd.none
                    , SharedMsg.NoOp
                    )

                Page.VolumeDetail.SharedMsg sharedMsg ->
                    ( model, Cmd.none, sharedMsg )

                Page.VolumeDetail.NoOp ->
                    ( model, Cmd.none, SharedMsg.NoOp )

        DataListMsg dataListMsg ->
            ( { model
                | dataListModel =
                    DataList.update dataListMsg model.dataListModel
              }
            , Cmd.none
            , SharedMsg.NoOp
            )


view : View.Types.Context -> Project -> Model -> Element.Element Msg
view context project model =
    let
        renderSuccessCase : List OSTypes.Volume -> Element.Element Msg
        renderSuccessCase volumes_ =
            -- TODO: use datalist instead
            DataList.view
                model.dataListModel
                DataListMsg
                context.palette
                []
                (volumeView model context project)
                (volumeRecords volumes_)
                []
                []
    in
    Element.column
        [ Element.spacing 20, Element.width Element.fill ]
        [ if model.showHeading then
            Element.row (VH.heading2 context.palette ++ [ Element.spacing 15 ])
                [ FeatherIcons.hardDrive |> FeatherIcons.toHtml [] |> Element.html |> Element.el []
                , Element.text
                    (context.localization.blockDevice
                        |> Helpers.String.pluralize
                        |> Helpers.String.toTitleCase
                    )
                ]

          else
            Element.none
        , Element.column VH.contentContainer
            [ Page.QuotaUsage.view context Page.QuotaUsage.Full (Page.QuotaUsage.Volume project.volumeQuota)
            , VH.renderWebData
                context
                project.volumes
                (Helpers.String.pluralize context.localization.blockDevice)
                renderSuccessCase
            ]
        ]


renderVolumeCard : View.Types.Context -> Project -> Model -> OSTypes.Volume -> Element.Element Msg
renderVolumeCard context project model volume =
    ExoCard.expandoCard
        context.palette
        (Set.member volume.uuid model.expandedVols)
        (GotExpandCard volume.uuid)
        (VH.possiblyUntitledResource volume.name context.localization.blockDevice
            |> Element.text
        )
        (Element.text <| String.fromInt volume.size ++ " GB")
    <|
        (Page.VolumeDetail.view
            context
            project
            { showHeading = False, volumeUuid = volume.uuid, deleteConfirmations = model.deleteConfirmations }
            |> Element.map (VolumeDetailMsg volume.uuid)
        )


type alias VolumeRecord =
    DataList.DataRecord
        { name : OSTypes.VolumeName
        , size : OSTypes.VolumeSize
        , status : OSTypes.VolumeStatus
        , attachment : Maybe OSTypes.VolumeAttachment

        -- TODO: figure how to fetch
        -- , creationTime : Time.Posix
        -- , creator : String
        }


volumeRecords : List OSTypes.Volume -> List VolumeRecord
volumeRecords volumes =
    List.map
        (\volume ->
            { id = volume.uuid
            , selectable = False
            , name = volume.name
            , size = volume.size
            , status = volume.status
            , attachment = List.head volume.attachments
            }
        )
        volumes


volumeView :
    Model
    -> View.Types.Context
    -> Project
    -> VolumeRecord
    -> Element.Element msg
volumeView model context project volumeRecord =
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
                        [ Font.size 18
                        , Font.color (SH.toElementColor context.palette.primary)
                        ]
                        (Element.text volumeRecord.name)
                }

        volumeAttachment =
            let
                serverName serverUuid =
                    case GetterSetters.serverLookup project serverUuid of
                        Just server ->
                            server.osProps.name

                        Nothing ->
                            "unresolvable " ++ context.localization.virtualComputer ++ " name"
            in
            Element.row [ Element.alignRight ]
                [ Element.text "Attached to "
                , case volumeRecord.attachment of
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
                        Element.text <| "No " ++ context.localization.virtualComputer
                ]

        volumeActions =
            -- TODO: Add no action, detach, attach + delete for 3 possible cases
            Element.text "Action"
    in
    Element.column
        [ Element.spacing 12
        , Element.width Element.fill
        , Font.color (SH.toElementColorWithOpacity context.palette.on.background 0.62)
        ]
        [ Element.row [ Element.spacing 10, Element.width Element.fill ]
            [ volumeLink
            , volumeAttachment
            ]
        , Element.row
            [ Element.spacing 8
            , Element.width Element.fill
            ]
            [ Element.el [] (Element.text <| String.fromInt volumeRecord.size ++ " GB")

            -- , Element.text "·"
            -- , Element.paragraph []
            --     [ Element.text "created "
            --     , Element.el [ Font.color (SH.toElementColor context.palette.on.background) ]
            --         (Element.text <|
            --             DateFormat.Relative.relativeTime currentTime
            --                 serverRecord.creationTime
            --         )
            --     , Element.text " by "
            --     , Element.el [ Font.color (SH.toElementColor context.palette.on.background) ]
            --         (Element.text serverRecord.creator)
            --     ]
            , Element.el [ Element.alignRight ]
                volumeActions
            ]
        ]
