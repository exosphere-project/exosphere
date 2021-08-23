module Page.VolumeList exposing (Model, Msg, init, update, view)

import Element
import FeatherIcons
import Helpers.String
import OpenStack.Types as OSTypes
import Page.QuotaUsage
import Page.VolumeDetail
import Set
import Style.Widgets.Card as ExoCard
import Types.Project exposing (Project)
import Types.SharedMsg as SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))
import View.Helpers as VH
import View.Types


type alias Model =
    { expandedVols : Set.Set OSTypes.VolumeUuid
    , deleteConfirmations : Set.Set OSTypes.VolumeUuid
    }


type Msg
    = GotExpandCard OSTypes.VolumeUuid Bool
    | VolumeDetailMsg OSTypes.VolumeUuid Page.VolumeDetail.Msg
    | NavigateToView SharedMsg.NavigableView


init : Model
init =
    Model Set.empty Set.empty


update : Msg -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg project model =
    let
        navigateToView view_ =
            ( model, Cmd.none, SharedMsg.NavigateToView view_ )
    in
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
            -- TODO handle this more like AllResources Page?
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
                    , SharedMsg.ProjectMsg project.auth.project.uuid <| SharedMsg.RequestDeleteVolume uuid
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

                Page.VolumeDetail.RequestDetachVolume ->
                    ( model
                    , Cmd.none
                    , SharedMsg.ProjectMsg project.auth.project.uuid <| SharedMsg.RequestDetachVolume uuid
                    )

        NavigateToView view_ ->
            navigateToView view_


view : View.Types.Context -> Bool -> Project -> Model -> Element.Element Msg
view context showHeading project model =
    let
        renderSuccessCase : List OSTypes.Volume -> Element.Element Msg
        renderSuccessCase volumes_ =
            Element.column
                (VH.exoColumnAttributes
                    ++ [ Element.paddingXY 10 0
                       , Element.spacing 15
                       , Element.width Element.fill
                       ]
                )
                (List.map
                    (renderVolumeCard context project model)
                    volumes_
                )
    in
    Element.column
        [ Element.spacing 20, Element.width Element.fill ]
        [ if showHeading then
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
            [ Page.QuotaUsage.view context (Page.QuotaUsage.Volume project.volumeQuota)
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
            { volumeUuid = volume.uuid, deleteConfirmations = model.deleteConfirmations }
            False
            |> Element.map (VolumeDetailMsg volume.uuid)
        )
