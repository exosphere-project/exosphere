module Page.ServerResize exposing (Model, Msg, init, update, view)

import Element
import Helpers.GetterSetters as GetterSetters
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.String
import OpenStack.Types as OSTypes
import Route
import Style.Widgets.Button as Button
import Style.Widgets.Spacer exposing (spacer)
import Style.Widgets.Text as Text
import Types.Project exposing (Project)
import Types.Server exposing (Server)
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg as SharedMsg exposing (ProjectSpecificMsgConstructor(..), ServerSpecificMsgConstructor(..))
import View.Helpers as VH
import View.Types


type alias Model =
    { serverUuid : OSTypes.ServerUuid
    , flavorId : Maybe OSTypes.FlavorId
    }


type Msg
    = GotFlavorId OSTypes.FlavorId
    | GotSubmit OSTypes.FlavorId
    | SharedMsg SharedMsg.SharedMsg


init : OSTypes.ServerUuid -> Model
init serverUuid =
    Model serverUuid Nothing


update : Msg -> SharedModel -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg { viewContext } project model =
    case msg of
        GotFlavorId flavorId ->
            let
                newModel =
                    { model | flavorId = Just flavorId }
            in
            ( newModel
            , Cmd.none
            , SharedMsg.NoOp
            )

        GotSubmit flavorId ->
            ( model
            , Route.pushUrl viewContext <|
                Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                    Route.ServerDetail model.serverUuid
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                ServerMsg model.serverUuid <|
                    RequestResizeServer flavorId
            )

        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )


view : View.Types.Context -> Project -> Model -> Element.Element Msg
view context project model =
    VH.renderRDPP
        context
        project.computeQuota
        context.localization.maxResourcesPerProject
        (view_ context project model)


view_ : View.Types.Context -> Project -> Model -> OSTypes.ComputeQuota -> Element.Element Msg
view_ context project model computeQuota =
    let
        restrictFlavorIds =
            GetterSetters.serverLookup project model.serverUuid
                |> Maybe.map restrictFlavorIds_

        restrictFlavorIds_ : Server -> List OSTypes.FlavorId
        restrictFlavorIds_ server =
            let
                minRootDiskSize =
                    case GetterSetters.getBootVolume (RDPP.withDefault [] project.volumes) model.serverUuid of
                        Just _ ->
                            -- Server is volume-backed, so root disk size of flavor does not matter
                            0

                        Nothing ->
                            case GetterSetters.flavorLookup project server.osProps.details.flavorId of
                                Just flavor ->
                                    flavor.disk_root

                                Nothing ->
                                    0
            in
            project.flavors
                |> RDPP.withDefault []
                |> List.filter (\flavor -> flavor.disk_root >= minRootDiskSize)
                |> List.map .id

        currentFlavorId =
            GetterSetters.serverLookup project model.serverUuid |> Maybe.map (\server -> server.osProps.details.flavorId)
    in
    Element.column VH.formContainer
        [ Text.heading context.palette
            []
            Element.none
            (String.join
                " "
                [ "Resize"
                , context.localization.virtualComputer
                    |> Helpers.String.toTitleCase
                , case currentFlavorId of
                    Just flavorId ->
                        "(Current Size: " ++ (GetterSetters.flavorLookup project flavorId |> Maybe.map .name |> Maybe.withDefault "") ++ ")"

                    Nothing ->
                        ""
                ]
            )
        , Element.column [ Element.spacing spacer.px16 ]
            [ VH.flavorPicker context
                project
                restrictFlavorIds
                (Just ("This flavor has a root disk smaller than your current " ++ context.localization.virtualComputer))
                computeQuota
                (\flavorGroupTipId -> SharedMsg <| SharedMsg.TogglePopover flavorGroupTipId)
                (Helpers.String.hyphenate [ "serverResizeFlavorGroupTip", project.auth.project.uuid ])
                currentFlavorId
                model.flavorId
                GotFlavorId
            , Element.row [ Element.width Element.fill ]
                [ Element.el [ Element.alignRight ]
                    (Button.primary
                        context.palette
                        { text = "Resize"
                        , onPress =
                            if model.flavorId == currentFlavorId then
                                Nothing

                            else
                                model.flavorId |> Maybe.map GotSubmit
                        }
                    )
                ]
            ]
        ]
