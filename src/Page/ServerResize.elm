module Page.ServerResize exposing (Model, Msg, init, update, view)

import Element
import Helpers.GetterSetters as GetterSetters
import Helpers.String
import OpenStack.Types as OSTypes
import RemoteData
import Route
import Style.Helpers as SH
import Types.Project exposing (Project)
import Types.Server exposing (Server)
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg as SharedMsg exposing (ProjectSpecificMsgConstructor(..), ServerSpecificMsgConstructor(..))
import View.Helpers as VH
import View.Types
import Widget


type alias Model =
    { serverUuid : OSTypes.ServerUuid
    , flavorId : Maybe OSTypes.FlavorId
    }


type Msg
    = GotFlavorId OSTypes.FlavorId
    | GotSubmit OSTypes.FlavorId


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


view : View.Types.Context -> Project -> Model -> Element.Element Msg
view context project model =
    VH.renderWebData
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
                currentFlavorRootDiskSize =
                    case GetterSetters.flavorLookup project server.osProps.details.flavorId of
                        Just flavor ->
                            flavor.disk_root

                        Nothing ->
                            0

                minRootDiskSize =
                    case GetterSetters.getBootVolume (RemoteData.withDefault [] project.volumes) model.serverUuid of
                        Just _ ->
                            -- Server is volume-backed, so root disk size of flavor does not matter
                            0

                        Nothing ->
                            currentFlavorRootDiskSize
            in
            project.flavors
                |> List.filter (\flavor -> flavor.id /= server.osProps.details.flavorId)
                |> List.filter (\flavor -> flavor.disk_root >= minRootDiskSize)
                |> List.map .id
    in
    Element.column (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ Element.el
            (VH.heading2 context.palette)
            (Element.text <|
                String.join
                    " "
                    [ "Resize"
                    , context.localization.virtualComputer
                        |> Helpers.String.toTitleCase
                    ]
            )
        , Element.column VH.formContainer
            [ VH.flavorPicker context
                project
                restrictFlavorIds
                computeQuota
                (Maybe.withDefault "" model.flavorId)
                GotFlavorId
            , Element.row [ Element.width Element.fill ]
                [ Element.el [ Element.alignRight ]
                    (Widget.textButton
                        (SH.materialStyle context.palette).primaryButton
                        { text = "Resize"
                        , onPress = model.flavorId |> Maybe.map GotSubmit
                        }
                    )
                ]
            ]
        ]
