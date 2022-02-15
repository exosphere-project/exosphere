module Page.ServerResize exposing (Model, Msg, init, update, view)

import Element
import Element.Input as Input
import Helpers.GetterSetters as GetterSetters
import Helpers.String
import OpenStack.Types as OSTypes
import Route
import Style.Helpers as SH
import Types.Project exposing (Project)
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
    | GotSubmit


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

        GotSubmit ->
            -- TODO navigate to instance details page
            ( model
            , Cmd.none
              -- TODO fire a msg to resize server
            , SharedMsg.NoOp
              {-
                 , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                     ServerMsg model.serverUuid <|
                         RequestCreateServerImage model.imageName
              -}
            )


view : View.Types.Context -> Model -> Element.Element Msg
view context model =
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
            [ -- TODO flavor picker
              Element.none
            , Element.row [ Element.width Element.fill ]
                [ Element.el [ Element.alignRight ]
                    (Widget.textButton
                        (SH.materialStyle context.palette).primaryButton
                        { text = "Create"
                        , onPress = Just GotSubmit
                        }
                    )
                ]
            ]
        ]
