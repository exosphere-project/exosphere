module Page.ServerCreateImage exposing (Model, Msg, init, update, view)

import Element
import Element.Input as Input
import Helpers.GetterSetters as GetterSetters
import Helpers.String
import OpenStack.Types as OSTypes
import Route
import Style.Widgets.Button as Button
import Style.Widgets.Text as Text
import Types.Project exposing (Project)
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg as SharedMsg exposing (ProjectSpecificMsgConstructor(..), ServerSpecificMsgConstructor(..))
import View.Helpers as VH
import View.Types


type alias Model =
    { serverUuid : OSTypes.ServerUuid
    , imageName : String
    }


type Msg
    = GotImageName String
    | GotSubmit


init : OSTypes.ServerUuid -> Maybe String -> Model
init serverUuid maybeImageName =
    Model serverUuid (Maybe.withDefault "" maybeImageName)


update : Msg -> SharedModel -> Project -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg { viewContext } project model =
    case msg of
        GotImageName imageName ->
            let
                newModel =
                    { model | imageName = imageName }
            in
            ( newModel
            , Route.replaceUrl viewContext <|
                Route.ProjectRoute (GetterSetters.projectIdentifier project) <|
                    Route.ServerCreateImage newModel.serverUuid (Just newModel.imageName)
            , SharedMsg.NoOp
            )

        GotSubmit ->
            ( model
            , Cmd.none
            , SharedMsg.ProjectMsg (GetterSetters.projectIdentifier project) <|
                ServerMsg model.serverUuid <|
                    RequestCreateServerImage model.imageName
            )


view : View.Types.Context -> Model -> Element.Element Msg
view context model =
    Element.column VH.formContainer
        [ Text.heading context.palette
            []
            Element.none
            (String.join
                " "
                [ String.join " "
                    [ "Create"
                    , context.localization.staticRepresentationOfBlockDeviceContents
                        |> Helpers.String.toTitleCase
                    , "from"
                    ]
                , context.localization.virtualComputer
                    |> Helpers.String.toTitleCase
                ]
            )
        , Element.column [ Element.spacing 16, Element.width Element.fill ]
            [ Input.text
                [ Element.spacing 12 ]
                { text = model.imageName
                , placeholder = Nothing
                , onChange = GotImageName
                , label =
                    Input.labelAbove []
                        (Element.text <|
                            String.join " "
                                [ context.localization.staticRepresentationOfBlockDeviceContents
                                    |> Helpers.String.toTitleCase
                                , "name"
                                ]
                        )
                }
            , Element.row [ Element.width Element.fill ]
                [ Element.el [ Element.alignRight ]
                    (Button.primary
                        context.palette
                        { text = "Create"
                        , onPress = Just GotSubmit
                        }
                    )
                ]
            ]
        ]
