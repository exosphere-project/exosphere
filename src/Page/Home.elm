module Page.Home exposing (Model, Msg, init, update, view)

import Dict
import Element
import Element.Font as Font
import Helpers.GetterSetters as GetterSetters
import Helpers.Url as UrlHelpers
import Set
import Style.Helpers as SH
import Types.HelperTypes as HelperTypes
import Types.Project as Project
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types
import Widget


type alias Model =
    ()


type Msg
    = NoOp


init : Model
init =
    ()


update : Msg -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg model =
    ( model, Cmd.none, SharedMsg.NoOp )



-- TODO show, as separate cards, any unscoped providers that the user needs to choose projects for


view : View.Types.Context -> SharedModel -> Model -> Element.Element Msg
view context sharedModel model =
    let
        uniqueKeystoneHostnames : List HelperTypes.KeystoneHostname
        uniqueKeystoneHostnames =
            sharedModel.projects
                |> List.map .endpoints
                |> List.map .keystone
                |> List.map UrlHelpers.hostnameFromUrl
                -- convert list to set and then back to remove duplicate values
                |> Set.fromList
                |> Set.toList
    in
    Element.column [ Element.padding 10, Element.spacing 10 ]
        [ Element.el (VH.heading2 context.palette) <| Element.text "Clouds"
        , Element.wrappedRow
            [ Element.padding 10, Element.spacing 10 ]
            (List.map (renderCloud context sharedModel) uniqueKeystoneHostnames)
        ]



-- TODO need renderCloud AND renderProject


renderCloud : View.Types.Context -> SharedModel -> HelperTypes.KeystoneHostname -> Element.Element Msg
renderCloud context sharedModel keystoneHostname =
    let
        projects =
            GetterSetters.projectsForCloud sharedModel keystoneHostname

        maybeCloudSpecificConfig =
            Dict.get keystoneHostname context.cloudSpecificConfigs

        friendlyCloudName =
            maybeCloudSpecificConfig
                |> Maybe.map .friendlyName
                |> Maybe.withDefault keystoneHostname
    in
    Widget.column
        (SH.materialStyle context.palette).cardColumn
        [ Element.column
            [ Element.centerX
            , Element.paddingXY 10 15
            , Element.spacing 15
            ]
          <|
            [ Element.el
                [ Element.centerX
                , Font.bold
                ]
              <|
                Element.text friendlyCloudName
            , maybeCloudSpecificConfig
                |> Maybe.andThen .friendlySubName
                |> Maybe.map Element.text
                |> Maybe.withDefault Element.none
            ]
        , Element.column
            [ Element.padding 10
            , Element.spacing 10
            , Element.centerX
            ]
            (projects
                |> List.map (\p -> p.auth.project.name)
                |> List.map Element.text
            )
        ]
