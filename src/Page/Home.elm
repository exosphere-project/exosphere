module Page.Home exposing (Model, Msg, init, update, view)

import Dict
import Element
import Element.Font as Font
import Helpers.Url as UrlHelpers
import Style.Helpers as SH
import Types.HelperTypes as HelperTypes
import Types.Project as Project
import Types.SharedModel as SharedModel
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


view : View.Types.Context -> SharedModel.SharedModel -> Model -> Element.Element Msg
view context sharedModel model =
    let
        cloudConfigs : List HelperTypes.CloudSpecificConfig
        cloudConfigs =
            sharedModel.projects
                |> List.map .endpoints
                |> List.map .keystone
                |> List.map UrlHelpers.hostnameFromUrl
                |> List.map (\hostname -> Dict.get hostname context.cloudSpecificConfigs)
                |> List.filterMap identity
    in
    Element.column [ Element.padding 10, Element.spacing 10 ]
        [ Element.el (VH.heading2 context.palette) <| Element.text "Clouds"
        , Element.wrappedRow
            [ Element.padding 10, Element.spacing 10 ]
            (List.map (renderCloud context) cloudConfigs)
        ]



-- TODO need renderCloud AND renderProject


renderCloud : View.Types.Context -> HelperTypes.CloudSpecificConfig -> Element.Element Msg
renderCloud context cloudSpecificConfig =
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
                Element.text cloudSpecificConfig.friendlyName
            , cloudSpecificConfig.friendlySubName
                |> Maybe.map Element.text
                |> Maybe.withDefault Element.none
            ]
        , Element.column
            [ Element.padding 10
            , Element.spacing 10
            , Element.centerX
            ]
            []
        ]
