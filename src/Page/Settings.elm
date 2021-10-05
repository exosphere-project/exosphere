module Page.Settings exposing (Model, Msg(..), init, update, view)

import Element
import Element.Input as Input
import FeatherIcons
import Style.Types
import Style.Widgets.ToggleTip
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types


type alias Model =
    { showExperimentalFeaturesToggleTip : Bool
    }


type Msg
    = GotStyleMode Style.Types.StyleMode
    | GotEnableExperimentalFeatures Bool
    | GotShowExperimentalFeaturesToggleTip


init : Model
init =
    { showExperimentalFeaturesToggleTip = False
    }


update : Msg -> SharedModel -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg _ model =
    case msg of
        GotStyleMode mode ->
            ( model, Cmd.none, SharedMsg.SetStyle mode )

        GotEnableExperimentalFeatures choice ->
            ( model, Cmd.none, SharedMsg.SetExperimentalFeaturesEnabled choice )

        GotShowExperimentalFeaturesToggleTip ->
            ( { model | showExperimentalFeaturesToggleTip = not model.showExperimentalFeaturesToggleTip }, Cmd.none, SharedMsg.NoOp )


view : View.Types.Context -> SharedModel -> Model -> Element.Element Msg
view context sharedModel model =
    let
        experimentalFeatureToggleTip =
            Style.Widgets.ToggleTip.toggleTip
                context.palette
                (Element.column
                    [ Element.width
                        (Element.fill
                            |> Element.minimum 100
                            |> Element.maximum 300
                        )
                    , Element.spacing 7
                    ]
                    [ Element.text "New features in development. An "
                    , Element.text "example is adding a custom workflow "
                    , Element.text "when you launch a server."
                    ]
                )
                model.showExperimentalFeaturesToggleTip
                GotShowExperimentalFeaturesToggleTip
    in
    Element.column
        (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ Element.row (VH.heading2 context.palette ++ [ Element.spacing 12 ])
            [ FeatherIcons.settings
                |> FeatherIcons.toHtml []
                |> Element.html
                |> Element.el []
            , Element.text "Settings"
            ]
        , Element.column VH.formContainer
            [ Input.radio
                VH.exoColumnAttributes
                { onChange =
                    \newStyleMode ->
                        GotStyleMode newStyleMode
                , options =
                    [ Input.option Style.Types.LightMode (Element.text "Light")
                    , Input.option Style.Types.DarkMode (Element.text "Dark")
                    ]
                , selected =
                    Just sharedModel.style.styleMode
                , label = Input.labelAbove VH.heading4 (Element.text "Color theme")
                }
            , Input.radio
                VH.exoColumnAttributes
                { onChange =
                    \newChoice ->
                        GotEnableExperimentalFeatures newChoice
                , options =
                    [ Input.option False (Element.text "Disabled")
                    , Input.option True (Element.text "Enabled")
                    ]
                , selected =
                    Just sharedModel.viewContext.experimentalFeaturesEnabled
                , label =
                    Input.labelAbove VH.heading4
                        (Element.el
                            [ Element.onRight experimentalFeatureToggleTip
                            ]
                            (Element.text "Experimental features")
                        )
                }
            ]
        ]
