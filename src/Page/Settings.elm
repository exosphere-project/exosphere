module Page.Settings exposing (Model, Msg(..), init, update, view)

import Element
import Element.Input as Input
import FeatherIcons
import Style.Types as ST
import Style.Widgets.Text as Text
import Style.Widgets.ToggleTip
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types


type alias Model =
    { showExperimentalFeaturesToggleTip : Bool
    }


type Msg
    = SelectTheme ST.ThemeChoice
    | GotEnableExperimentalFeatures Bool
    | GotShowExperimentalFeaturesToggleTip


init : Model
init =
    { showExperimentalFeaturesToggleTip = False
    }


update : Msg -> SharedModel -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg _ model =
    case msg of
        SelectTheme mode ->
            ( model, Cmd.none, SharedMsg.SelectTheme mode )

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
        [ Text.heading context.palette
            []
            (FeatherIcons.settings
                |> FeatherIcons.toHtml []
                |> Element.html
                |> Element.el []
            )
            "Settings"
        , Element.column VH.formContainer
            [ Input.radio
                VH.exoColumnAttributes
                { onChange = SelectTheme
                , options =
                    [ Input.option (ST.Override ST.Light) (Element.text "Light")
                    , Input.option (ST.Override ST.Dark) (Element.text "Dark")
                    , Input.option ST.System (Element.text "System")
                    ]
                , selected =
                    Just sharedModel.style.styleMode.theme
                , label = Input.labelAbove [] (Text.text Text.H4 [] "Color theme")
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
                    Input.labelAbove
                        []
                        (Text.text Text.H4
                            [ Element.onRight experimentalFeatureToggleTip
                            ]
                            "Experimental features"
                        )
                }
            ]
        ]
