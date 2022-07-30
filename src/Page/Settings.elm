module Page.Settings exposing (Model, Msg(..), headerView, init, update, view)

import Element
import Element.Font as Font
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
    {}


type Msg
    = SelectTheme ST.ThemeChoice
    | GotEnableExperimentalFeatures Bool
    | SharedMsg SharedMsg.SharedMsg


init : Model
init =
    {}


update : Msg -> SharedModel -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg _ model =
    case msg of
        SelectTheme mode ->
            ( model, Cmd.none, SharedMsg.SelectTheme mode )

        GotEnableExperimentalFeatures choice ->
            ( model, Cmd.none, SharedMsg.SetExperimentalFeaturesEnabled choice )

        SharedMsg sharedMsg ->
            ( model, Cmd.none, sharedMsg )


headerView : View.Types.Context -> Element.Element msg
headerView context =
    Text.heading context.palette
        VH.headerHeadingAttributes
        (FeatherIcons.settings
            |> FeatherIcons.toHtml []
            |> Element.html
            |> Element.el []
        )
        "Settings"


view : View.Types.Context -> SharedModel -> Model -> Element.Element Msg
view context sharedModel _ =
    let
        experimentalFeatureToggleTip =
            Style.Widgets.ToggleTip.toggleTip
                context
                (\experimentalFeaturesTipId -> SharedMsg <| SharedMsg.TogglePopover experimentalFeaturesTipId)
                "settingsExperimentalFeaturesToggleTip"
                (Element.paragraph
                    [ Element.width (Element.fill |> Element.minimum 300)
                    , Element.spacing 7
                    , Element.padding 6
                    , Font.regular
                    ]
                    [ Element.text "New features in development. An example is adding a custom workflow when you launch a server." ]
                )
                ST.PositionRight
    in
    Element.column
        (VH.exoColumnAttributes ++ [ Element.width Element.fill ])
        [ Element.column VH.formContainer
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
