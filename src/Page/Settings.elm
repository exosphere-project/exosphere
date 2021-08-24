module Page.Settings exposing (Model, Msg(..), init, update, view)

import Element
import Element.Input as Input
import FeatherIcons
import Style.Types
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg as SharedMsg
import View.Helpers as VH
import View.Types


type alias Model =
    ()


type Msg
    = GotStyleMode Style.Types.StyleMode
    | GotEnableExperimentalFeatures Bool


init : Model
init =
    ()


update : Msg -> SharedModel -> Model -> ( Model, Cmd Msg, SharedMsg.SharedMsg )
update msg _ model =
    case msg of
        GotStyleMode mode ->
            ( model, Cmd.none, SharedMsg.SetStyle mode )

        GotEnableExperimentalFeatures choice ->
            ( model, Cmd.none, SharedMsg.SetExperimentalFeaturesEnabled choice )


view : View.Types.Context -> SharedModel -> Model -> Element.Element Msg
view context sharedModel _ =
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
                , label = Input.labelAbove [] (Element.text "Color theme")
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
                    Just sharedModel.experimentalFeaturesEnabled
                , label = Input.labelAbove [] (Element.text "Experimental features")
                }
            ]
        ]
