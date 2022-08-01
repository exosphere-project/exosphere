module DesignSystem.Stories.Toast exposing (ToastModel, ToastState, addToastIfUnique, initialModel, stories, update)

import DesignSystem.Helpers exposing (Plugins, Renderer, palettize)
import Element
import Html
import Style.Toast
import Style.Types
import Style.Widgets.Button as Button
import Toasty
import Toasty.Defaults
import UIExplorer
    exposing
        ( storiesOf
        )


type alias ToastModel model =
    { model
        | toasts : ToastState
    }


{-| The stack of toasties used by Toasty.
-}
type alias ToastState =
    { toasties : Toasty.Stack Toasty.Defaults.Toast }


initialModel : { toasties : Toasty.Stack a }
initialModel =
    { toasties = Toasty.initialState }


config : Toasty.Config msg
config =
    Style.Toast.toastConfig


addToastIfUnique :
    Toasty.Defaults.Toast
    -> (Toasty.Msg Toasty.Defaults.Toast -> msg)
    ->
        ( ToastState
        , Cmd msg
        )
    -> ( ToastState, Cmd msg )
addToastIfUnique toast tagger ( model, cmd ) =
    Toasty.addToastIfUnique config tagger toast ( model, cmd )


update : (Toasty.Msg Toasty.Defaults.Toast -> msg) -> Toasty.Msg Toasty.Defaults.Toast -> ToastState -> ( ToastState, Cmd msg )
update tagger msg model =
    Toasty.update config tagger msg model


{-| Creates stories for UIExplorer.

    renderer – An elm-ui to html converter
    palette  – Takes a UIExplorer.Model and produces an ExoPalette
    plugins  – UIExplorer plugins (can be empty {})

-}
stories :
    Renderer msg
    -> (Toasty.Msg Toasty.Defaults.Toast -> msg)
    ->
        { toast
            | onPress : Maybe msg
        }
    ->
        UIExplorer.UI
            { model
                | toasts : ToastState
                , deployerColors : Style.Types.DeployerColorThemes
            }
            msg
            Plugins
stories renderer tagger { onPress } =
    storiesOf
        "Toast"
        (List.map
            (\message ->
                ( message.name
                , \m ->
                    let
                        button press =
                            Button.primary (palettize m)
                                { text = "Show toast"
                                , onPress = press
                                }
                    in
                    Html.div []
                        [ renderer (palettize m) <|
                            Element.el [ Element.paddingXY 400 100 ]
                                (button onPress)
                        , Toasty.view config Toasty.Defaults.view tagger m.customModel.toasts.toasties
                        ]
                , { note = note }
                )
            )
            [ { name = "warning" }
            ]
        )


note : String
note =
    """
## Usage
    """
