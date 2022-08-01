module DesignSystem.Stories.Toast exposing (ToastModel, ToastState, addToastIfUnique, initialModel, stories, update)

import DesignSystem.Helpers exposing (Plugins, Renderer, palettize)
import Element
import Html
import Page.Toast
import Style.Toast
import Style.Types
import Style.Widgets.Button as Button
import Toasty
import Types.Error exposing (Toast)
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
    { toasties : Toasty.Stack Toast }


initialModel : { toasties : Toasty.Stack a }
initialModel =
    { toasties = Toasty.initialState }


config : Toasty.Config msg
config =
    Style.Toast.toastConfig


addToastIfUnique :
    Toast
    -> (Toasty.Msg Toast -> msg)
    ->
        ( ToastState
        , Cmd msg
        )
    -> ( ToastState, Cmd msg )
addToastIfUnique toast tagger ( model, cmd ) =
    Toasty.addToastIfUnique config tagger toast ( model, cmd )


update : (Toasty.Msg Toast -> msg) -> Toasty.Msg Toast -> ToastState -> ( ToastState, Cmd msg )
update tagger msg model =
    Toasty.update config tagger msg model


{-| Creates stories for UIExplorer.

    renderer – An elm-ui to html converter
    palette  – Takes a UIExplorer.Model and produces an ExoPalette
    plugins  – UIExplorer plugins (can be empty {})

-}
stories :
    Renderer msg
    -> (Toasty.Msg Toast -> msg)
    ->
        { toast
            | onPress : Maybe msg
        }
    ->
        UIExplorer.UI
            { model
                | deployerColors : Style.Types.DeployerColorThemes
                , toasts : ToastState
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
                        , Toasty.view config (Page.Toast.view { palette = palettize m } { showDebugMsgs = True }) tagger m.customModel.toasts.toasties
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
