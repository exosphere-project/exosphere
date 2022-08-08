module DesignSystem.Stories.Toast exposing (ToastModel, ToastState, initialModel, showToast, stories, update)

import DesignSystem.Helpers exposing (Plugins, Renderer, palettize)
import Element
import Html
import Page.Toast
import Style.Toast
import Style.Types
import Style.Widgets.Button as Button
import Toasty
import Types.Error exposing (ErrorLevel(..), Toast)
import UIExplorer
    exposing
        ( storiesOf
        )


{-| Toasts need to be accumulated in a stack & have their own identifiers.
-}
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
    -> ( ToastState, Cmd msg )
    -> ( ToastState, Cmd msg )
addToastIfUnique toast tagger ( model, cmd ) =
    Toasty.addToastIfUnique config tagger toast ( model, cmd )


{-| Configure an example toast based on error level.
-}
makeToast : ErrorLevel -> Toast
makeToast level =
    case level of
        ErrorDebug ->
            Toast
                { actionContext = "request console log for server 5b8ad28f-3a82-4eec-aee6-7389f62ce04e"
                , level = level
                , recoveryHint = Nothing
                }
                "Server not found."

        ErrorInfo ->
            Toast
                { actionContext = "format volume b2b1a743-9c27-41bd-a430-4b38ae65fb5f"
                , level = level
                , recoveryHint = Just "Try choose a different volume."
                }
                "Volume not found."

        ErrorWarn ->
            Toast
                { actionContext = "decode stored application state retrieved from browser local storage"
                , level = level
                , recoveryHint = Nothing
                }
                "Missing OpenStack username."

        ErrorCrit ->
            Toast
                { actionContext = "get a list of volumes"
                , level = level
                , recoveryHint = Nothing
                }
                """
<html> <head><title>404 Not
Found</title></head> <body> <center>
<h1>404 Not Found</h1></center>
<hr><center>nginx/1.21.6</center>
</body> </html> <!-- a padding to
disable MSIE and Chrome friendly error
page -> <- a padding to disable MSIE
and Chrome friendly error page -> <!-
a padding to disable MSIE and Chrome
friendly error page -> <- a padding to
disable MSIE and Chrome friendly error
page -> <!- a padding to disable MSIE
and Chrome friendly error page -> <!-
a padding to disable MSIE and Chrome
friendly error page -> (response code:
404)
"""


{-| Show an example toast for the given error level.
-}
showToast :
    ErrorLevel
    -> (Toasty.Msg Toast -> msg)
    -> ( ToastState, Cmd msg )
    -> ( ToastState, Cmd msg )
showToast level tagger ( model, cmd ) =
    let
        toast =
            makeToast level
    in
    addToastIfUnique toast tagger ( model, cmd )


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
    -> List { name : String, onPress : Maybe msg }
    ->
        UIExplorer.UI
            { model
                | deployerColors : Style.Types.DeployerColorThemes
                , toasts : ToastState
            }
            msg
            Plugins
stories renderer tagger levels =
    storiesOf
        "Toast"
        (List.map
            (\level ->
                ( level.name
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
                                (button level.onPress)
                        , Toasty.view config (Page.Toast.view { palette = palettize m } { showDebugMsgs = True }) tagger m.customModel.toasts.toasties
                        ]
                , { note = note }
                )
            )
            levels
        )


note : String
note =
    """
## Usage
    """
