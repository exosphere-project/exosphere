module ExosphereElectron exposing (main)

import Browser exposing (element)
import State.Init
import State.State as State
import State.Subscriptions
import Types.Types exposing (Flags, Model, Msg(..))
import View.View exposing (viewElectron)



-- Elm's Browser.application cannot handle file:// URLs (https://github.com/elm/url/issues/10), so we must use
-- Browser.element for the Electron app, which requires another entry point and module separate from Exosphere.elm
{- App Setup -}


main : Program Flags Model Msg
main =
    element
        { init = \flags -> State.Init.init flags Nothing
        , view = viewElectron
        , update = State.update
        , subscriptions = State.Subscriptions.subscriptions
        }
