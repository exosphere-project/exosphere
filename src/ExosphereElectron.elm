module ExosphereElectron exposing (main)

import Browser exposing (element)
import State
import Types.Types exposing (Flags, Model, Msg(..))
import View.View exposing (viewElectron)



{- App Setup -}


main : Program Flags Model Msg
main =
    element
        { init = \flags -> State.init flags Nothing
        , view = viewElectron
        , update = State.update
        , subscriptions = State.subscriptions
        }
