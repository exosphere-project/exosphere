module Main exposing (main)

import Browser exposing (element)
import State
import Types.Types exposing (Flags, Model, Msg)
import View.View exposing (view)



{- App Setup -}


main : Program Flags Model Msg
main =
    element
        { init = State.init
        , view = view
        , update = State.update
        , subscriptions = State.subscriptions
        }
