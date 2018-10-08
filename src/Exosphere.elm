module Main exposing (main)

import Browser exposing (element)
import State
import Types.Types exposing (Model, Msg)
import View



{- App Setup -}


main =
    element
        { init = State.init
        , view = View.view
        , update = State.update
        , subscriptions = State.subscriptions
        }
