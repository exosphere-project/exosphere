module Main exposing (main)

import Html exposing (program)
import State
import Types exposing (Model, Msg)
import View


{- App Setup -}


main : Program Never Model Msg
main =
    program
        { init = State.init
        , view = View.view
        , update = State.update
        , subscriptions = State.subscriptions
        }
