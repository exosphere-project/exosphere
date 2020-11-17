module Exosphere exposing (main)

import Browser exposing (application)
import State
import Types.Types exposing (Flags, Model, Msg(..))
import View.View exposing (view)



{- App Setup -}


main : Program Flags Model Msg
main =
    application
        { init = State.init
        , view = view
        , update = State.update
        , subscriptions = State.subscriptions
        , onUrlRequest = \_ -> NoOp

        -- Ignoring this for the moment, will need to handle it for browser back/foward button clicks
        , onUrlChange = \_ -> NoOp
        }
