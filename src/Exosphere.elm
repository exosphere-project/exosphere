module Exosphere exposing (main)

import Browser exposing (application)
import State.Init
import State.State as State
import State.Subscriptions
import Types.Msg exposing (Msg(..))
import Types.OuterModel exposing (OuterModel)
import Types.Types exposing (Flags)
import View.View exposing (view)



{- App Setup -}


main : Program Flags OuterModel Msg
main =
    application
        { init = \flags url key -> State.Init.init flags ( url, key )
        , view = view
        , update = State.update
        , subscriptions = State.Subscriptions.subscriptions

        -- Not needed because all of our <a hrefs load another domain in a new window
        , onUrlRequest = \_ -> NoOp
        , onUrlChange = UrlChange
        }
