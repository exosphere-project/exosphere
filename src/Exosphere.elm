module Main exposing (main)

import BeautifulExample
import Color
import State
import Types.Types exposing (Model, Msg)
import View


{- App Setup -}


main : Program Never Model Msg
main =
    BeautifulExample.program
        { title = "Exosphere"
        , details =
            Just """User-friendly, extensible client for cloud computing. Currently targeting OpenStack."""
        , color = Just Color.blue
        , maxWidth = 640
        , githubUrl = Just "https://github.com/exosphere-project/exosphere"
        , documentationUrl = Just "https://github.com/exosphere-project/exosphere"
        }
        { init = State.init
        , view = View.view
        , update = State.update
        , subscriptions = State.subscriptions
        }
