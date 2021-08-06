module Exosphere exposing (main)

import Browser exposing (application)
import LegacyView.View exposing (view)
import State.Init
import State.State as State
import State.Subscriptions
import Types.Flags exposing (Flags)
import Types.OuterModel exposing (OuterModel)
import Types.OuterMsg exposing (OuterMsg(..))
import Types.SharedMsg exposing (SharedMsg(..))



{- App Setup -}


main : Program Flags OuterModel OuterMsg
main =
    application
        { init = \flags url key -> State.Init.init flags ( url, key )
        , view = view
        , update = State.update
        , subscriptions = State.Subscriptions.subscriptions
        , onUrlRequest =
            \urlRequest ->
                case urlRequest of
                    Browser.Internal url ->
                        -- TODO need a way to handle these which includes a Browser.Navigation.pushUrl
                        SharedMsg <| UrlChange url

                    Browser.External _ ->
                        SharedMsg NoOp
        , onUrlChange = \u -> SharedMsg <| UrlChange u
        }
