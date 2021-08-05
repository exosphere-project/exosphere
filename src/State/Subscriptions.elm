module State.Subscriptions exposing (subscriptions)

import Browser.Events
import Time
import Types.Msg exposing (SharedMsg(..))
import Types.OuterModel exposing (OuterModel)
import Types.OuterMsg exposing (OuterMsg(..))


subscriptions : OuterModel -> Sub OuterMsg
subscriptions _ =
    Sub.batch
        [ Time.every (5 * 1000) (\x -> SharedMsg <| Tick 5 x)
        , Time.every (10 * 1000) (\x -> SharedMsg <| Tick 10 x)
        , Time.every (60 * 1000) (\x -> SharedMsg <| Tick 60 x)
        , Time.every (300 * 1000) (\x -> SharedMsg <| Tick 300 x)
        , Browser.Events.onResize (\x y -> SharedMsg <| MsgChangeWindowSize x y)
        ]
