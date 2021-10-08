module State.Subscriptions exposing (subscriptions)

import Browser.Events
import Time
import Types.Error exposing (AppError)
import Types.OuterModel exposing (OuterModel)
import Types.OuterMsg exposing (OuterMsg(..))
import Types.SharedMsg exposing (SharedMsg(..))


subscriptions : Result AppError OuterModel -> Sub OuterMsg
subscriptions result =
    case result of
        Err _ ->
            Sub.none

        Ok model ->
            subscriptionsValid model


subscriptionsValid : OuterModel -> Sub OuterMsg
subscriptionsValid _ =
    Sub.batch
        [ Time.every (5 * 1000) (\x -> SharedMsg <| Tick 5 x)
        , Time.every (10 * 1000) (\x -> SharedMsg <| Tick 10 x)
        , Time.every (60 * 1000) (\x -> SharedMsg <| Tick 60 x)
        , Time.every (300 * 1000) (\x -> SharedMsg <| Tick 300 x)
        , Browser.Events.onResize (\x y -> SharedMsg <| MsgChangeWindowSize x y)
        ]
