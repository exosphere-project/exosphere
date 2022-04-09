module State.Subscriptions exposing (subscriptions)

import Browser.Events
import Json.Decode as Decode
import Ports exposing (changeThemePreference)
import Set
import Style.Theme exposing (decodeThemePreference)
import Style.Types as ST
import Time
import Types.Error exposing (AppError)
import Types.OuterModel exposing (OuterModel)
import Types.OuterMsg exposing (OuterMsg(..))
import Types.SharedMsg exposing (SharedMsg(..))
import View.Types exposing (PopoverId)


subscriptions : Result AppError OuterModel -> Sub OuterMsg
subscriptions result =
    case result of
        Err _ ->
            Sub.none

        Ok model ->
            subscriptionsValid model


subscriptionsValid : OuterModel -> Sub OuterMsg
subscriptionsValid outerModel =
    Sub.batch
        ([ Time.every (5 * 1000) (\x -> SharedMsg <| Tick 5 x)
         , Time.every (10 * 1000) (\x -> SharedMsg <| Tick 10 x)
         , Time.every (60 * 1000) (\x -> SharedMsg <| Tick 60 x)
         , Time.every (300 * 1000) (\x -> SharedMsg <| Tick 300 x)
         , Browser.Events.onResize (\x y -> SharedMsg <| MsgChangeWindowSize x y)
         , changeThemePreference (decodeThemePreference >> sendThemeUpdate)
         ]
            -- Close popovers if cliked outside. Based on: https://dev.to/margaretkrutikova/elm-dom-node-decoder-to-detect-click-outside-3ioh
            ++ List.map
                (\popoverId -> Browser.Events.onMouseDown (outsideTarget popoverId))
                (Set.toList outerModel.sharedModel.viewContext.showPopovers)
        )


sendThemeUpdate : Maybe ST.Theme -> OuterMsg
sendThemeUpdate update =
    case update of
        Just theme ->
            SharedMsg (ChangeSystemThemePreference theme)

        Nothing ->
            SharedMsg NoOp


outsideTarget : PopoverId -> Decode.Decoder OuterMsg
outsideTarget popoverId =
    Decode.field "target" (isOutsidePopover popoverId)
        |> Decode.andThen
            (\isOutside ->
                if isOutside then
                    Decode.succeed (SharedMsg <| TogglePopover popoverId)

                else
                    Decode.fail "inside dropdown"
            )


isOutsidePopover : PopoverId -> Decode.Decoder Bool
isOutsidePopover popoverId =
    Decode.oneOf
        [ Decode.field "id" Decode.string
            |> Decode.andThen
                (\id ->
                    if popoverId == id then
                        -- found match by id
                        Decode.succeed False

                    else
                        -- try next decoder
                        Decode.fail "check parent node"
                )
        , Decode.lazy (\_ -> isOutsidePopover popoverId |> Decode.field "parentNode")

        -- fallback if all previous decoders failed
        , Decode.succeed True
        ]
