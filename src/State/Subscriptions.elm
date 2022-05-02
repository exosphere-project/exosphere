module State.Subscriptions exposing (subscriptions)

import Browser.Events
import Ports exposing (changeThemePreference)
import Set
import Style.Theme exposing (decodeThemePreference)
import Style.Types as ST
import Style.Widgets.Popover.Popover exposing (toggleIfTargetIsOutside)
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
                (\popoverId ->
                    Browser.Events.onMouseDown
                        (toggleIfTargetIsOutside popoverId
                            (\popoverId_ -> SharedMsg <| TogglePopover popoverId_)
                        )
                )
                (Set.toList outerModel.sharedModel.viewContext.showPopovers)
        )


sendThemeUpdate : Maybe ST.Theme -> OuterMsg
sendThemeUpdate update =
    case update of
        Just theme ->
            SharedMsg (ChangeSystemThemePreference theme)

        Nothing ->
            SharedMsg NoOp
