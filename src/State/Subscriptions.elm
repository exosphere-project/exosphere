module State.Subscriptions exposing (subscriptions)

import Browser.Events
import Helpers.GetterSetters as GetterSetters
import Page.ServerDetail
import Ports exposing (changeThemePreference, receiveWebLock, updateNetworkConnectivity)
import Set
import Style.Theme exposing (decodeThemePreference)
import Style.Types as ST
import Style.Widgets.Popover.Popover exposing (toggleIfTargetIsOutside)
import Time
import Types.Error exposing (AppError)
import Types.OuterModel exposing (OuterModel)
import Types.OuterMsg exposing (OuterMsg(..))
import Types.SharedMsg exposing (SharedMsg(..))
import Types.View exposing (ProjectViewConstructor(..), ViewState(..))


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
         , Time.every (300 * 1000) (\_ -> SharedMsg <| RequestBanners)
         , Time.every (3600 * 1000) (\_ -> SharedMsg <| RequestAppVersion)
         , Browser.Events.onResize (\x y -> SharedMsg <| MsgChangeWindowSize x y)
         , Browser.Events.onVisibilityChange (\visibility -> SharedMsg <| VisibilityChanged visibility)
         , changeThemePreference (decodeThemePreference >> sendThemeUpdate)
         , updateNetworkConnectivity (\online -> SharedMsg (NetworkConnection online))
         , receiveWebLock (\result -> SharedMsg (ReceiveWebLock result))
         ]
            -- A 1s clock tick ONLY while a CloudShield scan is actively counting on the open
            -- ServerDetail page, to smooth its elapsed timer (the shared 5s tick is visibly
            -- chunky). Gated tightly so it exists only during the ~scan window and drops away
            -- the moment the run is terminal; it sends the pure `ClockTick` (no API polling).
            ++ (if exoextScanCounting outerModel then
                    [ Time.every 1000 (\x -> SharedMsg <| ClockTick x) ]

                else
                    []
               )
            -- A 5s server refresh ONLY while an exoext request is pending on the open
            -- ServerDetail page. The handler fetches that one publishing server and dedupes on
            -- the server's `loadingSeparately` flag.
            ++ (if exoextRequestsPending outerModel then
                    [ Time.every (5 * 1000) (\x -> SharedMsg <| ExoextPoll x) ]

                else
                    []
               )
            -- Close popovers if clicked outside. Based on: https://dev.to/margaretkrutikova/elm-dom-node-decoder-to-detect-click-outside-3ioh
            ++ List.map
                (\popoverId ->
                    Browser.Events.onMouseDown
                        (toggleIfTargetIsOutside popoverId
                            (\popoverId_ -> SharedMsg <| TogglePopover popoverId_)
                        )
                )
                (Set.toList outerModel.sharedModel.viewContext.showPopovers)
        )


{-| True exactly when the open page is a ServerDetail whose CloudShield card has a tracked
scan (`pending` set) in a non-terminal state — i.e. `scanTimerView` is in its counting phase.
Mirrors the host-side gate in `ServerDetail.cloudShieldViewConfig`: an absent/uncorrelated
status defaults to "queued" (counting); done/error/cancelled/expired stop the tick.
-}
exoextScanCounting : OuterModel -> Bool
exoextScanCounting outerModel =
    case outerModel.viewState of
        ProjectView projectId (ServerDetail pageModel) ->
            GetterSetters.projectLookup outerModel.sharedModel projectId
                |> Maybe.map (\project -> Page.ServerDetail.exoextScanRequestPending project pageModel)
                |> Maybe.withDefault False

        _ ->
            False


exoextRequestsPending : OuterModel -> Bool
exoextRequestsPending outerModel =
    case outerModel.viewState of
        ProjectView projectId (ServerDetail pageModel) ->
            GetterSetters.projectLookup outerModel.sharedModel projectId
                |> Maybe.map (\project -> Page.ServerDetail.exoextRequestsPending project pageModel)
                |> Maybe.withDefault False

        _ ->
            False


sendThemeUpdate : Maybe ST.Theme -> OuterMsg
sendThemeUpdate update =
    case update of
        Just theme ->
            SharedMsg (ChangeSystemThemePreference theme)

        Nothing ->
            SharedMsg NoOp
