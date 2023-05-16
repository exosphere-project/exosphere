module Helpers.RemoteDataPlusPlus exposing
    ( Haveness(..)
    , ReceivedTime
    , RefreshStatus(..)
    , RemoteDataPlusPlus
    , RequestedTime
    , andMap
    , empty
    , isPollableWithInterval
    , map
    , map2
    , setData
    , setLoading
    , setNotLoading
    , setRefreshStatus
    , toMaybe
    , withDefault
    )

import Time



{-
   Like https://package.elm-lang.org/packages/krisajenkins/remotedata but with blackjack and timestamps
-}


type alias RemoteDataPlusPlus error data =
    { data : Haveness data
    , refreshStatus : RefreshStatus error
    }


type Haveness data
    = DontHave
    | DoHave data ReceivedTime


type RefreshStatus e
    = NotLoading (Maybe ( e, ReceivedTime ))
    | Loading


type alias RequestedTime =
    Time.Posix


type alias ReceivedTime =
    Time.Posix



-- Convenience functions


andMap : RemoteDataPlusPlus e a -> RemoteDataPlusPlus e (a -> b) -> RemoteDataPlusPlus e b
andMap wrappedValue wrappedFunction =
    case ( wrappedFunction.data, wrappedValue.data ) of
        ( DoHave f _, DoHave b t ) ->
            RemoteDataPlusPlus (DoHave (f b) t) wrappedValue.refreshStatus

        ( DontHave, _ ) ->
            RemoteDataPlusPlus DontHave wrappedFunction.refreshStatus

        ( _, DontHave ) ->
            RemoteDataPlusPlus DontHave wrappedValue.refreshStatus


map : (a -> b) -> RemoteDataPlusPlus error a -> RemoteDataPlusPlus error b
map f rdpp =
    let
        newData =
            case rdpp.data of
                DoHave data time ->
                    DoHave (f data) time

                DontHave ->
                    DontHave
    in
    RemoteDataPlusPlus newData rdpp.refreshStatus


map2 : (a -> b -> c) -> RemoteDataPlusPlus error a -> RemoteDataPlusPlus error b -> RemoteDataPlusPlus error c
map2 f a b =
    map f a |> andMap b


withDefault : data -> RemoteDataPlusPlus error data -> data
withDefault default rdpp =
    -- Returns data, or the default
    case rdpp.data of
        DoHave data _ ->
            data

        DontHave ->
            default


empty : RemoteDataPlusPlus x y
empty =
    RemoteDataPlusPlus DontHave (NotLoading Nothing)


setRefreshStatus : RefreshStatus error -> RemoteDataPlusPlus error data -> RemoteDataPlusPlus error data
setRefreshStatus refreshStatus rdpp =
    { rdpp | refreshStatus = refreshStatus }


setData : Haveness data -> RemoteDataPlusPlus error data -> RemoteDataPlusPlus error data
setData haveness rdpp =
    RemoteDataPlusPlus haveness rdpp.refreshStatus

setLoading : RemoteDataPlusPlus error data -> RemoteDataPlusPlus error data
setLoading rdpp =
    setRefreshStatus Loading rdpp


setNotLoading : Maybe ( error, ReceivedTime ) -> RemoteDataPlusPlus error data -> RemoteDataPlusPlus error data
setNotLoading error rdpp =
    setRefreshStatus (NotLoading error) rdpp


toMaybe : RemoteDataPlusPlus error data -> Maybe data
toMaybe rdpp =
    case rdpp.data of
        DoHave data _ ->
            Just data

        _ ->
            Nothing


isPollableWithInterval : RemoteDataPlusPlus x y -> Time.Posix -> Int -> Bool
isPollableWithInterval rdpp currentTime pollIntervalMillis =
    let
        timeTooRecent : Time.Posix -> Bool
        timeTooRecent time =
            Time.posixToMillis currentTime - Time.posixToMillis time < pollIntervalMillis

        receivedTimeTooRecent =
            case rdpp.data of
                DontHave ->
                    False

                DoHave _ receivedTime ->
                    timeTooRecent receivedTime

        errorTooRecentOrLoading =
            case rdpp.refreshStatus of
                NotLoading Nothing ->
                    False

                NotLoading (Just ( _, receivedTime )) ->
                    timeTooRecent receivedTime

                Loading ->
                    True
    in
    not (receivedTimeTooRecent || errorTooRecentOrLoading)
