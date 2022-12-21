module Helpers.RemoteDataPlusPlus exposing
    ( Haveness(..)
    , ReceivedTime
    , RefreshStatus(..)
    , RemoteDataPlusPlus
    , RequestedTime
    , empty
    , isPollableWithInterval
    , setLoading
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


setLoading : RemoteDataPlusPlus x y -> RemoteDataPlusPlus x y
setLoading rdpp =
    { rdpp | refreshStatus = Loading }


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
