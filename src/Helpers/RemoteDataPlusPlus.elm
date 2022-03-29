module Helpers.RemoteDataPlusPlus exposing
    ( Haveness(..)
    , ReceivedTime
    , RefreshStatus(..)
    , RemoteDataPlusPlus
    , RequestedTime
    , empty
    , setLoading
    , setNotLoading
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


setNotLoading : RemoteDataPlusPlus x y -> RemoteDataPlusPlus x y
setNotLoading rdpp =
    { rdpp | refreshStatus = NotLoading Nothing }
