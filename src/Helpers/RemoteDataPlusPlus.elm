module Helpers.RemoteDataPlusPlus exposing
    ( Haveness(..)
    , ReceivedTime
    , RefreshStatus(..)
    , RemoteDataPlusPlus
    , RequestedTime
    , withDefault
    )

import Time



{-
   Like https://package.elm-lang.org/packages/krisajenkins/remotedata but with blackjack and timestamps
-}


type alias RemoteDataPlusPlus data e =
    { data : Haveness data
    , refreshStatus : RefreshStatus e
    }


type Haveness data
    = DontHave
    | DoHave data ReceivedTime


type RefreshStatus e
    = NotLoading (Maybe ( e, ReceivedTime ))
    | Loading RequestedTime


type alias RequestedTime =
    Time.Posix


type alias ReceivedTime =
    Time.Posix



-- Convenience functions


withDefault : a -> RemoteDataPlusPlus a e -> a
withDefault default rdpp =
    -- Returns data, or the default
    case rdpp.data of
        DoHave data _ ->
            data

        DontHave ->
            default
