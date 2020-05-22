module Helpers.RemoteDataPlusPlus exposing
    ( Haveness(..)
    , ReceivedTime
    , RefreshStatus(..)
    , RemoteDataPlusPlus
    , RequestedTime
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
    = NotLoading
    | Loading RequestedTime
    | Error ReceivedTime e


type alias RequestedTime =
    Time.Posix


type alias ReceivedTime =
    Time.Posix
