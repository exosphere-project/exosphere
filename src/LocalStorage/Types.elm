module LocalStorage.Types exposing
    ( StoredProject
    , StoredState
    )

import OpenStack.Types as OSTypes
import Types.Types exposing (..)


type alias StoredProject =
    { creds : Creds
    , auth : OSTypes.AuthToken
    }


type alias StoredState =
    { projects : List StoredProject
    }
