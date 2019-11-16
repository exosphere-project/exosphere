module LocalStorage.Types exposing
    ( StoredProject
    , StoredState
    )

import OpenStack.Types as OSTypes


type alias StoredProject =
    { password : String
    , auth : OSTypes.AuthToken
    }


type alias StoredState =
    { projects : List StoredProject
    }
