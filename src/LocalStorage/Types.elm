module LocalStorage.Types exposing
    ( StoredProject
    , StoredState
    )

import OpenStack.Types as OSTypes
import Types.Types exposing (OpenstackCreds)


type alias StoredProject =
    { creds : OpenstackCreds
    , auth : OSTypes.AuthToken
    }


type alias StoredState =
    { projects : List StoredProject
    }
