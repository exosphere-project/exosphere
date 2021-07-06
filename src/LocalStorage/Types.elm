module LocalStorage.Types exposing
    ( StoredProject
    , StoredProject2
    , StoredState
    )

import OpenStack.Types as OSTypes
import Style.Types
import Types.Types exposing (Endpoints, ProjectSecret)
import UUID


type alias StoredProject =
    { secret : ProjectSecret
    , auth : OSTypes.ScopedAuthToken
    , endpoints : Endpoints
    }


type alias StoredProject2 =
    { secret : ProjectSecret
    , auth : OSTypes.ScopedAuthToken
    }


type alias StoredState =
    { projects : List StoredProject
    , clientUuid : Maybe UUID.UUID
    , styleMode : Maybe Style.Types.StyleMode
    }
