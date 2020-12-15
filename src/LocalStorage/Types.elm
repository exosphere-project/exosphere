module LocalStorage.Types exposing
    ( StoredProject
    , StoredProject1
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


type alias StoredProject1 =
    { password : String
    , auth : OSTypes.ScopedAuthToken
    , projDomain : OSTypes.NameAndUuid
    , userDomain : OSTypes.NameAndUuid
    }


type alias StoredState =
    { projects : List StoredProject
    , clientUuid : Maybe UUID.UUID
    , styleMode : Maybe Style.Types.StyleMode
    }
