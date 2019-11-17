module LocalStorage.Types exposing
    ( StoredProject
    , StoredProject1Or2
    , StoredState
    )

import OpenStack.Types as OSTypes
import Types.Types exposing (ProjectSecret)


type alias StoredProject =
    { secret : ProjectSecret
    , auth : OSTypes.AuthToken
    }


type alias StoredProject1Or2 =
    { password : String
    , auth : OSTypes.AuthToken
    , projDomain : OSTypes.NameAndUuid
    , userDomain : OSTypes.NameAndUuid
    }


type alias StoredState =
    { projects : List StoredProject
    }
