module LocalStorage.Types exposing
    ( StoredProject
    , StoredProject1
    , StoredState
    )

import OpenStack.Types as OSTypes
import Types.Types exposing (ProjectSecret, UnscopedProvider)


type alias StoredProject =
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
    , unscopedProviders : List UnscopedProvider
    }
