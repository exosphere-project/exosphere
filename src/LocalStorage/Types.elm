module LocalStorage.Types exposing
    ( StoredProvider
    , StoredState
    )

import Types.OpenstackTypes as OSTypes
import Types.Types exposing (..)


type alias StoredProvider =
    { name : ProviderName
    , creds : Creds
    , auth : OSTypes.AuthToken
    }


type alias StoredState =
    { providers : List StoredProvider
    }
