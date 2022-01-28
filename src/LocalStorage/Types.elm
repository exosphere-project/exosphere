module LocalStorage.Types exposing
    ( StoredProject
    , StoredProject2
    , StoredProject3
    , StoredProject4
    , StoredState
    )

import OpenStack.Types as OSTypes
import Style.Types
import Types.Project
import UUID


type alias StoredProject =
    { secret : Types.Project.ProjectSecret
    , auth : OSTypes.ScopedAuthToken
    , region : Maybe OSTypes.Region
    , endpoints : Types.Project.Endpoints
    , description : Maybe OSTypes.ProjectDescription
    }


type alias StoredProject4 =
    { secret : Types.Project.ProjectSecret
    , auth : OSTypes.ScopedAuthToken
    , endpoints : Types.Project.Endpoints
    , description : Maybe OSTypes.ProjectDescription
    }


type alias StoredProject3 =
    { secret : Types.Project.ProjectSecret
    , auth : OSTypes.ScopedAuthToken
    , endpoints : Types.Project.Endpoints
    }


type alias StoredProject2 =
    { secret : Types.Project.ProjectSecret
    , auth : OSTypes.ScopedAuthToken
    }


type alias StoredState =
    { projects : List StoredProject
    , clientUuid : Maybe UUID.UUID
    , styleMode : Maybe Style.Types.StyleMode
    , experimentalFeaturesEnabled : Maybe Bool
    }
