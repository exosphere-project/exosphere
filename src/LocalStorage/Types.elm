module LocalStorage.Types exposing
    ( StoredProject
    , StoredProject2
    , StoredProject3
    , StoredProject4
    , StoredProject5
    , StoredProject6
    , StoredState
    )

import Dict exposing (Dict)
import OpenStack.Types as OSTypes
import Set exposing (Set)
import Style.Types
import Types.Project
import Types.Server exposing (ServerExoActions)
import UUID


type alias StoredProject =
    StoredProject6


type alias StoredProject6 =
    { secret : Types.Project.ProjectSecret
    , auth : OSTypes.ScopedAuthToken
    , region : Maybe OSTypes.Region
    , endpoints : Types.Project.Endpoints
    , description : Maybe OSTypes.ProjectDescription
    , serverExoActions : Dict OSTypes.ServerUuid ServerExoActions
    }


type alias StoredProject5 =
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
    , dismissedBanners : Set String
    }
