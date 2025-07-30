module Types.ServerActionRequestQueue exposing (ServerActionRequest(..), ServerActionRequestJob)

import Helpers.Queue exposing (JobStatus)
import OpenStack.Types exposing (ServerSecurityGroup)


{-| A request to perform a server action.
-}
type ServerActionRequest
    = RemoveServerSecurityGroup ServerSecurityGroup


type alias ServerActionRequestJob =
    { job : ServerActionRequest
    , status : JobStatus
    }
