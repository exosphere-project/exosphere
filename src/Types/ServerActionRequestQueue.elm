module Types.ServerActionRequestQueue exposing (ServerActionRequest(..), ServerActionRequestJob)

import Helpers.Queue exposing (Job)
import OpenStack.Types exposing (ServerSecurityGroup)


{-| A request to perform a server action.
-}
type ServerActionRequest
    = RemoveServerSecurityGroup ServerSecurityGroup


type alias ServerActionRequestJob =
    Job ServerActionRequest
