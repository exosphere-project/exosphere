module Types.OuterModel exposing (OuterModel)

import OpenStack.Types as OSTypes
import Types.HelperTypes as HelperTypes
import Types.SharedMsg exposing (SharedMsg)
import Types.Types exposing (SharedModel)
import Types.View exposing (ViewState)


type alias OuterModel =
    { sharedModel : SharedModel
    , viewState : ViewState
    , pendingCredentialedRequests : List ( HelperTypes.ProjectIdentifier, OSTypes.AuthTokenString -> Cmd SharedMsg )
    }
