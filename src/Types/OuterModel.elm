module Types.OuterModel exposing (OuterModel)

import OpenStack.Types as OSTypes
import Types.HelperTypes as HelperTypes
import Types.Msg exposing (Msg)
import Types.Types exposing (SharedModel)
import Types.View exposing (ViewState)


type alias OuterModel =
    { sharedModel : SharedModel
    , viewState : ViewState
    , pendingCredentialedRequests : List ( HelperTypes.ProjectIdentifier, OSTypes.AuthTokenString -> Cmd Msg )
    }
