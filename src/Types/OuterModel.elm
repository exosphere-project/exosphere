module Types.OuterModel exposing (OuterModel)

import Types.Types exposing (SharedModel)
import Types.View exposing (ViewState)


type alias OuterModel =
    { sharedModel : SharedModel
    , viewState : ViewState
    }
