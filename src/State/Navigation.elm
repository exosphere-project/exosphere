module State.Navigation exposing (Navigation(..), classify)

import Route
import State.LoginChain
import Types.View exposing (NonProjectViewConstructor(..), ViewState(..))


type Navigation
    = NewPage
    | NewPageAbandoningCredentialFileChain


classify : { credentialFileLoginChainIsActive : Bool } -> ViewState -> Route.Route -> Navigation
classify { credentialFileLoginChainIsActive } viewState route =
    if
        credentialFileLoginChainIsActive
            && State.LoginChain.shouldAbandon (isSelectingRegions viewState) (selectsRegions route)
    then
        NewPageAbandoningCredentialFileChain

    else
        NewPage


isSelectingRegions : ViewState -> Bool
isSelectingRegions viewState =
    case viewState of
        NonProjectView (SelectProjectRegions _) ->
            True

        _ ->
            False


selectsRegions : Route.Route -> Bool
selectsRegions route =
    case route of
        Route.SelectProjectRegions _ _ ->
            True

        _ ->
            False
