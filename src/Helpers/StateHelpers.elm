module Helpers.StateHelpers exposing (updateViewState)

import AppUrl
import Browser.Navigation
import Types.Types
    exposing
        ( Model
        , Msg
        , ViewState
        )


updateViewState : Model -> Cmd Msg -> ViewState -> ( Model, Cmd Msg )
updateViewState model cmd viewState =
    -- the cmd argument is just a "passthrough", added to the Cmd that sets new URL
    let
        newModel =
            { model | viewState = viewState }

        newUrl =
            AppUrl.viewStateToUrl newModel

        urlCmd =
            Browser.Navigation.replaceUrl model.navigationKey newUrl
    in
    ( newModel, Cmd.batch [ cmd, urlCmd ] )
