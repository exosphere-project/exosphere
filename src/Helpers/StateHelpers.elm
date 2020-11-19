module Helpers.StateHelpers exposing (updateViewState)

import AppUrl.Builder
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
        urlWithoutQuery url =
            String.split "?" url
                |> List.head
                |> Maybe.withDefault ""

        prevUrl =
            model.prevUrl

        newUrl =
            AppUrl.Builder.viewStateToUrl viewState

        newModel =
            { model
                | viewState = viewState
                , prevUrl = newUrl
            }

        -- We should `pushUrl` when modifying the path (moving between views), `replaceUrl` when just modifying the query string (setting parameters of views)
        updateUrlFunc =
            if urlWithoutQuery newUrl == urlWithoutQuery prevUrl then
                Browser.Navigation.replaceUrl

            else
                Browser.Navigation.pushUrl

        urlCmd =
            case model.navigationKey of
                Just key ->
                    updateUrlFunc key newUrl

                Nothing ->
                    Cmd.none
    in
    ( newModel, Cmd.batch [ cmd, urlCmd ] )
