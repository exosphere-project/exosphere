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
            AppUrl.Builder.viewStateToUrl model.urlPathPrefix viewState

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
            -- This case statement prevents us from trying to update the URL in the electron app (where we don't have
            -- a navigation key)
            case model.maybeNavigationKey of
                Just key ->
                    updateUrlFunc key newUrl

                Nothing ->
                    Cmd.none
    in
    ( newModel, Cmd.batch [ cmd, urlCmd ] )
