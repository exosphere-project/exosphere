module State.Navigation exposing (Navigation(..), classify, viewStateToRoute)

import Route
import State.LoginChain
import Types.View exposing (NonProjectViewConstructor(..), ProjectViewConstructor(..), ViewState(..))


type Navigation
    = SamePage
    | NewPage
    | NewPageAbandoningCredentialFileChain


classify : { credentialFileLoginChainIsActive : Bool } -> ViewState -> Route.Route -> Navigation
classify { credentialFileLoginChainIsActive } viewState route =
    if
        credentialFileLoginChainIsActive
            && State.LoginChain.shouldAbandon (isSelectingRegions viewState) (selectsRegions route)
    then
        NewPageAbandoningCredentialFileChain

    else if viewStateToRoute viewState == Just route then
        SamePage

    else
        NewPage


{-| The route that rebuilds the current page exactly as it stands, for the pages whose whole state
lives in their URL. Such a page mirrors each change into the URL with `Route.replaceUrl`, which
fires `onUrlChange` even for a URL it just set; recognising that echo is what spares the page a
rebuild and a refetch.

A page belongs here only if its route survives `Route.toUrl` then `Route.fromUrl` unchanged, or its
own echo would not be recognised. Every other page returns `Nothing` and is rebuilt on any URL
change, including the start-up placeholder view that `State.Init` navigates away from.

-}
viewStateToRoute : ViewState -> Maybe Route.Route
viewStateToRoute viewState =
    case viewState of
        NonProjectView (MessageLog model) ->
            Just (Route.MessageLog model.showDebugMsgs)

        NonProjectView _ ->
            Nothing

        ProjectView projectIdentifier projectViewConstructor ->
            Maybe.map (Route.ProjectRoute projectIdentifier) <|
                case projectViewConstructor of
                    FloatingIpAssign model ->
                        Just (Route.FloatingIpAssign model.ipUuid model.serverUuid)

                    ServerCreateImage model ->
                        Just (Route.ServerCreateImage model.serverUuid (Just model.imageName))

                    VolumeAttach model ->
                        Just (Route.VolumeAttach model.maybeServerUuid model.maybeVolumeUuid)

                    _ ->
                        Nothing


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
