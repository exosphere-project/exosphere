module Helpers.Interactivity exposing (updateServerInteractivity)

import Helpers.GetterSetters exposing (projectLookup, projectUpdateServer, serverLookup)
import OpenStack.Types exposing (ServerUuid)
import Types.HelperTypes exposing (ProjectIdentifier)
import Types.Interactivity exposing (InteractionLevel)
import Types.Project exposing (Project)
import Types.Server exposing (Server)
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg exposing (SharedMsg)


updateServerInteractivity : InteractionLevel -> ProjectIdentifier -> ServerUuid -> SharedModel -> ( SharedModel, Cmd SharedMsg )
updateServerInteractivity interactionLevel projectUuid serverUuid sharedModel =
    let
        maybeProject : Maybe Project
        maybeProject =
            projectLookup sharedModel projectUuid

        maybeServer : Maybe Server
        maybeServer =
            maybeProject |> Maybe.andThen (\project -> serverLookup project serverUuid)
    in
    case ( maybeProject, maybeServer ) of
        ( Just project, Just server ) ->
            ( Helpers.GetterSetters.modelUpdateProject sharedModel
                (projectUpdateServer project { server | interaction = interactionLevel })
            , Cmd.none
            )

        _ ->
            ( sharedModel, Cmd.none )
