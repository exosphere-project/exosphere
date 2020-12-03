module Rest.ApiModelHelpers exposing (pipelineCmd, requestNetworks, requestServer, requestServers)

import Helpers.GetterSetters as GetterSetters
import OpenStack.Types as OSTypes
import Rest.Neutron
import Rest.Nova
import Types.Types exposing (Model, Msg, ProjectIdentifier)



{- This module assists with making API calls that also require updating the model when the API call is placed. Typically, we set the resource to "loading" status while we wait for a response from the API. -}


requestServers : ProjectIdentifier -> Model -> ( Model, Cmd Msg )
requestServers projectUuid model =
    case GetterSetters.projectLookup model projectUuid of
        Just project ->
            ( project
                |> GetterSetters.projectSetServersLoading model.clientCurrentTime
                |> GetterSetters.modelUpdateProject model
            , Rest.Nova.requestServers project
            )

        Nothing ->
            ( model, Cmd.none )


requestServer : ProjectIdentifier -> OSTypes.ServerUuid -> Model -> ( Model, Cmd Msg )
requestServer projectUuid serverUuid model =
    case GetterSetters.projectLookup model projectUuid of
        Just project ->
            ( project
                |> (\p -> GetterSetters.projectSetServerLoading p serverUuid)
                |> GetterSetters.modelUpdateProject model
            , Rest.Nova.requestServer project serverUuid
            )

        Nothing ->
            ( model, Cmd.none )


requestNetworks : ProjectIdentifier -> Model -> ( Model, Cmd Msg )
requestNetworks projectUuid model =
    case GetterSetters.projectLookup model projectUuid of
        Just project ->
            ( project
                |> (\p -> GetterSetters.projectSetNetworksLoading model.clientCurrentTime p)
                |> GetterSetters.modelUpdateProject model
            , Rest.Neutron.requestNetworks project
            )

        Nothing ->
            ( model, Cmd.none )


pipelineCmd : (Model -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
pipelineCmd fn ( model, cmd ) =
    let
        ( newModel, newCmd ) =
            fn model
    in
    ( newModel, Cmd.batch [ cmd, newCmd ] )
