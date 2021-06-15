module Rest.ApiModelHelpers exposing
    ( requestAutoAllocatedNetwork
    , requestComputeQuota
    , requestFloatingIps
    , requestNetworks
    , requestPorts
    , requestServer
    , requestServers
    , requestVolumeQuota
    )

import Helpers.GetterSetters as GetterSetters
import OpenStack.Quotas
import OpenStack.Types as OSTypes
import RemoteData
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


requestAutoAllocatedNetwork : ProjectIdentifier -> Model -> ( Model, Cmd Msg )
requestAutoAllocatedNetwork projectUuid model =
    case GetterSetters.projectLookup model projectUuid of
        Just project ->
            ( project
                |> (\p -> GetterSetters.projectSetAutoAllocatedNetworkUuidLoading model.clientCurrentTime p)
                |> GetterSetters.modelUpdateProject model
            , Rest.Neutron.requestAutoAllocatedNetwork project
            )

        Nothing ->
            ( model, Cmd.none )


requestComputeQuota : ProjectIdentifier -> Model -> ( Model, Cmd Msg )
requestComputeQuota projectUuid model =
    case GetterSetters.projectLookup model projectUuid of
        Just project ->
            ( { project
                | computeQuota = RemoteData.Loading
              }
                |> GetterSetters.modelUpdateProject model
            , OpenStack.Quotas.requestComputeQuota project
            )

        Nothing ->
            ( model, Cmd.none )


requestVolumeQuota : ProjectIdentifier -> Model -> ( Model, Cmd Msg )
requestVolumeQuota projectUuid model =
    case GetterSetters.projectLookup model projectUuid of
        Just project ->
            ( { project
                | volumeQuota = RemoteData.Loading
              }
                |> GetterSetters.modelUpdateProject model
            , OpenStack.Quotas.requestVolumeQuota project
            )

        Nothing ->
            ( model, Cmd.none )


requestFloatingIps : ProjectIdentifier -> Model -> ( Model, Cmd Msg )
requestFloatingIps projectUuid model =
    case GetterSetters.projectLookup model projectUuid of
        Just project ->
            ( GetterSetters.projectSetFloatingIpsLoading model.clientCurrentTime project
                |> GetterSetters.modelUpdateProject model
            , Rest.Neutron.requestFloatingIps project
            )

        Nothing ->
            ( model, Cmd.none )


requestPorts : ProjectIdentifier -> Model -> ( Model, Cmd Msg )
requestPorts projectUuid model =
    case GetterSetters.projectLookup model projectUuid of
        Just project ->
            ( GetterSetters.projectSetPortsLoading model.clientCurrentTime project
                |> GetterSetters.modelUpdateProject model
            , Rest.Neutron.requestPorts project
            )

        Nothing ->
            ( model, Cmd.none )
