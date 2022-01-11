module Rest.ApiModelHelpers exposing
    ( requestAutoAllocatedNetwork
    , requestComputeQuota
    , requestFloatingIps
    , requestNetworkQuota
    , requestNetworks
    , requestPorts
    , requestServer
    , requestServers
    , requestVolumeQuota
    , requestVolumes
    )

import Helpers.GetterSetters as GetterSetters
import OpenStack.Quotas
import OpenStack.Types as OSTypes
import OpenStack.Volumes
import RemoteData
import Rest.Neutron
import Rest.Nova
import Types.HelperTypes exposing (ProjectIdentifier)
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg exposing (SharedMsg)



{- This module assists with making API calls that also require updating the model when the API call is placed. Typically, we set the resource to "loading" status while we wait for a response from the API. -}


requestServers : ProjectIdentifier -> SharedModel -> ( SharedModel, Cmd SharedMsg )
requestServers projectUuid model =
    case GetterSetters.projectLookup model projectUuid of
        Just project ->
            ( project
                |> GetterSetters.projectSetServersLoading
                |> GetterSetters.modelUpdateProject model
            , Rest.Nova.requestServers project
            )

        Nothing ->
            ( model, Cmd.none )


requestServer : ProjectIdentifier -> OSTypes.ServerUuid -> SharedModel -> ( SharedModel, Cmd SharedMsg )
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


requestVolumes : ProjectIdentifier -> SharedModel -> ( SharedModel, Cmd SharedMsg )
requestVolumes projectUuid model =
    case GetterSetters.projectLookup model projectUuid of
        Just project ->
            ( project
                |> GetterSetters.projectSetVolumesLoading
                |> GetterSetters.modelUpdateProject model
            , OpenStack.Volumes.requestVolumes project
            )

        Nothing ->
            ( model, Cmd.none )


requestNetworks : ProjectIdentifier -> SharedModel -> ( SharedModel, Cmd SharedMsg )
requestNetworks projectUuid model =
    case GetterSetters.projectLookup model projectUuid of
        Just project ->
            ( project
                |> GetterSetters.projectSetNetworksLoading
                |> GetterSetters.modelUpdateProject model
            , Rest.Neutron.requestNetworks project
            )

        Nothing ->
            ( model, Cmd.none )


requestAutoAllocatedNetwork : ProjectIdentifier -> SharedModel -> ( SharedModel, Cmd SharedMsg )
requestAutoAllocatedNetwork projectUuid model =
    case GetterSetters.projectLookup model projectUuid of
        Just project ->
            ( project
                |> GetterSetters.projectSetAutoAllocatedNetworkUuidLoading
                |> GetterSetters.modelUpdateProject model
            , Rest.Neutron.requestAutoAllocatedNetwork project
            )

        Nothing ->
            ( model, Cmd.none )


requestComputeQuota : ProjectIdentifier -> SharedModel -> ( SharedModel, Cmd SharedMsg )
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


requestVolumeQuota : ProjectIdentifier -> SharedModel -> ( SharedModel, Cmd SharedMsg )
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


requestNetworkQuota : ProjectIdentifier -> SharedModel -> ( SharedModel, Cmd SharedMsg )
requestNetworkQuota projectUuid model =
    case GetterSetters.projectLookup model projectUuid of
        Just project ->
            ( { project
                | networkQuota = RemoteData.Loading
              }
                |> GetterSetters.modelUpdateProject model
            , OpenStack.Quotas.requestNetworkQuota project
            )

        Nothing ->
            ( model, Cmd.none )


requestFloatingIps : ProjectIdentifier -> SharedModel -> ( SharedModel, Cmd SharedMsg )
requestFloatingIps projectUuid model =
    case GetterSetters.projectLookup model projectUuid of
        Just project ->
            ( GetterSetters.projectSetFloatingIpsLoading project
                |> GetterSetters.modelUpdateProject model
            , Rest.Neutron.requestFloatingIps project
            )

        Nothing ->
            ( model, Cmd.none )


requestPorts : ProjectIdentifier -> SharedModel -> ( SharedModel, Cmd SharedMsg )
requestPorts projectUuid model =
    case GetterSetters.projectLookup model projectUuid of
        Just project ->
            ( GetterSetters.projectSetPortsLoading project
                |> GetterSetters.modelUpdateProject model
            , Rest.Neutron.requestPorts project
            )

        Nothing ->
            ( model, Cmd.none )
