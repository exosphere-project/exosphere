module Rest.ApiModelHelpers exposing
    ( requestAllServerVolumeAttachments
    , requestAutoAllocatedNetwork
    , requestComputeQuota
    , requestFlavors
    , requestFloatingIps
    , requestImages
    , requestJetstream2Allocation
    , requestNetworkQuota
    , requestNetworks
    , requestPorts
    , requestRecordSets
    , requestSecurityGroups
    , requestServer
    , requestServerEvents
    , requestServerImageIfNotFound
    , requestServerSecurityGroups
    , requestServerVolumeAttachments
    , requestServers
    , requestShareAccessRules
    , requestShareExportLocations
    , requestShareQuotas
    , requestShares
    , requestVolumeQuota
    , requestVolumeSnapshots
    , requestVolumes
    )

import Helpers.GetterSetters as GetterSetters
import Helpers.RemoteDataPlusPlus as RDPP
import OpenStack.Quotas
import OpenStack.ServerVolumes
import OpenStack.Shares
import OpenStack.Types as OSTypes
import OpenStack.Volumes
import Rest.Designate
import Rest.Glance
import Rest.Jetstream2Accounting
import Rest.Neutron
import Rest.Nova
import Types.Error
import Types.HelperTypes exposing (ProjectIdentifier)
import Types.Interactivity exposing (InteractionLevel)
import Types.Project
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg exposing (SharedMsg)



{- This module assists with making API calls that also require updating the model when the API call is placed. Typically, we set the resource to "loading" status while we wait for a response from the API. -}


requestFlavors : ProjectIdentifier -> SharedModel -> ( SharedModel, Cmd SharedMsg )
requestFlavors projectUuid model =
    case GetterSetters.projectLookup model projectUuid of
        Nothing ->
            ( model, Cmd.none )

        Just project ->
            ( { project | flavors = RDPP.setLoading project.flavors }
                |> GetterSetters.modelUpdateProject model
            , Rest.Nova.requestFlavors project
            )


requestSecurityGroups : ProjectIdentifier -> SharedModel -> ( SharedModel, Cmd SharedMsg )
requestSecurityGroups projectUuid model =
    case GetterSetters.projectLookup model projectUuid of
        Just project ->
            case project.securityGroups.refreshStatus of
                -- Receiving security groups has a side-effect: ensuring the default exosphere security group exists.
                -- To avoid conflicts, we don't request security groups if they're already loading.
                RDPP.Loading ->
                    ( model, Cmd.none )

                _ ->
                    ( project
                        |> GetterSetters.projectSetSecurityGroupsLoading
                        |> GetterSetters.modelUpdateProject model
                    , Rest.Neutron.requestSecurityGroups project
                    )

        Nothing ->
            ( model, Cmd.none )


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


requestServer : ProjectIdentifier -> InteractionLevel -> OSTypes.ServerUuid -> SharedModel -> ( SharedModel, Cmd SharedMsg )
requestServer projectUuid interactionLevel serverUuid model =
    case GetterSetters.projectLookup model projectUuid of
        Just project ->
            ( project
                |> GetterSetters.projectSetServerLoading serverUuid
                |> GetterSetters.modelUpdateProject model
            , Rest.Nova.requestServer project interactionLevel serverUuid
            )

        Nothing ->
            ( model, Cmd.none )


{-| Requests server image if it's not found within project images
-}
requestServerImageIfNotFound : Types.Project.Project -> OSTypes.ServerUuid -> SharedModel -> ( SharedModel, Cmd SharedMsg )
requestServerImageIfNotFound project serverId model =
    case GetterSetters.serverLookup project serverId of
        Just server ->
            case GetterSetters.imageLookup project server.osProps.details.imageUuid of
                Nothing ->
                    ( model
                    , Rest.Glance.requestImage server.osProps.details.imageUuid
                        project
                        (Types.Error.ErrorContext
                            ("get an image \"" ++ server.osProps.details.imageUuid ++ "\"")
                            Types.Error.ErrorDebug
                            Nothing
                        )
                    )

                Just _ ->
                    ( model, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


requestServerEvents : ProjectIdentifier -> OSTypes.ServerUuid -> SharedModel -> ( SharedModel, Cmd SharedMsg )
requestServerEvents projectId serverUuid model =
    case GetterSetters.projectLookup model projectId of
        Just project ->
            ( project
                |> GetterSetters.projectSetServerEventsLoading serverUuid
                |> GetterSetters.modelUpdateProject model
            , Rest.Nova.requestServerEvents project serverUuid
            )

        Nothing ->
            ( model, Cmd.none )


requestServerVolumeAttachments : ProjectIdentifier -> OSTypes.ServerUuid -> SharedModel -> ( SharedModel, Cmd SharedMsg )
requestServerVolumeAttachments projectId serverUuid model =
    case GetterSetters.projectLookup model projectId of
        Just project ->
            ( project
                |> GetterSetters.projectSetServerVolumeAttachmentsLoading serverUuid
                |> GetterSetters.modelUpdateProject model
            , OpenStack.ServerVolumes.requestVolumeAttachments project serverUuid
            )

        Nothing ->
            ( model, Cmd.none )


requestAllServerVolumeAttachments : ProjectIdentifier -> SharedModel -> ( SharedModel, Cmd SharedMsg )
requestAllServerVolumeAttachments projectId model =
    case GetterSetters.projectLookup model projectId of
        Just project ->
            let
                serverUuids =
                    project.servers
                        |> RDPP.withDefault []
                        |> List.map (\server -> server.osProps.uuid)

                newProject =
                    serverUuids
                        |> List.filterMap
                            (\serverUuid ->
                                -- If the server volume attachment is already loading, we don't need to request it again.
                                if GetterSetters.getServerVolumeAttachments project serverUuid |> RDPP.isLoading then
                                    Nothing

                                else
                                    Just serverUuid
                            )
                        |> List.foldl
                            (\serverUuid accModel ->
                                accModel
                                    |> GetterSetters.projectSetServerVolumeAttachmentsLoading serverUuid
                            )
                            project

                newCmd =
                    serverUuids
                        |> List.map (OpenStack.ServerVolumes.requestVolumeAttachments newProject)
                        |> Cmd.batch
            in
            ( newProject |> GetterSetters.modelUpdateProject model
            , newCmd
            )

        Nothing ->
            ( model, Cmd.none )


requestServerSecurityGroups : ProjectIdentifier -> OSTypes.ServerUuid -> SharedModel -> ( SharedModel, Cmd SharedMsg )
requestServerSecurityGroups projectId serverUuid model =
    case GetterSetters.projectLookup model projectId of
        Just project ->
            ( project
                |> GetterSetters.projectSetServerSecurityGroupsLoading serverUuid
                |> GetterSetters.modelUpdateProject model
            , Rest.Nova.requestServerSecurityGroups project serverUuid
            )

        Nothing ->
            ( model, Cmd.none )


requestShares : ProjectIdentifier -> SharedModel -> ( SharedModel, Cmd SharedMsg )
requestShares projectUuid model =
    case GetterSetters.projectLookup model projectUuid of
        Just project ->
            case project.endpoints.manila of
                Just url ->
                    ( project
                        |> GetterSetters.projectSetSharesLoading
                        |> GetterSetters.modelUpdateProject model
                    , OpenStack.Shares.requestShares project url
                    )

                Nothing ->
                    ( model, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


requestShareAccessRules : ProjectIdentifier -> OSTypes.ShareUuid -> SharedModel -> ( SharedModel, Cmd SharedMsg )
requestShareAccessRules projectUuid shareUuid model =
    case GetterSetters.projectLookup model projectUuid of
        Just project ->
            case project.endpoints.manila of
                Just url ->
                    ( project
                        |> GetterSetters.projectSetShareAccessRulesLoading shareUuid
                        |> GetterSetters.modelUpdateProject model
                    , OpenStack.Shares.requestShareAccessRules project url shareUuid
                    )

                Nothing ->
                    ( model, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


requestShareQuotas : ProjectIdentifier -> SharedModel -> ( SharedModel, Cmd SharedMsg )
requestShareQuotas projectUuid model =
    case GetterSetters.projectLookup model projectUuid of
        Just project ->
            case project.endpoints.manila of
                Just url ->
                    ( { project
                        | shareQuota = RDPP.setLoading project.shareQuota
                      }
                        |> GetterSetters.modelUpdateProject model
                    , OpenStack.Quotas.requestShareQuota project url
                    )

                Nothing ->
                    ( model, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


requestShareExportLocations : ProjectIdentifier -> OSTypes.ShareUuid -> SharedModel -> ( SharedModel, Cmd SharedMsg )
requestShareExportLocations projectUuid shareUuid model =
    case GetterSetters.projectLookup model projectUuid of
        Just project ->
            case project.endpoints.manila of
                Just url ->
                    ( project
                        |> GetterSetters.projectSetShareExportLocationsLoading shareUuid
                        |> GetterSetters.modelUpdateProject model
                    , OpenStack.Shares.requestShareExportLocations project url shareUuid
                    )

                Nothing ->
                    ( model, Cmd.none )

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


requestVolumeSnapshots : ProjectIdentifier -> SharedModel -> ( SharedModel, Cmd SharedMsg )
requestVolumeSnapshots projectUuid model =
    case GetterSetters.projectLookup model projectUuid of
        Just project ->
            ( project
                |> GetterSetters.projectSetVolumeSnapshotsLoading
                |> GetterSetters.modelUpdateProject model
            , OpenStack.Volumes.requestVolumeSnapshots project
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
                | computeQuota = RDPP.setLoading project.computeQuota
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
                | volumeQuota = RDPP.setLoading project.volumeQuota
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
                | networkQuota = RDPP.setLoading project.networkQuota
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


requestRecordSets : ProjectIdentifier -> SharedModel -> ( SharedModel, Cmd SharedMsg )
requestRecordSets projectUuid model =
    case GetterSetters.projectLookup model projectUuid of
        Just project ->
            ( GetterSetters.projectSetDnsRecordSetsLoading project
                |> GetterSetters.modelUpdateProject model
            , Rest.Designate.requestRecordSets project
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



-- TODO rename all these arguments to `projectIdentifier`


requestImages : ProjectIdentifier -> SharedModel -> ( SharedModel, Cmd SharedMsg )
requestImages projectUuid model =
    case GetterSetters.projectLookup model projectUuid of
        Just project ->
            ( project
                |> GetterSetters.projectSetImagesLoading
                |> GetterSetters.modelUpdateProject model
            , Rest.Glance.requestImages model project
            )

        Nothing ->
            ( model, Cmd.none )


requestJetstream2Allocation : ProjectIdentifier -> SharedModel -> ( SharedModel, Cmd SharedMsg )
requestJetstream2Allocation projectIdentifier model =
    case GetterSetters.projectLookup model projectIdentifier of
        Just project ->
            case project.endpoints.jetstream2Accounting of
                Just accountingApiUrl ->
                    ( project
                        |> GetterSetters.projectSetJetstream2AllocationLoading
                        |> GetterSetters.modelUpdateProject model
                    , Rest.Jetstream2Accounting.requestAllocations project accountingApiUrl
                    )

                Nothing ->
                    ( model, Cmd.none )

        Nothing ->
            ( model, Cmd.none )
