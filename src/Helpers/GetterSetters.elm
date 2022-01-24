module Helpers.GetterSetters exposing
    ( cloudSpecificConfigLookup
    , flavorLookup
    , floatingIpLookup
    , getBootVolume
    , getCatalogRegions
    , getExternalNetwork
    , getFloatingIpServer
    , getServerExouserPassword
    , getServerFixedIps
    , getServerFloatingIps
    , getServerPorts
    , getServersWithVolAttached
    , getServicePublicUrl
    , getVolsAttachedToServer
    , imageLookup
    , isBootVolume
    , modelUpdateProject
    , modelUpdateUnscopedProvider
    , projectDeleteServer
    , projectIdentifier
    , projectLookup
    , projectSetAutoAllocatedNetworkUuidLoading
    , projectSetFloatingIpsLoading
    , projectSetNetworksLoading
    , projectSetPortsLoading
    , projectSetServerLoading
    , projectSetServersLoading
    , projectSetVolumesLoading
    , projectUpdateKeypair
    , projectUpdateServer
    , providerLookup
    , serverLookup
    , serverPresentNotDeleting
    , sortedFlavors
    , unscopedProjectLookup
    , unscopedProviderLookup
    , volDeviceToMountpoint
    , volumeDeviceRawName
    , volumeIsAttachedToServer
    , volumeLookup
    )

import Dict
import Helpers.List exposing (multiSortBy)
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.Url as UrlHelpers
import List.Extra
import OpenStack.Types as OSTypes
import RemoteData
import Time
import Types.HelperTypes as HelperTypes
import Types.Project exposing (Project)
import Types.Server exposing (Server)
import Types.SharedModel exposing (SharedModel)



-- Getters, i.e. lookup functions
-- Primitive getters


unscopedProviderLookup : SharedModel -> OSTypes.KeystoneUrl -> Maybe HelperTypes.UnscopedProvider
unscopedProviderLookup sharedModel keystoneUrl =
    sharedModel.unscopedProviders
        |> List.Extra.find (\provider -> provider.authUrl == keystoneUrl)


unscopedProjectLookup : HelperTypes.UnscopedProvider -> OSTypes.ProjectUuid -> Maybe HelperTypes.UnscopedProviderProject
unscopedProjectLookup provider projectUuid =
    provider.projectsAvailable
        |> RemoteData.withDefault []
        |> List.Extra.find (\project -> project.project.uuid == projectUuid)


serverLookup : Project -> OSTypes.ServerUuid -> Maybe Server
serverLookup project serverUuid =
    RDPP.withDefault [] project.servers
        |> List.Extra.find (\s -> s.osProps.uuid == serverUuid)


projectLookup : SharedModel -> HelperTypes.ProjectIdentifier -> Maybe Project
projectLookup model projectIdentifier_ =
    model.projects
        |> List.Extra.find
            (\p ->
                p.auth.project.uuid
                    == projectIdentifier_.projectUuid
                    && Maybe.map .id p.region
                    == projectIdentifier_.regionId
            )


projectIdentifier : Project -> HelperTypes.ProjectIdentifier
projectIdentifier project =
    HelperTypes.ProjectIdentifier project.auth.project.uuid (Maybe.map .id project.region)


flavorLookup : Project -> OSTypes.FlavorId -> Maybe OSTypes.Flavor
flavorLookup project flavorId =
    project.flavors
        |> List.Extra.find (\f -> f.id == flavorId)


imageLookup : Project -> OSTypes.ImageUuid -> Maybe OSTypes.Image
imageLookup project imageUuid =
    project.images
        |> List.Extra.find (\i -> i.uuid == imageUuid)


volumeLookup : Project -> OSTypes.VolumeUuid -> Maybe OSTypes.Volume
volumeLookup project volumeUuid =
    project.volumes
        |> RemoteData.withDefault []
        |> List.Extra.find (\v -> v.uuid == volumeUuid)


providerLookup : SharedModel -> OSTypes.KeystoneUrl -> Maybe HelperTypes.UnscopedProvider
providerLookup model keystoneUrl =
    model.unscopedProviders
        |> List.Extra.find (\uP -> uP.authUrl == keystoneUrl)


floatingIpLookup : Project -> OSTypes.IpAddressUuid -> Maybe OSTypes.FloatingIp
floatingIpLookup project ipUuid =
    project.floatingIps
        |> RDPP.withDefault []
        |> List.Extra.find (\i -> i.uuid == ipUuid)



-- Slightly smarter getters


getCatalogRegions : OSTypes.ServiceCatalog -> List OSTypes.RegionId
getCatalogRegions catalog =
    -- Given a service catalog, get a list of all region IDs that appear for at least one endpoint
    -- This allows administrators to restrict access to regions using the [OS-EP-FILTER](https://docs.openstack.org/api-ref/identity/v3-ext/#os-ep-filter-api) API.
    catalog
        |> List.concatMap .endpoints
        |> List.map .regionId


getServicePublicUrl : String -> OSTypes.ServiceCatalog -> Maybe HelperTypes.Url
getServicePublicUrl serviceType catalog =
    getServiceFromCatalog serviceType catalog
        |> Maybe.andThen getPublicEndpointFromService
        |> Maybe.map .url


getServiceFromCatalog : String -> OSTypes.ServiceCatalog -> Maybe OSTypes.Service
getServiceFromCatalog serviceType catalog =
    catalog
        |> List.Extra.find (\s -> s.type_ == serviceType)


getPublicEndpointFromService : OSTypes.Service -> Maybe OSTypes.Endpoint
getPublicEndpointFromService service =
    service.endpoints
        |> List.Extra.find (\e -> e.interface == OSTypes.Public)


getExternalNetwork : Project -> Maybe OSTypes.Network
getExternalNetwork project =
    case project.networks.data of
        RDPP.DoHave networks _ ->
            List.Extra.find .isExternal networks

        RDPP.DontHave ->
            Nothing


getServerPorts : Project -> OSTypes.ServerUuid -> List OSTypes.Port
getServerPorts project serverUuid =
    RDPP.withDefault [] project.ports
        |> List.filter (\port_ -> port_.deviceUuid == serverUuid)


getPortServer : Project -> OSTypes.Port -> Maybe Server
getPortServer project port_ =
    project.servers
        |> RDPP.withDefault []
        |> List.Extra.find (\server -> server.osProps.uuid == port_.deviceUuid)


getFloatingIpPort : Project -> OSTypes.FloatingIp -> Maybe OSTypes.Port
getFloatingIpPort project floatingIp =
    floatingIp.portUuid
        |> Maybe.andThen
            (\portUuid ->
                project.ports
                    |> RDPP.withDefault []
                    |> List.Extra.find (\p -> p.uuid == portUuid)
            )


getServerFixedIps : Project -> OSTypes.ServerUuid -> List OSTypes.IpAddressValue
getServerFixedIps project serverUuid =
    project.ports
        |> RDPP.withDefault []
        |> List.filter (\p -> p.deviceUuid == serverUuid)
        |> List.map .fixedIps
        |> List.concat


getServerFloatingIps : Project -> OSTypes.ServerUuid -> List OSTypes.FloatingIp
getServerFloatingIps project serverUuid =
    let
        serverPorts =
            getServerPorts project serverUuid
    in
    project.floatingIps
        |> RDPP.withDefault []
        |> List.filter
            (\ip ->
                case ip.portUuid of
                    Just portUuid ->
                        List.member portUuid (List.map .uuid serverPorts)

                    Nothing ->
                        False
            )


getFloatingIpServer : Project -> OSTypes.FloatingIp -> Maybe Server
getFloatingIpServer project ip =
    ip
        |> getFloatingIpPort project
        |> Maybe.andThen (getPortServer project)


getServerExouserPassword : OSTypes.ServerDetails -> Maybe String
getServerExouserPassword serverDetails =
    let
        newLocation =
            serverDetails.tags
                |> List.Extra.find (\t -> String.startsWith "exoPw:" t)
                |> Maybe.map (String.dropLeft 6)

        oldLocation =
            serverDetails.metadata
                |> List.Extra.find (\i -> i.key == "exouserPassword")
                |> Maybe.map .value
    in
    case newLocation of
        Just password ->
            Just password

        Nothing ->
            oldLocation


sortedFlavors : List OSTypes.Flavor -> List OSTypes.Flavor
sortedFlavors =
    multiSortBy [ .vcpu, .ram_mb, .disk_root, .disk_ephemeral ]


getVolsAttachedToServer : Project -> Server -> List OSTypes.Volume
getVolsAttachedToServer project server =
    project.volumes
        |> RemoteData.withDefault []
        |> List.filter (\v -> List.member v.uuid server.osProps.details.volumesAttached)


volumeIsAttachedToServer : OSTypes.VolumeUuid -> Server -> Bool
volumeIsAttachedToServer volumeUuid server =
    server.osProps.details.volumesAttached
        |> List.any (\v -> v == volumeUuid)


getServersWithVolAttached : Project -> OSTypes.Volume -> List OSTypes.ServerUuid
getServersWithVolAttached _ volume =
    volume.attachments |> List.map .serverUuid


volumeDeviceRawName : Server -> OSTypes.Volume -> Maybe OSTypes.VolumeAttachmentDevice
volumeDeviceRawName server volume =
    volume.attachments
        |> List.Extra.find (\a -> a.serverUuid == server.osProps.uuid)
        |> Maybe.map .device


isBootVolume : Maybe OSTypes.ServerUuid -> OSTypes.Volume -> Bool
isBootVolume maybeServerUuid volume =
    -- If a serverUuid is passed, determines whether volume backs that server; otherwise just determines whether volume backs any server
    volume.attachments
        |> List.filter
            (\a ->
                case maybeServerUuid of
                    Just serverUuid ->
                        a.serverUuid == serverUuid

                    Nothing ->
                        True
            )
        |> List.filter
            (\a ->
                List.member
                    a.device
                    [ "/dev/sda", "/dev/vda" ]
            )
        |> List.isEmpty
        |> not


getBootVolume : List OSTypes.Volume -> OSTypes.ServerUuid -> Maybe OSTypes.Volume
getBootVolume vols serverUuid =
    vols
        |> List.Extra.find (isBootVolume <| Just serverUuid)


volDeviceToMountpoint : OSTypes.VolumeAttachmentDevice -> Maybe String
volDeviceToMountpoint device =
    -- Converts e.g. "/dev/sdc" to "/media/volume/sdc"
    device
        |> String.split "/"
        |> List.reverse
        |> List.head
        |> Maybe.map (String.append "/media/volume/")


serverPresentNotDeleting : SharedModel -> OSTypes.ServerUuid -> Bool
serverPresentNotDeleting model serverUuid =
    let
        notDeletingServerUuids =
            model.projects
                |> List.concatMap (.servers >> RDPP.withDefault [])
                |> List.filter (\s -> not s.exoProps.deletionAttempted)
                |> List.map (.osProps >> .uuid)
    in
    List.member serverUuid notDeletingServerUuids



-- Setters, i.e. updater functions


modelUpdateProject : SharedModel -> Project -> SharedModel
modelUpdateProject model newProject =
    let
        otherProjects =
            List.filter (\p -> p.auth.project.uuid /= newProject.auth.project.uuid) model.projects

        newProjects =
            newProject :: otherProjects

        newProjectsSorted =
            multiSortBy
                [ \p -> UrlHelpers.hostnameFromUrl p.endpoints.keystone
                , \p -> p.auth.project.name
                ]
                newProjects
    in
    { model | projects = newProjectsSorted }


projectUpdateServer : Project -> Server -> Project
projectUpdateServer project server =
    case project.servers.data of
        RDPP.DontHave ->
            -- We don't do anything if we don't already have servers. Is this a silent failure that should be
            -- handled differently?
            project

        RDPP.DoHave servers recTime ->
            let
                otherServers =
                    List.filter
                        (\s -> s.osProps.uuid /= server.osProps.uuid)
                        servers

                newServers =
                    server :: otherServers

                newServersSorted =
                    -- Default sort order is by created timestamp, most recent first.
                    -- If those match, sort by name.
                    newServers
                        |> List.sortBy (\s -> s.osProps.name)
                        |> List.sortBy
                            (\s ->
                                Time.posixToMillis s.osProps.details.created
                                    |> negate
                            )

                oldServersRDPP =
                    project.servers

                newServersRDPP =
                    -- Should we update received time when we update a server? Thinking probably not given how this
                    -- function is actually used. We're generally updating exoProps, not osProps.
                    { oldServersRDPP | data = RDPP.DoHave newServersSorted recTime }
            in
            { project | servers = newServersRDPP }


projectDeleteServer : Project -> OSTypes.ServerUuid -> Project
projectDeleteServer project serverUuid =
    case project.servers.data of
        RDPP.DontHave ->
            project

        RDPP.DoHave servers recTime ->
            let
                otherServers =
                    List.filter
                        (\s -> s.osProps.uuid /= serverUuid)
                        servers

                oldServersRDPP =
                    project.servers

                newServersRDPP =
                    -- Should we update received time when we update a server? Thinking probably not given how this
                    -- function is actually used. We're generally updating exoProps, not osProps.
                    { oldServersRDPP | data = RDPP.DoHave otherServers recTime }
            in
            { project | servers = newServersRDPP }


projectSetServersLoading : Project -> Project
projectSetServersLoading project =
    { project | servers = RDPP.setLoading project.servers }


projectSetServerLoading : Project -> OSTypes.ServerUuid -> Project
projectSetServerLoading project serverUuid =
    case serverLookup project serverUuid of
        Nothing ->
            -- We can't do anything lol
            project

        Just server ->
            let
                oldExoProps =
                    server.exoProps

                newExoProps =
                    { oldExoProps
                        | loadingSeparately = True
                    }

                newServer =
                    { server | exoProps = newExoProps }
            in
            projectUpdateServer project newServer


projectSetVolumesLoading : Project -> Project
projectSetVolumesLoading project =
    { project | volumes = RemoteData.Loading }


projectSetNetworksLoading : Project -> Project
projectSetNetworksLoading project =
    { project | networks = RDPP.setLoading project.networks }


projectSetFloatingIpsLoading : Project -> Project
projectSetFloatingIpsLoading project =
    { project | floatingIps = RDPP.setLoading project.floatingIps }


projectSetPortsLoading : Project -> Project
projectSetPortsLoading project =
    { project | ports = RDPP.setLoading project.ports }


projectSetAutoAllocatedNetworkUuidLoading : Project -> Project
projectSetAutoAllocatedNetworkUuidLoading project =
    { project | autoAllocatedNetworkUuid = RDPP.setLoading project.autoAllocatedNetworkUuid }


modelUpdateUnscopedProvider : SharedModel -> HelperTypes.UnscopedProvider -> SharedModel
modelUpdateUnscopedProvider model newProvider =
    let
        otherProviders =
            List.filter
                (\p -> p.authUrl /= newProvider.authUrl)
                model.unscopedProviders

        newProviders =
            newProvider :: otherProviders

        newProvidersSorted =
            List.sortBy .authUrl newProviders
    in
    { model | unscopedProviders = newProvidersSorted }


cloudSpecificConfigLookup :
    Dict.Dict HelperTypes.KeystoneHostname HelperTypes.CloudSpecificConfig
    -> Project
    -> Maybe HelperTypes.CloudSpecificConfig
cloudSpecificConfigLookup cloudSpecificConfigs project =
    let
        projectKeystoneHostname =
            UrlHelpers.hostnameFromUrl project.endpoints.keystone
    in
    Dict.get projectKeystoneHostname cloudSpecificConfigs


projectUpdateKeypair : Project -> OSTypes.Keypair -> Project
projectUpdateKeypair project keypair =
    let
        otherKeypairs =
            project.keypairs
                |> RemoteData.withDefault []
                |> List.filter
                    (\k ->
                        k.fingerprint
                            /= keypair.fingerprint
                            && k.name
                            /= keypair.name
                    )

        keypairs =
            keypair
                :: otherKeypairs
                |> List.sortBy .name
    in
    { project | keypairs = RemoteData.Success keypairs }
