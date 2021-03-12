module Helpers.GetterSetters exposing
    ( flavorLookup
    , getExternalNetwork
    , getPublicEndpointFromService
    , getServerExouserPassword
    , getServerFloatingIp
    , getServersWithVolAttached
    , getServiceFromCatalog
    , getServicePublicUrl
    , getVolsAttachedToServer
    , imageLookup
    , modelUpdateProject
    , modelUpdateUnscopedProvider
    , projectDeleteServer
    , projectLookup
    , projectSetNetworksLoading
    , projectSetServerLoading
    , projectSetServersLoading
    , projectUpdateServer
    , providerLookup
    , serverLookup
    , sortedFlavors
    , userAppProxyLookup
    , volumeIsAttachedToServer
    , volumeLookup
    )

import Dict
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.Url as UrlHelpers
import OpenStack.Types as OSTypes
import RemoteData
import Time
import Types.HelperTypes as HelperTypes
import Types.Types exposing (Model, Project, ProjectIdentifier, Server, UnscopedProvider)
import View.Types



-- Getters, i.e. lookup functions
-- Primitive getters


serverLookup : Project -> OSTypes.ServerUuid -> Maybe Server
serverLookup project serverUuid =
    List.filter (\s -> s.osProps.uuid == serverUuid) (RDPP.withDefault [] project.servers) |> List.head


projectLookup : Model -> ProjectIdentifier -> Maybe Project
projectLookup model projectIdentifier =
    model.projects
        |> List.filter (\p -> p.auth.project.uuid == projectIdentifier)
        |> List.head


flavorLookup : Project -> OSTypes.FlavorUuid -> Maybe OSTypes.Flavor
flavorLookup project flavorUuid =
    List.filter
        (\f -> f.uuid == flavorUuid)
        project.flavors
        |> List.head


imageLookup : Project -> OSTypes.ImageUuid -> Maybe OSTypes.Image
imageLookup project imageUuid =
    List.filter
        (\i -> i.uuid == imageUuid)
        project.images
        |> List.head


volumeLookup : Project -> OSTypes.VolumeUuid -> Maybe OSTypes.Volume
volumeLookup project volumeUuid =
    List.filter
        (\v -> v.uuid == volumeUuid)
        (RemoteData.withDefault [] project.volumes)
        |> List.head


providerLookup : Model -> OSTypes.KeystoneUrl -> Maybe UnscopedProvider
providerLookup model keystoneUrl =
    List.filter
        (\uP -> uP.authUrl == keystoneUrl)
        model.unscopedProviders
        |> List.head



-- Slightly smarter getters


getServicePublicUrl : String -> OSTypes.ServiceCatalog -> Maybe HelperTypes.Url
getServicePublicUrl serviceType catalog =
    getServiceFromCatalog serviceType catalog
        |> Maybe.andThen getPublicEndpointFromService
        |> Maybe.map .url


getServiceFromCatalog : String -> OSTypes.ServiceCatalog -> Maybe OSTypes.Service
getServiceFromCatalog serviceType catalog =
    List.filter (\s -> s.type_ == serviceType) catalog
        |> List.head


getPublicEndpointFromService : OSTypes.Service -> Maybe OSTypes.Endpoint
getPublicEndpointFromService service =
    List.filter (\e -> e.interface == OSTypes.Public) service.endpoints
        |> List.head


getExternalNetwork : Project -> Maybe OSTypes.Network
getExternalNetwork project =
    case project.networks.data of
        RDPP.DoHave networks _ ->
            List.filter (\n -> n.isExternal) networks |> List.head

        RDPP.DontHave ->
            Nothing


getServerFloatingIp : List OSTypes.IpAddress -> Maybe String
getServerFloatingIp ipAddresses =
    let
        isFloating ipAddress =
            ipAddress.openstackType == OSTypes.IpAddressFloating
    in
    List.filter isFloating ipAddresses
        |> List.head
        |> Maybe.map .address


getServerExouserPassword : OSTypes.ServerDetails -> Maybe String
getServerExouserPassword serverDetails =
    let
        newLocation =
            List.filter (\t -> String.startsWith "exoPw:" t) serverDetails.tags
                |> List.head
                |> Maybe.map (String.dropLeft 6)

        oldLocation =
            List.filter (\i -> i.key == "exouserPassword") serverDetails.metadata
                |> List.head
                |> Maybe.map .value
    in
    case newLocation of
        Just password ->
            Just password

        Nothing ->
            oldLocation


sortedFlavors : List OSTypes.Flavor -> List OSTypes.Flavor
sortedFlavors flavors =
    flavors
        |> List.sortBy .disk_ephemeral
        |> List.sortBy .disk_root
        |> List.sortBy .ram_mb
        |> List.sortBy .vcpu


getVolsAttachedToServer : Project -> Server -> List OSTypes.Volume
getVolsAttachedToServer project server =
    project.volumes
        |> RemoteData.withDefault []
        |> List.filter (\v -> List.member v.uuid server.osProps.details.volumesAttached)


volumeIsAttachedToServer : OSTypes.VolumeUuid -> Server -> Bool
volumeIsAttachedToServer volumeUuid server =
    server.osProps.details.volumesAttached
        |> List.filter (\v -> v == volumeUuid)
        |> List.isEmpty
        |> not


getServersWithVolAttached : Project -> OSTypes.Volume -> List OSTypes.ServerUuid
getServersWithVolAttached _ volume =
    volume.attachments |> List.map .serverUuid



-- Setters, i.e. updater functions


modelUpdateProject : Model -> Project -> Model
modelUpdateProject model newProject =
    let
        otherProjects =
            List.filter (\p -> p.auth.project.uuid /= newProject.auth.project.uuid) model.projects

        newProjects =
            newProject :: otherProjects

        newProjectsSorted =
            newProjects
                |> List.sortBy (\p -> p.auth.project.name)
                |> List.sortBy (\p -> UrlHelpers.hostnameFromUrl p.endpoints.keystone)
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
                    List.sortBy (\s -> s.osProps.name) newServers

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


projectSetServersLoading : Time.Posix -> Project -> Project
projectSetServersLoading time project =
    { project | servers = RDPP.setLoading project.servers time }


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


projectSetNetworksLoading : Time.Posix -> Project -> Project
projectSetNetworksLoading time project =
    { project | networks = RDPP.setLoading project.networks time }


modelUpdateUnscopedProvider : Model -> UnscopedProvider -> Model
modelUpdateUnscopedProvider model newProvider =
    let
        otherProviders =
            List.filter
                (\p -> p.authUrl /= newProvider.authUrl)
                model.unscopedProviders

        newProviders =
            newProvider :: otherProviders

        newProvidersSorted =
            List.sortBy (\p -> p.authUrl) newProviders
    in
    { model | unscopedProviders = newProvidersSorted }


userAppProxyLookup : View.Types.Context -> Project -> Maybe Types.Types.UserAppProxyHostname
userAppProxyLookup context project =
    let
        projectKeystoneHostname =
            UrlHelpers.hostnameFromUrl project.endpoints.keystone
    in
    Dict.get projectKeystoneHostname context.cloudSpecificConfigs
        |> Maybe.andThen (\csc -> csc.userAppProxy)
