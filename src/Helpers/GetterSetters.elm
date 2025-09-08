module Helpers.GetterSetters exposing
    ( DataDependent(..)
    , LoadingProgress(..)
    , cloudSpecificConfigLookup
    , defaultShareTypeName
    , flavorLookup
    , floatingIpLookup
    , getBootVolume
    , getCatalogRegionIds
    , getExternalNetwork
    , getFloatingIpServer
    , getSecurityGroupActions
    , getServerActionRequestQueue
    , getServerDnsRecordSets
    , getServerEvents
    , getServerExouserPassphrase
    , getServerFixedIps
    , getServerFlavorGroup
    , getServerFloatingIps
    , getServerPorts
    , getServerSecurityGroups
    , getServerUuidsByVolumeAttached
    , getServerVolumeActions
    , getServerVolumeAttachments
    , getServicePublicUrl
    , getUserAppProxyFromCloudSpecificConfig
    , getUserAppProxyFromContext
    , getVolsAttachedToServer
    , getVolumeAttachments
    , imageGetDesktopMessage
    , imageLookup
    , isDefaultSecurityGroup
    , isDefaultShareTypeSupported
    , isSnapshotOfVolume
    , isVolumeCurrentlyBackingServer
    , isVolumeReservedForShelvedInstance
    , modelUpdateProject
    , modelUpdateUnscopedProvider
    , projectAddSecurityGroupRule
    , projectDefaultSecurityGroup
    , projectDeleteSecurityGroup
    , projectDeleteSecurityGroupActions
    , projectDeleteSecurityGroupRule
    , projectDeleteServer
    , projectDeleteServerVolumeAction
    , projectIdentifier
    , projectLookup
    , projectRemoveServerActionRequestJob
    , projectSetAutoAllocatedNetworkUuidLoading
    , projectSetDnsRecordSetsLoading
    , projectSetFloatingIpsLoading
    , projectSetImagesLoading
    , projectSetJetstream2AllocationLoading
    , projectSetNetworksLoading
    , projectSetPortsLoading
    , projectSetSecurityGroupsLoading
    , projectSetServerEventsLoading
    , projectSetServerLoading
    , projectSetServerSecurityGroupsLoading
    , projectSetServerVolumeAttachmentsLoading
    , projectSetServersLoading
    , projectSetShareAccessRulesLoading
    , projectSetShareExportLocationsLoading
    , projectSetShareTypesLoading
    , projectSetSharesLoading
    , projectSetVolumeSnapshotsLoading
    , projectSetVolumesLoading
    , projectUpdateKeypair
    , projectUpdateSecurityGroup
    , projectUpdateSecurityGroupActionsIfExists
    , projectUpdateServer
    , projectUpsertSecurityGroupActions
    , projectUpsertServerSecurityGroups
    , projectUpsertServerVolumeAction
    , projectUpsertServerVolumeAttachments
    , sanitizeMountpoint
    , securityGroupLookup
    , securityGroupsFromServerSecurityGroups
    , serverAttachmentsForVolume
    , serverCreatedByCurrentUser
    , serverExoServerVersion
    , serverLookup
    , serverNameLookup
    , serverPresentNotDeleting
    , serverSupportsFeature
    , serversForSecurityGroup
    , shareLookup
    , sortedFlavors
    , sortedSecurityGroupRules
    , sortedSecurityGroups
    , transformRDPP
    , unscopedProjectLookup
    , unscopedProviderLookup
    , unscopedRegionLookup
    , updateProjectWithTransformer
    , updateSharedModelWithTransformer
    , volDeviceToMountpoint
    , volNameToMountpoint
    , volumeDeviceRawName
    , volumeIsAttachedToServer
    , volumeLookup
    )

import Dict
import Helpers.List exposing (multiSortBy)
import Helpers.Queue
import Helpers.RemoteDataPlusPlus as RDPP exposing (RemoteDataPlusPlus)
import Helpers.String exposing (toTitleCase)
import Helpers.Url as UrlHelpers
import List.Extra
import OpenStack.DnsRecordSet
import OpenStack.SecurityGroupRule as SecurityGroupRule
import OpenStack.Types as OSTypes
import OpenStack.VolumeSnapshots as VS
import Regex
import Time
import Types.Error exposing (HttpErrorWithBody)
import Types.HelperTypes as HelperTypes
import Types.Project exposing (Project)
import Types.SecurityGroupActions as SecurityGroupActions exposing (SecurityGroupAction)
import Types.Server exposing (ExoServerVersion, Server, ServerOrigin(..))
import Types.ServerActionRequestQueue exposing (ServerActionRequest, ServerActionRequestJob)
import Types.ServerVolumeActions exposing (ServerVolumeActionRequest)
import Types.SharedModel exposing (SharedModel)
import View.Types exposing (Context)



-- Getters, i.e. lookup functions
-- Primitive getters


unscopedProviderLookup : SharedModel -> OSTypes.KeystoneUrl -> Maybe HelperTypes.UnscopedProvider
unscopedProviderLookup sharedModel keystoneUrl =
    sharedModel.unscopedProviders
        |> List.Extra.find (\provider -> provider.authUrl == keystoneUrl)


unscopedProjectLookup : HelperTypes.UnscopedProvider -> OSTypes.ProjectUuid -> Maybe HelperTypes.UnscopedProviderProject
unscopedProjectLookup provider projectUuid =
    provider.projectsAvailable
        |> RDPP.withDefault []
        |> List.Extra.find (\project -> project.project.uuid == projectUuid)


unscopedRegionLookup : HelperTypes.UnscopedProvider -> OSTypes.RegionId -> Maybe OSTypes.Region
unscopedRegionLookup provider regionId =
    provider.regionsAvailable
        |> RDPP.withDefault []
        |> List.Extra.find (\region -> region.id == regionId)


securityGroupLookup : Project -> OSTypes.SecurityGroupUuid -> Maybe OSTypes.SecurityGroup
securityGroupLookup project securityGroupUuid =
    RDPP.withDefault [] project.securityGroups
        |> List.Extra.find (\s -> s.uuid == securityGroupUuid)


projectDefaultSecurityGroup : Context -> Project -> OSTypes.SecurityGroupTemplate
projectDefaultSecurityGroup context project =
    let
        cloudSpecificConfig =
            cloudSpecificConfigLookup context.cloudSpecificConfigs project

        cloudConfigSecurityGroups =
            case cloudSpecificConfig of
                Just config ->
                    Dict.values (Maybe.withDefault Dict.empty config.securityGroups)

                Nothing ->
                    []

        cloudConfigRegionSecurityGroup =
            cloudConfigSecurityGroups |> List.Extra.find (\sg -> sg.regionId == (project.region |> Maybe.map .id))

        cloudConfigSecurityGroup : Maybe OSTypes.SecurityGroupTemplate
        cloudConfigSecurityGroup =
            case cloudConfigRegionSecurityGroup of
                Just sg ->
                    Just sg

                Nothing ->
                    -- If there is no region-specific security group, use the first without a region id.
                    cloudConfigSecurityGroups |> List.Extra.find (\sg -> sg.regionId == Nothing)
    in
    case cloudConfigSecurityGroup of
        Just sg ->
            sg

        Nothing ->
            { name = "exosphere"
            , description = Just <| toTitleCase context.localization.securityGroup ++ " for " ++ Helpers.String.pluralize context.localization.virtualComputer ++ " launched via Exosphere"
            , regionId = Nothing
            , rules = SecurityGroupRule.defaultRules
            }


isDefaultSecurityGroup : Context -> Project -> { a | name : String } -> Bool
isDefaultSecurityGroup context project sg =
    let
        defaultSecurityGroup =
            projectDefaultSecurityGroup context project
    in
    sg.name == defaultSecurityGroup.name


securityGroupsFromServerSecurityGroups : Project -> List OSTypes.ServerSecurityGroup -> List OSTypes.SecurityGroup
securityGroupsFromServerSecurityGroups project serverSecurityGroups =
    serverSecurityGroups
        |> List.map .uuid
        |> List.filterMap (securityGroupLookup project)


type LoadingProgress
    = NotSure
    | Loading
    | Done


type DataDependent a
    = Uninitialised
    | Ready a


getServerVolumeActions : Project -> OSTypes.ServerUuid -> List ServerVolumeActionRequest
getServerVolumeActions project serverUuid =
    Dict.get serverUuid project.serverVolumeActions
        |> Maybe.withDefault []


getServerSecurityGroups : Project -> OSTypes.ServerUuid -> RDPP.RemoteDataPlusPlus HttpErrorWithBody (List OSTypes.ServerSecurityGroup)
getServerSecurityGroups project serverUuid =
    Dict.get serverUuid project.serverSecurityGroups
        |> Maybe.withDefault RDPP.empty


serversForSecurityGroup : Project -> OSTypes.SecurityGroupUuid -> { servers : List Server, progress : LoadingProgress }
serversForSecurityGroup project securityGroupUuid =
    case project.servers.data of
        RDPP.DoHave servers _ ->
            let
                -- It may take a moment for all the server security groups to load.
                -- Wait until all the data is available before reporting the list.
                progress =
                    if Dict.values project.serverSecurityGroups |> List.all RDPP.gotData then
                        Done

                    else
                        Loading

                linked =
                    servers
                        |> List.filterMap
                            (\server ->
                                let
                                    serverSecurityGroups =
                                        getServerSecurityGroups project server.osProps.uuid
                                            |> RDPP.withDefault []

                                    hasSecurityGroup sgs uuid =
                                        sgs
                                            |> List.any (\sg -> sg.uuid == uuid)
                                in
                                if hasSecurityGroup serverSecurityGroups securityGroupUuid then
                                    Just server

                                else
                                    Nothing
                            )
            in
            { servers = linked, progress = progress }

        RDPP.DontHave ->
            { servers = [], progress = NotSure }


serverLookup : Project -> OSTypes.ServerUuid -> Maybe Server
serverLookup project serverUuid =
    RDPP.withDefault [] project.servers
        |> List.Extra.find (\s -> s.osProps.uuid == serverUuid)


serverNameLookup : Project -> OSTypes.ServerUuid -> Maybe String
serverNameLookup project serverUuid =
    serverLookup project serverUuid
        |> Maybe.map (\server -> server.osProps.name)


serverExoServerVersion : Server -> Maybe ExoServerVersion
serverExoServerVersion server =
    case server.exoProps.serverOrigin of
        ServerFromExo props ->
            Just props.exoServerVersion

        _ ->
            Nothing


serverSupportsFeature : Types.Server.ExoFeature -> Server -> Bool
serverSupportsFeature feature server =
    case serverExoServerVersion server of
        Just v ->
            Types.Server.exoVersionSupportsFeature feature v

        Nothing ->
            False


shareLookup : Project -> OSTypes.ShareUuid -> Maybe OSTypes.Share
shareLookup project shareUuid =
    RDPP.withDefault [] project.shares
        |> List.Extra.find (\s -> s.uuid == shareUuid)


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
        |> RDPP.withDefault []
        |> List.Extra.find (\f -> f.id == flavorId)


imageLookup : Project -> OSTypes.ImageUuid -> Maybe OSTypes.Image
imageLookup project imageUuid =
    RDPP.withDefault [] project.images
        |> List.append project.serverImages
        |> List.Extra.find (\i -> i.uuid == imageUuid)


imageGetDesktopMessage : OSTypes.Image -> Maybe String
imageGetDesktopMessage image =
    image.additionalProperties
        |> Dict.get "exoDesktopMessage"


volumeLookup : Project -> OSTypes.VolumeUuid -> Maybe OSTypes.Volume
volumeLookup project volumeUuid =
    project.volumes
        |> RDPP.withDefault []
        |> List.Extra.find (\v -> v.uuid == volumeUuid)


isSnapshotOfVolume : OSTypes.Volume -> VS.VolumeSnapshot -> Bool
isSnapshotOfVolume { uuid } { volumeId } =
    uuid == volumeId


floatingIpLookup : Project -> OSTypes.IpAddressUuid -> Maybe OSTypes.FloatingIp
floatingIpLookup project ipUuid =
    project.floatingIps
        |> RDPP.withDefault []
        |> List.Extra.find (\i -> i.uuid == ipUuid)



-- Slightly smarter getters


getCatalogRegionIds : OSTypes.ServiceCatalog -> List OSTypes.RegionId
getCatalogRegionIds catalog =
    -- Given a service catalog, get a list of all region IDs that appear for at least one endpoint
    -- This allows administrators to restrict access to regions using the [OS-EP-FILTER](https://docs.openstack.org/api-ref/identity/v3-ext/#os-ep-filter-api) API.
    catalog
        |> List.concatMap .endpoints
        |> List.map .regionId
        |> List.Extra.unique


getServicePublicUrl : OSTypes.ServiceCatalog -> Maybe OSTypes.RegionId -> String -> Maybe HelperTypes.Url
getServicePublicUrl catalog maybeRegionId serviceType =
    getServiceFromCatalog serviceType catalog
        |> Maybe.andThen (getPublicEndpointFromService maybeRegionId)
        |> Maybe.map .url


getServiceFromCatalog : String -> OSTypes.ServiceCatalog -> Maybe OSTypes.Service
getServiceFromCatalog serviceType catalog =
    catalog
        |> List.Extra.find (\s -> s.type_ == serviceType)


getPublicEndpointFromService : Maybe OSTypes.RegionId -> OSTypes.Service -> Maybe OSTypes.Endpoint
getPublicEndpointFromService maybeRegionId service =
    service.endpoints
        |> List.Extra.find
            (\e ->
                e.interface
                    == OSTypes.Public
                    && (case maybeRegionId of
                            Just regionId ->
                                e.regionId == regionId

                            Nothing ->
                                True
                       )
            )


getExternalNetwork : Project -> Maybe OSTypes.Network
getExternalNetwork project =
    case project.networks.data of
        RDPP.DoHave networks _ ->
            List.Extra.find .isExternal networks

        RDPP.DontHave ->
            Nothing


serverCreatedByCurrentUser : Project -> OSTypes.ServerUuid -> Maybe Bool
serverCreatedByCurrentUser project serverUuid =
    serverLookup project serverUuid
        |> Maybe.map (\s -> s.osProps.details.userUuid == project.auth.user.uuid)


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
        |> List.concatMap .fixedIps


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


{-| Get a list of all DNS record sets for a given server
-}
getServerDnsRecordSets : Project -> OSTypes.ServerUuid -> List OpenStack.DnsRecordSet.DnsRecordSet
getServerDnsRecordSets project uuid =
    getServerFloatingIps project uuid
        -- Get the DnsRecordSets matching this address
        |> List.concatMap
            (\{ address } ->
                OpenStack.DnsRecordSet.lookupRecordsByAddress
                    (RDPP.withDefault [] project.dnsRecordSets)
                    address
            )


getServerExouserPassphrase : OSTypes.ServerDetails -> Maybe String
getServerExouserPassphrase serverDetails =
    let
        newLocation =
            serverDetails.tags
                |> List.Extra.find (\t -> String.startsWith "exoPw:" t)
                |> Maybe.map (String.dropLeft 6)
    in
    case newLocation of
        Just passphrase ->
            Just passphrase

        Nothing ->
            serverDetails.metadata
                |> List.Extra.find (\i -> i.key == "exouserPassword")
                |> Maybe.map .value


sortedFlavors : List OSTypes.Flavor -> List OSTypes.Flavor
sortedFlavors =
    multiSortBy [ .vcpu, .ram_mb, .disk_root, .disk_ephemeral ]


sortedSecurityGroups : List OSTypes.SecurityGroup -> List OSTypes.SecurityGroup
sortedSecurityGroups =
    List.sortBy (\sg -> sg.name |> String.toLower)


sortedSecurityGroupRules : (OSTypes.SecurityGroupUuid -> Maybe OSTypes.SecurityGroup) -> List SecurityGroupRule.SecurityGroupRule -> List SecurityGroupRule.SecurityGroupRule
sortedSecurityGroupRules remoteSecurityGroupLookup =
    multiSortBy
        [ \item -> SecurityGroupRule.directionToString item.direction
        , \item -> SecurityGroupRule.etherTypeToString item.ethertype
        , \item -> SecurityGroupRule.protocolToString (Maybe.withDefault SecurityGroupRule.AnyProtocol item.protocol)
        , \item ->
            case SecurityGroupRule.portRangeToString item of
                -- List "Any" port range first.
                "Any" ->
                    ""

                str ->
                    str
        , \item ->
            case ( item.remoteIpPrefix, item.remoteGroupUuid ) of
                ( Just ipPrefix, _ ) ->
                    ipPrefix

                ( _, Just remoteGroupUuid ) ->
                    case remoteSecurityGroupLookup remoteGroupUuid of
                        Just securityGroup ->
                            case securityGroup.name of
                                "" ->
                                    securityGroup.uuid

                                name ->
                                    name

                        Nothing ->
                            remoteGroupUuid

                ( Nothing, Nothing ) ->
                    case item.ethertype of
                        SecurityGroupRule.Ipv4 ->
                            "0.0.0.0/0"

                        SecurityGroupRule.Ipv6 ->
                            "::/0"

                        _ ->
                            "-"
        ]


getFlavorFlavorGroup : OSTypes.Flavor -> List HelperTypes.FlavorGroup -> Maybe HelperTypes.FlavorGroup
getFlavorFlavorGroup flavor groups =
    let
        regex group =
            Regex.fromString group.matchOn
                |> Maybe.withDefault Regex.never
    in
    groups
        |> List.filter (\g -> Regex.contains (regex g) flavor.name)
        |> List.head


getServerFlavorGroup : Project -> View.Types.Context -> Server -> Maybe HelperTypes.FlavorGroup
getServerFlavorGroup project context server =
    let
        maybeServerFlavor =
            flavorLookup project server.osProps.details.flavorId

        maybeFlavorGroups =
            cloudSpecificConfigLookup context.cloudSpecificConfigs project
                |> Maybe.map .flavorGroups
    in
    case ( maybeServerFlavor, maybeFlavorGroups ) of
        ( Just flavor, Just flavorGroups ) ->
            getFlavorFlavorGroup flavor flavorGroups

        _ ->
            Nothing


getVolsAttachedToServer : Project -> Server -> List OSTypes.Volume
getVolsAttachedToServer project server =
    project.volumes
        |> RDPP.withDefault []
        |> List.filter (\v -> List.member v.uuid server.osProps.details.volumesAttached)


getVolumeAttachments : Project -> OSTypes.VolumeUuid -> List OSTypes.VolumeAttachment
getVolumeAttachments project volumeUuid =
    project.serverVolumeAttachments
        |> Dict.values
        |> List.concatMap (RDPP.withDefault [])
        |> List.filter (\a -> a.volumeUuid == volumeUuid)


serverAttachmentsForVolume : Project -> OSTypes.VolumeUuid -> { attachments : List OSTypes.VolumeAttachment, progress : LoadingProgress }
serverAttachmentsForVolume project volumeUuid =
    let
        progress =
            -- Attachment fetching depends on the servers list.
            if project.servers.data == RDPP.DontHave then
                NotSure

            else if Dict.values project.serverVolumeAttachments |> List.all RDPP.gotData then
                -- If all server volume attachments have data, then we're done.
                -- Otherwise, we're still loading.
                Done

            else
                Loading

        attached =
            project.serverVolumeAttachments
                |> Dict.values
                |> List.concatMap (RDPP.withDefault [])
                |> List.filter (\a -> a.volumeUuid == volumeUuid)
    in
    { attachments = attached, progress = progress }


volumeIsAttachedToServer : OSTypes.VolumeUuid -> Server -> Bool
volumeIsAttachedToServer volumeUuid server =
    server.osProps.details.volumesAttached
        |> List.member volumeUuid


getServerUuidsByVolumeAttached : Project -> OSTypes.VolumeUuid -> List OSTypes.ServerUuid
getServerUuidsByVolumeAttached project volumeUuid =
    getVolumeAttachments project volumeUuid
        |> List.map .serverUuid
        |> List.Extra.unique


volumeDeviceRawName : Project -> Server -> OSTypes.VolumeUuid -> OSTypes.VolumeAttachmentDevice
volumeDeviceRawName project server volumeUuid =
    getServerVolumeAttachments project server.osProps.uuid
        |> RDPP.withDefault []
        |> List.Extra.find (\a -> a.volumeUuid == volumeUuid)
        |> Maybe.map .device
        |> Maybe.withDefault Nothing


{-| If a `serverUuid` is passed, determines whether volume backs that server;
otherwise just determines whether volume is backing any server.
-}
isVolumeCurrentlyBackingServer : Project -> Maybe OSTypes.ServerUuid -> OSTypes.Volume -> Bool
isVolumeCurrentlyBackingServer project maybeServerUuid volume =
    project.serverVolumeAttachments
        |> Dict.filter
            (\serverId _ ->
                maybeServerUuid
                    |> Maybe.map (\s -> s == serverId)
                    |> Maybe.withDefault True
            )
        |> Dict.filter
            (\_ rdpp ->
                RDPP.withDefault [] rdpp
                    |> List.Extra.find (\a -> a.volumeUuid == volume.uuid)
                    |> Maybe.andThen .device
                    |> Maybe.withDefault ""
                    |> (\device -> List.member device [ "/dev/sda", "/dev/vda" ])
            )
        |> Dict.keys
        |> List.isEmpty
        |> not


getBootVolume : Project -> OSTypes.ServerUuid -> Maybe OSTypes.Volume
getBootVolume project serverUuid =
    project.volumes
        |> RDPP.withDefault []
        |> List.Extra.find (isVolumeCurrentlyBackingServer project <| Just serverUuid)


isVolumeReservedForShelvedInstance : Project -> OSTypes.Volume -> Bool
isVolumeReservedForShelvedInstance project volume =
    case volume.status of
        OSTypes.Reserved ->
            let
                maybeServerUuid =
                    -- Reserved volumes don't necessarily know their attachments but servers do.
                    List.head <| getServerUuidsByVolumeAttached project volume.uuid

                maybeServer =
                    maybeServerUuid |> Maybe.andThen (serverLookup project)
            in
            maybeServer
                |> Maybe.map (\s -> s.osProps.details.openstackStatus)
                |> Maybe.map (\status -> [ OSTypes.ServerShelved, OSTypes.ServerShelvedOffloaded ] |> List.member status)
                |> Maybe.withDefault False

        _ ->
            False


sanitizeMountpoint : String -> String
sanitizeMountpoint =
    Regex.replace
        (Regex.fromString "\\W+" |> Maybe.withDefault Regex.never)
        (always "-")


volNameToMountpoint : OSTypes.VolumeName -> Maybe String
volNameToMountpoint volName =
    Just <| "/media/volume/" ++ sanitizeMountpoint volName


volDeviceToMountpoint : OSTypes.VolumeAttachmentDevice -> Maybe String
volDeviceToMountpoint device =
    -- Converts e.g. "/dev/sdc" to "/media/volume/sdc", for exoServerVersion < 5
    device
        |> Maybe.andThen
            (\d ->
                d
                    |> String.split "/"
                    |> List.reverse
                    |> List.head
                    |> Maybe.map (String.append "/media/volume/")
            )


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


getUserAppProxyFromContext : Project -> View.Types.Context -> Maybe HelperTypes.UserAppProxyHostname
getUserAppProxyFromContext project context =
    let
        projectKeystoneHostname =
            UrlHelpers.hostnameFromUrl project.endpoints.keystone

        getCloudSpecificConfig : Maybe HelperTypes.CloudSpecificConfig
        getCloudSpecificConfig =
            Dict.get projectKeystoneHostname context.cloudSpecificConfigs
    in
    getCloudSpecificConfig
        |> Maybe.andThen (getUserAppProxyFromCloudSpecificConfig project)


getUserAppProxyFromCloudSpecificConfig : Project -> HelperTypes.CloudSpecificConfig -> Maybe HelperTypes.UserAppProxyHostname
getUserAppProxyFromCloudSpecificConfig project cloudSpecificConfig =
    let
        getUapHostname : List HelperTypes.UserAppProxyConfig -> Maybe HelperTypes.UserAppProxyHostname
        getUapHostname uapConfig =
            let
                hostnameFromMaybeRegionId maybeRegionId =
                    uapConfig
                        |> List.filter (\configItem -> configItem.regionId == maybeRegionId)
                        |> List.head
                        |> Maybe.map .hostname
            in
            case hostnameFromMaybeRegionId (Maybe.map .id project.region) of
                Just hostname ->
                    Just hostname

                Nothing ->
                    hostnameFromMaybeRegionId Nothing
    in
    cloudSpecificConfig.userAppProxy
        |> Maybe.andThen getUapHostname



-- Setters, i.e. updater functions


{-| JC Added transformRDPP

    Transform the contents of an RDPP value if it has any contents.

-}
transformRDPP : (a -> a) -> RDPP.RemoteDataPlusPlus Types.Error.HttpErrorWithBody a -> RDPP.RemoteDataPlusPlus Types.Error.HttpErrorWithBody a
transformRDPP transform rdpp =
    case rdpp.data of
        RDPP.DontHave ->
            rdpp

        RDPP.DoHave data receivedTime ->
            { data = RDPP.DoHave (transform data) receivedTime, refreshStatus = rdpp.refreshStatus }


{-| Update the given project with a transformer : Image -> Image
-}
updateProjectWithTransformer : (OSTypes.Image -> OSTypes.Image) -> Project -> Project
updateProjectWithTransformer transformer project_ =
    case project_.images.data of
        RDPP.DoHave images_ t ->
            { project_
                | images =
                    { data = RDPP.DoHave (List.map transformer images_) t
                    , refreshStatus = RDPP.NotLoading Nothing
                    }
            }

        RDPP.DontHave ->
            project_


{-| Update the given shared model with a transformer : Project -> Project
-}
updateSharedModelWithTransformer : (Project -> Project) -> SharedModel -> SharedModel
updateSharedModelWithTransformer transformer sharedModel_ =
    { sharedModel_ | projects = List.map transformer sharedModel_.projects }


modelUpdateProject : SharedModel -> Project -> SharedModel
modelUpdateProject model newProject =
    let
        otherProjects =
            List.filter (\p -> projectIdentifier p /= projectIdentifier newProject) model.projects

        newProjects =
            newProject :: otherProjects

        newProjectsSortedByRegion =
            List.sortBy
                (\p ->
                    case p.region of
                        Nothing ->
                            ""

                        Just region ->
                            region.id
                )
                newProjects

        newProjectsSorted =
            multiSortBy
                [ \p -> UrlHelpers.hostnameFromUrl p.endpoints.keystone
                , \p -> p.auth.project.name
                ]
                newProjectsSortedByRegion
    in
    { model | projects = newProjectsSorted }


projectUpdateServer : Project -> Server -> Project
projectUpdateServer project server =
    let
        { data, refreshStatus } =
            project.servers
    in
    case data of
        RDPP.DontHave ->
            -- To prevent dropping server data on the floor (e.g. if we requested a particular server before all servers),
            -- we initialise just one in the list & take received time as the start of the epoch.
            let
                newData =
                    RDPP.DoHave [ server ] (Time.millisToPosix 0)

                servers =
                    -- Preserve the current loading state.
                    RemoteDataPlusPlus newData refreshStatus
            in
            { project | servers = servers }

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


projectDeleteSecurityGroup : Project -> OSTypes.SecurityGroupUuid -> Project
projectDeleteSecurityGroup project securityGroupUuid =
    case project.securityGroups.data of
        RDPP.DontHave ->
            project

        RDPP.DoHave securityGroups recTime ->
            let
                otherSecurityGroups =
                    List.filter
                        (\s -> s.uuid /= securityGroupUuid)
                        securityGroups

                oldSecurityGroupsRDPP =
                    project.securityGroups

                newSecurityGroupsRDPP =
                    { oldSecurityGroupsRDPP | data = RDPP.DoHave otherSecurityGroups recTime }
            in
            { project | securityGroups = newSecurityGroupsRDPP }


projectSetSecurityGroupsLoading : Project -> Project
projectSetSecurityGroupsLoading project =
    { project | securityGroups = RDPP.setLoading project.securityGroups }


projectSetServersLoading : Project -> Project
projectSetServersLoading project =
    { project | servers = RDPP.setLoading project.servers }


projectSetServerLoading : OSTypes.ServerUuid -> Project -> Project
projectSetServerLoading serverUuid project =
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


projectSetServerEventsLoading : OSTypes.ServerUuid -> Project -> Project
projectSetServerEventsLoading serverId project =
    { project
        | serverEvents =
            Dict.update serverId
                (\entry ->
                    case entry of
                        Just serverEvents ->
                            Just (RDPP.setLoading serverEvents)

                        Nothing ->
                            Just (RDPP.setLoading RDPP.empty)
                )
                project.serverEvents
    }


projectSetServerSecurityGroupsLoading : OSTypes.ServerUuid -> Project -> Project
projectSetServerSecurityGroupsLoading serverId project =
    { project
        | serverSecurityGroups =
            Dict.update serverId
                (\entry ->
                    case entry of
                        Just serverSecurityGroups ->
                            Just (RDPP.setLoading serverSecurityGroups)

                        Nothing ->
                            Just (RDPP.setLoading RDPP.empty)
                )
                project.serverSecurityGroups
    }


projectSetServerVolumeAttachmentsLoading : OSTypes.ServerUuid -> Project -> Project
projectSetServerVolumeAttachmentsLoading serverId project =
    { project
        | serverVolumeAttachments =
            Dict.update serverId
                (\entry ->
                    case entry of
                        Just serverVolumeAttachments ->
                            Just (RDPP.setLoading serverVolumeAttachments)

                        Nothing ->
                            Just (RDPP.setLoading RDPP.empty)
                )
                project.serverVolumeAttachments
    }


projectSetSharesLoading : Project -> Project
projectSetSharesLoading project =
    { project | shares = RDPP.setLoading project.shares }


projectSetShareTypesLoading : Project -> Project
projectSetShareTypesLoading project =
    { project | shareTypes = RDPP.setLoading project.shareTypes }


projectSetShareAccessRulesLoading : OSTypes.ShareUuid -> Project -> Project
projectSetShareAccessRulesLoading shareUuid project =
    { project
        | shareAccessRules =
            Dict.update shareUuid
                (\entry ->
                    case entry of
                        Just accessRules ->
                            Just (RDPP.setLoading accessRules)

                        Nothing ->
                            Just (RDPP.setLoading RDPP.empty)
                )
                project.shareAccessRules
    }


projectSetShareExportLocationsLoading : OSTypes.ShareUuid -> Project -> Project
projectSetShareExportLocationsLoading shareUuid project =
    { project
        | shareExportLocations =
            Dict.update shareUuid
                (\entry ->
                    case entry of
                        Just exportLocations ->
                            Just (RDPP.setLoading exportLocations)

                        Nothing ->
                            Just (RDPP.setLoading RDPP.empty)
                )
                project.shareExportLocations
    }


defaultShareTypeName : OSTypes.ShareTypeName
defaultShareTypeName =
    OSTypes.defaultShareTypeNameForProtocol OSTypes.CephFS


isDefaultShareTypeSupported : Project -> DataDependent Bool
isDefaultShareTypeSupported project =
    case project.shareTypes.data of
        RDPP.DoHave shareTypes _ ->
            Ready <|
                List.any
                    (\st -> st.name == defaultShareTypeName)
                    shareTypes

        RDPP.DontHave ->
            Uninitialised


projectSetVolumesLoading : Project -> Project
projectSetVolumesLoading project =
    { project | volumes = RDPP.setLoading project.volumes }


projectSetVolumeSnapshotsLoading : Project -> Project
projectSetVolumeSnapshotsLoading project =
    { project | volumeSnapshots = RDPP.setLoading project.volumeSnapshots }


projectSetNetworksLoading : Project -> Project
projectSetNetworksLoading project =
    { project | networks = RDPP.setLoading project.networks }


projectSetFloatingIpsLoading : Project -> Project
projectSetFloatingIpsLoading project =
    { project | floatingIps = RDPP.setLoading project.floatingIps }


projectSetDnsRecordSetsLoading : Project -> Project
projectSetDnsRecordSetsLoading project =
    { project | dnsRecordSets = RDPP.setLoading project.dnsRecordSets }


projectSetPortsLoading : Project -> Project
projectSetPortsLoading project =
    { project | ports = RDPP.setLoading project.ports }


projectSetAutoAllocatedNetworkUuidLoading : Project -> Project
projectSetAutoAllocatedNetworkUuidLoading project =
    { project | autoAllocatedNetworkUuid = RDPP.setLoading project.autoAllocatedNetworkUuid }


projectSetImagesLoading : Project -> Project
projectSetImagesLoading project =
    { project | images = RDPP.setLoading project.images }


projectSetJetstream2AllocationLoading : Project -> Project
projectSetJetstream2AllocationLoading project =
    { project | jetstream2Allocations = RDPP.setLoading project.jetstream2Allocations }


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


projectUpdateKeypair : SharedModel -> Project -> OSTypes.Keypair -> Project
projectUpdateKeypair sharedModel project keypair =
    let
        otherKeypairs =
            project.keypairs
                |> RDPP.withDefault []
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
    { project
        | keypairs =
            RDPP.setData
                (RDPP.DoHave keypairs sharedModel.clientCurrentTime)
                project.keypairs
    }


projectUpsertServerSecurityGroups : Project -> OSTypes.ServerUuid -> RDPP.RemoteDataPlusPlus HttpErrorWithBody (List OSTypes.ServerSecurityGroup) -> Project
projectUpsertServerSecurityGroups project serverUuid serverSecurityGroups =
    { project
        | serverSecurityGroups =
            Dict.insert serverUuid
                serverSecurityGroups
                project.serverSecurityGroups
    }


projectUpdateSecurityGroup : Project -> OSTypes.SecurityGroup -> Project
projectUpdateSecurityGroup project securityGroup =
    let
        { data, refreshStatus } =
            project.securityGroups

        newSecurityGroups =
            case data of
                -- Preserve the current loading state of security groups.
                RDPP.DoHave groups receivedTime ->
                    let
                        otherSecurityGroups =
                            groups
                                |> List.filter
                                    (\sg ->
                                        sg.uuid
                                            /= securityGroup.uuid
                                    )

                        securityGroups =
                            securityGroup
                                :: otherSecurityGroups
                    in
                    RDPP.RemoteDataPlusPlus
                        (RDPP.DoHave securityGroups receivedTime)
                        refreshStatus

                _ ->
                    project.securityGroups
    in
    { project
        | securityGroups =
            newSecurityGroups
    }


projectAddSecurityGroupRule : Project -> OSTypes.SecurityGroupUuid -> SecurityGroupRule.SecurityGroupRule -> Project
projectAddSecurityGroupRule project securityGroupUuid rule =
    projectUpdateSecurityGroupRules project
        securityGroupUuid
        (\prevRules -> List.filter (\r -> r.uuid /= rule.uuid) prevRules ++ [ rule ])


projectDeleteSecurityGroupRule : Project -> OSTypes.SecurityGroupUuid -> SecurityGroupRule.SecurityGroupRuleUuid -> Project
projectDeleteSecurityGroupRule project securityGroupUuid ruleUuid =
    projectUpdateSecurityGroupRules project
        securityGroupUuid
        (List.filter (\rule -> rule.uuid /= ruleUuid))


projectUpdateSecurityGroupRules : Project -> OSTypes.SecurityGroupUuid -> (List SecurityGroupRule.SecurityGroupRule -> List SecurityGroupRule.SecurityGroupRule) -> Project
projectUpdateSecurityGroupRules project securityGroupUuid onUpdateRules =
    let
        { data, refreshStatus } =
            project.securityGroups

        newSecurityGroups =
            case data of
                -- Update rules, preserving loading/cache state of security groups.
                RDPP.DoHave groups receivedTime ->
                    RDPP.RemoteDataPlusPlus
                        (RDPP.DoHave
                            (List.map
                                (\sg ->
                                    if sg.uuid == securityGroupUuid then
                                        let
                                            rules =
                                                onUpdateRules sg.rules
                                        in
                                        { sg | rules = rules }

                                    else
                                        sg
                                )
                                groups
                            )
                            receivedTime
                        )
                        refreshStatus

                _ ->
                    project.securityGroups
    in
    { project
        | securityGroups =
            newSecurityGroups
    }


getSecurityGroupActions : Project -> SecurityGroupActions.SecurityGroupActionId -> Maybe SecurityGroupAction
getSecurityGroupActions project securityGroupUuid =
    Dict.get (SecurityGroupActions.toComparableSecurityGroupActionId securityGroupUuid) project.securityGroupActions


projectDeleteSecurityGroupActions : Project -> SecurityGroupActions.SecurityGroupActionId -> Project
projectDeleteSecurityGroupActions project securityGroupUuid =
    { project
        | securityGroupActions =
            Dict.remove
                (SecurityGroupActions.toComparableSecurityGroupActionId securityGroupUuid)
                project.securityGroupActions
    }


projectUpsertSecurityGroupActions : Project -> SecurityGroupActions.SecurityGroupActionId -> (SecurityGroupAction -> SecurityGroupAction) -> Project
projectUpsertSecurityGroupActions project securityGroupUuid onUpdateAction =
    let
        securityGroupActions =
            Dict.update (SecurityGroupActions.toComparableSecurityGroupActionId securityGroupUuid)
                (\actions_ ->
                    let
                        actions =
                            Maybe.withDefault SecurityGroupActions.initSecurityGroupAction actions_
                    in
                    Just <| onUpdateAction actions
                )
                project.securityGroupActions
    in
    { project
        | securityGroupActions =
            securityGroupActions
    }


projectUpdateSecurityGroupActionsIfExists : Project -> SecurityGroupActions.SecurityGroupActionId -> (SecurityGroupAction -> SecurityGroupAction) -> Project
projectUpdateSecurityGroupActionsIfExists project securityGroupUuid onUpdateActions =
    let
        securityGroupActions =
            Dict.update (SecurityGroupActions.toComparableSecurityGroupActionId securityGroupUuid)
                (Maybe.map onUpdateActions)
                project.securityGroupActions
    in
    { project
        | securityGroupActions =
            securityGroupActions
    }


getServerEvents : Project -> OSTypes.ServerUuid -> RDPP.RemoteDataPlusPlus HttpErrorWithBody (List OSTypes.ServerEvent)
getServerEvents project serverId =
    Dict.get serverId project.serverEvents
        |> Maybe.withDefault RDPP.empty


projectUpsertServerVolumeAction : Project -> OSTypes.ServerUuid -> ServerVolumeActionRequest -> Project
projectUpsertServerVolumeAction project serverId action =
    let
        serverVolumeActions =
            Dict.update serverId
                (\actions ->
                    -- Update the pending action if it exists, otherwise append it.
                    Maybe.withDefault [] actions
                        |> List.filter (\a -> a.action /= action.action)
                        |> List.append [ action ]
                        |> Just
                )
                project.serverVolumeActions
    in
    { project
        | serverVolumeActions =
            serverVolumeActions
    }


projectDeleteServerVolumeAction : Project -> OSTypes.ServerUuid -> ServerVolumeActionRequest -> Project
projectDeleteServerVolumeAction project serverId action =
    let
        serverVolumeActions =
            Dict.update serverId
                (Maybe.map (List.filter (\a -> a.action /= action.action)))
                project.serverVolumeActions
    in
    { project
        | serverVolumeActions =
            serverVolumeActions
    }


projectUpsertServerVolumeAttachments : Project -> OSTypes.ServerUuid -> RDPP.RemoteDataPlusPlus HttpErrorWithBody (List OSTypes.VolumeAttachment) -> Project
projectUpsertServerVolumeAttachments project serverId serverVolumeAttachments =
    { project
        | serverVolumeAttachments =
            Dict.insert serverId
                serverVolumeAttachments
                project.serverVolumeAttachments
    }


getServerVolumeAttachments : Project -> OSTypes.ServerUuid -> RDPP.RemoteDataPlusPlus HttpErrorWithBody (List OSTypes.VolumeAttachment)
getServerVolumeAttachments project serverId =
    Dict.get serverId project.serverVolumeAttachments
        |> Maybe.withDefault RDPP.empty


getServerActionRequestQueue : Project -> OSTypes.ServerUuid -> List ServerActionRequestJob
getServerActionRequestQueue project serverId =
    Dict.get serverId project.serverActionRequestQueue
        |> Maybe.withDefault []


projectRemoveServerActionRequestJob : Project -> OSTypes.ServerUuid -> ServerActionRequest -> Project
projectRemoveServerActionRequestJob project serverId job =
    let
        updatedQueue =
            Dict.update serverId
                (\maybeQueue ->
                    maybeQueue
                        |> Maybe.map
                            (Helpers.Queue.removeJobFromQueue job)
                )
                project.serverActionRequestQueue
    in
    { project
        | serverActionRequestQueue = updatedQueue
    }
