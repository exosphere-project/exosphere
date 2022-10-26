module State.ViewState exposing
    ( navigateToPage
    , viewStateToSupportableItem
    )

import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers as Helpers
import OpenStack.Quotas as OSQuotas
import OpenStack.Types as OSTypes
import OpenStack.Volumes as OSVolumes
import Page.FloatingIpAssign
import Page.FloatingIpList
import Page.GetSupport
import Page.Home
import Page.ImageList
import Page.InstanceSourcePicker
import Page.KeypairCreate
import Page.KeypairList
import Page.LoginOpenIdConnect
import Page.LoginOpenstack
import Page.MessageLog
import Page.ProjectOverview
import Page.SelectProjectRegions
import Page.SelectProjects
import Page.ServerCreate
import Page.ServerCreateImage
import Page.ServerDetail
import Page.ServerList
import Page.ServerResize
import Page.Settings
import Page.VolumeAttach
import Page.VolumeCreate
import Page.VolumeDetail
import Page.VolumeList
import Page.VolumeMountInstructions
import Ports
import RemoteData
import Rest.ApiModelHelpers as ApiModelHelpers
import Rest.Keystone
import Rest.Nova
import Route
import Time
import Types.HelperTypes as HelperTypes exposing (DefaultLoginView(..))
import Types.OuterModel exposing (OuterModel)
import Types.OuterMsg exposing (OuterMsg(..))
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))
import Types.View exposing (LoginView(..), NonProjectViewConstructor(..), ProjectViewConstructor(..), ViewState(..))
import Url
import View.PageTitle


navigateToPage : Url.Url -> OuterModel -> ( OuterModel, Cmd OuterMsg )
navigateToPage url outerModel =
    let
        route =
            Route.fromUrl outerModel.sharedModel.viewContext.urlPathPrefix Route.defaultRoute url

        ( newViewState, pageSpecificSharedModel, pageSpecificCmd ) =
            routeToViewStateModelCmd outerModel.sharedModel route

        newUrl =
            Route.toUrl outerModel.sharedModel.viewContext.urlPathPrefix route

        newOuterModel =
            { outerModel
                | viewState = newViewState
                , sharedModel = pageSpecificSharedModel
            }

        newPageTitle =
            View.PageTitle.pageTitle newOuterModel

        updateMatomoCmd =
            Ports.pushUrlAndTitleToMatomo { newUrl = newUrl, pageTitle = newPageTitle }
    in
    ( newOuterModel
    , Cmd.batch
        [ Cmd.map SharedMsg pageSpecificCmd
        , updateMatomoCmd
        ]
    )



-- TODO consider viewStateToRoute to help with generating the breadcrumb


routeToViewStateModelCmd : SharedModel -> Route.Route -> ( ViewState, SharedModel, Cmd SharedMsg )
routeToViewStateModelCmd sharedModel route =
    case route of
        Route.GetSupport maybeSupportableItemTuple ->
            ( NonProjectView <| GetSupport <| Page.GetSupport.init maybeSupportableItemTuple
            , sharedModel
            , Ports.instantiateClipboardJs ()
            )

        Route.HelpAbout ->
            ( NonProjectView <| HelpAbout
            , sharedModel
            , Ports.instantiateClipboardJs ()
            )

        Route.Home ->
            ( NonProjectView <| Home Page.Home.init
            , sharedModel
            , Cmd.none
            )

        Route.LoadingUnscopedProjects authTokenString ->
            let
                -- If we have just received an OpenID Connect auth token, store it as an unscoped provider and get projects
                ( newSharedModel, cmd ) =
                    case sharedModel.openIdConnectLoginConfig of
                        Nothing ->
                            ( sharedModel, Cmd.none )

                        Just openIdConnectLoginConfig ->
                            let
                                oneHourMillis =
                                    1000 * 60 * 60

                                tokenExpiry =
                                    -- One hour later? This should never matter
                                    Time.posixToMillis sharedModel.clientCurrentTime
                                        + oneHourMillis
                                        |> Time.millisToPosix

                                unscopedProvider =
                                    HelperTypes.UnscopedProvider
                                        openIdConnectLoginConfig.keystoneAuthUrl
                                        (OSTypes.UnscopedAuthToken
                                            tokenExpiry
                                            authTokenString
                                        )
                                        RemoteData.NotAsked
                                        RemoteData.NotAsked

                                newUnscopedProviders =
                                    unscopedProvider :: sharedModel.unscopedProviders
                            in
                            ( { sharedModel | unscopedProviders = newUnscopedProviders }
                            , Cmd.batch
                                [ Rest.Keystone.requestUnscopedProjects unscopedProvider sharedModel.cloudCorsProxyUrl
                                , Rest.Keystone.requestUnscopedRegions unscopedProvider sharedModel.cloudCorsProxyUrl
                                ]
                            )
            in
            ( NonProjectView <| LoadingUnscopedProjects authTokenString
            , newSharedModel
            , cmd
            )

        Route.LoginOpenstack maybeCreds ->
            ( NonProjectView <| Login <| LoginOpenstack <| Page.LoginOpenstack.init maybeCreds
            , sharedModel
            , Cmd.none
            )

        Route.LoginOpenIdConnect ->
            case sharedModel.openIdConnectLoginConfig of
                Just openIdConnectLoginConfig ->
                    ( NonProjectView <| Login <| LoginOpenIdConnect <| Page.LoginOpenIdConnect.init openIdConnectLoginConfig
                    , sharedModel
                    , Cmd.none
                    )

                Nothing ->
                    -- App is not set up for OIDC login, so just show login picker
                    ( NonProjectView <| LoginPicker, sharedModel, Cmd.none )

        Route.LoginPicker ->
            ( NonProjectView LoginPicker
            , sharedModel
            , Cmd.none
            )

        Route.MessageLog showDebugMsgs ->
            ( NonProjectView <| MessageLog <| Page.MessageLog.init (Just showDebugMsgs)
            , sharedModel
            , Cmd.none
            )

        Route.PageNotFound ->
            ( NonProjectView <| PageNotFound
            , sharedModel
            , Cmd.none
            )

        Route.ProjectRoute projectId projectPage ->
            case GetterSetters.projectLookup sharedModel projectId of
                Just project ->
                    let
                        projectViewProto =
                            ProjectView projectId
                    in
                    case projectPage of
                        Route.ProjectOverview ->
                            let
                                ( newSharedModel, newCmd ) =
                                    ( sharedModel
                                    , Cmd.batch
                                        [ OSVolumes.requestVolumes project
                                        , OSVolumes.requestVolumeSnapshots project
                                        , Rest.Nova.requestKeypairs project
                                        , OSQuotas.requestComputeQuota project
                                        , OSQuotas.requestVolumeQuota project
                                        , OSQuotas.requestNetworkQuota project
                                        , Ports.instantiateClipboardJs ()
                                        ]
                                    )
                                        |> Helpers.pipelineCmd
                                            (ApiModelHelpers.requestFloatingIps (GetterSetters.projectIdentifier project))
                                        |> Helpers.pipelineCmd
                                            (ApiModelHelpers.requestServers (GetterSetters.projectIdentifier project))
                                        |> Helpers.pipelineCmd
                                            (ApiModelHelpers.requestImages (GetterSetters.projectIdentifier project))
                                        |> Helpers.pipelineCmd
                                            (ApiModelHelpers.requestJetstream2Allocation (GetterSetters.projectIdentifier project))
                            in
                            ( projectViewProto <| ProjectOverview <| Page.ProjectOverview.init
                            , newSharedModel
                            , newCmd
                            )

                        Route.FloatingIpAssign maybeIpUuid maybeServerUuid ->
                            let
                                ( newSharedModel, newCmd ) =
                                    ( sharedModel, Cmd.none )
                                        |> Helpers.pipelineCmd
                                            (ApiModelHelpers.requestFloatingIps (GetterSetters.projectIdentifier project))
                                        |> Helpers.pipelineCmd (ApiModelHelpers.requestPorts (GetterSetters.projectIdentifier project))
                            in
                            ( projectViewProto <| FloatingIpAssign <| Page.FloatingIpAssign.init maybeIpUuid maybeServerUuid
                            , newSharedModel
                            , newCmd
                            )

                        Route.FloatingIpList ->
                            let
                                ( newSharedModel, newCmd ) =
                                    ( sharedModel, Ports.instantiateClipboardJs () )
                                        |> Helpers.pipelineCmd
                                            (ApiModelHelpers.requestFloatingIps (GetterSetters.projectIdentifier project))
                                        |> Helpers.pipelineCmd
                                            (ApiModelHelpers.requestNetworkQuota (GetterSetters.projectIdentifier project))
                                        |> Helpers.pipelineCmd
                                            (ApiModelHelpers.requestServers (GetterSetters.projectIdentifier project))
                            in
                            ( projectViewProto <| FloatingIpList <| Page.FloatingIpList.init True
                            , newSharedModel
                            , newCmd
                            )

                        Route.ImageList ->
                            let
                                ( newSharedModel, newCmd ) =
                                    ApiModelHelpers.requestImages (GetterSetters.projectIdentifier project) sharedModel
                            in
                            ( projectViewProto <| ImageList <| Page.ImageList.init True True
                            , newSharedModel
                            , newCmd
                            )

                        Route.InstanceSourcePicker ->
                            let
                                ( newSharedModel, newCmd ) =
                                    ( sharedModel, Rest.Nova.requestFlavors project )
                                        |> Helpers.pipelineCmd
                                            (ApiModelHelpers.requestImages (GetterSetters.projectIdentifier project))
                            in
                            ( projectViewProto <| InstanceSourcePicker <| Page.InstanceSourcePicker.init
                            , newSharedModel
                            , newCmd
                            )

                        Route.KeypairCreate ->
                            ( projectViewProto <| KeypairCreate Page.KeypairCreate.init
                            , sharedModel
                            , Cmd.none
                            )

                        Route.KeypairList ->
                            ( projectViewProto <| KeypairList <| Page.KeypairList.init True
                            , sharedModel
                            , Cmd.batch
                                [ Rest.Nova.requestKeypairs project
                                , OSQuotas.requestComputeQuota project
                                , Ports.instantiateClipboardJs ()
                                ]
                            )

                        Route.ServerCreate imageId imageName maybeRestrictFlavorIds maybeDeployGuac ->
                            let
                                cmd =
                                    Cmd.batch
                                        [ Rest.Nova.requestFlavors project
                                        , Rest.Nova.requestKeypairs project
                                        ]

                                ( newSharedModel, newCmd ) =
                                    ( sharedModel, cmd )
                                        |> Helpers.pipelineCmd (ApiModelHelpers.requestAutoAllocatedNetwork (GetterSetters.projectIdentifier project))
                                        |> Helpers.pipelineCmd (ApiModelHelpers.requestComputeQuota (GetterSetters.projectIdentifier project))
                                        |> Helpers.pipelineCmd (ApiModelHelpers.requestVolumeQuota (GetterSetters.projectIdentifier project))
                                        |> Helpers.pipelineCmd (ApiModelHelpers.requestNetworkQuota (GetterSetters.projectIdentifier project))
                            in
                            ( projectViewProto <|
                                ServerCreate
                                    (Page.ServerCreate.init
                                        project
                                        imageId
                                        imageName
                                        maybeRestrictFlavorIds
                                        maybeDeployGuac
                                    )
                            , newSharedModel
                            , newCmd
                            )

                        Route.ServerCreateImage serverId maybeImageName ->
                            ( projectViewProto <| ServerCreateImage (Page.ServerCreateImage.init serverId maybeImageName)
                            , sharedModel
                            , Cmd.none
                            )

                        Route.ServerDetail serverId ->
                            let
                                newSharedModel =
                                    project
                                        |> GetterSetters.modelUpdateProject sharedModel

                                cmd =
                                    Cmd.batch
                                        [ Rest.Nova.requestFlavors project
                                        , OSVolumes.requestVolumes project
                                        , Ports.instantiateClipboardJs ()
                                        ]

                                ( newNewSharedModel, newCmd ) =
                                    ( newSharedModel, cmd )
                                        |> Helpers.pipelineCmd
                                            (ApiModelHelpers.requestServer (GetterSetters.projectIdentifier project) serverId)
                                        |> Helpers.pipelineCmd
                                            (ApiModelHelpers.requestImages (GetterSetters.projectIdentifier project))
                            in
                            ( projectViewProto <| ServerDetail (Page.ServerDetail.init serverId)
                            , newNewSharedModel
                            , newCmd
                            )

                        Route.ServerList ->
                            let
                                ( newSharedModel, cmd ) =
                                    ApiModelHelpers.requestServers
                                        (GetterSetters.projectIdentifier project)
                                        sharedModel
                                        |> Helpers.pipelineCmd
                                            (ApiModelHelpers.requestFloatingIps
                                                (GetterSetters.projectIdentifier project)
                                            )
                            in
                            ( projectViewProto <| ServerList <| Page.ServerList.init project True
                            , newSharedModel
                            , Cmd.batch
                                [ cmd
                                , OSQuotas.requestComputeQuota project
                                , Rest.Nova.requestFlavors project
                                ]
                            )

                        Route.ServerResize serverId ->
                            ( projectViewProto <| ServerResize (Page.ServerResize.init serverId)
                            , sharedModel
                            , Cmd.batch
                                [ Rest.Nova.requestFlavors project
                                , OSQuotas.requestComputeQuota project
                                ]
                            )

                        Route.VolumeAttach maybeServerUuid maybeVolumeUuid ->
                            let
                                ( newSharedModel, newCmd ) =
                                    ( sharedModel, OSVolumes.requestVolumes project )
                                        |> Helpers.pipelineCmd (ApiModelHelpers.requestServers (GetterSetters.projectIdentifier project))
                            in
                            ( projectViewProto <|
                                VolumeAttach (Page.VolumeAttach.init maybeServerUuid maybeVolumeUuid)
                            , newSharedModel
                            , newCmd
                            )

                        Route.VolumeCreate ->
                            ( projectViewProto <| VolumeCreate Page.VolumeCreate.init
                            , sharedModel
                            , OSQuotas.requestComputeQuota project
                            )

                        Route.VolumeDetail volumeUuid ->
                            ( projectViewProto <| VolumeDetail <| Page.VolumeDetail.init True volumeUuid
                            , sharedModel
                            , Cmd.none
                            )

                        Route.VolumeList ->
                            ( projectViewProto <| VolumeList <| Page.VolumeList.init True
                            , sharedModel
                            , Cmd.batch
                                [ OSVolumes.requestVolumes project
                                , OSVolumes.requestVolumeSnapshots project
                                , Ports.instantiateClipboardJs ()
                                , OSQuotas.requestVolumeQuota project
                                ]
                            )

                        Route.VolumeMountInstructions attachment ->
                            ( projectViewProto <| VolumeMountInstructions <| Page.VolumeMountInstructions.init attachment
                            , sharedModel
                            , Cmd.none
                            )

                Nothing ->
                    -- Default view for non-matching project
                    ( NonProjectView <| LoginPicker, sharedModel, Cmd.none )

        Route.SelectProjectRegions keystoneUrl projectUuid ->
            ( NonProjectView <| SelectProjectRegions <| Page.SelectProjectRegions.init keystoneUrl projectUuid
            , sharedModel
            , Cmd.none
            )

        Route.SelectProjects keystoneUrl ->
            ( NonProjectView <| SelectProjects <| Page.SelectProjects.init keystoneUrl
            , sharedModel
            , Cmd.none
            )

        Route.Settings ->
            ( NonProjectView <| Settings <| Page.Settings.init
            , sharedModel
            , Cmd.none
            )


viewStateToSupportableItem :
    Types.View.ViewState
    -> Maybe ( HelperTypes.SupportableItemType, Maybe HelperTypes.Uuid )
viewStateToSupportableItem viewState =
    let
        supportableProjectItem :
            HelperTypes.ProjectIdentifier
            -> ProjectViewConstructor
            -> ( HelperTypes.SupportableItemType, Maybe HelperTypes.Uuid )
        supportableProjectItem projectIdentifier projectViewConstructor =
            case projectViewConstructor of
                ServerCreate pageModel ->
                    ( HelperTypes.SupportableImage, Just pageModel.imageUuid )

                ServerDetail pageModel ->
                    ( HelperTypes.SupportableServer, Just pageModel.serverUuid )

                ServerCreateImage pageModel ->
                    ( HelperTypes.SupportableServer, Just pageModel.serverUuid )

                VolumeDetail pageModel ->
                    ( HelperTypes.SupportableVolume, Just pageModel.volumeUuid )

                VolumeAttach pageModel ->
                    pageModel.maybeVolumeUuid
                        |> Maybe.map (\uuid -> ( HelperTypes.SupportableVolume, Just uuid ))
                        |> Maybe.withDefault ( HelperTypes.SupportableProject, Just projectIdentifier.projectUuid )

                VolumeMountInstructions pageModel ->
                    ( HelperTypes.SupportableServer, Just pageModel.serverUuid )

                _ ->
                    ( HelperTypes.SupportableProject, Just projectIdentifier.projectUuid )
    in
    case viewState of
        NonProjectView _ ->
            Nothing

        ProjectView projectUuid projectViewConstructor ->
            Just <| supportableProjectItem projectUuid projectViewConstructor
