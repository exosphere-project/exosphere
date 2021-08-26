module State.ViewState exposing
    ( defaultLoginPage
    , defaultRoute
    , navigateToPage
    , viewStateToSupportableItem
    )

import AppUrl.Builder
import Browser.Navigation
import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers as Helpers
import Helpers.Random as RandomHelpers
import OpenStack.Quotas as OSQuotas
import OpenStack.Volumes as OSVolumes
import Page.AllResourcesList
import Page.FloatingIpAssign
import Page.FloatingIpList
import Page.GetSupport
import Page.HelpAbout
import Page.ImageList
import Page.KeypairCreate
import Page.KeypairList
import Page.LoginJetstream
import Page.LoginOpenstack
import Page.MessageLog
import Page.SelectProjects
import Page.ServerCreate
import Page.ServerCreateImage
import Page.ServerDetail
import Page.ServerList
import Page.Settings
import Page.VolumeAttach
import Page.VolumeCreate
import Page.VolumeDetail
import Page.VolumeList
import Page.VolumeMountInstructions
import Ports
import Rest.ApiModelHelpers as ApiModelHelpers
import Rest.Glance
import Rest.Nova
import Route
import Types.HelperTypes as HelperTypes exposing (DefaultLoginView(..))
import Types.OuterModel exposing (OuterModel)
import Types.OuterMsg exposing (OuterMsg(..))
import Types.Project exposing (Project)
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))
import Types.View exposing (LoginView(..), NonProjectViewConstructor(..), ProjectViewConstructor(..), ViewState(..))
import View.Helpers
import View.PageTitle


navigateToPage : Route.NavigablePage -> OuterModel -> ( OuterModel, Cmd OuterMsg )
navigateToPage route outerModel =
    let
        ( newViewState, pageSpecificSharedModel, pageSpecificCmd ) =
            routeToViewStateModelCmd outerModel.sharedModel route

        ( newOuterModel, newCmd ) =
            modelUpdateViewState newViewState { outerModel | sharedModel = pageSpecificSharedModel }
    in
    ( newOuterModel, Cmd.batch [ Cmd.map SharedMsg pageSpecificCmd, newCmd ] )


modelUpdateViewState : ViewState -> OuterModel -> ( OuterModel, Cmd OuterMsg )
modelUpdateViewState viewState outerModel =
    -- the cmd argument is just a "passthrough", added to the Cmd that sets new URL
    let
        urlWithoutQuery url =
            String.split "?" url
                |> List.head
                |> Maybe.withDefault ""

        prevUrl =
            outerModel.sharedModel.prevUrl

        newUrl =
            AppUrl.Builder.viewStateToUrl outerModel.sharedModel.urlPathPrefix viewState

        oldSharedModel =
            outerModel.sharedModel

        newSharedModel =
            { oldSharedModel | prevUrl = newUrl }

        newOuterModel =
            { outerModel
                | viewState = viewState
                , sharedModel = newSharedModel
            }

        newViewContext =
            View.Helpers.toViewContext newOuterModel.sharedModel

        newPageTitle =
            View.PageTitle.pageTitle newOuterModel newViewContext

        ( updateUrlFunc, updateMatomoCmd ) =
            if urlWithoutQuery newUrl == urlWithoutQuery prevUrl then
                -- We should `replaceUrl` and not update Matomo when just modifying the query string (setting parameters of views)
                ( Browser.Navigation.replaceUrl, Cmd.none )

            else
                -- We should `pushUrl` and update Matomo when modifying the path (moving between views)
                ( Browser.Navigation.pushUrl, Ports.pushUrlAndTitleToMatomo { newUrl = newUrl, pageTitle = newPageTitle } )

        urlCmd =
            Cmd.batch
                [ updateUrlFunc newOuterModel.sharedModel.navigationKey newUrl
                , updateMatomoCmd
                ]
    in
    ( newOuterModel, urlCmd )


routeToViewStateModelCmd : SharedModel -> Route.NavigablePage -> ( ViewState, SharedModel, Cmd SharedMsg )
routeToViewStateModelCmd sharedModel route =
    case route of
        Route.GetSupport maybeSupportableItemTuple ->
            let
                -- TODO clean this up once ViewStateHelpers is no longer needed
                ( pageModel, cmd ) =
                    Page.GetSupport.init maybeSupportableItemTuple
            in
            ( NonProjectView <| GetSupport pageModel
            , sharedModel
            , cmd
            )

        Route.HelpAbout ->
            let
                ( _, cmd ) =
                    Page.HelpAbout.init
            in
            ( NonProjectView <| HelpAbout
            , sharedModel
            , cmd
            )

        Route.LoadingUnscopedProjects authTokenString ->
            -- TODO move stuff from State.init here to request projects from unscoped provider?
            ( NonProjectView <| LoadingUnscopedProjects authTokenString
            , sharedModel
            , Cmd.none
            )

        Route.LoginJetstream maybeCreds ->
            ( NonProjectView <| Login <| LoginJetstream <| Page.LoginJetstream.init maybeCreds
            , sharedModel
            , Cmd.none
            )

        Route.LoginOpenstack maybeCreds ->
            ( NonProjectView <| Login <| LoginOpenstack <| Page.LoginOpenstack.init maybeCreds
            , sharedModel
            , Cmd.none
            )

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

        Route.ProjectPage projectId projectPage ->
            case GetterSetters.projectLookup sharedModel projectId of
                Just project ->
                    let
                        projectViewProto =
                            ProjectView projectId { createPopup = False }
                    in
                    case projectPage of
                        Route.AllResourcesList ->
                            let
                                ( newSharedModel, newCmd ) =
                                    ( sharedModel
                                    , Cmd.batch
                                        [ OSVolumes.requestVolumes project
                                        , Rest.Nova.requestKeypairs project
                                        , OSQuotas.requestComputeQuota project
                                        , OSQuotas.requestVolumeQuota project
                                        , Ports.instantiateClipboardJs ()
                                        ]
                                    )
                                        |> Helpers.pipelineCmd
                                            (ApiModelHelpers.requestFloatingIps project.auth.project.uuid)
                                        |> Helpers.pipelineCmd
                                            (ApiModelHelpers.requestServers project.auth.project.uuid)
                            in
                            ( projectViewProto <| AllResourcesList <| Page.AllResourcesList.init
                            , newSharedModel
                            , newCmd
                            )

                        Route.FloatingIpAssign maybeIpUuid maybeServerUuid ->
                            let
                                ( newSharedModel, newCmd ) =
                                    ( sharedModel, Cmd.none )
                                        |> Helpers.pipelineCmd
                                            (ApiModelHelpers.requestFloatingIps project.auth.project.uuid)
                                        |> Helpers.pipelineCmd (ApiModelHelpers.requestPorts project.auth.project.uuid)
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
                                            (ApiModelHelpers.requestFloatingIps project.auth.project.uuid)
                                        |> Helpers.pipelineCmd
                                            (ApiModelHelpers.requestComputeQuota project.auth.project.uuid)
                                        |> Helpers.pipelineCmd
                                            (ApiModelHelpers.requestServers project.auth.project.uuid)
                            in
                            ( projectViewProto <| FloatingIpList <| Page.FloatingIpList.init True
                            , newSharedModel
                            , newCmd
                            )

                        Route.ImageList ->
                            let
                                ( pageModel, pageCmd ) =
                                    Page.ImageList.init sharedModel project
                            in
                            ( projectViewProto <| ImageList pageModel
                            , sharedModel
                            , pageCmd
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
                                , Ports.instantiateClipboardJs ()
                                ]
                            )

                        Route.ServerCreate imageId imageName maybeDeployGuac ->
                            let
                                cmd =
                                    Cmd.batch
                                        [ Rest.Nova.requestFlavors project
                                        , Rest.Nova.requestKeypairs project
                                        , RandomHelpers.generateServerName
                                            (\serverName ->
                                                ProjectMsg project.auth.project.uuid <|
                                                    ReceiveRandomServerName serverName
                                            )
                                        ]

                                ( newSharedModel, newCmd ) =
                                    ( sharedModel, cmd )
                                        |> Helpers.pipelineCmd (ApiModelHelpers.requestAutoAllocatedNetwork project.auth.project.uuid)
                                        |> Helpers.pipelineCmd (ApiModelHelpers.requestComputeQuota project.auth.project.uuid)
                                        |> Helpers.pipelineCmd (ApiModelHelpers.requestVolumeQuota project.auth.project.uuid)
                            in
                            ( projectViewProto <| ServerCreate (Page.ServerCreate.init imageId imageName maybeDeployGuac)
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
                                        , Rest.Glance.requestImages sharedModel project
                                        , OSVolumes.requestVolumes project
                                        , Ports.instantiateClipboardJs ()
                                        ]

                                ( newNewSharedModel, newCmd ) =
                                    ( newSharedModel, cmd )
                                        |> Helpers.pipelineCmd
                                            (ApiModelHelpers.requestServer project.auth.project.uuid serverId)
                            in
                            ( projectViewProto <| ServerDetail (Page.ServerDetail.init serverId)
                            , newNewSharedModel
                            , newCmd
                            )

                        Route.ServerList ->
                            let
                                ( newSharedModel, cmd ) =
                                    ApiModelHelpers.requestServers
                                        project.auth.project.uuid
                                        sharedModel
                                        |> Helpers.pipelineCmd
                                            (ApiModelHelpers.requestFloatingIps
                                                project.auth.project.uuid
                                            )
                            in
                            ( projectViewProto <| ServerList <| Page.ServerList.init True
                            , newSharedModel
                            , cmd
                            )

                        Route.VolumeAttach maybeServerUuid maybeVolumeUuid ->
                            let
                                ( newSharedModel, newCmd ) =
                                    ( sharedModel, OSVolumes.requestVolumes project )
                                        |> Helpers.pipelineCmd (ApiModelHelpers.requestServers project.auth.project.uuid)
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
                                , Ports.instantiateClipboardJs ()
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


defaultRoute : SharedModel -> Route.NavigablePage
defaultRoute sharedModel =
    -- TODO move this
    case sharedModel.projects of
        [] ->
            defaultLoginPage sharedModel.style.defaultLoginView

        firstProject :: _ ->
            Route.ProjectPage
                firstProject.auth.project.uuid
                Route.AllResourcesList


defaultLoginPage : Maybe DefaultLoginView -> Route.NavigablePage
defaultLoginPage maybeDefaultLoginView =
    case maybeDefaultLoginView of
        Nothing ->
            Route.LoginPicker

        Just defaultLoginView ->
            case defaultLoginView of
                DefaultLoginOpenstack ->
                    Route.LoginOpenstack Nothing

                DefaultLoginJetstream ->
                    Route.LoginJetstream Nothing


viewStateToSupportableItem :
    Types.View.ViewState
    -> Maybe ( HelperTypes.SupportableItemType, Maybe HelperTypes.Uuid )
viewStateToSupportableItem viewState =
    let
        supportableProjectItem :
            HelperTypes.ProjectIdentifier
            -> ProjectViewConstructor
            -> ( HelperTypes.SupportableItemType, Maybe HelperTypes.Uuid )
        supportableProjectItem projectUuid projectViewConstructor =
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
                        |> Maybe.withDefault ( HelperTypes.SupportableProject, Just projectUuid )

                VolumeMountInstructions pageModel ->
                    ( HelperTypes.SupportableServer, Just pageModel.serverUuid )

                _ ->
                    ( HelperTypes.SupportableProject, Just projectUuid )
    in
    case viewState of
        NonProjectView _ ->
            Nothing

        ProjectView projectUuid _ projectViewConstructor ->
            Just <| supportableProjectItem projectUuid projectViewConstructor
