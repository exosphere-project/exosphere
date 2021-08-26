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
navigateToPage navigableView outerModel =
    let
        sharedModel =
            outerModel.sharedModel

        pipelineCmd : Cmd OuterMsg -> ( OuterModel, Cmd OuterMsg ) -> ( OuterModel, Cmd OuterMsg )
        pipelineCmd cmd__ ( outerModel_, cmd_ ) =
            -- This is sort of ugly and temporary
            ( outerModel_, Cmd.batch [ cmd_, cmd__ ] )
    in
    case navigableView of
        Route.GetSupport maybeSupportableItemTuple ->
            let
                -- TODO clean this up once ViewStateHelpers is no longer needed
                ( pageModel, cmd ) =
                    Page.GetSupport.init maybeSupportableItemTuple

                ( newOuterModel, otherCmd ) =
                    modelUpdateViewState (NonProjectView <| GetSupport pageModel) outerModel
            in
            ( newOuterModel
            , Cmd.batch [ Cmd.map SharedMsg cmd, otherCmd ]
            )

        Route.HelpAbout ->
            let
                ( _, cmd ) =
                    Page.HelpAbout.init

                ( newOuterModel, otherCmd ) =
                    modelUpdateViewState (NonProjectView <| HelpAbout) outerModel
            in
            ( newOuterModel, Cmd.batch [ Cmd.map SharedMsg cmd, otherCmd ] )

        Route.LoadingUnscopedProjects authTokenString ->
            -- TODO move stuff from State.init here to request projects from unscoped provider?
            modelUpdateViewState
                (NonProjectView <| LoadingUnscopedProjects authTokenString)
                outerModel

        Route.LoginJetstream maybeCreds ->
            modelUpdateViewState
                (NonProjectView <| Login <| LoginJetstream <| Page.LoginJetstream.init maybeCreds)
                outerModel

        Route.LoginOpenstack maybeCreds ->
            modelUpdateViewState
                (NonProjectView <| Login <| LoginOpenstack <| Page.LoginOpenstack.init maybeCreds)
                outerModel

        Route.LoginPicker ->
            modelUpdateViewState (NonProjectView LoginPicker) outerModel

        Route.MessageLog showDebugMsgs ->
            modelUpdateViewState
                (NonProjectView <| MessageLog <| Page.MessageLog.init (Just showDebugMsgs))
                outerModel

        Route.PageNotFound ->
            modelUpdateViewState (NonProjectView <| PageNotFound) outerModel

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
                            modelUpdateViewState
                                (projectViewProto <| AllResourcesList <| Page.AllResourcesList.init)
                                { outerModel | sharedModel = newSharedModel }
                                |> pipelineCmd (Cmd.map SharedMsg newCmd)

                        Route.FloatingIpAssign maybeIpUuid maybeServerUuid ->
                            let
                                ( newSharedModel, newCmd ) =
                                    ( outerModel.sharedModel, Cmd.none )
                                        |> Helpers.pipelineCmd
                                            (ApiModelHelpers.requestFloatingIps project.auth.project.uuid)
                                        |> Helpers.pipelineCmd (ApiModelHelpers.requestPorts project.auth.project.uuid)
                            in
                            modelUpdateViewState
                                (projectViewProto <| FloatingIpAssign <| Page.FloatingIpAssign.init maybeIpUuid maybeServerUuid)
                                { outerModel | sharedModel = newSharedModel }
                                |> pipelineCmd (Cmd.map SharedMsg newCmd)

                        Route.FloatingIpList ->
                            let
                                ( newSharedModel, newCmd ) =
                                    ( outerModel.sharedModel, Ports.instantiateClipboardJs () )
                                        |> Helpers.pipelineCmd
                                            (ApiModelHelpers.requestFloatingIps project.auth.project.uuid)
                                        |> Helpers.pipelineCmd
                                            (ApiModelHelpers.requestComputeQuota project.auth.project.uuid)
                                        |> Helpers.pipelineCmd
                                            (ApiModelHelpers.requestServers project.auth.project.uuid)
                            in
                            modelUpdateViewState
                                (projectViewProto <| FloatingIpList <| Page.FloatingIpList.init True)
                                { outerModel | sharedModel = newSharedModel }
                                |> pipelineCmd (Cmd.map SharedMsg newCmd)

                        Route.ImageList ->
                            let
                                ( pageModel, pageCmd ) =
                                    Page.ImageList.init sharedModel project

                                ( newOuterModel, setViewCmd ) =
                                    modelUpdateViewState
                                        (projectViewProto <| ImageList pageModel)
                                        outerModel
                            in
                            ( newOuterModel, Cmd.batch [ Cmd.map SharedMsg pageCmd, setViewCmd ] )

                        Route.KeypairCreate ->
                            modelUpdateViewState
                                (projectViewProto <| KeypairCreate Page.KeypairCreate.init)
                                outerModel

                        Route.KeypairList ->
                            let
                                cmd =
                                    Cmd.batch
                                        [ Rest.Nova.requestKeypairs project
                                        , Ports.instantiateClipboardJs ()
                                        ]
                            in
                            modelUpdateViewState
                                (projectViewProto <| KeypairList <| Page.KeypairList.init True)
                                outerModel
                                |> pipelineCmd (Cmd.map SharedMsg cmd)

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
                                    ( outerModel.sharedModel, cmd )
                                        |> Helpers.pipelineCmd (ApiModelHelpers.requestAutoAllocatedNetwork project.auth.project.uuid)
                                        |> Helpers.pipelineCmd (ApiModelHelpers.requestComputeQuota project.auth.project.uuid)
                                        |> Helpers.pipelineCmd (ApiModelHelpers.requestVolumeQuota project.auth.project.uuid)
                            in
                            modelUpdateViewState
                                (projectViewProto <| ServerCreate (Page.ServerCreate.init imageId imageName maybeDeployGuac))
                                { outerModel | sharedModel = newSharedModel }
                                |> pipelineCmd (Cmd.map SharedMsg newCmd)

                        Route.ServerCreateImage serverId maybeImageName ->
                            modelUpdateViewState
                                (projectViewProto <| ServerCreateImage (Page.ServerCreateImage.init serverId maybeImageName))
                                outerModel

                        Route.ServerDetail serverId ->
                            let
                                newSharedModel =
                                    project
                                        |> GetterSetters.modelUpdateProject outerModel.sharedModel

                                cmd =
                                    Cmd.batch
                                        [ Rest.Nova.requestFlavors project
                                        , Rest.Glance.requestImages outerModel.sharedModel project
                                        , OSVolumes.requestVolumes project
                                        , Ports.instantiateClipboardJs ()
                                        ]

                                ( newNewSharedModel, newCmd ) =
                                    ( newSharedModel, cmd )
                                        |> Helpers.pipelineCmd
                                            (ApiModelHelpers.requestServer project.auth.project.uuid serverId)
                            in
                            modelUpdateViewState
                                (projectViewProto <| ServerDetail (Page.ServerDetail.init serverId))
                                { outerModel | sharedModel = newNewSharedModel }
                                |> pipelineCmd (Cmd.map SharedMsg newCmd)

                        Route.ServerList ->
                            let
                                ( newSharedModel, cmd ) =
                                    ApiModelHelpers.requestServers
                                        project.auth.project.uuid
                                        outerModel.sharedModel
                                        |> Helpers.pipelineCmd
                                            (ApiModelHelpers.requestFloatingIps
                                                project.auth.project.uuid
                                            )
                            in
                            modelUpdateViewState
                                (projectViewProto <| ServerList <| Page.ServerList.init True)
                                { outerModel | sharedModel = newSharedModel }
                                |> pipelineCmd (Cmd.map SharedMsg cmd)

                        Route.VolumeAttach maybeServerUuid maybeVolumeUuid ->
                            let
                                ( newSharedModel, newCmd ) =
                                    ( outerModel.sharedModel, OSVolumes.requestVolumes project )
                                        |> Helpers.pipelineCmd (ApiModelHelpers.requestServers project.auth.project.uuid)
                            in
                            modelUpdateViewState
                                (projectViewProto <|
                                    VolumeAttach (Page.VolumeAttach.init maybeServerUuid maybeVolumeUuid)
                                )
                                { outerModel | sharedModel = newSharedModel }
                                |> pipelineCmd (Cmd.map SharedMsg newCmd)

                        Route.VolumeCreate ->
                            modelUpdateViewState
                                (projectViewProto <| VolumeCreate Page.VolumeCreate.init)
                                outerModel
                                |> pipelineCmd (Cmd.map SharedMsg <| OSQuotas.requestComputeQuota project)

                        Route.VolumeDetail volumeUuid ->
                            modelUpdateViewState
                                (projectViewProto <| VolumeDetail <| Page.VolumeDetail.init True volumeUuid)
                                outerModel

                        Route.VolumeList ->
                            let
                                cmd =
                                    Cmd.batch
                                        [ OSVolumes.requestVolumes project
                                        , Ports.instantiateClipboardJs ()
                                        ]
                            in
                            modelUpdateViewState
                                (projectViewProto <| VolumeList <| Page.VolumeList.init True)
                                outerModel
                                |> pipelineCmd (Cmd.map SharedMsg cmd)

                        Route.VolumeMountInstructions attachment ->
                            modelUpdateViewState
                                (projectViewProto <| VolumeMountInstructions <| Page.VolumeMountInstructions.init attachment)
                                outerModel

                Nothing ->
                    ( outerModel, Cmd.none )

        Route.SelectProjects keystoneUrl ->
            modelUpdateViewState (NonProjectView <| SelectProjects <| Page.SelectProjects.init keystoneUrl)
                outerModel

        Route.Settings ->
            modelUpdateViewState
                (NonProjectView <| Settings <| Page.Settings.init)
                outerModel


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
