module State.ViewState exposing
    ( defaultLoginPage
    , defaultRoute
    , modelUpdateViewState
    , navigateToPage
    , setProjectView
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
import RemoteData
import Rest.ApiModelHelpers as ApiModelHelpers
import Rest.Glance
import Rest.Nova
import Route
import Style.Widgets.NumericTextInput.NumericTextInput
import Types.HelperTypes as HelperTypes exposing (DefaultLoginView(..))
import Types.OuterModel exposing (OuterModel)
import Types.OuterMsg exposing (OuterMsg(..))
import Types.Project exposing (Project)
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))
import Types.View exposing (LoginView(..), NonProjectViewConstructor(..), ProjectViewConstructor(..), ViewState(..))
import View.Helpers
import View.PageTitle


navigateToPage : OuterModel -> Route.NavigablePage -> ( OuterModel, Cmd OuterMsg )
navigateToPage outerModel navigableView =
    let
        sharedModel =
            outerModel.sharedModel
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
                    case projectPage of
                        Route.AllResourcesList ->
                            setProjectView
                                project
                                (AllResourcesList <| Page.AllResourcesList.init)
                                outerModel

                        Route.FloatingIpAssign maybeIpUuid maybeServerUuid ->
                            setProjectView
                                project
                                (FloatingIpAssign <| Page.FloatingIpAssign.init maybeIpUuid maybeServerUuid)
                                outerModel

                        Route.FloatingIpList ->
                            setProjectView
                                project
                                (FloatingIpList <| Page.FloatingIpList.init True)
                                outerModel

                        Route.ImageList ->
                            setProjectView
                                project
                                (ImageList Page.ImageList.init)
                                outerModel

                        Route.KeypairCreate ->
                            setProjectView
                                project
                                (KeypairCreate Page.KeypairCreate.init)
                                outerModel

                        Route.KeypairList ->
                            setProjectView
                                project
                                (KeypairList <| Page.KeypairList.init True)
                                outerModel

                        Route.ServerCreate imageId imageName maybeDeployGuac ->
                            setProjectView
                                project
                                (ServerCreate (Page.ServerCreate.init imageId imageName maybeDeployGuac))
                                outerModel

                        Route.ServerCreateImage serverId maybeImageName ->
                            setProjectView
                                project
                                (ServerCreateImage (Page.ServerCreateImage.init serverId maybeImageName))
                                outerModel

                        Route.ServerDetail serverId ->
                            setProjectView
                                project
                                (ServerDetail (Page.ServerDetail.init serverId))
                                outerModel

                        Route.ServerList ->
                            setProjectView
                                project
                                (ServerList <| Page.ServerList.init True)
                                outerModel

                        Route.VolumeAttach maybeServerUuid maybeVolumeUuid ->
                            setProjectView
                                project
                                (VolumeAttach (Page.VolumeAttach.init maybeServerUuid maybeVolumeUuid))
                                outerModel

                        Route.VolumeCreate ->
                            setProjectView
                                project
                                (VolumeCreate Page.VolumeCreate.init)
                                outerModel

                        Route.VolumeDetail volumeUuid ->
                            setProjectView
                                project
                                (VolumeDetail <| Page.VolumeDetail.init True volumeUuid)
                                outerModel

                        Route.VolumeList ->
                            setProjectView
                                project
                                (VolumeList <| Page.VolumeList.init True)
                                outerModel

                        Route.VolumeMountInstructions attachment ->
                            setProjectView
                                project
                                (VolumeMountInstructions <| Page.VolumeMountInstructions.init attachment)
                                outerModel

                Nothing ->
                    ( outerModel, Cmd.none )

        Route.Settings ->
            modelUpdateViewState
                (NonProjectView <| Settings <| Page.Settings.init)
                outerModel


setProjectView : Project -> ProjectViewConstructor -> OuterModel -> ( OuterModel, Cmd OuterMsg )
setProjectView project projectViewConstructor outerModel =
    let
        prevProjectViewConstructor =
            case outerModel.viewState of
                ProjectView projectId _ projectViewConstructor_ ->
                    if projectId == project.auth.project.uuid then
                        Just projectViewConstructor_

                    else
                        Nothing

                _ ->
                    Nothing

        newViewState =
            ProjectView project.auth.project.uuid { createPopup = False } projectViewConstructor

        ( viewSpecificOuterModel, viewSpecificCmd ) =
            case projectViewConstructor of
                AllResourcesList _ ->
                    -- Don't fire cmds if we're already in this view
                    case prevProjectViewConstructor of
                        Just (AllResourcesList _) ->
                            ( outerModel, Cmd.none )

                        _ ->
                            let
                                ( newSharedModel, newCmd ) =
                                    ( outerModel.sharedModel
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
                            ( { outerModel | sharedModel = newSharedModel }, newCmd )

                ImageList _ ->
                    let
                        cmd =
                            -- Don't fire cmds if we're already in this view
                            case prevProjectViewConstructor of
                                Just (ImageList _) ->
                                    Cmd.none

                                _ ->
                                    Rest.Glance.requestImages outerModel.sharedModel project
                    in
                    ( outerModel, cmd )

                ServerList _ ->
                    -- Don't fire cmds if we're already in this view
                    case prevProjectViewConstructor of
                        Just (ServerList _) ->
                            ( outerModel, Cmd.none )

                        _ ->
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
                            ( { outerModel | sharedModel = newSharedModel }, cmd )

                ServerDetail pageModel ->
                    -- Don't fire cmds if we're already in this view
                    case prevProjectViewConstructor of
                        Just (ServerDetail _) ->
                            ( outerModel, Cmd.none )

                        _ ->
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
                                            (ApiModelHelpers.requestServer project.auth.project.uuid pageModel.serverUuid)
                            in
                            ( { outerModel | sharedModel = newNewSharedModel }, newCmd )

                ServerCreateImage _ ->
                    ( outerModel, Cmd.none )

                ServerCreate pageModel ->
                    case outerModel.viewState of
                        -- If we are already in this view state then ensure user isn't trying to choose a server count
                        -- that would exceed quota; if so, reduce server count to comply with quota.
                        -- TODO double-check that this code still actually works.
                        ProjectView _ _ (ServerCreate _) ->
                            let
                                newPageModel =
                                    case
                                        ( GetterSetters.flavorLookup project pageModel.flavorUuid
                                        , project.computeQuota
                                        , project.volumeQuota
                                        )
                                    of
                                        ( Just flavor, RemoteData.Success computeQuota, RemoteData.Success volumeQuota ) ->
                                            let
                                                availServers =
                                                    OSQuotas.overallQuotaAvailServers
                                                        (pageModel.volSizeTextInput
                                                            |> Maybe.andThen Style.Widgets.NumericTextInput.NumericTextInput.toMaybe
                                                        )
                                                        flavor
                                                        computeQuota
                                                        volumeQuota
                                            in
                                            { pageModel
                                                | count =
                                                    case availServers of
                                                        Just availServers_ ->
                                                            if pageModel.count > availServers_ then
                                                                availServers_

                                                            else
                                                                pageModel.count

                                                        Nothing ->
                                                            pageModel.count
                                            }

                                        ( _, _, _ ) ->
                                            pageModel

                                newViewState_ =
                                    ProjectView
                                        project.auth.project.uuid
                                        { createPopup = False }
                                    <|
                                        ServerCreate newPageModel
                            in
                            ( { outerModel | viewState = newViewState_ }
                            , Cmd.none
                            )

                        -- If we are just entering this view then gather everything we need
                        _ ->
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
                            ( { outerModel | sharedModel = newSharedModel }, newCmd )

                VolumeList _ ->
                    let
                        cmd =
                            -- Don't fire cmds if we're already in this view
                            case prevProjectViewConstructor of
                                Just (VolumeList _) ->
                                    Cmd.none

                                _ ->
                                    Cmd.batch
                                        [ OSVolumes.requestVolumes project
                                        , Ports.instantiateClipboardJs ()
                                        ]
                    in
                    ( outerModel, cmd )

                FloatingIpList _ ->
                    case prevProjectViewConstructor of
                        Just (FloatingIpList _) ->
                            ( outerModel, Cmd.none )

                        _ ->
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
                            ( { outerModel | sharedModel = newSharedModel }, newCmd )

                FloatingIpAssign _ ->
                    case prevProjectViewConstructor of
                        Just (FloatingIpAssign _) ->
                            ( outerModel, Cmd.none )

                        _ ->
                            let
                                ( newSharedModel, newCmd ) =
                                    ( outerModel.sharedModel, Cmd.none )
                                        |> Helpers.pipelineCmd
                                            (ApiModelHelpers.requestFloatingIps project.auth.project.uuid)
                                        |> Helpers.pipelineCmd (ApiModelHelpers.requestPorts project.auth.project.uuid)
                            in
                            ( { outerModel | sharedModel = newSharedModel }, newCmd )

                KeypairList _ ->
                    let
                        cmd =
                            -- Don't fire cmds if we're already in this view
                            case prevProjectViewConstructor of
                                Just (KeypairList _) ->
                                    Cmd.none

                                _ ->
                                    Cmd.batch
                                        [ Rest.Nova.requestKeypairs project
                                        , Ports.instantiateClipboardJs ()
                                        ]
                    in
                    ( outerModel, cmd )

                KeypairCreate _ ->
                    ( outerModel, Cmd.none )

                VolumeDetail _ ->
                    ( outerModel, Cmd.none )

                VolumeAttach _ ->
                    case prevProjectViewConstructor of
                        Just (VolumeAttach _) ->
                            ( outerModel, Cmd.none )

                        _ ->
                            let
                                ( newSharedModel, newCmd ) =
                                    ( outerModel.sharedModel, OSVolumes.requestVolumes project )
                                        |> Helpers.pipelineCmd (ApiModelHelpers.requestServers project.auth.project.uuid)
                            in
                            ( { outerModel | sharedModel = newSharedModel }, newCmd )

                VolumeMountInstructions _ ->
                    ( outerModel, Cmd.none )

                VolumeCreate _ ->
                    let
                        cmd =
                            -- If just entering this view, get volume quota
                            case outerModel.viewState of
                                ProjectView _ _ (VolumeCreate _) ->
                                    Cmd.none

                                _ ->
                                    OSQuotas.requestVolumeQuota project
                    in
                    ( outerModel, cmd )

        ( newOuterModel, viewStateCmd ) =
            modelUpdateViewState newViewState viewSpecificOuterModel
    in
    ( newOuterModel
    , Cmd.batch
        [ viewStateCmd
        , Cmd.map SharedMsg viewSpecificCmd
        ]
    )


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
