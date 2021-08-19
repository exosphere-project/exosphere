module State.ViewState exposing
    ( defaultLoginViewState
    , defaultViewState
    , modelUpdateViewState
    , setNonProjectView
    , setProjectView
    , viewStateToSupportableItem
    )

import AppUrl.Builder
import Browser.Navigation
import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers as Helpers
import Helpers.Random as RandomHelpers
import OpenStack.Quotas as OSQuotas
import OpenStack.Types as OSTypes
import OpenStack.Volumes as OSVolumes
import Page.AllResources
import Page.LoginJetstream
import Page.LoginOpenstack
import Ports
import RemoteData
import Rest.ApiModelHelpers as ApiModelHelpers
import Rest.Glance
import Rest.Keystone
import Rest.Nova
import State.Error
import Style.Widgets.NumericTextInput.NumericTextInput
import Time
import Types.Error as Error
import Types.HelperTypes as HelperTypes exposing (DefaultLoginView(..), UnscopedProvider)
import Types.OuterModel exposing (OuterModel)
import Types.OuterMsg exposing (OuterMsg(..))
import Types.Project exposing (Project)
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))
import Types.View exposing (LoginView(..), NonProjectViewConstructor(..), ProjectViewConstructor(..), ViewState(..))
import View.Helpers
import View.PageTitle


setNonProjectView : NonProjectViewConstructor -> OuterModel -> ( OuterModel, Cmd OuterMsg )
setNonProjectView nonProjectViewConstructor outerModel =
    let
        ( viewSpecificSharedModel, viewSpecificCmd ) =
            case nonProjectViewConstructor of
                LoadingUnscopedProjects authTokenStr ->
                    -- TODO revisit this
                    -- This is a smell. We're using view state solely to pass information for an XHR, and we're figuring out here whether we can actually make that XHR. This logic should probably live somewhere else.
                    case outerModel.sharedModel.openIdConnectLoginConfig of
                        Nothing ->
                            let
                                errorContext =
                                    Error.ErrorContext
                                        "Load projects for provider authenticated via OpenID Connect"
                                        Error.ErrorCrit
                                        Nothing
                            in
                            State.Error.processStringError
                                outerModel.sharedModel
                                errorContext
                                "This deployment of Exosphere is not configured to use OpenID Connect."

                        Just openIdConnectLoginConfig ->
                            let
                                oneHourMillis =
                                    1000 * 60 * 60

                                tokenExpiry =
                                    -- One hour later? This should never matter
                                    Time.posixToMillis outerModel.sharedModel.clientCurrentTime
                                        + oneHourMillis
                                        |> Time.millisToPosix

                                unscopedProvider =
                                    UnscopedProvider
                                        openIdConnectLoginConfig.keystoneAuthUrl
                                        (OSTypes.UnscopedAuthToken
                                            tokenExpiry
                                            authTokenStr
                                        )
                                        RemoteData.NotAsked

                                newUnscopedProviders =
                                    unscopedProvider :: outerModel.sharedModel.unscopedProviders

                                oldSharedModel =
                                    outerModel.sharedModel
                            in
                            ( { oldSharedModel | unscopedProviders = newUnscopedProviders }
                            , Rest.Keystone.requestUnscopedProjects unscopedProvider oldSharedModel.cloudCorsProxyUrl
                            )

                _ ->
                    ( outerModel.sharedModel, Cmd.none )

        newViewState =
            NonProjectView nonProjectViewConstructor

        ( newOuterModel, viewStateCmd ) =
            modelUpdateViewState newViewState { outerModel | sharedModel = viewSpecificSharedModel }
    in
    ( newOuterModel
    , Cmd.batch
        [ viewStateCmd
        , Cmd.map SharedMsg viewSpecificCmd
        ]
    )


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
                AllResources _ ->
                    -- Don't fire cmds if we're already in this view
                    case prevProjectViewConstructor of
                        Just (AllResources _) ->
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

                ServerDetail model ->
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
                                            (ApiModelHelpers.requestServer project.auth.project.uuid model.serverUuid)
                            in
                            ( { outerModel | sharedModel = newNewSharedModel }, newCmd )

                ServerCreateImage _ ->
                    ( outerModel, Cmd.none )

                ServerCreate viewParams ->
                    case outerModel.viewState of
                        -- If we are already in this view state then ensure user isn't trying to choose a server count
                        -- that would exceed quota; if so, reduce server count to comply with quota.
                        -- TODO double-check that this code still actually works.
                        ProjectView _ _ (ServerCreate _) ->
                            let
                                newViewParams =
                                    case
                                        ( GetterSetters.flavorLookup project viewParams.flavorUuid
                                        , project.computeQuota
                                        , project.volumeQuota
                                        )
                                    of
                                        ( Just flavor, RemoteData.Success computeQuota, RemoteData.Success volumeQuota ) ->
                                            let
                                                availServers =
                                                    OSQuotas.overallQuotaAvailServers
                                                        (viewParams.volSizeTextInput
                                                            |> Maybe.andThen Style.Widgets.NumericTextInput.NumericTextInput.toMaybe
                                                        )
                                                        flavor
                                                        computeQuota
                                                        volumeQuota
                                            in
                                            { viewParams
                                                | count =
                                                    case availServers of
                                                        Just availServers_ ->
                                                            if viewParams.count > availServers_ then
                                                                availServers_

                                                            else
                                                                viewParams.count

                                                        Nothing ->
                                                            viewParams.count
                                            }

                                        ( _, _, _ ) ->
                                            viewParams

                                newViewState_ =
                                    ProjectView
                                        project.auth.project.uuid
                                        { createPopup = False }
                                    <|
                                        ServerCreate newViewParams
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
                    -- TODO move stuff out of here
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


defaultViewState : SharedModel -> ViewState
defaultViewState model =
    case model.projects of
        [] ->
            NonProjectView <| defaultLoginViewState model.style.defaultLoginView

        firstProject :: _ ->
            ProjectView
                firstProject.auth.project.uuid
                { createPopup = False }
                (AllResources Page.AllResources.init)


defaultLoginViewState : Maybe DefaultLoginView -> NonProjectViewConstructor
defaultLoginViewState maybeDefaultLoginView =
    case maybeDefaultLoginView of
        Nothing ->
            LoginPicker

        Just defaultLoginView ->
            case defaultLoginView of
                DefaultLoginOpenstack ->
                    Login <| LoginOpenstack Page.LoginOpenstack.init

                DefaultLoginJetstream ->
                    Login <| LoginJetstream Page.LoginJetstream.init


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
                ServerCreate createServerViewParams ->
                    ( HelperTypes.SupportableImage, Just createServerViewParams.imageUuid )

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

                VolumeMountInstructions attachment ->
                    ( HelperTypes.SupportableServer, Just attachment.serverUuid )

                _ ->
                    ( HelperTypes.SupportableProject, Just projectUuid )
    in
    case viewState of
        NonProjectView _ ->
            Nothing

        ProjectView projectUuid _ projectViewConstructor ->
            Just <| supportableProjectItem projectUuid projectViewConstructor
