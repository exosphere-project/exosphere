module State.ViewState exposing
    ( defaultViewState
    , modelUpdateViewState
    , setNonProjectView
    , setProjectView
    )

import AppUrl.Builder
import Browser.Navigation
import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers as Helpers
import Helpers.Random as RandomHelpers
import Helpers.RemoteDataPlusPlus as RDPP
import OpenStack.Quotas as OSQuotas
import OpenStack.Volumes as OSVolumes
import Ports
import RemoteData
import Rest.ApiModelHelpers as ApiModelHelpers
import Rest.Glance
import Rest.Neutron
import Rest.Nova
import Style.Widgets.NumericTextInput.NumericTextInput
import Types.Defaults as Defaults
import Types.Types
    exposing
        ( CockpitLoginStatus(..)
        , Model
        , Msg(..)
        , NonProjectViewConstructor(..)
        , Project
        , ProjectSpecificMsgConstructor(..)
        , ProjectViewConstructor(..)
        , ServerOrigin(..)
        , ViewState(..)
        )
import View.PageTitle


setNonProjectView : Model -> NonProjectViewConstructor -> ( Model, Cmd Msg )
setNonProjectView model nonProjectViewConstructor =
    let
        prevNonProjectViewConstructor =
            case model.viewState of
                NonProjectView nonProjectViewConstructor_ ->
                    if nonProjectViewConstructor == nonProjectViewConstructor_ then
                        Nothing

                    else
                        Just nonProjectViewConstructor_

                _ ->
                    Nothing

        viewSpecificCmd =
            case nonProjectViewConstructor of
                GetSupport _ _ _ ->
                    case prevNonProjectViewConstructor of
                        Just (GetSupport _ _ _) ->
                            Cmd.none

                        _ ->
                            Ports.instantiateClipboardJs ()

                HelpAbout ->
                    case prevNonProjectViewConstructor of
                        Just HelpAbout ->
                            Cmd.none

                        _ ->
                            Ports.instantiateClipboardJs ()

                _ ->
                    Cmd.none

        newViewState =
            NonProjectView nonProjectViewConstructor
    in
    ( model, viewSpecificCmd )
        |> Helpers.pipelineCmd (modelUpdateViewState newViewState)


setProjectView : Model -> Project -> ProjectViewConstructor -> ( Model, Cmd Msg )
setProjectView model project projectViewConstructor =
    let
        prevProjectViewConstructor =
            case model.viewState of
                ProjectView projectId _ projectViewConstructor_ ->
                    if projectId == project.auth.project.uuid then
                        Just projectViewConstructor_

                    else
                        Nothing

                _ ->
                    Nothing

        newViewState =
            ProjectView project.auth.project.uuid { createPopup = False } projectViewConstructor

        projectResetCockpitStatuses project_ =
            -- We need to re-poll Cockpit to determine its availability and get a session cookie
            -- See merge request 289
            let
                serverResetCockpitStatus s =
                    case s.exoProps.serverOrigin of
                        ServerNotFromExo ->
                            s

                        ServerFromExo serverFromExoProps ->
                            let
                                newCockpitStatus =
                                    case serverFromExoProps.cockpitStatus of
                                        Ready ->
                                            ReadyButRecheck

                                        _ ->
                                            serverFromExoProps.cockpitStatus

                                newOriginProps =
                                    ServerFromExo { serverFromExoProps | cockpitStatus = newCockpitStatus }

                                newExoProps =
                                    let
                                        oldExoProps =
                                            s.exoProps
                                    in
                                    { oldExoProps | serverOrigin = newOriginProps }
                            in
                            { s | exoProps = newExoProps }
            in
            RDPP.withDefault [] project_.servers
                |> List.map serverResetCockpitStatus
                |> List.foldl (\s p -> GetterSetters.projectUpdateServer p s) project_

        viewSpecificModelAndCmd =
            case projectViewConstructor of
                ListImages _ _ ->
                    let
                        cmd =
                            -- Don't fire cmds if we're already in this view
                            case prevProjectViewConstructor of
                                Just (ListImages _ _) ->
                                    Cmd.none

                                _ ->
                                    Rest.Glance.requestImages project
                    in
                    ( model, cmd )

                ListProjectServers _ ->
                    -- Don't fire cmds if we're already in this view
                    case prevProjectViewConstructor of
                        Just (ListProjectServers _) ->
                            ( model, Cmd.none )

                        _ ->
                            let
                                newProject =
                                    project
                                        |> projectResetCockpitStatuses

                                ( newModel, newCmd ) =
                                    ApiModelHelpers.requestServers project.auth.project.uuid model
                            in
                            ( newModel
                            , Cmd.batch
                                [ newCmd
                                , Rest.Neutron.requestFloatingIps newProject
                                ]
                            )

                ServerDetail serverUuid _ ->
                    -- Don't fire cmds if we're already in this view
                    case prevProjectViewConstructor of
                        Just (ServerDetail _ _) ->
                            ( model, Cmd.none )

                        _ ->
                            let
                                newModel =
                                    project
                                        |> projectResetCockpitStatuses
                                        |> GetterSetters.modelUpdateProject model

                                cmd =
                                    Cmd.batch
                                        [ Rest.Nova.requestFlavors project
                                        , Rest.Glance.requestImages project
                                        , OSVolumes.requestVolumes project
                                        , Ports.instantiateClipboardJs ()
                                        ]
                            in
                            ( newModel, cmd )
                                |> Helpers.pipelineCmd
                                    (ApiModelHelpers.requestServer project.auth.project.uuid serverUuid)

                CreateServerImage _ _ ->
                    ( model, Cmd.none )

                CreateServer viewParams ->
                    case model.viewState of
                        -- If we are already in this view state then ensure user isn't trying to choose a server count
                        -- that would exceed quota; if so, reduce server count to comply with quota.
                        ProjectView _ _ (CreateServer _) ->
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

                                newModel =
                                    { model
                                        | viewState =
                                            ProjectView
                                                project.auth.project.uuid
                                                { createPopup = False }
                                            <|
                                                CreateServer newViewParams
                                    }
                            in
                            ( newModel
                            , Cmd.none
                            )

                        -- If we are just entering this view then gather everything we need
                        _ ->
                            let
                                newViewParamsMsg serverName_ =
                                    ProjectMsg project.auth.project.uuid <|
                                        SetProjectView <|
                                            CreateServer { viewParams | serverName = serverName_ }

                                cmd =
                                    Cmd.batch
                                        [ Rest.Nova.requestFlavors project
                                        , Rest.Nova.requestKeypairs project
                                        , RandomHelpers.generateServerName newViewParamsMsg
                                        ]
                            in
                            ( model, cmd )
                                |> Helpers.pipelineCmd (ApiModelHelpers.requestNetworks project.auth.project.uuid)
                                |> Helpers.pipelineCmd (ApiModelHelpers.requestComputeQuota project.auth.project.uuid)
                                |> Helpers.pipelineCmd (ApiModelHelpers.requestVolumeQuota project.auth.project.uuid)

                ListProjectVolumes _ ->
                    let
                        cmd =
                            -- Don't fire cmds if we're already in this view
                            case prevProjectViewConstructor of
                                Just (ListProjectVolumes _) ->
                                    Cmd.none

                                _ ->
                                    Cmd.batch
                                        [ OSVolumes.requestVolumes project
                                        , Ports.instantiateClipboardJs ()
                                        ]
                    in
                    ( model, cmd )

                ListQuotaUsage ->
                    let
                        cmd =
                            -- Don't fire cmds if we're already in this view
                            case prevProjectViewConstructor of
                                Just ListQuotaUsage ->
                                    Cmd.none

                                _ ->
                                    Cmd.batch
                                        [ OSQuotas.requestComputeQuota project
                                        , OSQuotas.requestVolumeQuota project
                                        ]
                    in
                    ( model, cmd )

                VolumeDetail _ _ ->
                    ( model, Cmd.none )

                AttachVolumeModal _ _ ->
                    case prevProjectViewConstructor of
                        Just (AttachVolumeModal _ _) ->
                            ( model, Cmd.none )

                        _ ->
                            let
                                cmd =
                                    OSVolumes.requestVolumes project
                            in
                            ( model, cmd )
                                |> Helpers.pipelineCmd (ApiModelHelpers.requestServers project.auth.project.uuid)

                MountVolInstructions _ ->
                    ( model, Cmd.none )

                CreateVolume _ _ ->
                    let
                        cmd =
                            -- If just entering this view, get volume quota
                            case model.viewState of
                                ProjectView _ _ (CreateVolume _ _) ->
                                    Cmd.none

                                _ ->
                                    OSQuotas.requestVolumeQuota project
                    in
                    ( model, cmd )
    in
    viewSpecificModelAndCmd
        |> Helpers.pipelineCmd (modelUpdateViewState newViewState)


modelUpdateViewState : ViewState -> Model -> ( Model, Cmd Msg )
modelUpdateViewState viewState model =
    -- the cmd argument is just a "passthrough", added to the Cmd that sets new URL
    let
        urlWithoutQuery url =
            String.split "?" url
                |> List.head
                |> Maybe.withDefault ""

        prevUrl =
            model.prevUrl

        newUrl =
            AppUrl.Builder.viewStateToUrl model.urlPathPrefix viewState

        newModel =
            { model
                | viewState = viewState
                , prevUrl = newUrl
            }

        newPageTitle =
            View.PageTitle.pageTitle newModel

        ( updateUrlFunc, updateMatomoCmd ) =
            if urlWithoutQuery newUrl == urlWithoutQuery prevUrl then
                -- We should `replaceUrl` and not update Matomo when just modifying the query string (setting parameters of views)
                ( Browser.Navigation.replaceUrl, Cmd.none )

            else
                -- We should `pushUrl` and update Matomo when modifying the path (moving between views)
                ( Browser.Navigation.pushUrl, Ports.pushUrlAndTitleToMatomo newPageTitle )

        urlCmd =
            -- This case statement prevents us from trying to update the URL/Matomo in the electron app (where we don't have
            -- a navigation key)
            case model.maybeNavigationKey of
                Just key ->
                    Cmd.batch
                        [ updateUrlFunc key newUrl
                        , updateMatomoCmd
                        ]

                Nothing ->
                    Cmd.none
    in
    ( newModel, urlCmd )


defaultViewState : Model -> ViewState
defaultViewState model =
    let
        defaultLoginViewState =
            model.style.defaultLoginView
                |> Maybe.map (\loginView -> NonProjectView (Login loginView))
                |> Maybe.withDefault (NonProjectView LoginPicker)
    in
    case model.projects of
        [] ->
            defaultLoginViewState

        firstProject :: _ ->
            ProjectView
                firstProject.auth.project.uuid
                { createPopup = False }
                (ListProjectServers
                    Defaults.serverListViewParams
                )
