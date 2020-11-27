module State.ViewState exposing (setProjectView, updateViewState)

import AppUrl.Builder
import Browser.Navigation
import Helpers.Helpers as Helpers
import Helpers.ModelLookups as ModelLookups
import Helpers.Random as RandomHelpers
import Helpers.RemoteDataPlusPlus as RDPP
import OpenStack.Quotas
import OpenStack.Volumes as OSVolumes
import Ports
import RemoteData
import Rest.Glance
import Rest.Neutron
import Rest.Nova
import Style.Widgets.NumericTextInput.NumericTextInput
import Types.Types
    exposing
        ( CockpitLoginStatus(..)
        , Model
        , Msg(..)
        , Project
        , ProjectSpecificMsgConstructor(..)
        , ProjectViewConstructor(..)
        , ServerOrigin(..)
        , ViewState(..)
        )


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
                |> List.foldl (\s p -> Helpers.projectUpdateServer p s) project_

        ( viewSpecificModel, viewSpecificCmd ) =
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
                                newModel =
                                    project
                                        |> Helpers.projectSetServersLoading model.clientCurrentTime
                                        |> projectResetCockpitStatuses
                                        |> Helpers.modelUpdateProject model

                                cmd =
                                    [ Rest.Nova.requestServers
                                    , Rest.Neutron.requestFloatingIps
                                    ]
                                        |> List.map (\x -> x project)
                                        |> Cmd.batch
                            in
                            ( newModel, cmd )

                ServerDetail serverUuid _ ->
                    -- Don't fire cmds if we're already in this view
                    case prevProjectViewConstructor of
                        Just (ServerDetail _ _) ->
                            ( model, Cmd.none )

                        _ ->
                            let
                                newModel =
                                    project
                                        |> (\p -> Helpers.projectSetServerLoading p serverUuid)
                                        |> projectResetCockpitStatuses
                                        |> Helpers.modelUpdateProject model

                                cmd =
                                    Cmd.batch
                                        [ Rest.Nova.requestServer project serverUuid
                                        , Rest.Nova.requestFlavors project
                                        , Rest.Glance.requestImages project
                                        , OSVolumes.requestVolumes project
                                        , Ports.instantiateClipboardJs ()
                                        ]
                            in
                            ( newModel, cmd )

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
                                        ( ModelLookups.flavorLookup project viewParams.flavorUuid
                                        , project.computeQuota
                                        , project.volumeQuota
                                        )
                                    of
                                        ( Just flavor, RemoteData.Success computeQuota, RemoteData.Success volumeQuota ) ->
                                            let
                                                availServers =
                                                    Helpers.overallQuotaAvailServers
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

                                newProject =
                                    { project
                                        | computeQuota = RemoteData.Loading
                                        , volumeQuota = RemoteData.Loading
                                        , networks = RDPP.setLoading project.networks model.clientCurrentTime
                                    }

                                newModel =
                                    Helpers.modelUpdateProject model newProject

                                cmd =
                                    Cmd.batch
                                        [ Rest.Nova.requestFlavors project
                                        , Rest.Nova.requestKeypairs project
                                        , Rest.Neutron.requestNetworks project
                                        , RandomHelpers.generateServerName newViewParamsMsg
                                        , OpenStack.Quotas.requestComputeQuota project
                                        , OpenStack.Quotas.requestVolumeQuota project
                                        ]
                            in
                            ( newModel, cmd )

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
                                        [ OpenStack.Quotas.requestComputeQuota project
                                        , OpenStack.Quotas.requestVolumeQuota project
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
                                newModel =
                                    project
                                        |> Helpers.projectSetServersLoading model.clientCurrentTime
                                        |> Helpers.modelUpdateProject model

                                cmd =
                                    Cmd.batch
                                        [ Rest.Nova.requestServers project
                                        , OSVolumes.requestVolumes project
                                        ]
                            in
                            ( newModel, cmd )

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
                                    OpenStack.Quotas.requestVolumeQuota project
                    in
                    ( model, cmd )
    in
    updateViewState viewSpecificModel viewSpecificCmd newViewState


updateViewState : Model -> Cmd Msg -> ViewState -> ( Model, Cmd Msg )
updateViewState model cmd viewState =
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

        -- We should `pushUrl` when modifying the path (moving between views), `replaceUrl` when just modifying the query string (setting parameters of views)
        updateUrlFunc =
            if urlWithoutQuery newUrl == urlWithoutQuery prevUrl then
                Browser.Navigation.replaceUrl

            else
                Browser.Navigation.pushUrl

        urlCmd =
            -- This case statement prevents us from trying to update the URL in the electron app (where we don't have
            -- a navigation key)
            case model.maybeNavigationKey of
                Just key ->
                    updateUrlFunc key newUrl

                Nothing ->
                    Cmd.none
    in
    ( newModel, Cmd.batch [ cmd, urlCmd ] )
