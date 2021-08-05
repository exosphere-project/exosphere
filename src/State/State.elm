module State.State exposing (update)

import AppUrl.Builder
import AppUrl.Parser
import Browser.Navigation
import Helpers.ExoSetupStatus
import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers as Helpers
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.ServerResourceUsage
import Helpers.Url as UrlHelpers
import Http
import LocalStorage.LocalStorage as LocalStorage
import Maybe
import OpenStack.ServerPassword as OSServerPassword
import OpenStack.ServerTags as OSServerTags
import OpenStack.ServerVolumes as OSSvrVols
import OpenStack.Types as OSTypes
import OpenStack.Volumes as OSVolumes
import Orchestration.Orchestration as Orchestration
import Ports
import RemoteData
import Rest.ApiModelHelpers as ApiModelHelpers
import Rest.Glance
import Rest.Keystone
import Rest.Neutron
import Rest.Nova
import State.Auth
import State.Error
import State.ViewState as ViewStateHelpers
import Style.Toast
import Style.Widgets.NumericTextInput.NumericTextInput
import Task
import Time
import Toasty
import Types.Defaults as Defaults
import Types.Error as Error exposing (ErrorContext, ErrorLevel(..))
import Types.Guacamole as GuacTypes
import Types.HelperTypes as HelperTypes exposing (HttpRequestMethod(..), UnscopedProviderProject)
import Types.Msg exposing (Msg(..), ProjectSpecificMsgConstructor(..), ServerSpecificMsgConstructor(..), TickInterval)
import Types.OuterModel exposing (OuterModel)
import Types.Project exposing (Endpoints, Project, ProjectSecret(..))
import Types.Server exposing (ExoSetupStatus(..), NewServerNetworkOptions(..), Server, ServerFromExoProps, ServerOrigin(..), currentExoServerVersion)
import Types.ServerResourceUsage
import Types.Types exposing (SharedModel)
import Types.View
    exposing
        ( LoginView(..)
        , NonProjectViewConstructor(..)
        , OpenstackLoginFormEntryType(..)
        , OpenstackLoginViewParams
        , ProjectViewConstructor(..)
        , ViewState(..)
        )


update : Msg -> OuterModel -> ( OuterModel, Cmd Msg )
update msg outerModel =
    {- We want to `setStorage` on every update. This function adds the setStorage
       command for every step of the update function.
    -}
    let
        ( newOuterModel, cmds ) =
            updateUnderlying msg outerModel

        orchestrationTimeCmd =
            -- Each trip through the runtime, we get the time and feed it to orchestration module
            case msg of
                DoOrchestration _ ->
                    Cmd.none

                Tick _ _ ->
                    Cmd.none

                _ ->
                    Task.perform (\posix -> DoOrchestration posix) Time.now
    in
    ( newOuterModel
    , Cmd.batch
        [ Ports.setStorage (LocalStorage.generateStoredState newOuterModel.sharedModel)
        , orchestrationTimeCmd
        , cmds
        ]
    )


mapSharedToOuter : OuterModel -> ( SharedModel, Cmd Msg ) -> ( OuterModel, Cmd Msg )
mapSharedToOuter outerModel ( newSharedModel, cmd ) =
    -- hopefully temporary function
    ( { sharedModel = newSharedModel, viewState = outerModel.viewState }, cmd )


updateUnderlying : Msg -> OuterModel -> ( OuterModel, Cmd Msg )
updateUnderlying msg outerModel =
    let
        sharedModel =
            outerModel.sharedModel
    in
    case msg of
        NestedViewMsg _ ->
            ( outerModel, Cmd.none )

        ToastyMsg subMsg ->
            Toasty.update Style.Toast.toastConfig ToastyMsg subMsg outerModel.sharedModel
                |> mapSharedToOuter outerModel

        MsgChangeWindowSize x y ->
            ( { sharedModel | windowSize = { width = x, height = y } }, Cmd.none )
                |> mapSharedToOuter outerModel

        Tick interval time ->
            processTick outerModel interval time
                |> mapSharedToOuter outerModel

        DoOrchestration posixTime ->
            Orchestration.orchModel sharedModel posixTime
                |> mapSharedToOuter outerModel

        SetNonProjectView nonProjectViewConstructor ->
            ViewStateHelpers.setNonProjectView nonProjectViewConstructor outerModel

        HandleApiErrorWithBody errorContext error ->
            State.Error.processSynchronousApiError sharedModel errorContext error
                |> mapSharedToOuter outerModel

        RequestUnscopedToken openstackLoginUnscoped ->
            let
                creds =
                    -- Ensure auth URL includes port number and version
                    { openstackLoginUnscoped
                        | authUrl =
                            State.Auth.authUrlWithPortAndVersion openstackLoginUnscoped.authUrl
                    }
            in
            ( outerModel, Rest.Keystone.requestUnscopedAuthToken sharedModel.cloudCorsProxyUrl creds )

        JetstreamLogin jetstreamCreds ->
            let
                openstackCredsList =
                    State.Auth.jetstreamToOpenstackCreds jetstreamCreds

                cmds =
                    List.map
                        (\creds -> Rest.Keystone.requestUnscopedAuthToken sharedModel.cloudCorsProxyUrl creds)
                        openstackCredsList
            in
            ( outerModel, Cmd.batch cmds )

        ReceiveScopedAuthToken ( metadata, response ) ->
            case Rest.Keystone.decodeScopedAuthToken <| Http.GoodStatus_ metadata response of
                Err error ->
                    State.Error.processStringError
                        sharedModel
                        (ErrorContext
                            "decode scoped auth token"
                            ErrorCrit
                            Nothing
                        )
                        error
                        |> mapSharedToOuter outerModel

                Ok authToken ->
                    case Helpers.serviceCatalogToEndpoints authToken.catalog of
                        Err e ->
                            State.Error.processStringError
                                sharedModel
                                (ErrorContext
                                    "Decode project endpoints"
                                    ErrorCrit
                                    (Just "Please check with your cloud administrator or the Exosphere developers.")
                                )
                                e
                                |> mapSharedToOuter outerModel

                        Ok endpoints ->
                            let
                                projectId =
                                    authToken.project.uuid
                            in
                            -- If we don't have a project with same name + authUrl then create one, if we do then update its OSTypes.AuthToken
                            -- This code ensures we don't end up with duplicate projects on the same provider in our model.
                            case
                                GetterSetters.projectLookup sharedModel <| projectId
                            of
                                Nothing ->
                                    createProject outerModel authToken endpoints

                                Just project ->
                                    -- If we don't have an application credential for this project yet, then get one
                                    let
                                        appCredCmd =
                                            case project.secret of
                                                ApplicationCredential _ ->
                                                    Cmd.none

                                                _ ->
                                                    Rest.Keystone.requestAppCredential
                                                        sharedModel.clientUuid
                                                        sharedModel.clientCurrentTime
                                                        project

                                        ( newModel, updateTokenCmd ) =
                                            State.Auth.projectUpdateAuthToken sharedModel project authToken
                                    in
                                    ( newModel, Cmd.batch [ appCredCmd, updateTokenCmd ] )
                                        |> mapSharedToOuter outerModel

        ReceiveUnscopedAuthToken keystoneUrl ( metadata, response ) ->
            case Rest.Keystone.decodeUnscopedAuthToken <| Http.GoodStatus_ metadata response of
                Err error ->
                    State.Error.processStringError
                        sharedModel
                        (ErrorContext
                            "decode scoped auth token"
                            ErrorCrit
                            Nothing
                        )
                        error
                        |> mapSharedToOuter outerModel

                Ok authToken ->
                    case
                        GetterSetters.providerLookup sharedModel keystoneUrl
                    of
                        Just unscopedProvider ->
                            -- We already have an unscoped provider in the model with the same auth URL, update its token
                            State.Auth.unscopedProviderUpdateAuthToken
                                sharedModel
                                unscopedProvider
                                authToken
                                |> mapSharedToOuter outerModel

                        Nothing ->
                            -- We don't have an unscoped provider with the same auth URL, create it
                            createUnscopedProvider sharedModel authToken keystoneUrl
                                |> mapSharedToOuter outerModel

        ReceiveUnscopedProjects keystoneUrl unscopedProjects ->
            case
                GetterSetters.providerLookup sharedModel keystoneUrl
            of
                Just provider ->
                    let
                        newProvider =
                            { provider | projectsAvailable = RemoteData.Success unscopedProjects }

                        newSharedModel =
                            GetterSetters.modelUpdateUnscopedProvider sharedModel newProvider

                        newOuterModel =
                            { outerModel | sharedModel = newSharedModel }
                    in
                    -- If we are not already on a SelectProjects view, then go there
                    case outerModel.viewState of
                        NonProjectView (SelectProjects _ _) ->
                            ( newOuterModel, Cmd.none )

                        _ ->
                            ViewStateHelpers.modelUpdateViewState
                                (NonProjectView <|
                                    SelectProjects newProvider.authUrl []
                                )
                                newOuterModel

                Nothing ->
                    -- Provider not found, may have been removed, nothing to do
                    ( outerModel, Cmd.none )

        RequestProjectLoginFromProvider keystoneUrl desiredProjects ->
            case GetterSetters.providerLookup sharedModel keystoneUrl of
                Just provider ->
                    let
                        buildLoginRequest : UnscopedProviderProject -> Cmd Msg
                        buildLoginRequest project =
                            Rest.Keystone.requestScopedAuthToken
                                sharedModel.cloudCorsProxyUrl
                            <|
                                OSTypes.TokenCreds
                                    keystoneUrl
                                    provider.token
                                    project.project.uuid

                        loginRequests =
                            List.map buildLoginRequest desiredProjects
                                |> Cmd.batch

                        -- Remove unscoped provider from model now that we have selected projects from it
                        newUnscopedProviders =
                            List.filter
                                (\p -> p.authUrl /= keystoneUrl)
                                sharedModel.unscopedProviders

                        -- If we still have at least one unscoped provider in the model then ask the user to choose projects from it
                        newViewStateFunc =
                            case List.head newUnscopedProviders of
                                Just unscopedProvider ->
                                    ViewStateHelpers.setNonProjectView
                                        (SelectProjects unscopedProvider.authUrl [])

                                Nothing ->
                                    -- If we have at least one project then show it, else show the login page
                                    case List.head sharedModel.projects of
                                        Just project ->
                                            ViewStateHelpers.setProjectView
                                                project
                                            <|
                                                AllResources Defaults.allResourcesListViewParams

                                        Nothing ->
                                            ViewStateHelpers.setNonProjectView
                                                LoginPicker

                        sharedModelUpdatedUnscopedProviders =
                            { sharedModel | unscopedProviders = newUnscopedProviders }

                        ( newOuterModel, viewStateCmds ) =
                            newViewStateFunc { outerModel | sharedModel = sharedModelUpdatedUnscopedProviders }
                    in
                    ( newOuterModel, Cmd.batch [ loginRequests, viewStateCmds ] )

                Nothing ->
                    State.Error.processStringError
                        sharedModel
                        (ErrorContext
                            ("look for OpenStack provider with Keystone URL " ++ keystoneUrl)
                            ErrorCrit
                            Nothing
                        )
                        "Provider could not found in Exosphere's list of Providers."
                        |> mapSharedToOuter outerModel

        ProjectMsg projectIdentifier innerMsg ->
            case GetterSetters.projectLookup sharedModel projectIdentifier of
                Nothing ->
                    -- Project not found, may have been removed, nothing to do
                    ( outerModel, Cmd.none )

                Just project ->
                    processProjectSpecificMsg outerModel project innerMsg

        {- Form inputs -}
        SubmitOpenRc openstackCreds openRc ->
            let
                newCreds =
                    State.Auth.processOpenRc openstackCreds openRc

                newViewState =
                    NonProjectView <| Login <| LoginOpenstack <| OpenstackLoginViewParams newCreds openRc LoginViewCredsEntry
            in
            ViewStateHelpers.modelUpdateViewState newViewState outerModel

        OpenNewWindow url ->
            ( outerModel, Ports.openNewWindow url )

        NavigateToUrl url ->
            ( outerModel, Browser.Navigation.load url )

        UrlChange url ->
            -- This handles presses of the browser back/forward button
            let
                exoJustSetThisUrl =
                    -- If this is a URL that Exosphere just set via StateHelpers.updateViewState, then ignore it
                    UrlHelpers.urlPathQueryMatches url sharedModel.prevUrl
            in
            if exoJustSetThisUrl then
                ( outerModel, Cmd.none )

            else
                case
                    AppUrl.Parser.urlToViewState
                        sharedModel.urlPathPrefix
                        (ViewStateHelpers.defaultViewState sharedModel)
                        url
                of
                    Just newViewState ->
                        ( { outerModel
                            | viewState = newViewState
                            , sharedModel =
                                { sharedModel
                                    | prevUrl = AppUrl.Builder.viewStateToUrl sharedModel.urlPathPrefix newViewState
                                }
                          }
                        , Cmd.none
                        )

                    Nothing ->
                        ( { outerModel
                            | viewState = NonProjectView PageNotFound
                          }
                        , Cmd.none
                        )

        SetStyle styleMode ->
            let
                oldStyle =
                    sharedModel.style

                newStyle =
                    { oldStyle | styleMode = styleMode }
            in
            ( { sharedModel | style = newStyle }, Cmd.none )
                |> mapSharedToOuter outerModel

        NoOp ->
            ( outerModel, Cmd.none )


processTick : OuterModel -> TickInterval -> Time.Posix -> ( SharedModel, Cmd Msg )
processTick outerModel interval time =
    let
        serverVolsNeedFrequentPoll : Project -> Server -> Bool
        serverVolsNeedFrequentPoll project server =
            GetterSetters.getVolsAttachedToServer project server
                |> List.any volNeedsFrequentPoll

        volNeedsFrequentPoll volume =
            not <|
                List.member
                    volume.status
                    [ OSTypes.Available
                    , OSTypes.Maintenance
                    , OSTypes.InUse
                    , OSTypes.Error
                    , OSTypes.ErrorDeleting
                    , OSTypes.ErrorBackingUp
                    , OSTypes.ErrorRestoring
                    , OSTypes.ErrorExtending
                    ]

        viewIndependentCmd =
            if interval == 5 then
                Task.perform (\posix -> DoOrchestration posix) Time.now

            else
                Cmd.none

        ( viewDependentModel, viewDependentCmd ) =
            {- TODO move some of this to Orchestration? -}
            case outerModel.viewState of
                ExampleNestedView _ ->
                    ( outerModel.sharedModel, Cmd.none )

                NonProjectView _ ->
                    ( outerModel.sharedModel, Cmd.none )

                ProjectView projectName _ projectViewState ->
                    case GetterSetters.projectLookup outerModel.sharedModel projectName of
                        Nothing ->
                            {- Should this throw an error? -}
                            ( outerModel.sharedModel, Cmd.none )

                        Just project ->
                            let
                                pollVolumes : ( SharedModel, Cmd Msg )
                                pollVolumes =
                                    ( outerModel.sharedModel
                                    , case interval of
                                        5 ->
                                            if List.any volNeedsFrequentPoll (RemoteData.withDefault [] project.volumes) then
                                                OSVolumes.requestVolumes project

                                            else
                                                Cmd.none

                                        60 ->
                                            OSVolumes.requestVolumes project

                                        _ ->
                                            Cmd.none
                                    )
                            in
                            case projectViewState of
                                AllResources _ ->
                                    pollVolumes

                                ServerDetail serverUuid _ ->
                                    let
                                        volCmd =
                                            OSVolumes.requestVolumes project
                                    in
                                    case interval of
                                        5 ->
                                            case GetterSetters.serverLookup project serverUuid of
                                                Just server ->
                                                    ( outerModel.sharedModel
                                                    , if serverVolsNeedFrequentPoll project server then
                                                        volCmd

                                                      else
                                                        Cmd.none
                                                    )

                                                Nothing ->
                                                    ( outerModel.sharedModel, Cmd.none )

                                        300 ->
                                            ( outerModel.sharedModel, volCmd )

                                        _ ->
                                            ( outerModel.sharedModel, Cmd.none )

                                ListProjectVolumes _ ->
                                    pollVolumes

                                VolumeDetail volumeUuid _ ->
                                    ( outerModel.sharedModel
                                    , case interval of
                                        5 ->
                                            case GetterSetters.volumeLookup project volumeUuid of
                                                Nothing ->
                                                    Cmd.none

                                                Just volume ->
                                                    if volNeedsFrequentPoll volume then
                                                        OSVolumes.requestVolumes project

                                                    else
                                                        Cmd.none

                                        60 ->
                                            OSVolumes.requestVolumes project

                                        _ ->
                                            Cmd.none
                                    )

                                _ ->
                                    ( outerModel.sharedModel, Cmd.none )
    in
    ( { viewDependentModel | clientCurrentTime = time }
    , Cmd.batch
        [ viewDependentCmd
        , viewIndependentCmd
        ]
    )


processProjectSpecificMsg : OuterModel -> Project -> ProjectSpecificMsgConstructor -> ( OuterModel, Cmd Msg )
processProjectSpecificMsg outerModel project msg =
    let
        sharedModel =
            outerModel.sharedModel
    in
    case msg of
        SetProjectView projectViewConstructor ->
            ViewStateHelpers.setProjectView project projectViewConstructor outerModel

        PrepareCredentialedRequest requestProto posixTime ->
            let
                -- Add proxy URL
                requestNeedingToken =
                    requestProto sharedModel.cloudCorsProxyUrl

                currentTimeMillis =
                    posixTime |> Time.posixToMillis

                tokenExpireTimeMillis =
                    project.auth.expiresAt |> Time.posixToMillis

                tokenExpired =
                    -- Token expiring within 5 minutes
                    tokenExpireTimeMillis < currentTimeMillis + 300000
            in
            if not tokenExpired then
                -- Token still valid, fire the request with current token
                ( sharedModel, requestNeedingToken project.auth.tokenValue )
                    |> mapSharedToOuter outerModel

            else
                -- Token is expired (or nearly expired) so we add request to list of pending requests and refresh that token
                let
                    newPQRs =
                        requestNeedingToken :: project.pendingCredentialedRequests

                    newProject =
                        { project | pendingCredentialedRequests = newPQRs }

                    newSharedModel =
                        GetterSetters.modelUpdateProject sharedModel newProject

                    cmdResult =
                        State.Auth.requestAuthToken newSharedModel newProject
                in
                case cmdResult of
                    Err e ->
                        let
                            errorContext =
                                ErrorContext
                                    ("Refresh authentication token for project " ++ project.auth.project.name)
                                    ErrorCrit
                                    (Just "Please remove this project from Exosphere and add it again.")
                        in
                        State.Error.processStringError newSharedModel errorContext e
                            |> mapSharedToOuter outerModel

                    Ok cmd ->
                        ( newSharedModel, cmd )
                            |> mapSharedToOuter outerModel

        ToggleCreatePopup ->
            case outerModel.viewState of
                ProjectView projectId viewParams viewConstructor ->
                    let
                        newViewState =
                            ProjectView
                                projectId
                                { viewParams
                                    | createPopup = not viewParams.createPopup
                                }
                                viewConstructor
                    in
                    ViewStateHelpers.modelUpdateViewState newViewState outerModel

                _ ->
                    ( outerModel, Cmd.none )

        RemoveProject ->
            let
                newProjects =
                    List.filter (\p -> p.auth.project.uuid /= project.auth.project.uuid) sharedModel.projects

                newViewState =
                    case outerModel.viewState of
                        ExampleNestedView _ ->
                            outerModel.viewState

                        NonProjectView _ ->
                            -- If we are not in a project-specific view then stay there
                            outerModel.viewState

                        ProjectView _ _ _ ->
                            -- If we have any projects switch to the first one in the list, otherwise switch to login view
                            case List.head newProjects of
                                Just p ->
                                    ProjectView
                                        p.auth.project.uuid
                                        Defaults.projectViewParams
                                    <|
                                        AllResources
                                            Defaults.allResourcesListViewParams

                                Nothing ->
                                    NonProjectView <| LoginPicker

                sharedModelUpdatedProjects =
                    { sharedModel | projects = newProjects }
            in
            ViewStateHelpers.modelUpdateViewState newViewState { outerModel | sharedModel = sharedModelUpdatedProjects }

        ServerMsg serverUuid serverMsgConstructor ->
            case GetterSetters.serverLookup project serverUuid of
                Nothing ->
                    let
                        errorContext =
                            ErrorContext
                                "receive results of API call for a specific server"
                                ErrorDebug
                                Nothing
                    in
                    State.Error.processStringError
                        sharedModel
                        errorContext
                        (String.join " "
                            [ "Instance"
                            , serverUuid
                            , "does not exist in the model, it may have been deleted."
                            ]
                        )
                        |> mapSharedToOuter outerModel

                Just server ->
                    processServerSpecificMsg outerModel project server serverMsgConstructor

        RequestServers ->
            ApiModelHelpers.requestServers project.auth.project.uuid sharedModel
                |> mapSharedToOuter outerModel

        RequestCreateServer viewParams networkUuid ->
            let
                createServerRequest =
                    { name = viewParams.serverName
                    , count = viewParams.count
                    , imageUuid = viewParams.imageUuid
                    , flavorUuid = viewParams.flavorUuid
                    , volBackedSizeGb =
                        viewParams.volSizeTextInput
                            |> Maybe.andThen Style.Widgets.NumericTextInput.NumericTextInput.toMaybe
                    , networkUuid = networkUuid
                    , keypairName = viewParams.keypairName
                    , userData =
                        Helpers.renderUserDataTemplate
                            project
                            viewParams.userDataTemplate
                            viewParams.keypairName
                            (viewParams.deployGuacamole |> Maybe.withDefault False)
                            viewParams.deployDesktopEnvironment
                            viewParams.installOperatingSystemUpdates
                            sharedModel.instanceConfigMgtRepoUrl
                            sharedModel.instanceConfigMgtRepoCheckout
                    , metadata =
                        Helpers.newServerMetadata
                            currentExoServerVersion
                            sharedModel.clientUuid
                            (viewParams.deployGuacamole |> Maybe.withDefault False)
                            viewParams.deployDesktopEnvironment
                            project.auth.user.name
                            viewParams.floatingIpCreationOption
                    }
            in
            ( outerModel, Rest.Nova.requestCreateServer project createServerRequest )

        RequestCreateVolume name size ->
            let
                createVolumeRequest =
                    { name = name
                    , size = size
                    }
            in
            ( outerModel, OSVolumes.requestCreateVolume project createVolumeRequest )

        RequestDeleteVolume volumeUuid ->
            ( outerModel, OSVolumes.requestDeleteVolume project volumeUuid )

        RequestDetachVolume volumeUuid ->
            let
                maybeVolume =
                    OSVolumes.volumeLookup project volumeUuid

                maybeServerUuid =
                    maybeVolume
                        |> Maybe.map (GetterSetters.getServersWithVolAttached project)
                        |> Maybe.andThen List.head
            in
            case maybeServerUuid of
                Just serverUuid ->
                    ( outerModel, OSSvrVols.requestDetachVolume project serverUuid volumeUuid )

                Nothing ->
                    State.Error.processStringError
                        sharedModel
                        (ErrorContext
                            ("look for server UUID with attached volume " ++ volumeUuid)
                            ErrorCrit
                            Nothing
                        )
                        "Could not determine server attached to this volume."
                        |> mapSharedToOuter outerModel

        RequestDeleteFloatingIp floatingIpAddress ->
            ( outerModel, Rest.Neutron.requestDeleteFloatingIp project floatingIpAddress )

        RequestAssignFloatingIp port_ floatingIpUuid ->
            let
                ( newOuterModel, setViewCmd ) =
                    ViewStateHelpers.setProjectView project (ListFloatingIps Defaults.floatingIpListViewParams) outerModel
            in
            ( newOuterModel
            , Cmd.batch
                [ setViewCmd
                , Rest.Neutron.requestAssignFloatingIp project port_ floatingIpUuid
                ]
            )

        RequestUnassignFloatingIp floatingIpUuid ->
            ( outerModel, Rest.Neutron.requestUnassignFloatingIp project floatingIpUuid )

        ReceiveImages images ->
            Rest.Glance.receiveImages sharedModel project images
                |> mapSharedToOuter outerModel

        RequestDeleteServers serverUuidsToDelete ->
            let
                applyDelete : OSTypes.ServerUuid -> ( Project, Cmd Msg ) -> ( Project, Cmd Msg )
                applyDelete serverUuid projCmdTuple =
                    let
                        ( delServerProj, delServerCmd ) =
                            requestDeleteServer (Tuple.first projCmdTuple) serverUuid False
                    in
                    ( delServerProj, Cmd.batch [ Tuple.second projCmdTuple, delServerCmd ] )

                ( newProject, cmd ) =
                    List.foldl
                        applyDelete
                        ( project, Cmd.none )
                        serverUuidsToDelete
            in
            ( GetterSetters.modelUpdateProject sharedModel newProject
            , cmd
            )
                |> mapSharedToOuter outerModel

        ReceiveServers errorContext result ->
            case result of
                Ok servers ->
                    Rest.Nova.receiveServers sharedModel project servers
                        |> mapSharedToOuter outerModel

                Err e ->
                    let
                        oldServersData =
                            project.servers.data

                        newProject =
                            { project
                                | servers =
                                    RDPP.RemoteDataPlusPlus
                                        oldServersData
                                        (RDPP.NotLoading (Just ( e, sharedModel.clientCurrentTime )))
                            }

                        newSharedModel =
                            GetterSetters.modelUpdateProject sharedModel newProject
                    in
                    State.Error.processSynchronousApiError newSharedModel errorContext e
                        |> mapSharedToOuter outerModel

        ReceiveServer serverUuid errorContext result ->
            case result of
                Ok server ->
                    Rest.Nova.receiveServer sharedModel project server
                        |> mapSharedToOuter outerModel

                Err httpErrorWithBody ->
                    let
                        httpError =
                            httpErrorWithBody.error

                        non404 =
                            case GetterSetters.serverLookup project serverUuid of
                                Nothing ->
                                    -- Server not in project, may have been deleted, ignoring this error
                                    ( outerModel, Cmd.none )

                                Just server ->
                                    -- Reset receivedTime and loadingSeparately
                                    let
                                        oldExoProps =
                                            server.exoProps

                                        newExoProps =
                                            { oldExoProps
                                                | receivedTime = Nothing
                                                , loadingSeparately = False
                                            }

                                        newServer =
                                            { server | exoProps = newExoProps }

                                        newProject =
                                            GetterSetters.projectUpdateServer project newServer

                                        newSharedModel =
                                            GetterSetters.modelUpdateProject sharedModel newProject
                                    in
                                    State.Error.processSynchronousApiError newSharedModel errorContext httpErrorWithBody
                                        |> mapSharedToOuter outerModel
                    in
                    case httpError of
                        Http.BadStatus code ->
                            if code == 404 then
                                let
                                    newErrorContext =
                                        { errorContext
                                            | level = Error.ErrorDebug
                                            , actionContext =
                                                errorContext.actionContext
                                                    ++ " -- 404 means server may have been deleted"
                                        }

                                    newProject =
                                        GetterSetters.projectDeleteServer project serverUuid

                                    newModel =
                                        GetterSetters.modelUpdateProject sharedModel newProject
                                in
                                State.Error.processSynchronousApiError newModel newErrorContext httpErrorWithBody
                                    |> mapSharedToOuter outerModel

                            else
                                non404

                        _ ->
                            non404

        ReceiveFlavors flavors ->
            Rest.Nova.receiveFlavors outerModel project flavors

        RequestKeypairs ->
            let
                newKeypairs =
                    case project.keypairs of
                        RemoteData.Success _ ->
                            project.keypairs

                        _ ->
                            RemoteData.Loading

                newProject =
                    { project | keypairs = newKeypairs }

                newSharedModel =
                    GetterSetters.modelUpdateProject sharedModel newProject
            in
            ( newSharedModel
            , Rest.Nova.requestKeypairs newProject
            )
                |> mapSharedToOuter outerModel

        ReceiveKeypairs keypairs ->
            Rest.Nova.receiveKeypairs sharedModel project keypairs
                |> mapSharedToOuter outerModel

        RequestCreateKeypair keypairName publicKey ->
            ( outerModel, Rest.Nova.requestCreateKeypair project keypairName publicKey )

        ReceiveCreateKeypair keypair ->
            let
                newProject =
                    GetterSetters.projectUpdateKeypair project keypair

                newSharedModel =
                    GetterSetters.modelUpdateProject sharedModel newProject
            in
            ViewStateHelpers.setProjectView newProject (ListKeypairs Defaults.keypairListViewParams) { outerModel | sharedModel = newSharedModel }

        RequestDeleteKeypair keypairName ->
            ( outerModel, Rest.Nova.requestDeleteKeypair project keypairName )

        ReceiveDeleteKeypair errorContext keypairName result ->
            case result of
                Err httpError ->
                    State.Error.processStringError sharedModel errorContext (Helpers.httpErrorToString httpError)
                        |> mapSharedToOuter outerModel

                Ok () ->
                    let
                        newKeypairs =
                            case project.keypairs of
                                RemoteData.Success keypairs ->
                                    keypairs
                                        |> List.filter (\k -> k.name /= keypairName)
                                        |> RemoteData.Success

                                _ ->
                                    project.keypairs

                        newProject =
                            { project | keypairs = newKeypairs }

                        newSharedModel =
                            GetterSetters.modelUpdateProject sharedModel newProject
                    in
                    ( newSharedModel, Cmd.none )
                        |> mapSharedToOuter outerModel

        ReceiveCreateServer _ ->
            let
                newViewState =
                    ProjectView
                        project.auth.project.uuid
                        Defaults.projectViewParams
                    <|
                        AllResources
                            Defaults.allResourcesListViewParams

                ( newSharedModel, newCmd ) =
                    ( sharedModel, Cmd.none )
                        |> Helpers.pipelineCmd
                            (ApiModelHelpers.requestServers project.auth.project.uuid)
                        |> Helpers.pipelineCmd
                            (ApiModelHelpers.requestNetworks project.auth.project.uuid)
                        |> Helpers.pipelineCmd
                            (ApiModelHelpers.requestPorts project.auth.project.uuid)

                ( newOuterModel, changedViewCmd ) =
                    ViewStateHelpers.modelUpdateViewState newViewState { outerModel | sharedModel = newSharedModel }
            in
            ( newOuterModel, Cmd.batch [ newCmd, changedViewCmd ] )

        ReceiveNetworks errorContext result ->
            case result of
                Ok networks ->
                    Rest.Neutron.receiveNetworks outerModel project networks

                Err httpError ->
                    let
                        oldNetworksData =
                            project.networks.data

                        newProject =
                            { project
                                | networks =
                                    RDPP.RemoteDataPlusPlus
                                        oldNetworksData
                                        (RDPP.NotLoading (Just ( httpError, sharedModel.clientCurrentTime )))
                            }

                        newModel =
                            GetterSetters.modelUpdateProject sharedModel newProject
                    in
                    State.Error.processSynchronousApiError newModel errorContext httpError
                        |> mapSharedToOuter outerModel

        ReceiveAutoAllocatedNetwork errorContext result ->
            let
                newProject =
                    case result of
                        Ok netUuid ->
                            { project
                                | autoAllocatedNetworkUuid =
                                    RDPP.RemoteDataPlusPlus
                                        (RDPP.DoHave netUuid sharedModel.clientCurrentTime)
                                        (RDPP.NotLoading Nothing)
                            }

                        Err httpError ->
                            { project
                                | autoAllocatedNetworkUuid =
                                    RDPP.RemoteDataPlusPlus
                                        project.autoAllocatedNetworkUuid.data
                                        (RDPP.NotLoading
                                            (Just
                                                ( httpError
                                                , sharedModel.clientCurrentTime
                                                )
                                            )
                                        )
                            }

                newViewState =
                    case outerModel.viewState of
                        ProjectView _ viewParams projectViewConstructor ->
                            case projectViewConstructor of
                                CreateServer createServerViewParams ->
                                    if createServerViewParams.networkUuid == Nothing then
                                        case Helpers.newServerNetworkOptions newProject of
                                            AutoSelectedNetwork netUuid ->
                                                ProjectView
                                                    project.auth.project.uuid
                                                    viewParams
                                                    (CreateServer
                                                        { createServerViewParams
                                                            | networkUuid = Just netUuid
                                                        }
                                                    )

                                            _ ->
                                                outerModel.viewState

                                    else
                                        outerModel.viewState

                                _ ->
                                    outerModel.viewState

                        _ ->
                            outerModel.viewState

                newSharedModel =
                    GetterSetters.modelUpdateProject
                        sharedModel
                        newProject

                ( newNewSharedModel, newCmd ) =
                    case result of
                        Ok _ ->
                            ApiModelHelpers.requestNetworks project.auth.project.uuid newSharedModel

                        Err httpError ->
                            State.Error.processSynchronousApiError newSharedModel errorContext httpError
                                |> Helpers.pipelineCmd (ApiModelHelpers.requestNetworks project.auth.project.uuid)

                ( newOuterModel, setViewCmd ) =
                    -- TODO ensure this code works: sets URL if needed, page title if needed, etc.
                    ViewStateHelpers.modelUpdateViewState newViewState { outerModel | sharedModel = newNewSharedModel }
            in
            ( newOuterModel, Cmd.batch [ newCmd, setViewCmd ] )

        ReceiveFloatingIps ips ->
            Rest.Neutron.receiveFloatingIps sharedModel project ips
                |> mapSharedToOuter outerModel

        ReceivePorts errorContext result ->
            case result of
                Ok ports ->
                    let
                        newProject =
                            { project
                                | ports =
                                    RDPP.RemoteDataPlusPlus
                                        (RDPP.DoHave ports sharedModel.clientCurrentTime)
                                        (RDPP.NotLoading Nothing)
                            }
                    in
                    ( GetterSetters.modelUpdateProject sharedModel newProject, Cmd.none )
                        |> mapSharedToOuter outerModel

                Err httpError ->
                    let
                        oldPortsData =
                            project.ports.data

                        newProject =
                            { project
                                | ports =
                                    RDPP.RemoteDataPlusPlus
                                        oldPortsData
                                        (RDPP.NotLoading (Just ( httpError, sharedModel.clientCurrentTime )))
                            }

                        newSharedModel =
                            GetterSetters.modelUpdateProject sharedModel newProject
                    in
                    State.Error.processSynchronousApiError newSharedModel errorContext httpError
                        |> mapSharedToOuter outerModel

        ReceiveDeleteFloatingIp uuid ->
            Rest.Neutron.receiveDeleteFloatingIp sharedModel project uuid
                |> mapSharedToOuter outerModel

        ReceiveAssignFloatingIp floatingIp ->
            -- TODO update servers so that new assignment is reflected in the UI
            let
                newProject =
                    processNewFloatingIp sharedModel.clientCurrentTime project floatingIp

                newSharedModel =
                    GetterSetters.modelUpdateProject sharedModel newProject
            in
            ( newSharedModel, Cmd.none )
                |> mapSharedToOuter outerModel

        ReceiveUnassignFloatingIp floatingIp ->
            -- TODO update servers so that unassignment is reflected in the UI
            let
                newProject =
                    processNewFloatingIp sharedModel.clientCurrentTime project floatingIp
            in
            ( GetterSetters.modelUpdateProject sharedModel newProject, Cmd.none )
                |> mapSharedToOuter outerModel

        ReceiveSecurityGroups groups ->
            Rest.Neutron.receiveSecurityGroupsAndEnsureExoGroup sharedModel project groups
                |> mapSharedToOuter outerModel

        ReceiveCreateExoSecurityGroup group ->
            Rest.Neutron.receiveCreateExoSecurityGroupAndRequestCreateRules sharedModel project group
                |> mapSharedToOuter outerModel

        ReceiveCreateVolume ->
            {- Should we add new volume to model now? -}
            ViewStateHelpers.setProjectView project (ListProjectVolumes Defaults.volumeListViewParams) outerModel

        ReceiveVolumes volumes ->
            let
                -- Look for any server backing volumes that were created with no name, and give them a reasonable name
                updateVolNameCmds : List (Cmd Msg)
                updateVolNameCmds =
                    RDPP.withDefault [] project.servers
                        -- List of tuples containing server and Maybe boot vol
                        |> List.map
                            (\s ->
                                ( s
                                , Helpers.getBootVol
                                    (RemoteData.withDefault
                                        []
                                        project.volumes
                                    )
                                    s.osProps.uuid
                                )
                            )
                        -- We only care about servers created by exosphere
                        |> List.filter
                            (\t ->
                                case Tuple.first t |> .exoProps |> .serverOrigin of
                                    ServerFromExo _ ->
                                        True

                                    ServerNotFromExo ->
                                        False
                            )
                        -- We only care about servers created as current OpenStack user
                        |> List.filter
                            (\t ->
                                (Tuple.first t).osProps.details.userUuid
                                    == project.auth.user.uuid
                            )
                        -- We only care about servers with a non-empty name
                        |> List.filter
                            (\t ->
                                Tuple.first t
                                    |> .osProps
                                    |> .name
                                    |> String.isEmpty
                                    |> not
                            )
                        -- We only care about volume-backed servers
                        |> List.filterMap
                            (\t ->
                                case t of
                                    ( server, Just vol ) ->
                                        -- Flatten second part of tuple
                                        Just ( server, vol )

                                    _ ->
                                        Nothing
                            )
                        -- We only care about unnamed backing volumes
                        |> List.filter
                            (\t ->
                                Tuple.second t
                                    |> .name
                                    |> String.isEmpty
                            )
                        |> List.map
                            (\t ->
                                OSVolumes.requestUpdateVolumeName
                                    project
                                    (t |> Tuple.second |> .uuid)
                                    ("boot-vol-"
                                        ++ (t |> Tuple.first |> .osProps |> .name)
                                    )
                            )

                newProject =
                    { project | volumes = RemoteData.succeed volumes }

                newSharedModel =
                    GetterSetters.modelUpdateProject sharedModel newProject
            in
            ( newSharedModel, Cmd.batch updateVolNameCmds )
                |> mapSharedToOuter outerModel

        ReceiveDeleteVolume ->
            ( outerModel, OSVolumes.requestVolumes project )

        ReceiveUpdateVolumeName ->
            ( outerModel, OSVolumes.requestVolumes project )

        ReceiveAttachVolume attachment ->
            ViewStateHelpers.setProjectView project (MountVolInstructions attachment) outerModel

        ReceiveDetachVolume ->
            ViewStateHelpers.setProjectView project (ListProjectVolumes Defaults.volumeListViewParams) outerModel

        ReceiveAppCredential appCredential ->
            let
                newProject =
                    { project | secret = ApplicationCredential appCredential }
            in
            ( GetterSetters.modelUpdateProject sharedModel newProject, Cmd.none )
                |> mapSharedToOuter outerModel

        ReceiveComputeQuota quota ->
            let
                newProject =
                    { project | computeQuota = RemoteData.Success quota }
            in
            ( GetterSetters.modelUpdateProject sharedModel newProject, Cmd.none )
                |> mapSharedToOuter outerModel

        ReceiveVolumeQuota quota ->
            let
                newProject =
                    { project | volumeQuota = RemoteData.Success quota }
            in
            ( GetterSetters.modelUpdateProject sharedModel newProject, Cmd.none )
                |> mapSharedToOuter outerModel


processServerSpecificMsg : OuterModel -> Project -> Server -> ServerSpecificMsgConstructor -> ( OuterModel, Cmd Msg )
processServerSpecificMsg outerModel project server serverMsgConstructor =
    let
        sharedModel =
            outerModel.sharedModel
    in
    case serverMsgConstructor of
        RequestServer ->
            ApiModelHelpers.requestServer project.auth.project.uuid server.osProps.uuid sharedModel
                |> mapSharedToOuter outerModel

        RequestDeleteServer retainFloatingIps ->
            let
                ( newProject, cmd ) =
                    requestDeleteServer project server.osProps.uuid retainFloatingIps
            in
            ( GetterSetters.modelUpdateProject sharedModel newProject, cmd )
                |> mapSharedToOuter outerModel

        RequestAttachVolume volumeUuid ->
            ( outerModel, OSSvrVols.requestAttachVolume project server.osProps.uuid volumeUuid )

        RequestCreateServerImage imageName ->
            let
                newViewState =
                    ProjectView
                        project.auth.project.uuid
                        { createPopup = False }
                    <|
                        AllResources
                            Defaults.allResourcesListViewParams

                createImageCmd =
                    Rest.Nova.requestCreateServerImage project server.osProps.uuid imageName

                ( newOuterModel, setViewCmd ) =
                    ViewStateHelpers.modelUpdateViewState newViewState outerModel
            in
            ( newOuterModel, Cmd.batch [ createImageCmd, setViewCmd ] )

        RequestSetServerName newServerName ->
            ( outerModel, Rest.Nova.requestSetServerName project server.osProps.uuid newServerName )

        ReceiveServerEvents _ result ->
            case result of
                Ok serverEvents ->
                    let
                        newServer =
                            { server | events = RemoteData.Success serverEvents }

                        newProject =
                            GetterSetters.projectUpdateServer project newServer
                    in
                    ( GetterSetters.modelUpdateProject sharedModel newProject
                    , Cmd.none
                    )
                        |> mapSharedToOuter outerModel

                Err _ ->
                    -- Dropping this on the floor for now, someday we may want to do something different
                    ( outerModel, Cmd.none )

        ReceiveConsoleUrl url ->
            Rest.Nova.receiveConsoleUrl sharedModel project server url
                |> mapSharedToOuter outerModel

        ReceiveDeleteServer ->
            let
                ( serverDeletedModel, urlCmd ) =
                    let
                        newViewState =
                            case outerModel.viewState of
                                ProjectView projectId viewParams (ServerDetail viewServerUuid _) ->
                                    if viewServerUuid == server.osProps.uuid then
                                        ProjectView
                                            projectId
                                            viewParams
                                            (AllResources
                                                Defaults.allResourcesListViewParams
                                            )

                                    else
                                        outerModel.viewState

                                _ ->
                                    outerModel.viewState

                        newProject =
                            let
                                oldExoProps =
                                    server.exoProps

                                newExoProps =
                                    { oldExoProps | deletionAttempted = True }

                                newServer =
                                    { server | exoProps = newExoProps }
                            in
                            GetterSetters.projectUpdateServer project newServer

                        sharedModelUpdatedProject =
                            GetterSetters.modelUpdateProject sharedModel newProject
                    in
                    ViewStateHelpers.modelUpdateViewState
                        newViewState
                        { outerModel | sharedModel = sharedModelUpdatedProject }
            in
            ( serverDeletedModel, urlCmd )

        ReceiveCreateFloatingIp errorContext result ->
            case result of
                Ok ip ->
                    Rest.Neutron.receiveCreateFloatingIp sharedModel project server ip
                        |> mapSharedToOuter outerModel

                Err httpErrorWithBody ->
                    let
                        newErrorContext =
                            if GetterSetters.serverPresentNotDeleting sharedModel server.osProps.uuid then
                                errorContext

                            else
                                { errorContext | level = ErrorDebug }
                    in
                    State.Error.processSynchronousApiError sharedModel newErrorContext httpErrorWithBody
                        |> mapSharedToOuter outerModel

        ReceiveServerPassword password ->
            if String.isEmpty password then
                ( outerModel, Cmd.none )

            else
                let
                    tag =
                        "exoPw:" ++ password

                    cmd =
                        case server.exoProps.serverOrigin of
                            ServerNotFromExo ->
                                Cmd.none

                            ServerFromExo serverFromExoProps ->
                                if serverFromExoProps.exoServerVersion >= 1 then
                                    Cmd.batch
                                        [ OSServerTags.requestCreateServerTag project server.osProps.uuid tag
                                        , OSServerPassword.requestClearServerPassword project server.osProps.uuid
                                        ]

                                else
                                    Cmd.none
                in
                ( outerModel, cmd )

        ReceiveSetServerName _ errorContext result ->
            case result of
                Err e ->
                    State.Error.processSynchronousApiError sharedModel errorContext e
                        |> mapSharedToOuter outerModel

                Ok actualNewServerName ->
                    let
                        oldOsProps =
                            server.osProps

                        newOsProps =
                            { oldOsProps | name = actualNewServerName }

                        newServer =
                            { server | osProps = newOsProps }

                        newProject =
                            GetterSetters.projectUpdateServer project newServer

                        sharedModelWithUpdatedProject =
                            GetterSetters.modelUpdateProject sharedModel newProject

                        -- Only update the view if we are on the server details view for the server we're interested in
                        updatedView =
                            case outerModel.viewState of
                                ProjectView projectIdentifier projectViewParams (ServerDetail serverUuid_ serverDetailViewParams) ->
                                    if server.osProps.uuid == serverUuid_ then
                                        let
                                            newServerDetailsViewParams =
                                                { serverDetailViewParams
                                                    | serverNamePendingConfirmation = Nothing
                                                }
                                        in
                                        ProjectView projectIdentifier
                                            projectViewParams
                                            (ServerDetail serverUuid_ newServerDetailsViewParams)

                                    else
                                        outerModel.viewState

                                _ ->
                                    outerModel.viewState

                        -- Later, maybe: Check that newServerName == actualNewServerName
                    in
                    ViewStateHelpers.modelUpdateViewState
                        updatedView
                        { outerModel | sharedModel = sharedModelWithUpdatedProject }

        ReceiveSetServerMetadata intendedMetadataItem errorContext result ->
            case result of
                Err e ->
                    -- Error from API
                    State.Error.processSynchronousApiError sharedModel errorContext e
                        |> mapSharedToOuter outerModel

                Ok newServerMetadata ->
                    -- Update the model after ensuring the intended metadata item was actually added
                    if List.member intendedMetadataItem newServerMetadata then
                        let
                            oldServerDetails =
                                server.osProps.details

                            newServerDetails =
                                { oldServerDetails | metadata = newServerMetadata }

                            oldOsProps =
                                server.osProps

                            newOsProps =
                                { oldOsProps | details = newServerDetails }

                            newServer =
                                { server | osProps = newOsProps }

                            newProject =
                                GetterSetters.projectUpdateServer project newServer

                            newSharedModel =
                                GetterSetters.modelUpdateProject sharedModel newProject
                        in
                        ( newSharedModel, Cmd.none )
                            |> mapSharedToOuter outerModel

                    else
                        -- This is bonkers, throw an error
                        State.Error.processStringError
                            sharedModel
                            errorContext
                            "The metadata items returned by OpenStack did not include the metadata item that we tried to set."
                            |> mapSharedToOuter outerModel

        ReceiveDeleteServerMetadata metadataKey errorContext result ->
            case result of
                Err e ->
                    -- Error from API
                    State.Error.processSynchronousApiError sharedModel errorContext e
                        |> mapSharedToOuter outerModel

                Ok _ ->
                    let
                        oldServerDetails =
                            server.osProps.details

                        newServerDetails =
                            { oldServerDetails | metadata = List.filter (\i -> i.key /= metadataKey) oldServerDetails.metadata }

                        oldOsProps =
                            server.osProps

                        newOsProps =
                            { oldOsProps | details = newServerDetails }

                        newServer =
                            { server | osProps = newOsProps }

                        newProject =
                            GetterSetters.projectUpdateServer project newServer

                        newSharedModel =
                            GetterSetters.modelUpdateProject sharedModel newProject
                    in
                    ( newSharedModel, Cmd.none )
                        |> mapSharedToOuter outerModel

        ReceiveGuacamoleAuthToken result ->
            let
                errorContext =
                    ErrorContext
                        "Receive a response from Guacamole auth token API"
                        ErrorDebug
                        Nothing

                sharedModelUpdateGuacProps : ServerFromExoProps -> GuacTypes.LaunchedWithGuacProps -> SharedModel
                sharedModelUpdateGuacProps exoOriginProps guacProps =
                    let
                        newOriginProps =
                            { exoOriginProps | guacamoleStatus = GuacTypes.LaunchedWithGuacamole guacProps }

                        oldExoProps =
                            server.exoProps

                        newExoProps =
                            { oldExoProps | serverOrigin = ServerFromExo newOriginProps }

                        newServer =
                            { server | exoProps = newExoProps }

                        newProject =
                            GetterSetters.projectUpdateServer project newServer

                        newSharedModel =
                            GetterSetters.modelUpdateProject sharedModel newProject
                    in
                    newSharedModel
            in
            case server.exoProps.serverOrigin of
                ServerFromExo exoOriginProps ->
                    case exoOriginProps.guacamoleStatus of
                        GuacTypes.LaunchedWithGuacamole oldGuacProps ->
                            let
                                newGuacProps =
                                    case result of
                                        Ok tokenValue ->
                                            if server.osProps.details.openstackStatus == OSTypes.ServerActive then
                                                { oldGuacProps
                                                    | authToken =
                                                        RDPP.RemoteDataPlusPlus
                                                            (RDPP.DoHave
                                                                tokenValue
                                                                sharedModel.clientCurrentTime
                                                            )
                                                            (RDPP.NotLoading Nothing)
                                                }

                                            else
                                                -- Server is not active, this token won't work, so we don't store it
                                                { oldGuacProps
                                                    | authToken =
                                                        RDPP.empty
                                                }

                                        Err e ->
                                            { oldGuacProps
                                                | authToken =
                                                    RDPP.RemoteDataPlusPlus
                                                        oldGuacProps.authToken.data
                                                        (RDPP.NotLoading (Just ( e, sharedModel.clientCurrentTime )))
                                            }
                            in
                            ( sharedModelUpdateGuacProps
                                exoOriginProps
                                newGuacProps
                            , Cmd.none
                            )
                                |> mapSharedToOuter outerModel

                        GuacTypes.NotLaunchedWithGuacamole ->
                            State.Error.processStringError
                                sharedModel
                                errorContext
                                "Server does not appear to have been launched with Guacamole support"
                                |> mapSharedToOuter outerModel

                ServerNotFromExo ->
                    State.Error.processStringError
                        sharedModel
                        errorContext
                        "Server does not appear to have been launched from Exosphere"
                        |> mapSharedToOuter outerModel

        RequestServerAction func targetStatuses ->
            let
                oldExoProps =
                    server.exoProps

                newServer =
                    Server server.osProps { oldExoProps | targetOpenstackStatus = targetStatuses } server.events

                newProject =
                    GetterSetters.projectUpdateServer project newServer

                newSharedModel =
                    GetterSetters.modelUpdateProject sharedModel newProject
            in
            ( newSharedModel, func newProject.auth.project.uuid newProject.endpoints.nova newServer.osProps.uuid )
                |> mapSharedToOuter outerModel

        ReceiveConsoleLog errorContext result ->
            case server.exoProps.serverOrigin of
                ServerNotFromExo ->
                    ( outerModel, Cmd.none )

                ServerFromExo exoOriginProps ->
                    let
                        oldExoSetupStatus =
                            case exoOriginProps.exoSetupStatus.data of
                                RDPP.DontHave ->
                                    ExoSetupUnknown

                                RDPP.DoHave s _ ->
                                    s

                        ( newExoSetupStatusRDPP, exoSetupStatusMetadataCmd ) =
                            case result of
                                Err httpError ->
                                    ( RDPP.RemoteDataPlusPlus
                                        exoOriginProps.exoSetupStatus.data
                                        (RDPP.NotLoading (Just ( httpError, sharedModel.clientCurrentTime )))
                                    , Cmd.none
                                    )

                                Ok consoleLog ->
                                    let
                                        newExoSetupStatus =
                                            Helpers.ExoSetupStatus.parseConsoleLogExoSetupStatus
                                                oldExoSetupStatus
                                                consoleLog
                                                server.osProps.details.created
                                                sharedModel.clientCurrentTime

                                        cmd =
                                            if newExoSetupStatus == oldExoSetupStatus then
                                                Cmd.none

                                            else
                                                let
                                                    value =
                                                        Helpers.ExoSetupStatus.exoSetupStatusToStr newExoSetupStatus

                                                    metadataItem =
                                                        OSTypes.MetadataItem
                                                            "exoSetup"
                                                            value
                                                in
                                                Rest.Nova.requestSetServerMetadata project server.osProps.uuid metadataItem
                                    in
                                    ( RDPP.RemoteDataPlusPlus
                                        (RDPP.DoHave
                                            newExoSetupStatus
                                            sharedModel.clientCurrentTime
                                        )
                                        (RDPP.NotLoading Nothing)
                                    , cmd
                                    )

                        newResourceUsage =
                            case result of
                                Err httpError ->
                                    RDPP.RemoteDataPlusPlus
                                        exoOriginProps.resourceUsage.data
                                        (RDPP.NotLoading (Just ( httpError, sharedModel.clientCurrentTime )))

                                Ok consoleLog ->
                                    RDPP.RemoteDataPlusPlus
                                        (RDPP.DoHave
                                            (Helpers.ServerResourceUsage.parseConsoleLog
                                                consoleLog
                                                (RDPP.withDefault
                                                    Types.ServerResourceUsage.emptyResourceUsageHistory
                                                    exoOriginProps.resourceUsage
                                                )
                                            )
                                            sharedModel.clientCurrentTime
                                        )
                                        (RDPP.NotLoading Nothing)

                        newOriginProps =
                            { exoOriginProps
                                | resourceUsage = newResourceUsage
                                , exoSetupStatus = newExoSetupStatusRDPP
                            }

                        oldExoProps =
                            server.exoProps

                        newExoProps =
                            { oldExoProps | serverOrigin = ServerFromExo newOriginProps }

                        newServer =
                            { server | exoProps = newExoProps }

                        newProject =
                            GetterSetters.projectUpdateServer project newServer

                        newSharedModel =
                            GetterSetters.modelUpdateProject sharedModel newProject
                    in
                    case result of
                        Err httpError ->
                            State.Error.processSynchronousApiError newSharedModel errorContext httpError
                                |> mapSharedToOuter outerModel

                        Ok _ ->
                            ( newSharedModel, exoSetupStatusMetadataCmd )
                                |> mapSharedToOuter outerModel


processNewFloatingIp : Time.Posix -> Project -> OSTypes.FloatingIp -> Project
processNewFloatingIp time project floatingIp =
    let
        otherIps =
            project.floatingIps
                |> RDPP.withDefault []
                |> List.filter (\i -> i.uuid /= floatingIp.uuid)

        newIps =
            floatingIp :: otherIps
    in
    { project
        | floatingIps =
            RDPP.RemoteDataPlusPlus
                (RDPP.DoHave newIps time)
                (RDPP.NotLoading Nothing)
    }


createProject : OuterModel -> OSTypes.ScopedAuthToken -> Endpoints -> ( OuterModel, Cmd Msg )
createProject outerModel authToken endpoints =
    let
        sharedModel =
            outerModel.sharedModel

        newProject =
            { secret = NoProjectSecret
            , auth = authToken

            -- Maybe todo, eliminate parallel data structures in auth and endpoints?
            , endpoints = endpoints
            , images = []
            , servers = RDPP.RemoteDataPlusPlus RDPP.DontHave RDPP.Loading
            , flavors = []
            , keypairs = RemoteData.NotAsked
            , volumes = RemoteData.NotAsked
            , networks = RDPP.empty
            , autoAllocatedNetworkUuid = RDPP.empty
            , floatingIps = RDPP.empty
            , ports = RDPP.empty
            , securityGroups = []
            , computeQuota = RemoteData.NotAsked
            , volumeQuota = RemoteData.NotAsked
            , pendingCredentialedRequests = []
            }

        newProjects =
            newProject :: outerModel.sharedModel.projects

        newViewStateFunc =
            -- If the user is selecting projects from an unscoped provider then don't interrupt them
            case outerModel.viewState of
                ExampleNestedView _ ->
                    \model_ -> ( model_, Cmd.none )

                NonProjectView (SelectProjects _ _) ->
                    \model_ -> ( model_, Cmd.none )

                NonProjectView _ ->
                    ViewStateHelpers.setProjectView newProject <|
                        AllResources Defaults.allResourcesListViewParams

                ProjectView _ _ _ ->
                    ViewStateHelpers.setProjectView newProject <|
                        AllResources Defaults.allResourcesListViewParams

        ( newSharedModel, newCmd ) =
            ( { sharedModel
                | projects = newProjects
              }
            , [ Rest.Nova.requestServers
              , Rest.Neutron.requestSecurityGroups
              , Rest.Keystone.requestAppCredential sharedModel.clientUuid sharedModel.clientCurrentTime
              ]
                |> List.map (\x -> x newProject)
                |> Cmd.batch
            )
                |> Helpers.pipelineCmd
                    (ApiModelHelpers.requestFloatingIps newProject.auth.project.uuid)
                |> Helpers.pipelineCmd
                    (ApiModelHelpers.requestPorts newProject.auth.project.uuid)

        ( newOuterModel, viewStateCmd ) =
            newViewStateFunc { outerModel | sharedModel = newSharedModel }
    in
    ( newOuterModel, Cmd.batch [ viewStateCmd, newCmd ] )


createUnscopedProvider : SharedModel -> OSTypes.UnscopedAuthToken -> HelperTypes.Url -> ( SharedModel, Cmd Msg )
createUnscopedProvider model authToken authUrl =
    let
        newProvider =
            { authUrl = authUrl
            , token = authToken
            , projectsAvailable = RemoteData.Loading
            }

        newProviders =
            newProvider :: model.unscopedProviders
    in
    ( { model | unscopedProviders = newProviders }
    , Rest.Keystone.requestUnscopedProjects newProvider model.cloudCorsProxyUrl
    )


requestDeleteServer : Project -> OSTypes.ServerUuid -> Bool -> ( Project, Cmd Msg )
requestDeleteServer project serverUuid retainFloatingIps =
    case GetterSetters.serverLookup project serverUuid of
        Nothing ->
            -- Server likely deleted already, do nothing
            ( project, Cmd.none )

        Just server ->
            let
                oldExoProps =
                    server.exoProps

                newServer =
                    { server | exoProps = { oldExoProps | deletionAttempted = True } }

                deleteFloatingIpCmds =
                    if retainFloatingIps then
                        []

                    else
                        GetterSetters.getServerFloatingIps project server.osProps.uuid
                            |> List.map .uuid
                            |> List.map (Rest.Neutron.requestDeleteFloatingIp project)

                newProject =
                    GetterSetters.projectUpdateServer project newServer
            in
            ( newProject
            , Cmd.batch
                [ Rest.Nova.requestDeleteServer
                    newProject.auth.project.uuid
                    newProject.endpoints.nova
                    newServer.osProps.uuid
                , Cmd.batch deleteFloatingIpCmds
                ]
            )
