module State.State exposing (update)

import AppUrl.Builder
import AppUrl.Parser
import Dict
import Helpers.ExoSetupStatus
import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers as Helpers
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.ServerResourceUsage
import Helpers.Time as TimeHelpers
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
import Rest.Cockpit
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
import Types.HelperTypes as HelperTypes
import Types.ServerResourceUsage
import Types.Types
    exposing
        ( CockpitLoginStatus(..)
        , Endpoints
        , ExoSetupStatus(..)
        , FloatingIpState(..)
        , HttpRequestMethod(..)
        , LoginView(..)
        , Model
        , Msg(..)
        , NewServerNetworkOptions(..)
        , NonProjectViewConstructor(..)
        , Project
        , ProjectSecret(..)
        , ProjectSpecificMsgConstructor(..)
        , ProjectViewConstructor(..)
        , Server
        , ServerFromExoProps
        , ServerOrigin(..)
        , TickInterval
        , UnscopedProviderProject
        , ViewState(..)
        , currentExoServerVersion
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    {- We want to `setStorage` on every update. This function adds the setStorage
       command for every step of the update function.
    -}
    let
        ( newModel, cmds ) =
            updateUnderlying msg model

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
    ( newModel
    , Cmd.batch
        [ Ports.setStorage (LocalStorage.generateStoredState newModel)
        , orchestrationTimeCmd
        , cmds
        ]
    )


updateUnderlying : Msg -> Model -> ( Model, Cmd Msg )
updateUnderlying msg model =
    case msg of
        ToastyMsg subMsg ->
            Toasty.update Style.Toast.toastConfig ToastyMsg subMsg model

        MsgChangeWindowSize x y ->
            ( { model | maybeWindowSize = Just { width = x, height = y } }, Cmd.none )

        Tick interval time ->
            processTick model interval time

        DoOrchestration posixTime ->
            Orchestration.orchModel model posixTime

        SetNonProjectView nonProjectViewConstructor ->
            ViewStateHelpers.setNonProjectView model nonProjectViewConstructor

        HandleApiErrorWithBody errorContext error ->
            State.Error.processSynchronousApiError model errorContext error

        RequestUnscopedToken openstackLoginUnscoped ->
            ( model, Rest.Keystone.requestUnscopedAuthToken model.cloudCorsProxyUrl openstackLoginUnscoped )

        RequestNewProjectToken openstackCreds ->
            let
                -- If user does not provide a port number and path (API version) then we guess it
                newOpenstackCreds =
                    { openstackCreds | authUrl = State.Auth.authUrlWithPortAndVersion openstackCreds.authUrl }
            in
            ( model, Rest.Keystone.requestScopedAuthToken model.cloudCorsProxyUrl <| OSTypes.PasswordCreds newOpenstackCreds )

        JetstreamLogin jetstreamCreds ->
            let
                openstackCredsList =
                    State.Auth.jetstreamToOpenstackCreds jetstreamCreds

                cmds =
                    List.map
                        (\creds -> Rest.Keystone.requestUnscopedAuthToken model.cloudCorsProxyUrl creds)
                        openstackCredsList
            in
            ( model, Cmd.batch cmds )

        ReceiveScopedAuthToken maybePassword ( metadata, response ) ->
            case Rest.Keystone.decodeScopedAuthToken <| Http.GoodStatus_ metadata response of
                Err error ->
                    State.Error.processStringError
                        model
                        (ErrorContext
                            "decode scoped auth token"
                            ErrorCrit
                            Nothing
                        )
                        error

                Ok authToken ->
                    case Helpers.serviceCatalogToEndpoints authToken.catalog of
                        Err e ->
                            State.Error.processStringError
                                model
                                (ErrorContext
                                    "Decode project endpoints"
                                    ErrorCrit
                                    (Just "Please check with your cloud administrator or the Exosphere developers.")
                                )
                                e

                        Ok endpoints ->
                            let
                                projectId =
                                    authToken.project.uuid
                            in
                            -- If we don't have a project with same name + authUrl then create one, if we do then update its OSTypes.AuthToken
                            -- This code ensures we don't end up with duplicate projects on the same provider in our model.
                            case
                                ( GetterSetters.projectLookup model <| projectId, maybePassword )
                            of
                                ( Nothing, Nothing ) ->
                                    State.Error.processStringError
                                        model
                                        (ErrorContext
                                            "this is an impossible state"
                                            ErrorCrit
                                            (Just "The laws of physics and logic have been violated, check with your universe administrator")
                                        )
                                        "This is an impossible state"

                                ( Nothing, Just password ) ->
                                    createProject model password authToken endpoints

                                ( Just project, _ ) ->
                                    -- If we don't have an application credential for this project yet, then get one
                                    let
                                        appCredCmd =
                                            case project.secret of
                                                ApplicationCredential _ ->
                                                    Cmd.none

                                                _ ->
                                                    Rest.Keystone.requestAppCredential
                                                        model.clientUuid
                                                        model.clientCurrentTime
                                                        project

                                        ( newModel, updateTokenCmd ) =
                                            State.Auth.projectUpdateAuthToken model project authToken
                                    in
                                    ( newModel, Cmd.batch [ appCredCmd, updateTokenCmd ] )

        ReceiveUnscopedAuthToken keystoneUrl password ( metadata, response ) ->
            case Rest.Keystone.decodeUnscopedAuthToken <| Http.GoodStatus_ metadata response of
                Err error ->
                    State.Error.processStringError
                        model
                        (ErrorContext
                            "decode scoped auth token"
                            ErrorCrit
                            Nothing
                        )
                        error

                Ok authToken ->
                    case
                        GetterSetters.providerLookup model keystoneUrl
                    of
                        Just unscopedProvider ->
                            -- We already have an unscoped provider in the model with the same auth URL, update its token
                            State.Auth.unscopedProviderUpdateAuthToken
                                model
                                unscopedProvider
                                authToken

                        Nothing ->
                            -- We don't have an unscoped provider with the same auth URL, create it
                            createUnscopedProvider model password authToken keystoneUrl

        ReceiveUnscopedProjects keystoneUrl unscopedProjects ->
            case
                GetterSetters.providerLookup model keystoneUrl
            of
                Just provider ->
                    let
                        newProvider =
                            { provider | projectsAvailable = RemoteData.Success unscopedProjects }

                        newModel =
                            GetterSetters.modelUpdateUnscopedProvider model newProvider
                    in
                    -- If we are not already on a SelectProjects view, then go there
                    case newModel.viewState of
                        NonProjectView (SelectProjects _ _) ->
                            ( newModel, Cmd.none )

                        _ ->
                            ViewStateHelpers.modelUpdateViewState
                                (NonProjectView <|
                                    SelectProjects newProvider.authUrl []
                                )
                                newModel

                Nothing ->
                    -- Provider not found, may have been removed, nothing to do
                    ( model, Cmd.none )

        RequestProjectLoginFromProvider keystoneUrl password desiredProjects ->
            case GetterSetters.providerLookup model keystoneUrl of
                Just provider ->
                    let
                        buildLoginRequest : UnscopedProviderProject -> Cmd Msg
                        buildLoginRequest project =
                            Rest.Keystone.requestScopedAuthToken
                                model.cloudCorsProxyUrl
                            <|
                                OSTypes.PasswordCreds <|
                                    OSTypes.OpenstackLogin
                                        keystoneUrl
                                        project.domainId
                                        project.project.name
                                        provider.token.userDomain.uuid
                                        provider.token.user.name
                                        password

                        loginRequests =
                            List.map buildLoginRequest desiredProjects
                                |> Cmd.batch

                        -- Remove unscoped provider from model now that we have selected projects from it
                        newUnscopedProviders =
                            List.filter
                                (\p -> p.authUrl /= keystoneUrl)
                                model.unscopedProviders

                        -- If we still have at least one unscoped provider in the model then ask the user to choose projects from it
                        newViewState =
                            case List.head newUnscopedProviders of
                                Just unscopedProvider ->
                                    NonProjectView <|
                                        SelectProjects unscopedProvider.authUrl []

                                Nothing ->
                                    -- If we have at least one project then show it, else show the login page
                                    case List.head model.projects of
                                        Just project ->
                                            ProjectView
                                                project.auth.project.uuid
                                                { createPopup = False }
                                            <|
                                                ListProjectServers Defaults.serverListViewParams

                                        Nothing ->
                                            NonProjectView LoginPicker

                        modelUpdatedUnscopedProviders =
                            { model | unscopedProviders = newUnscopedProviders }
                    in
                    ( modelUpdatedUnscopedProviders, loginRequests )
                        |> Helpers.pipelineCmd
                            (ViewStateHelpers.modelUpdateViewState newViewState)

                Nothing ->
                    State.Error.processStringError
                        model
                        (ErrorContext
                            ("look for OpenStack provider with Keystone URL " ++ keystoneUrl)
                            ErrorCrit
                            Nothing
                        )
                        "Provider could not found in Exosphere's list of Providers."

        ProjectMsg projectIdentifier innerMsg ->
            case GetterSetters.projectLookup model projectIdentifier of
                Nothing ->
                    -- Project not found, may have been removed, nothing to do
                    ( model, Cmd.none )

                Just project ->
                    processProjectSpecificMsg model project innerMsg

        {- Form inputs -}
        InputOpenRc openstackCreds openRc ->
            let
                newCreds =
                    State.Auth.processOpenRc openstackCreds openRc

                newViewState =
                    NonProjectView <| Login <| LoginOpenstack newCreds
            in
            ViewStateHelpers.modelUpdateViewState newViewState model

        OpenInBrowser url ->
            ( model, Ports.openInBrowser url )

        OpenNewWindow url ->
            ( model, Ports.openNewWindow url )

        UrlChange url ->
            -- This handles presses of the browser back/forward button
            let
                exoJustSetThisUrl =
                    -- If this is a URL that Exosphere just set via StateHelpers.updateViewState, then ignore it
                    UrlHelpers.urlPathQueryMatches url model.prevUrl
            in
            if exoJustSetThisUrl then
                ( model, Cmd.none )

            else
                case
                    AppUrl.Parser.urlToViewState
                        model.urlPathPrefix
                        (ViewStateHelpers.defaultViewState model)
                        url
                of
                    Just newViewState ->
                        ( { model
                            | viewState = newViewState
                            , prevUrl = AppUrl.Builder.viewStateToUrl model.urlPathPrefix newViewState
                          }
                        , Cmd.none
                        )

                    Nothing ->
                        ( { model
                            | viewState = NonProjectView PageNotFound
                          }
                        , Cmd.none
                        )

        SetStyle styleMode ->
            let
                oldStyle =
                    model.style

                newStyle =
                    { oldStyle | styleMode = styleMode }
            in
            ( { model | style = newStyle }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


processTick : Model -> TickInterval -> Time.Posix -> ( Model, Cmd Msg )
processTick model interval time =
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
            case model.viewState of
                NonProjectView _ ->
                    ( model, Cmd.none )

                ProjectView projectName _ projectViewState ->
                    case GetterSetters.projectLookup model projectName of
                        Nothing ->
                            {- Should this throw an error? -}
                            ( model, Cmd.none )

                        Just project ->
                            case projectViewState of
                                ServerDetail serverUuid _ ->
                                    let
                                        volCmd =
                                            OSVolumes.requestVolumes project
                                    in
                                    case interval of
                                        5 ->
                                            case GetterSetters.serverLookup project serverUuid of
                                                Just server ->
                                                    ( model
                                                    , if serverVolsNeedFrequentPoll project server then
                                                        volCmd

                                                      else
                                                        Cmd.none
                                                    )

                                                Nothing ->
                                                    ( model, Cmd.none )

                                        300 ->
                                            ( model, volCmd )

                                        _ ->
                                            ( model, Cmd.none )

                                ListProjectVolumes _ ->
                                    ( model
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

                                VolumeDetail volumeUuid _ ->
                                    ( model
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
                                    ( model, Cmd.none )
    in
    ( { viewDependentModel | clientCurrentTime = time }
    , Cmd.batch
        [ viewDependentCmd
        , viewIndependentCmd
        ]
    )


processProjectSpecificMsg : Model -> Project -> ProjectSpecificMsgConstructor -> ( Model, Cmd Msg )
processProjectSpecificMsg model project msg =
    case msg of
        SetProjectView projectViewConstructor ->
            ViewStateHelpers.setProjectView model project projectViewConstructor

        PrepareCredentialedRequest requestProto posixTime ->
            let
                -- Add proxy URL
                requestNeedingToken =
                    requestProto model.cloudCorsProxyUrl

                currentTimeMillis =
                    posixTime |> Time.posixToMillis

                tokenExpireTimeMillis =
                    project.auth.expiresAt |> Time.posixToMillis

                tokenExpired =
                    -- Token expiring within 10 minutes
                    tokenExpireTimeMillis < currentTimeMillis + 600000
            in
            if not tokenExpired then
                -- Token still valid, fire the request with current token
                ( model, requestNeedingToken project.auth.tokenValue )

            else
                -- Token is expired (or nearly expired) so we add request to list of pending requests and refresh that token
                let
                    newPQRs =
                        requestNeedingToken :: project.pendingCredentialedRequests

                    newProject =
                        { project | pendingCredentialedRequests = newPQRs }

                    newModel =
                        GetterSetters.modelUpdateProject model newProject
                in
                ( newModel, State.Auth.requestAuthToken newModel newProject )

        ToggleCreatePopup ->
            case model.viewState of
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
                    ViewStateHelpers.modelUpdateViewState newViewState model

                _ ->
                    ( model, Cmd.none )

        RemoveProject ->
            let
                newProjects =
                    List.filter (\p -> p.auth.project.uuid /= project.auth.project.uuid) model.projects

                newViewState =
                    case model.viewState of
                        NonProjectView _ ->
                            -- If we are not in a project-specific view then stay there
                            model.viewState

                        ProjectView _ _ _ ->
                            -- If we have any projects switch to the first one in the list, otherwise switch to login view
                            case List.head newProjects of
                                Just p ->
                                    ProjectView
                                        p.auth.project.uuid
                                        { createPopup = False }
                                    <|
                                        ListProjectServers
                                            Defaults.serverListViewParams

                                Nothing ->
                                    NonProjectView <| LoginPicker

                modelUpdatedProjects =
                    { model | projects = newProjects }
            in
            ViewStateHelpers.modelUpdateViewState newViewState modelUpdatedProjects

        RequestServers ->
            ApiModelHelpers.requestServers project.auth.project.uuid model

        RequestServer serverUuid ->
            ApiModelHelpers.requestServer project.auth.project.uuid serverUuid model

        RequestCreateServer viewParams ->
            let
                createServerRequest =
                    { name = viewParams.serverName
                    , count = viewParams.count
                    , imageUuid = viewParams.imageUuid
                    , flavorUuid = viewParams.flavorUuid
                    , volBackedSizeGb =
                        viewParams.volSizeTextInput
                            |> Maybe.andThen Style.Widgets.NumericTextInput.NumericTextInput.toMaybe
                    , networkUuid = viewParams.networkUuid
                    , keypairName = viewParams.keypairName
                    , userData =
                        Helpers.renderUserDataTemplate
                            project
                            viewParams.userDataTemplate
                            viewParams.keypairName
                            (viewParams.deployGuacamole |> Maybe.withDefault False)
                    , metadata =
                        Helpers.newServerMetadata
                            currentExoServerVersion
                            model.clientUuid
                            (viewParams.deployGuacamole |> Maybe.withDefault False)
                            project.auth.user.name
                    }
            in
            ( model, Rest.Nova.requestCreateServer project createServerRequest )

        RequestDeleteServer serverUuid ->
            let
                ( newProject, cmd ) =
                    requestDeleteServer project serverUuid
            in
            ( GetterSetters.modelUpdateProject model newProject, cmd )

        RequestServerAction server func targetStatus ->
            let
                oldExoProps =
                    server.exoProps

                newServer =
                    Server server.osProps { oldExoProps | targetOpenstackStatus = targetStatus }

                newProject =
                    GetterSetters.projectUpdateServer project newServer

                newModel =
                    GetterSetters.modelUpdateProject model newProject
            in
            ( newModel, func newProject newServer )

        RequestCreateVolume name size ->
            let
                createVolumeRequest =
                    { name = name
                    , size = size
                    }
            in
            ( model, OSVolumes.requestCreateVolume project createVolumeRequest )

        RequestDeleteVolume volumeUuid ->
            ( model, OSVolumes.requestDeleteVolume project volumeUuid )

        RequestAttachVolume serverUuid volumeUuid ->
            ( model, OSSvrVols.requestAttachVolume project serverUuid volumeUuid )

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
                    ( model, OSSvrVols.requestDetachVolume project serverUuid volumeUuid )

                Nothing ->
                    State.Error.processStringError
                        model
                        (ErrorContext
                            ("look for server UUID with attached volume " ++ volumeUuid)
                            ErrorCrit
                            Nothing
                        )
                        "Could not determine server attached to this volume."

        RequestCreateServerImage serverUuid imageName ->
            let
                newViewState =
                    ProjectView
                        project.auth.project.uuid
                        { createPopup = False }
                    <|
                        ListProjectServers
                            Defaults.serverListViewParams

                createImageCmd =
                    Rest.Nova.requestCreateServerImage project serverUuid imageName
            in
            ( model, createImageCmd )
                |> Helpers.pipelineCmd
                    (ViewStateHelpers.modelUpdateViewState newViewState)

        RequestSetServerName serverUuid newServerName ->
            ( model, Rest.Nova.requestSetServerName project serverUuid newServerName )

        ReceiveImages images ->
            Rest.Glance.receiveImages model project images

        RequestDeleteServers serverUuidsToDelete ->
            let
                applyDelete : OSTypes.ServerUuid -> ( Project, Cmd Msg ) -> ( Project, Cmd Msg )
                applyDelete serverUuid projCmdTuple =
                    let
                        ( delServerProj, delServerCmd ) =
                            requestDeleteServer (Tuple.first projCmdTuple) serverUuid
                    in
                    ( delServerProj, Cmd.batch [ Tuple.second projCmdTuple, delServerCmd ] )

                ( newProject, cmd ) =
                    List.foldl
                        applyDelete
                        ( project, Cmd.none )
                        serverUuidsToDelete
            in
            ( GetterSetters.modelUpdateProject model newProject
            , cmd
            )

        ReceiveServers errorContext result ->
            case result of
                Ok servers ->
                    Rest.Nova.receiveServers model project servers

                Err e ->
                    let
                        oldServersData =
                            project.servers.data

                        newProject =
                            { project
                                | servers =
                                    RDPP.RemoteDataPlusPlus
                                        oldServersData
                                        (RDPP.NotLoading (Just ( e, model.clientCurrentTime )))
                            }

                        newModel =
                            GetterSetters.modelUpdateProject model newProject
                    in
                    State.Error.processSynchronousApiError newModel errorContext e

        ReceiveServer serverUuid errorContext result ->
            case result of
                Ok server ->
                    Rest.Nova.receiveServer model project server

                Err httpErrorWithBody ->
                    let
                        httpError =
                            httpErrorWithBody.error

                        non404 =
                            case GetterSetters.serverLookup project serverUuid of
                                Nothing ->
                                    -- Server not in project, may have been deleted, ignoring this error
                                    ( model, Cmd.none )

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

                                        newModel =
                                            GetterSetters.modelUpdateProject model newProject
                                    in
                                    State.Error.processSynchronousApiError newModel errorContext httpErrorWithBody
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
                                        GetterSetters.modelUpdateProject model newProject
                                in
                                State.Error.processSynchronousApiError newModel newErrorContext httpErrorWithBody

                            else
                                non404

                        _ ->
                            non404

        ReceiveConsoleUrl serverUuid url ->
            Rest.Nova.receiveConsoleUrl model project serverUuid url

        ReceiveFlavors flavors ->
            Rest.Nova.receiveFlavors model project flavors

        ReceiveKeypairs keypairs ->
            Rest.Nova.receiveKeypairs model project keypairs

        ReceiveCreateServer _ ->
            let
                newViewState =
                    ProjectView
                        project.auth.project.uuid
                        { createPopup = False }
                    <|
                        ListProjectServers
                            Defaults.serverListViewParams
            in
            ( model, Cmd.none )
                |> Helpers.pipelineCmd
                    (ApiModelHelpers.requestServers project.auth.project.uuid)
                |> Helpers.pipelineCmd
                    (ApiModelHelpers.requestNetworks project.auth.project.uuid)
                |> Helpers.pipelineCmd
                    (ViewStateHelpers.modelUpdateViewState newViewState)

        ReceiveDeleteServer serverUuid maybeIpAddress ->
            let
                ( serverDeletedModel, urlCmd ) =
                    let
                        newViewState =
                            case model.viewState of
                                ProjectView projectId viewParams (ServerDetail viewServerUuid _) ->
                                    if viewServerUuid == serverUuid then
                                        ProjectView
                                            projectId
                                            viewParams
                                            (ListProjectServers
                                                Defaults.serverListViewParams
                                            )

                                    else
                                        model.viewState

                                _ ->
                                    model.viewState

                        newProject =
                            case GetterSetters.serverLookup project serverUuid of
                                Just server ->
                                    let
                                        oldExoProps =
                                            server.exoProps

                                        newExoProps =
                                            { oldExoProps | deletionAttempted = True }

                                        newServer =
                                            { server | exoProps = newExoProps }
                                    in
                                    GetterSetters.projectUpdateServer project newServer

                                Nothing ->
                                    project

                        modelUpdatedProject =
                            GetterSetters.modelUpdateProject model newProject
                    in
                    ViewStateHelpers.modelUpdateViewState newViewState modelUpdatedProject
            in
            case maybeIpAddress of
                Nothing ->
                    ( serverDeletedModel, urlCmd )

                Just ipAddress ->
                    let
                        maybeFloatingIpUuid =
                            project.floatingIps
                                |> List.filter (\i -> i.address == ipAddress)
                                |> List.head
                                |> Maybe.andThen .uuid
                    in
                    case maybeFloatingIpUuid of
                        Nothing ->
                            let
                                errorContext =
                                    ErrorContext
                                        "Look for a floating IP address to delete now that we have just deleted its server"
                                        ErrorInfo
                                        Nothing
                            in
                            State.Error.processStringError
                                serverDeletedModel
                                errorContext
                                ("Could not find a UUID for floating IP address from deleted server with UUID " ++ serverUuid)

                        Just uuid ->
                            ( serverDeletedModel
                            , Cmd.batch [ urlCmd, Rest.Neutron.requestDeleteFloatingIp project uuid ]
                            )

        ReceiveNetworks errorContext result ->
            case result of
                Ok networks ->
                    Rest.Neutron.receiveNetworks model project networks

                Err httpError ->
                    let
                        oldNetworksData =
                            project.networks.data

                        newProject =
                            { project
                                | networks =
                                    RDPP.RemoteDataPlusPlus
                                        oldNetworksData
                                        (RDPP.NotLoading (Just ( httpError, model.clientCurrentTime )))
                            }

                        newModel =
                            GetterSetters.modelUpdateProject model newProject
                    in
                    State.Error.processSynchronousApiError newModel errorContext httpError

        ReceiveFloatingIps ips ->
            Rest.Neutron.receiveFloatingIps model project ips

        ReceivePorts errorContext result ->
            case result of
                Ok ports ->
                    let
                        newProject =
                            { project
                                | ports =
                                    RDPP.RemoteDataPlusPlus
                                        (RDPP.DoHave ports model.clientCurrentTime)
                                        (RDPP.NotLoading Nothing)
                            }
                    in
                    ( GetterSetters.modelUpdateProject model newProject, Cmd.none )

                Err httpError ->
                    let
                        oldPortsData =
                            project.ports.data

                        newProject =
                            { project
                                | ports =
                                    RDPP.RemoteDataPlusPlus
                                        oldPortsData
                                        (RDPP.NotLoading (Just ( httpError, model.clientCurrentTime )))
                            }

                        newModel =
                            GetterSetters.modelUpdateProject model newProject
                    in
                    State.Error.processSynchronousApiError newModel errorContext httpError

        ReceiveCreateFloatingIp serverUuid ip ->
            Rest.Neutron.receiveCreateFloatingIp model project serverUuid ip

        ReceiveDeleteFloatingIp uuid ->
            Rest.Neutron.receiveDeleteFloatingIp model project uuid

        ReceiveSecurityGroups groups ->
            Rest.Neutron.receiveSecurityGroupsAndEnsureExoGroup model project groups

        ReceiveCreateExoSecurityGroup group ->
            Rest.Neutron.receiveCreateExoSecurityGroupAndRequestCreateRules model project group

        ReceiveCockpitLoginStatus serverUuid result ->
            Rest.Cockpit.receiveCockpitLoginStatus model project serverUuid result

        ReceiveCreateVolume ->
            {- Should we add new volume to model now? -}
            ViewStateHelpers.setProjectView model project (ListProjectVolumes [])

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

                newModel =
                    GetterSetters.modelUpdateProject model newProject
            in
            ( newModel, Cmd.batch updateVolNameCmds )

        ReceiveDeleteVolume ->
            ( model, OSVolumes.requestVolumes project )

        ReceiveUpdateVolumeName ->
            ( model, OSVolumes.requestVolumes project )

        ReceiveAttachVolume attachment ->
            ViewStateHelpers.setProjectView model project (MountVolInstructions attachment)

        ReceiveDetachVolume ->
            ViewStateHelpers.setProjectView model project (ListProjectVolumes [])

        ReceiveAppCredential appCredential ->
            let
                newProject =
                    { project | secret = ApplicationCredential appCredential }
            in
            ( GetterSetters.modelUpdateProject model newProject, Cmd.none )

        ReceiveComputeQuota quota ->
            let
                newProject =
                    { project | computeQuota = RemoteData.Success quota }
            in
            ( GetterSetters.modelUpdateProject model newProject, Cmd.none )

        ReceiveVolumeQuota quota ->
            let
                newProject =
                    { project | volumeQuota = RemoteData.Success quota }
            in
            ( GetterSetters.modelUpdateProject model newProject, Cmd.none )

        ReceiveServerPassword serverUuid password ->
            if String.isEmpty password then
                ( model, Cmd.none )

            else
                let
                    tag =
                        "exoPw:" ++ password

                    cmd =
                        case GetterSetters.serverLookup project serverUuid of
                            Just server ->
                                case server.exoProps.serverOrigin of
                                    ServerNotFromExo ->
                                        Cmd.none

                                    ServerFromExo serverFromExoProps ->
                                        if serverFromExoProps.exoServerVersion >= 1 then
                                            Cmd.batch
                                                [ OSServerTags.requestCreateServerTag project serverUuid tag
                                                , OSServerPassword.requestClearServerPassword project serverUuid
                                                ]

                                        else
                                            Cmd.none

                            Nothing ->
                                Cmd.none
                in
                ( model, cmd )

        ReceiveConsoleLog errorContext serverUuid result ->
            case GetterSetters.serverLookup project serverUuid of
                Nothing ->
                    ( model, Cmd.none )

                Just server ->
                    case server.exoProps.serverOrigin of
                        ServerNotFromExo ->
                            ( model, Cmd.none )

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
                                                (RDPP.NotLoading (Just ( httpError, model.clientCurrentTime )))
                                            , Cmd.none
                                            )

                                        Ok consoleLog ->
                                            let
                                                newExoSetupStatus =
                                                    Helpers.ExoSetupStatus.parseConsoleLogExoSetupStatus
                                                        oldExoSetupStatus
                                                        consoleLog
                                                        (TimeHelpers.iso8601StringToPosix
                                                            server.osProps.details.created
                                                            |> Result.withDefault
                                                                (Time.millisToPosix 0)
                                                        )
                                                        model.clientCurrentTime

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
                                                        Rest.Nova.requestSetServerMetadata project serverUuid metadataItem
                                            in
                                            ( RDPP.RemoteDataPlusPlus
                                                (RDPP.DoHave
                                                    newExoSetupStatus
                                                    model.clientCurrentTime
                                                )
                                                (RDPP.NotLoading Nothing)
                                            , cmd
                                            )

                                newResourceUsage =
                                    case result of
                                        Err httpError ->
                                            RDPP.RemoteDataPlusPlus
                                                exoOriginProps.resourceUsage.data
                                                (RDPP.NotLoading (Just ( httpError, model.clientCurrentTime )))

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
                                                    model.clientCurrentTime
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

                                newModel =
                                    GetterSetters.modelUpdateProject model newProject
                            in
                            case result of
                                Err httpError ->
                                    State.Error.processSynchronousApiError newModel errorContext httpError

                                Ok _ ->
                                    ( newModel, exoSetupStatusMetadataCmd )

        ReceiveSetServerName serverUuid _ errorContext result ->
            case ( GetterSetters.serverLookup project serverUuid, result ) of
                ( Nothing, _ ) ->
                    -- Ensure that the server UUID we get back exists in the model. If not, ignore.
                    ( model, Cmd.none )

                ( _, Err e ) ->
                    State.Error.processSynchronousApiError model errorContext e

                ( Just server, Ok actualNewServerName ) ->
                    let
                        oldOsProps =
                            server.osProps

                        newOsProps =
                            { oldOsProps | name = actualNewServerName }

                        newServer =
                            { server | osProps = newOsProps }

                        newProject =
                            GetterSetters.projectUpdateServer project newServer

                        modelWithUpdatedProject =
                            GetterSetters.modelUpdateProject model newProject

                        -- Only update the view if we are on the server details view for the server we're interested in
                        updatedView =
                            case model.viewState of
                                ProjectView projectIdentifier projectViewParams (ServerDetail serverUuid_ serverDetailViewParams) ->
                                    if serverUuid == serverUuid_ then
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
                                        model.viewState

                                _ ->
                                    model.viewState

                        -- Later, maybe: Check that newServerName == actualNewServerName
                    in
                    ViewStateHelpers.modelUpdateViewState updatedView modelWithUpdatedProject

        ReceiveSetServerMetadata serverUuid intendedMetadataItem errorContext result ->
            case ( GetterSetters.serverLookup project serverUuid, result ) of
                ( Nothing, _ ) ->
                    -- Server does not exist in the model, ignore it
                    ( model, Cmd.none )

                ( _, Err e ) ->
                    -- Error from API
                    State.Error.processSynchronousApiError model errorContext e

                ( Just server, Ok newServerMetadata ) ->
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

                            newModel =
                                GetterSetters.modelUpdateProject model newProject
                        in
                        ( newModel, Cmd.none )

                    else
                        -- This is bonkers, throw an error
                        State.Error.processStringError
                            model
                            errorContext
                            "The metadata items returned by OpenStack did not include the metadata item that we tried to set."

        ReceiveGuacamoleAuthToken serverUuid result ->
            let
                errorContext =
                    ErrorContext
                        "Receive a response from Guacamole auth token API"
                        ErrorDebug
                        Nothing

                modelUpdateGuacProps : Server -> ServerFromExoProps -> GuacTypes.LaunchedWithGuacProps -> Model
                modelUpdateGuacProps server exoOriginProps guacProps =
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

                        newModel =
                            GetterSetters.modelUpdateProject model newProject
                    in
                    newModel

                serverMetadataSetGuacDeployComplete : GuacTypes.LaunchedWithGuacProps -> Cmd Msg
                serverMetadataSetGuacDeployComplete launchedWithGuacProps =
                    let
                        value =
                            Helpers.newGuacMetadata launchedWithGuacProps

                        metadataItem =
                            OSTypes.MetadataItem
                                "exoGuac"
                                value
                    in
                    Rest.Nova.requestSetServerMetadata project serverUuid metadataItem
            in
            case GetterSetters.serverLookup project serverUuid of
                Just server ->
                    case server.exoProps.serverOrigin of
                        ServerFromExo exoOriginProps ->
                            case exoOriginProps.guacamoleStatus of
                                GuacTypes.LaunchedWithGuacamole oldGuacProps ->
                                    let
                                        newGuacProps =
                                            case result of
                                                Ok tokenValue ->
                                                    { oldGuacProps
                                                        | authToken =
                                                            RDPP.RemoteDataPlusPlus
                                                                (RDPP.DoHave
                                                                    tokenValue
                                                                    model.clientCurrentTime
                                                                )
                                                                (RDPP.NotLoading Nothing)
                                                    }

                                                Err e ->
                                                    { oldGuacProps
                                                        | authToken =
                                                            RDPP.RemoteDataPlusPlus
                                                                oldGuacProps.authToken.data
                                                                (RDPP.NotLoading (Just ( e, model.clientCurrentTime )))
                                                    }

                                        updateMetadataCmd =
                                            -- TODO not super happy with this factoring
                                            if oldGuacProps.deployComplete then
                                                Cmd.none

                                            else
                                                case result of
                                                    Ok _ ->
                                                        serverMetadataSetGuacDeployComplete newGuacProps

                                                    Err _ ->
                                                        Cmd.none
                                    in
                                    ( modelUpdateGuacProps
                                        server
                                        exoOriginProps
                                        newGuacProps
                                    , updateMetadataCmd
                                    )

                                GuacTypes.NotLaunchedWithGuacamole ->
                                    State.Error.processStringError
                                        model
                                        errorContext
                                        "Server does not appear to have been launched with Guacamole support"

                        ServerNotFromExo ->
                            State.Error.processStringError
                                model
                                errorContext
                                "Server does not appear to have been launched from Exosphere"

                Nothing ->
                    State.Error.processStringError
                        model
                        errorContext
                        "Could not find server in the model, maybe it has been deleted."


createProject : Model -> HelperTypes.Password -> OSTypes.ScopedAuthToken -> Endpoints -> ( Model, Cmd Msg )
createProject model password authToken endpoints =
    let
        newProject =
            { secret = OpenstackPassword password
            , auth = authToken

            -- Maybe todo, eliminate parallel data structures in auth and endpoints?
            , endpoints = endpoints
            , images = []
            , servers = RDPP.RemoteDataPlusPlus RDPP.DontHave (RDPP.Loading model.clientCurrentTime)
            , flavors = []
            , keypairs = []
            , volumes = RemoteData.NotAsked
            , networks = RDPP.empty
            , floatingIps = []
            , ports = RDPP.empty
            , securityGroups = []
            , computeQuota = RemoteData.NotAsked
            , volumeQuota = RemoteData.NotAsked
            , pendingCredentialedRequests = []
            , userAppProxyHostname =
                endpoints.keystone
                    |> UrlHelpers.hostnameFromUrl
                    |> (\h -> Dict.get h model.cloudsWithUserAppProxy)
            }

        newProjects =
            newProject :: model.projects

        newViewState =
            -- If the user is selecting projects from an unscoped provider then don't interrupt them
            case model.viewState of
                NonProjectView (SelectProjects _ _) ->
                    model.viewState

                NonProjectView _ ->
                    ProjectView
                        newProject.auth.project.uuid
                        { createPopup = False }
                    <|
                        ListProjectServers Defaults.serverListViewParams

                ProjectView _ projectViewParams _ ->
                    ProjectView
                        newProject.auth.project.uuid
                        projectViewParams
                    <|
                        ListProjectServers Defaults.serverListViewParams

        newModel =
            { model
                | projects = newProjects
                , viewState = newViewState
            }
    in
    ( newModel
    , [ Rest.Nova.requestServers
      , Rest.Neutron.requestSecurityGroups
      , Rest.Neutron.requestFloatingIps
      , Rest.Keystone.requestAppCredential model.clientUuid model.clientCurrentTime
      ]
        |> List.map (\x -> x newProject)
        |> Cmd.batch
    )


createUnscopedProvider : Model -> HelperTypes.Password -> OSTypes.UnscopedAuthToken -> HelperTypes.Url -> ( Model, Cmd Msg )
createUnscopedProvider model password authToken authUrl =
    let
        newProvider =
            { authUrl = authUrl
            , keystonePassword = password
            , token = authToken
            , projectsAvailable = RemoteData.Loading
            }

        newProviders =
            newProvider :: model.unscopedProviders
    in
    ( { model | unscopedProviders = newProviders }
    , Rest.Keystone.requestUnscopedProjects newProvider model.cloudCorsProxyUrl
    )


requestDeleteServer : Project -> OSTypes.ServerUuid -> ( Project, Cmd Msg )
requestDeleteServer project serverUuid =
    case GetterSetters.serverLookup project serverUuid of
        Nothing ->
            -- Server likely deleted already, do nothing
            ( project, Cmd.none )

        Just server ->
            let
                oldExoProps =
                    server.exoProps

                newServer =
                    Server server.osProps { oldExoProps | deletionAttempted = True }

                newProject =
                    GetterSetters.projectUpdateServer project newServer
            in
            ( newProject, Rest.Nova.requestDeleteServer newProject newServer )
