module State exposing (init, subscriptions, update)

import AppUrl.Parser
import Browser.Events
import Browser.Navigation
import Dict
import Helpers.Error as Error exposing (ErrorContext, ErrorLevel(..))
import Helpers.ExoSetupStatus
import Helpers.Helpers as Helpers
import Helpers.Random as RandomHelpers
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.ServerResourceUsage
import Helpers.StateHelpers as StateHelpers
import Helpers.Time as TimeHelpers
import Http
import Json.Decode as Decode
import LocalStorage.LocalStorage as LocalStorage
import LocalStorage.Types as LocalStorageTypes
import Maybe
import OpenStack.Quotas
import OpenStack.ServerPassword as OSServerPassword
import OpenStack.ServerTags as OSServerTags
import OpenStack.ServerVolumes as OSSvrVols
import OpenStack.Types as OSTypes
import OpenStack.Volumes as OSVolumes
import Orchestration.Orchestration as Orchestration
import Ports
import Random
import RemoteData
import Rest.Cockpit
import Rest.Glance
import Rest.Keystone
import Rest.Neutron
import Rest.Nova
import Style.Widgets.NumericTextInput.NumericTextInput
import Task
import Time
import Toasty
import Types.Defaults as Defaults
import Types.Guacamole as GuacTypes
import Types.HelperTypes as HelperTypes
import Types.ServerResourceUsage
import Types.Types
    exposing
        ( CockpitLoginStatus(..)
        , Endpoints
        , ExoSetupStatus(..)
        , Flags
        , FloatingIpState(..)
        , HttpRequestMethod(..)
        , Model
        , Msg(..)
        , NewServerNetworkOptions(..)
        , NonProjectViewConstructor(..)
        , Project
        , ProjectIdentifier
        , ProjectSecret(..)
        , ProjectSpecificMsgConstructor(..)
        , ProjectViewConstructor(..)
        , Server
        , ServerFromExoProps
        , ServerOrigin(..)
        , TickInterval
        , UnscopedProvider
        , UnscopedProviderProject
        , ViewState(..)
        , currentExoServerVersion
        )
import UUID
import Url


init : Flags -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        currentTime =
            Time.millisToPosix flags.epoch

        timeZone =
            -- The minus sign is important here, as getTimezoneOffset() in JS uses the opposite sign of Elm's customZone
            Time.customZone -flags.timeZone []

        emptyStoredState : LocalStorageTypes.StoredState
        emptyStoredState =
            { projects = []
            , clientUuid = Nothing
            }

        emptyModel : Bool -> UUID.UUID -> Model
        emptyModel showDebugMsgs uuid =
            { logMessages = []
            , viewState = NonProjectView LoginPicker
            , navigationKey = key
            , maybeWindowSize = Just { width = flags.width, height = flags.height }
            , unscopedProviders = []
            , projects = []
            , toasties = Toasty.initialState
            , cloudCorsProxyUrl = flags.cloudCorsProxyUrl
            , cloudsWithUserAppProxy = Dict.fromList flags.cloudsWithUserAppProxy
            , isElectron = flags.isElectron
            , clientUuid = uuid
            , clientCurrentTime = currentTime
            , timeZone = timeZone
            , showDebugMsgs = showDebugMsgs
            }

        -- This only gets used if we do not find a client UUID in stored state
        newClientUuid : UUID.UUID
        newClientUuid =
            let
                seeds =
                    UUID.Seeds
                        (Random.initialSeed flags.randomSeed0)
                        (Random.initialSeed flags.randomSeed1)
                        (Random.initialSeed flags.randomSeed2)
                        (Random.initialSeed flags.randomSeed3)
            in
            UUID.step seeds |> Tuple.first

        storedState : LocalStorageTypes.StoredState
        storedState =
            case flags.storedState of
                Nothing ->
                    emptyStoredState

                Just storedStateValue ->
                    let
                        decodedValueResult =
                            Decode.decodeValue LocalStorage.decodeStoredState storedStateValue
                    in
                    case decodedValueResult of
                        Result.Err _ ->
                            emptyStoredState

                        Result.Ok decodedValue ->
                            decodedValue

        hydratedModel : Model
        hydratedModel =
            LocalStorage.hydrateModelFromStoredState (emptyModel flags.showDebugMsgs) newClientUuid storedState

        viewState =
            let
                defaultViewState =
                    case hydratedModel.projects of
                        [] ->
                            NonProjectView LoginPicker

                        firstProject :: _ ->
                            ProjectView
                                (Helpers.getProjectId firstProject)
                                { createPopup = False }
                                (ListProjectServers
                                    Defaults.serverListViewParams
                                )
            in
            AppUrl.Parser.urlToViewState url
                |> Maybe.withDefault defaultViewState

        -- If any projects are password-authenticated, get Application Credentials for them so we can forget the passwords
        projectsNeedingAppCredentials : List Project
        projectsNeedingAppCredentials =
            let
                projectNeedsAppCredential p =
                    case p.secret of
                        OpenstackPassword _ ->
                            True

                        ApplicationCredential _ ->
                            False
            in
            List.filter projectNeedsAppCredential hydratedModel.projects

        otherCmds =
            [ List.map getTimeForAppCredential projectsNeedingAppCredentials |> Cmd.batch
            , List.map Rest.Neutron.requestFloatingIps hydratedModel.projects |> Cmd.batch
            , List.map Rest.Nova.requestServers hydratedModel.projects |> Cmd.batch
            ]
                |> Cmd.batch

        newModel =
            let
                projectsServersLoading =
                    List.map
                        (Helpers.projectSetServersLoading currentTime)
                        hydratedModel.projects
            in
            { hydratedModel | projects = projectsServersLoading, viewState = viewState }
    in
    ( newModel
    , otherCmds
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every (1 * 1000) (Tick 1)
        , Time.every (5 * 1000) (Tick 5)
        , Time.every (10 * 1000) (Tick 10)
        , Time.every (60 * 1000) (Tick 60)
        , Time.every (300 * 1000) (Tick 300)
        , Browser.Events.onResize MsgChangeWindowSize
        ]


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
            Toasty.update Helpers.toastConfig ToastyMsg subMsg model

        NewLogMessage logMessage ->
            -- TODO This no longer requires a trip through the runtime now that we're storing current time in model?
            let
                newLogMessages =
                    logMessage :: model.logMessages
            in
            ( { model | logMessages = newLogMessages }, Cmd.none )

        MsgChangeWindowSize x y ->
            ( { model | maybeWindowSize = Just { width = x, height = y } }, Cmd.none )

        Tick interval time ->
            processTick model interval time

        DoOrchestration posixTime ->
            Orchestration.orchModel model posixTime

        SetNonProjectView nonProjectViewConstructor ->
            StateHelpers.updateViewState model Cmd.none (NonProjectView nonProjectViewConstructor)

        HandleApiErrorWithBody errorContext error ->
            Helpers.processSynchronousApiError model errorContext error

        RequestUnscopedToken openstackLoginUnscoped ->
            ( model, Rest.Keystone.requestUnscopedAuthToken model.cloudCorsProxyUrl openstackLoginUnscoped )

        RequestNewProjectToken openstackCreds ->
            let
                -- If user does not provide a port number and path (API version) then we guess it
                newOpenstackCreds =
                    { openstackCreds | authUrl = Helpers.authUrlWithPortAndVersion openstackCreds.authUrl }
            in
            ( model, Rest.Keystone.requestScopedAuthToken model.cloudCorsProxyUrl <| OSTypes.PasswordCreds newOpenstackCreds )

        JetstreamLogin jetstreamCreds ->
            let
                openstackCredsList =
                    Helpers.jetstreamToOpenstackCreds jetstreamCreds

                cmds =
                    List.map
                        (\creds -> Rest.Keystone.requestUnscopedAuthToken model.cloudCorsProxyUrl creds)
                        openstackCredsList
            in
            ( model, Cmd.batch cmds )

        ReceiveScopedAuthToken maybePassword ( metadata, response ) ->
            case Rest.Keystone.decodeScopedAuthToken <| Http.GoodStatus_ metadata response of
                Err error ->
                    Helpers.processStringError
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
                            Helpers.processStringError
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
                                    ProjectIdentifier
                                        authToken.project.name
                                        endpoints.keystone
                            in
                            -- If we don't have a project with same name + authUrl then create one, if we do then update its OSTypes.AuthToken
                            -- This code ensures we don't end up with duplicate projects on the same provider in our model.
                            case
                                ( Helpers.projectLookup model <| projectId, maybePassword )
                            of
                                ( Nothing, Nothing ) ->
                                    Helpers.processStringError
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
                                                    getTimeForAppCredential project

                                        ( newModel, updateTokenCmd ) =
                                            projectUpdateAuthToken model project authToken
                                    in
                                    ( newModel, Cmd.batch [ appCredCmd, updateTokenCmd ] )

        ReceiveUnscopedAuthToken keystoneUrl password ( metadata, response ) ->
            case Rest.Keystone.decodeUnscopedAuthToken <| Http.GoodStatus_ metadata response of
                Err error ->
                    Helpers.processStringError
                        model
                        (ErrorContext
                            "decode scoped auth token"
                            ErrorCrit
                            Nothing
                        )
                        error

                Ok authToken ->
                    case
                        Helpers.providerLookup model keystoneUrl
                    of
                        Just unscopedProvider ->
                            -- We already have an unscoped provider in the model with the same auth URL, update its token
                            unscopedProviderUpdateAuthToken model unscopedProvider authToken

                        Nothing ->
                            -- We don't have an unscoped provider with the same auth URL, create it
                            createUnscopedProvider model password authToken keystoneUrl

        ReceiveUnscopedProjects keystoneUrl unscopedProjects ->
            case
                Helpers.providerLookup model keystoneUrl
            of
                Just provider ->
                    let
                        newProvider =
                            { provider | projectsAvailable = RemoteData.Success unscopedProjects }

                        newModel =
                            Helpers.modelUpdateUnscopedProvider model newProvider
                    in
                    -- If we are not already on a SelectProjects view, then go there
                    case newModel.viewState of
                        NonProjectView (SelectProjects _ _) ->
                            ( newModel, Cmd.none )

                        _ ->
                            StateHelpers.updateViewState newModel
                                Cmd.none
                                (NonProjectView <|
                                    SelectProjects newProvider.authUrl []
                                )

                Nothing ->
                    -- Provider not found, may have been removed, nothing to do
                    ( model, Cmd.none )

        RequestProjectLoginFromProvider keystoneUrl password desiredProjects ->
            case Helpers.providerLookup model keystoneUrl of
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
                                        project.name
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
                                                (Helpers.getProjectId project)
                                                { createPopup = False }
                                            <|
                                                ListProjectServers Defaults.serverListViewParams

                                        Nothing ->
                                            NonProjectView LoginPicker

                        modelUpdatedUnscopedProviders =
                            { model | unscopedProviders = newUnscopedProviders }
                    in
                    StateHelpers.updateViewState modelUpdatedUnscopedProviders loginRequests newViewState

                Nothing ->
                    Helpers.processStringError
                        model
                        (ErrorContext
                            ("look for OpenStack provider with Keystone URL " ++ keystoneUrl)
                            ErrorCrit
                            Nothing
                        )
                        "Provider could not found in Exosphere's list of Providers."

        ProjectMsg projectIdentifier innerMsg ->
            case Helpers.projectLookup model projectIdentifier of
                Nothing ->
                    -- Project not found, may have been removed, nothing to do
                    ( model, Cmd.none )

                Just project ->
                    processProjectSpecificMsg model project innerMsg

        {- Form inputs -}
        InputOpenRc openstackCreds openRc ->
            let
                newCreds =
                    Helpers.processOpenRc openstackCreds openRc

                newViewState =
                    NonProjectView <| LoginOpenstack newCreds
            in
            StateHelpers.updateViewState model Cmd.none newViewState

        OpenInBrowser url ->
            ( model, Ports.openInBrowser url )

        OpenNewWindow url ->
            ( model, Ports.openNewWindow url )

        UrlChange url ->
            let
                newViewState =
                    AppUrl.Parser.urlToViewState url
                        |> Maybe.withDefault model.viewState
            in
            ( { model | viewState = newViewState }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


processTick : Model -> TickInterval -> Time.Posix -> ( Model, Cmd Msg )
processTick model interval time =
    let
        serverVolsNeedFrequentPoll : Project -> Server -> Bool
        serverVolsNeedFrequentPoll project server =
            Helpers.getVolsAttachedToServer project server
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
                    case Helpers.projectLookup model projectName of
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
                                            case Helpers.serverLookup project serverUuid of
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
                                            case Helpers.volumeLookup project volumeUuid of
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
            setProjectView model project projectViewConstructor

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
                        Helpers.modelUpdateProject model newProject
                in
                ( newModel, requestAuthToken newModel newProject )

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
                    StateHelpers.updateViewState model Cmd.none newViewState

                _ ->
                    ( model, Cmd.none )

        RemoveProject ->
            let
                newProjects =
                    List.filter (\p -> Helpers.getProjectId p /= Helpers.getProjectId project) model.projects

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
                                        (Helpers.getProjectId p)
                                        { createPopup = False }
                                    <|
                                        ListProjectServers
                                            Defaults.serverListViewParams

                                Nothing ->
                                    NonProjectView <| LoginPicker

                modelUpdatedProjects =
                    { model | projects = newProjects }
            in
            StateHelpers.updateViewState modelUpdatedProjects Cmd.none newViewState

        RequestServers ->
            let
                newProject =
                    Helpers.projectSetServersLoading model.clientCurrentTime project
            in
            ( Helpers.modelUpdateProject model newProject
            , Rest.Nova.requestServers project
            )

        RequestServer serverUuid ->
            let
                newProject =
                    Helpers.projectSetServerLoading project serverUuid
            in
            ( Helpers.modelUpdateProject model newProject
            , Rest.Nova.requestServer project serverUuid
            )

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
            ( Helpers.modelUpdateProject model newProject, cmd )

        RequestServerAction server func targetStatus ->
            let
                oldExoProps =
                    server.exoProps

                newServer =
                    Server server.osProps { oldExoProps | targetOpenstackStatus = targetStatus }

                newProject =
                    Helpers.projectUpdateServer project newServer

                newModel =
                    Helpers.modelUpdateProject model newProject
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
                        |> Maybe.map (Helpers.getServersWithVolAttached project)
                        |> Maybe.andThen List.head
            in
            case maybeServerUuid of
                Just serverUuid ->
                    ( model, OSSvrVols.requestDetachVolume project serverUuid volumeUuid )

                Nothing ->
                    Helpers.processStringError
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
                        (Helpers.getProjectId project)
                        { createPopup = False }
                    <|
                        ListProjectServers
                            Defaults.serverListViewParams

                createImageCmd =
                    Rest.Nova.requestCreateServerImage project serverUuid imageName
            in
            StateHelpers.updateViewState model createImageCmd newViewState

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
            ( Helpers.modelUpdateProject model newProject
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
                            Helpers.modelUpdateProject model newProject
                    in
                    Helpers.processSynchronousApiError newModel errorContext e

        ReceiveServer serverUuid errorContext result ->
            case result of
                Ok server ->
                    Rest.Nova.receiveServer model project server

                Err httpErrorWithBody ->
                    let
                        httpError =
                            httpErrorWithBody.error

                        non404 =
                            case Helpers.serverLookup project serverUuid of
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
                                            Helpers.projectUpdateServer project newServer

                                        newModel =
                                            Helpers.modelUpdateProject model newProject
                                    in
                                    Helpers.processSynchronousApiError newModel errorContext httpErrorWithBody
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
                                        Helpers.projectDeleteServer project serverUuid

                                    newModel =
                                        Helpers.modelUpdateProject model newProject
                                in
                                Helpers.processSynchronousApiError newModel newErrorContext httpErrorWithBody

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

        ReceiveCreateServer serverUuid ->
            Rest.Nova.receiveCreateServer model project serverUuid

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
                            case Helpers.serverLookup project serverUuid of
                                Just server ->
                                    let
                                        oldExoProps =
                                            server.exoProps

                                        newExoProps =
                                            { oldExoProps | deletionAttempted = True }

                                        newServer =
                                            { server | exoProps = newExoProps }
                                    in
                                    Helpers.projectUpdateServer project newServer

                                Nothing ->
                                    project

                        modelUpdatedProject =
                            Helpers.modelUpdateProject model newProject
                    in
                    StateHelpers.updateViewState modelUpdatedProject Cmd.none newViewState
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
                            Helpers.processStringError
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
                            Helpers.modelUpdateProject model newProject
                    in
                    Helpers.processSynchronousApiError newModel errorContext httpError

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
                    ( Helpers.modelUpdateProject model newProject, Cmd.none )

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
                            Helpers.modelUpdateProject model newProject
                    in
                    Helpers.processSynchronousApiError newModel errorContext httpError

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
            update (ProjectMsg (Helpers.getProjectId project) <| SetProjectView <| ListProjectVolumes []) model

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
                    Helpers.modelUpdateProject model newProject
            in
            ( newModel, Cmd.batch updateVolNameCmds )

        ReceiveDeleteVolume ->
            ( model, OSVolumes.requestVolumes project )

        ReceiveUpdateVolumeName ->
            ( model, OSVolumes.requestVolumes project )

        ReceiveAttachVolume attachment ->
            {- TODO opportunity for future optimization, just update the model instead of doing another API roundtrip -}
            update (ProjectMsg (Helpers.getProjectId project) <| SetProjectView <| MountVolInstructions attachment) model

        ReceiveDetachVolume ->
            {- TODO opportunity for future optimization, just update the model instead of doing another API roundtrip -}
            update (ProjectMsg (Helpers.getProjectId project) <| SetProjectView <| ListProjectVolumes []) model

        ReceiveAppCredential appCredential ->
            let
                newProject =
                    { project | secret = ApplicationCredential appCredential }
            in
            ( Helpers.modelUpdateProject model newProject, Cmd.none )

        RequestAppCredential posix ->
            ( model, Rest.Keystone.requestAppCredential project model.clientUuid posix )

        ReceiveComputeQuota quota ->
            let
                newProject =
                    { project | computeQuota = RemoteData.Success quota }
            in
            ( Helpers.modelUpdateProject model newProject, Cmd.none )

        ReceiveVolumeQuota quota ->
            let
                newProject =
                    { project | volumeQuota = RemoteData.Success quota }
            in
            ( Helpers.modelUpdateProject model newProject, Cmd.none )

        ReceiveServerPassword serverUuid password ->
            if String.isEmpty password then
                ( model, Cmd.none )

            else
                let
                    tag =
                        "exoPw:" ++ password

                    cmd =
                        case Helpers.serverLookup project serverUuid of
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
            case Helpers.serverLookup project serverUuid of
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
                                    Helpers.projectUpdateServer project newServer

                                newModel =
                                    Helpers.modelUpdateProject model newProject
                            in
                            case result of
                                Err httpError ->
                                    Helpers.processSynchronousApiError newModel errorContext httpError

                                Ok _ ->
                                    ( newModel, exoSetupStatusMetadataCmd )

        ReceiveSetServerName serverUuid _ errorContext result ->
            case ( Helpers.serverLookup project serverUuid, result ) of
                ( Nothing, _ ) ->
                    -- Ensure that the server UUID we get back exists in the model. If not, ignore.
                    ( model, Cmd.none )

                ( _, Err e ) ->
                    Helpers.processSynchronousApiError model errorContext e

                ( Just server, Ok actualNewServerName ) ->
                    let
                        oldOsProps =
                            server.osProps

                        newOsProps =
                            { oldOsProps | name = actualNewServerName }

                        newServer =
                            { server | osProps = newOsProps }

                        newProject =
                            Helpers.projectUpdateServer project newServer

                        modelWithUpdatedProject =
                            Helpers.modelUpdateProject model newProject

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
                    StateHelpers.updateViewState modelWithUpdatedProject Cmd.none updatedView

        ReceiveSetServerMetadata serverUuid intendedMetadataItem errorContext result ->
            case ( Helpers.serverLookup project serverUuid, result ) of
                ( Nothing, _ ) ->
                    -- Server does not exist in the model, ignore it
                    ( model, Cmd.none )

                ( _, Err e ) ->
                    -- Error from API
                    Helpers.processSynchronousApiError model errorContext e

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
                                Helpers.projectUpdateServer project newServer

                            newModel =
                                Helpers.modelUpdateProject model newProject
                        in
                        ( newModel, Cmd.none )

                    else
                        -- This is bonkers, throw an error
                        Helpers.processStringError
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
                            Helpers.projectUpdateServer project newServer

                        newModel =
                            Helpers.modelUpdateProject model newProject
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
            case Helpers.serverLookup project serverUuid of
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
                                    Helpers.processStringError
                                        model
                                        errorContext
                                        "Server does not appear to have been launched with Guacamole support"

                        ServerNotFromExo ->
                            Helpers.processStringError
                                model
                                errorContext
                                "Server does not appear to have been launched from Exosphere"

                Nothing ->
                    Helpers.processStringError
                        model
                        errorContext
                        "Could not find server in the model, maybe it has been deleted."


setProjectView : Model -> Project -> ProjectViewConstructor -> ( Model, Cmd Msg )
setProjectView model project projectViewConstructor =
    let
        prevProjectViewConstructor =
            case model.viewState of
                ProjectView projectId _ projectViewConstructor_ ->
                    if projectId == Helpers.getProjectId project then
                        Just projectViewConstructor_

                    else
                        Nothing

                _ ->
                    Nothing

        newViewState =
            ProjectView (Helpers.getProjectId project) { createPopup = False } projectViewConstructor

        updatedViewModelAndCmd model_ cmd_ =
            StateHelpers.updateViewState model_ cmd_ newViewState

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
    in
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
            updatedViewModelAndCmd model cmd

        ListProjectServers _ ->
            -- Don't fire cmds if we're already in this view
            case prevProjectViewConstructor of
                Just (ListProjectServers _) ->
                    updatedViewModelAndCmd model Cmd.none

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
                    updatedViewModelAndCmd newModel cmd

        ServerDetail serverUuid _ ->
            -- Don't fire cmds if we're already in this view
            case prevProjectViewConstructor of
                Just (ServerDetail _ _) ->
                    updatedViewModelAndCmd model Cmd.none

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
                    updatedViewModelAndCmd newModel cmd

        CreateServerImage _ _ ->
            updatedViewModelAndCmd model Cmd.none

        CreateServer viewParams ->
            case model.viewState of
                -- If we are already in this view state then ensure user isn't trying to choose a server count
                -- that would exceed quota; if so, reduce server count to comply with quota.
                ProjectView _ _ (CreateServer _) ->
                    let
                        newViewParams =
                            case
                                ( Helpers.flavorLookup project viewParams.flavorUuid
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
                                        (Helpers.getProjectId project)
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
                            ProjectMsg (Helpers.getProjectId project) <|
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
                    updatedViewModelAndCmd newModel cmd

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
            updatedViewModelAndCmd model cmd

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
            ( modelUpdatedView model, cmd )

        VolumeDetail _ _ ->
            updatedViewModelAndCmd model Cmd.none

        AttachVolumeModal _ _ ->
            case prevProjectViewConstructor of
                Just (AttachVolumeModal _ _) ->
                    updatedViewModelAndCmd model Cmd.none

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
                    updatedViewModelAndCmd newModel cmd

        MountVolInstructions _ ->
            updatedViewModelAndCmd model Cmd.none

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
            updatedViewModelAndCmd model cmd


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
                    |> Helpers.hostnameFromUrl
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
                        (Helpers.getProjectId newProject)
                        { createPopup = False }
                    <|
                        ListProjectServers Defaults.serverListViewParams

                ProjectView _ projectViewParams _ ->
                    ProjectView
                        (Helpers.getProjectId newProject)
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
      ]
        |> List.map (\x -> x newProject)
        |> (\l -> getTimeForAppCredential newProject :: l)
        |> Cmd.batch
    )


projectUpdateAuthToken : Model -> Project -> OSTypes.ScopedAuthToken -> ( Model, Cmd Msg )
projectUpdateAuthToken model project authToken =
    -- Update auth token for existing project
    let
        newProject =
            { project | auth = authToken }

        newModel =
            Helpers.modelUpdateProject model newProject
    in
    sendPendingRequests newModel newProject


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


unscopedProviderUpdateAuthToken : Model -> UnscopedProvider -> OSTypes.UnscopedAuthToken -> ( Model, Cmd Msg )
unscopedProviderUpdateAuthToken model provider authToken =
    let
        newProvider =
            { provider | token = authToken }

        newModel =
            Helpers.modelUpdateUnscopedProvider model newProvider
    in
    ( newModel, Cmd.none )


sendPendingRequests : Model -> Project -> ( Model, Cmd Msg )
sendPendingRequests model project =
    -- Fires any pending commands which were waiting for auth token renewal
    -- This function assumes our token is valid (does not check for expiry).
    let
        -- Hydrate cmds with auth token
        cmds =
            List.map (\pqr -> pqr project.auth.tokenValue) project.pendingCredentialedRequests

        -- Clear out pendingCredentialedRequests
        newProject =
            { project | pendingCredentialedRequests = [] }

        newModel =
            Helpers.modelUpdateProject model newProject
    in
    ( newModel, Cmd.batch cmds )


getTimeForAppCredential : Project -> Cmd Msg
getTimeForAppCredential project =
    Task.perform (\posixTime -> ProjectMsg (Helpers.getProjectId project) (RequestAppCredential posixTime)) Time.now


requestAuthToken : Model -> Project -> Cmd Msg
requestAuthToken model project =
    -- Wraps Rest.RequestAuthToken, builds OSTypes.PasswordCreds if needed
    let
        creds =
            case project.secret of
                OpenstackPassword password ->
                    OSTypes.PasswordCreds <|
                        OSTypes.OpenstackLogin
                            project.endpoints.keystone
                            (if String.isEmpty project.auth.projectDomain.name then
                                project.auth.projectDomain.uuid

                             else
                                project.auth.projectDomain.name
                            )
                            project.auth.project.name
                            (if String.isEmpty project.auth.userDomain.name then
                                project.auth.userDomain.uuid

                             else
                                project.auth.userDomain.name
                            )
                            project.auth.user.name
                            password

                ApplicationCredential appCred ->
                    OSTypes.AppCreds project.endpoints.keystone project.auth.project.name appCred
    in
    Rest.Keystone.requestScopedAuthToken model.cloudCorsProxyUrl creds


requestDeleteServer : Project -> OSTypes.ServerUuid -> ( Project, Cmd Msg )
requestDeleteServer project serverUuid =
    case Helpers.serverLookup project serverUuid of
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
                    Helpers.projectUpdateServer project newServer
            in
            ( newProject, Rest.Nova.requestDeleteServer newProject newServer )
