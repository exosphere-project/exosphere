module State.State exposing (update)

import Browser
import Browser.Navigation
import Helpers.ExoSetupStatus
import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers as Helpers
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.ServerResourceUsage
import Http
import LocalStorage.LocalStorage as LocalStorage
import Maybe
import OpenStack.ServerPassword as OSServerPassword
import OpenStack.ServerTags as OSServerTags
import OpenStack.ServerVolumes as OSSvrVols
import OpenStack.Types as OSTypes
import OpenStack.Volumes as OSVolumes
import Orchestration.Orchestration as Orchestration
import Page.AllResourcesList
import Page.FloatingIpAssign
import Page.FloatingIpList
import Page.GetSupport
import Page.Home
import Page.InstanceSourcePicker
import Page.KeypairCreate
import Page.KeypairList
import Page.LoginJetstream
import Page.LoginOpenstack
import Page.LoginPicker
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
import Ports
import RemoteData
import Rest.ApiModelHelpers as ApiModelHelpers
import Rest.Glance
import Rest.Keystone
import Rest.Neutron
import Rest.Nova
import Route
import Set
import State.Auth
import State.Error
import State.ViewState as ViewStateHelpers
import Style.Toast
import Style.Widgets.NumericTextInput.NumericTextInput
import Task
import Time
import Toasty
import Types.Error as Error exposing (ErrorContext, ErrorLevel(..))
import Types.Guacamole as GuacTypes
import Types.HelperTypes as HelperTypes exposing (HttpRequestMethod(..), UnscopedProviderProject)
import Types.OuterModel exposing (OuterModel)
import Types.OuterMsg exposing (OuterMsg(..))
import Types.Project exposing (Endpoints, Project, ProjectSecret(..))
import Types.Server exposing (ExoSetupStatus(..), NewServerNetworkOptions(..), Server, ServerFromExoProps, ServerOrigin(..), currentExoServerVersion)
import Types.ServerResourceUsage
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), ServerSpecificMsgConstructor(..), SharedMsg(..), TickInterval)
import Types.View
    exposing
        ( LoginView(..)
        , NonProjectViewConstructor(..)
        , ProjectViewConstructor(..)
        , ViewState(..)
        )
import Types.Workflow
    exposing
        ( CustomWorkflow
        , CustomWorkflowTokenRDPP
        , ServerCustomWorkflowStatus(..)
        )
import Url
import View.Helpers exposing (toExoPalette)


update : OuterMsg -> OuterModel -> ( OuterModel, Cmd OuterMsg )
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
                SharedMsg (DoOrchestration _) ->
                    Cmd.none

                SharedMsg (Tick _ _) ->
                    Cmd.none

                _ ->
                    Task.perform (\posix -> SharedMsg <| DoOrchestration posix) Time.now
    in
    ( newOuterModel
    , Cmd.batch
        [ Ports.setStorage (LocalStorage.generateStoredState newOuterModel.sharedModel)
        , orchestrationTimeCmd
        , cmds
        ]
    )



-- There may be a better approach than these mapping functions, I'm not sure yet.


mapToOuterMsg : ( a, Cmd SharedMsg ) -> ( a, Cmd OuterMsg )
mapToOuterMsg ( model, cmdSharedMsg ) =
    ( model, Cmd.map SharedMsg cmdSharedMsg )


mapToOuterModel : OuterModel -> ( SharedModel, Cmd a ) -> ( OuterModel, Cmd a )
mapToOuterModel outerModel ( newSharedModel, cmd ) =
    ( { outerModel | sharedModel = newSharedModel }, cmd )


pipelineCmdOuterModelMsg : (OuterModel -> ( OuterModel, Cmd OuterMsg )) -> ( OuterModel, Cmd OuterMsg ) -> ( OuterModel, Cmd OuterMsg )
pipelineCmdOuterModelMsg fn ( outerModel, outerCmd ) =
    let
        ( newModel, newCmd ) =
            fn outerModel
    in
    ( newModel, Cmd.batch [ outerCmd, newCmd ] )


updateUnderlying : OuterMsg -> OuterModel -> ( OuterModel, Cmd OuterMsg )
updateUnderlying outerMsg outerModel =
    let
        sharedModel =
            outerModel.sharedModel
    in
    case ( outerMsg, outerModel.viewState ) of
        ( SharedMsg sharedMsg, _ ) ->
            processSharedMsg sharedMsg outerModel

        -- TODO exact same structure for each page-specific case here. Is there a way to deduplicate or factor out?
        ( GetSupportMsg pageMsg, NonProjectView (GetSupport pageModel) ) ->
            let
                ( newPageModel, cmd, sharedMsg ) =
                    Page.GetSupport.update pageMsg sharedModel pageModel
            in
            ( { outerModel
                | viewState = NonProjectView <| GetSupport newPageModel
              }
            , Cmd.map (\msg -> GetSupportMsg msg) cmd
            )
                |> pipelineCmdOuterModelMsg
                    (processSharedMsg sharedMsg)

        ( HomeMsg pageMsg, NonProjectView (Home pageModel) ) ->
            let
                ( newPageModel, cmd, sharedMsg ) =
                    Page.Home.update pageMsg pageModel
            in
            ( { outerModel
                | viewState = NonProjectView <| Home newPageModel
              }
            , Cmd.map (\msg -> HomeMsg msg) cmd
            )
                |> pipelineCmdOuterModelMsg
                    (processSharedMsg sharedMsg)

        ( LoginJetstreamMsg pageMsg, NonProjectView (Login (LoginJetstream pageModel)) ) ->
            let
                ( newPageModel, cmd, sharedMsg ) =
                    Page.LoginJetstream.update pageMsg sharedModel pageModel
            in
            ( { outerModel
                | viewState = NonProjectView <| Login <| LoginJetstream newPageModel
              }
            , Cmd.map (\msg -> LoginJetstreamMsg msg) cmd
            )
                |> pipelineCmdOuterModelMsg
                    (processSharedMsg sharedMsg)

        ( LoginOpenstackMsg pageMsg, NonProjectView (Login (LoginOpenstack pageModel)) ) ->
            let
                ( newPageModel, cmd, sharedMsg ) =
                    Page.LoginOpenstack.update pageMsg sharedModel pageModel
            in
            ( { outerModel
                | viewState = NonProjectView <| Login <| LoginOpenstack newPageModel
              }
            , Cmd.map (\msg -> LoginOpenstackMsg msg) cmd
            )
                |> pipelineCmdOuterModelMsg
                    (processSharedMsg sharedMsg)

        ( LoginPickerMsg pageMsg, NonProjectView LoginPicker ) ->
            let
                ( _, cmd, sharedMsg ) =
                    Page.LoginPicker.update pageMsg
            in
            ( { outerModel
                | viewState = NonProjectView <| LoginPicker
              }
            , Cmd.map (\msg -> LoginPickerMsg msg) cmd
            )
                |> pipelineCmdOuterModelMsg
                    (processSharedMsg sharedMsg)

        ( MessageLogMsg pageMsg, NonProjectView (MessageLog pageModel) ) ->
            let
                ( newPageModel, cmd, sharedMsg ) =
                    Page.MessageLog.update pageMsg sharedModel pageModel
            in
            ( { outerModel
                | viewState = NonProjectView <| MessageLog newPageModel
              }
            , Cmd.map (\msg -> MessageLogMsg msg) cmd
            )
                |> pipelineCmdOuterModelMsg
                    (processSharedMsg sharedMsg)

        ( SelectProjectsMsg pageMsg, NonProjectView (SelectProjects pageModel) ) ->
            let
                ( newPageModel, cmd, sharedMsg ) =
                    Page.SelectProjects.update pageMsg sharedModel pageModel
            in
            ( { outerModel
                | viewState = NonProjectView <| SelectProjects newPageModel
              }
            , Cmd.map (\msg -> SelectProjectsMsg msg) cmd
            )
                |> pipelineCmdOuterModelMsg
                    (processSharedMsg sharedMsg)

        ( SettingsMsg pageMsg, NonProjectView (Settings pageModel) ) ->
            let
                ( newPageModel, cmd, sharedMsg ) =
                    Page.Settings.update pageMsg sharedModel pageModel
            in
            ( { outerModel
                | viewState = NonProjectView <| Settings newPageModel
              }
            , Cmd.map (\msg -> SettingsMsg msg) cmd
            )
                |> pipelineCmdOuterModelMsg
                    (processSharedMsg sharedMsg)

        ( pageSpecificMsg, ProjectView projectId projectViewModel projectViewConstructor ) ->
            case GetterSetters.projectLookup sharedModel projectId of
                Just project ->
                    case ( pageSpecificMsg, projectViewConstructor ) of
                        -- This is the repetitive dispatch code that people warn you about when you use the nested Elm architecture.
                        -- Maybe there is a way to make it less repetitive (or at least more compact) in the future.
                        ( AllResourcesListMsg pageMsg, AllResourcesList pageModel ) ->
                            let
                                ( newSharedModel, cmd, sharedMsg ) =
                                    Page.AllResourcesList.update pageMsg project pageModel
                            in
                            ( { outerModel
                                | viewState =
                                    ProjectView projectId projectViewModel <|
                                        AllResourcesList newSharedModel
                              }
                            , Cmd.map (\msg -> AllResourcesListMsg msg) cmd
                            )
                                |> pipelineCmdOuterModelMsg
                                    (processSharedMsg sharedMsg)

                        ( FloatingIpAssignMsg pageMsg, FloatingIpAssign pageModel ) ->
                            let
                                ( newSharedModel, cmd, sharedMsg ) =
                                    Page.FloatingIpAssign.update pageMsg sharedModel project pageModel
                            in
                            ( { outerModel
                                | viewState =
                                    ProjectView projectId projectViewModel <|
                                        FloatingIpAssign newSharedModel
                              }
                            , Cmd.map (\msg -> FloatingIpAssignMsg msg) cmd
                            )
                                |> pipelineCmdOuterModelMsg
                                    (processSharedMsg sharedMsg)

                        ( FloatingIpListMsg pageMsg, FloatingIpList pageModel ) ->
                            let
                                ( newSharedModel, cmd, sharedMsg ) =
                                    Page.FloatingIpList.update pageMsg project pageModel
                            in
                            ( { outerModel
                                | viewState =
                                    ProjectView projectId projectViewModel <|
                                        FloatingIpList newSharedModel
                              }
                            , Cmd.map (\msg -> FloatingIpListMsg msg) cmd
                            )
                                |> pipelineCmdOuterModelMsg
                                    (processSharedMsg sharedMsg)

                        ( InstanceSourcePickerMsg pageMsg, InstanceSourcePicker pageModel ) ->
                            let
                                ( newSharedModel, cmd, sharedMsg ) =
                                    Page.InstanceSourcePicker.update pageMsg project pageModel
                            in
                            ( { outerModel
                                | viewState =
                                    ProjectView projectId projectViewModel <|
                                        InstanceSourcePicker newSharedModel
                              }
                            , Cmd.map (\msg -> InstanceSourcePickerMsg msg) cmd
                            )
                                |> pipelineCmdOuterModelMsg
                                    (processSharedMsg sharedMsg)

                        ( KeypairCreateMsg pageMsg, KeypairCreate pageModel ) ->
                            let
                                ( newSharedModel, cmd, sharedMsg ) =
                                    Page.KeypairCreate.update pageMsg project pageModel
                            in
                            ( { outerModel
                                | viewState =
                                    ProjectView projectId projectViewModel <|
                                        KeypairCreate newSharedModel
                              }
                            , Cmd.map (\msg -> KeypairCreateMsg msg) cmd
                            )
                                |> pipelineCmdOuterModelMsg
                                    (processSharedMsg sharedMsg)

                        ( KeypairListMsg pageMsg, KeypairList pageModel ) ->
                            let
                                ( newSharedModel, cmd, sharedMsg ) =
                                    Page.KeypairList.update pageMsg project pageModel
                            in
                            ( { outerModel
                                | viewState =
                                    ProjectView projectId projectViewModel <|
                                        KeypairList newSharedModel
                              }
                            , Cmd.map (\msg -> KeypairListMsg msg) cmd
                            )
                                |> pipelineCmdOuterModelMsg
                                    (processSharedMsg sharedMsg)

                        ( ServerCreateMsg pageMsg, ServerCreate pageModel ) ->
                            let
                                ( newSharedModel, cmd, sharedMsg ) =
                                    Page.ServerCreate.update pageMsg project pageModel
                            in
                            ( { outerModel
                                | viewState =
                                    ProjectView projectId projectViewModel <|
                                        ServerCreate newSharedModel
                              }
                            , Cmd.map (\msg -> ServerCreateMsg msg) cmd
                            )
                                |> pipelineCmdOuterModelMsg
                                    (processSharedMsg sharedMsg)

                        ( ServerCreateImageMsg pageMsg, ServerCreateImage pageModel ) ->
                            let
                                ( newSharedModel, cmd, sharedMsg ) =
                                    Page.ServerCreateImage.update pageMsg sharedModel project pageModel
                            in
                            ( { outerModel
                                | viewState =
                                    ProjectView projectId projectViewModel <|
                                        ServerCreateImage newSharedModel
                              }
                            , Cmd.map (\msg -> ServerCreateImageMsg msg) cmd
                            )
                                |> pipelineCmdOuterModelMsg
                                    (processSharedMsg sharedMsg)

                        ( ServerDetailMsg pageMsg, ServerDetail pageModel ) ->
                            let
                                ( newSharedModel, cmd, sharedMsg ) =
                                    Page.ServerDetail.update pageMsg project pageModel
                            in
                            ( { outerModel
                                | viewState =
                                    ProjectView projectId projectViewModel <|
                                        ServerDetail newSharedModel
                              }
                            , Cmd.map (\msg -> ServerDetailMsg msg) cmd
                            )
                                |> pipelineCmdOuterModelMsg
                                    (processSharedMsg sharedMsg)

                        ( ServerListMsg pageMsg, ServerList pageModel ) ->
                            let
                                ( newSharedModel, cmd, sharedMsg ) =
                                    Page.ServerList.update pageMsg project pageModel
                            in
                            ( { outerModel
                                | viewState =
                                    ProjectView projectId projectViewModel <|
                                        ServerList newSharedModel
                              }
                            , Cmd.map (\msg -> ServerListMsg msg) cmd
                            )
                                |> pipelineCmdOuterModelMsg
                                    (processSharedMsg sharedMsg)

                        ( VolumeAttachMsg pageMsg, VolumeAttach pageModel ) ->
                            let
                                ( newSharedModel, cmd, sharedMsg ) =
                                    Page.VolumeAttach.update pageMsg sharedModel project pageModel
                            in
                            ( { outerModel
                                | viewState =
                                    ProjectView projectId projectViewModel <|
                                        VolumeAttach newSharedModel
                              }
                            , Cmd.map (\msg -> VolumeAttachMsg msg) cmd
                            )
                                |> pipelineCmdOuterModelMsg
                                    (processSharedMsg sharedMsg)

                        ( VolumeCreateMsg pageMsg, VolumeCreate pageModel ) ->
                            let
                                ( newSharedModel, cmd, sharedMsg ) =
                                    Page.VolumeCreate.update pageMsg project pageModel
                            in
                            ( { outerModel
                                | viewState =
                                    ProjectView projectId projectViewModel <|
                                        VolumeCreate newSharedModel
                              }
                            , Cmd.map (\msg -> VolumeCreateMsg msg) cmd
                            )
                                |> pipelineCmdOuterModelMsg
                                    (processSharedMsg sharedMsg)

                        ( VolumeDetailMsg pageMsg, VolumeDetail pageModel ) ->
                            let
                                ( newSharedModel, cmd, sharedMsg ) =
                                    Page.VolumeDetail.update pageMsg project pageModel
                            in
                            ( { outerModel
                                | viewState =
                                    ProjectView projectId projectViewModel <|
                                        VolumeDetail newSharedModel
                              }
                            , Cmd.map (\msg -> VolumeDetailMsg msg) cmd
                            )
                                |> pipelineCmdOuterModelMsg
                                    (processSharedMsg sharedMsg)

                        ( VolumeListMsg pageMsg, VolumeList pageModel ) ->
                            let
                                ( newSharedModel, cmd, sharedMsg ) =
                                    Page.VolumeList.update pageMsg project pageModel
                            in
                            ( { outerModel
                                | viewState =
                                    ProjectView projectId projectViewModel <|
                                        VolumeList newSharedModel
                              }
                            , Cmd.map (\msg -> VolumeListMsg msg) cmd
                            )
                                |> pipelineCmdOuterModelMsg
                                    (processSharedMsg sharedMsg)

                        _ ->
                            -- This is not great because it allows us to forget to write a case statement above, but I don't know of a nicer way to write a catchall case for when a page-specific Msg is received for an inapplicable page.
                            ( outerModel, Cmd.none )

                Nothing ->
                    ( outerModel, Cmd.none )

        ( _, _ ) ->
            -- This is not great because it allows us to forget to write a case statement above, but I don't know of a nicer way to write a catchall case for when a page-specific Msg is received for an inapplicable page.
            ( outerModel, Cmd.none )


processSharedMsg : SharedMsg -> OuterModel -> ( OuterModel, Cmd OuterMsg )
processSharedMsg sharedMsg outerModel =
    let
        { sharedModel } =
            outerModel

        { viewContext } =
            sharedModel
    in
    case sharedMsg of
        ToastyMsg subMsg ->
            Toasty.update Style.Toast.toastConfig ToastyMsg subMsg outerModel.sharedModel
                |> mapToOuterMsg
                |> mapToOuterModel outerModel

        MsgChangeWindowSize x y ->
            ( { sharedModel
                | viewContext =
                    { viewContext
                        | windowSize = { width = x, height = y }
                    }
              }
            , Cmd.none
            )
                |> mapToOuterModel outerModel

        Tick interval time ->
            processTick outerModel interval time
                |> mapToOuterModel outerModel

        DoOrchestration posixTime ->
            Orchestration.orchModel sharedModel posixTime
                |> mapToOuterMsg
                |> mapToOuterModel outerModel

        HandleApiErrorWithBody errorContext error ->
            State.Error.processSynchronousApiError sharedModel errorContext error
                |> mapToOuterMsg
                |> mapToOuterModel outerModel

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
                |> mapToOuterMsg

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
                |> mapToOuterMsg

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
                        |> mapToOuterMsg
                        |> mapToOuterModel outerModel

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
                                |> mapToOuterMsg
                                |> mapToOuterModel outerModel

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

                                        ( newOuterModel, updateTokenCmd ) =
                                            State.Auth.projectUpdateAuthToken outerModel project authToken
                                    in
                                    ( newOuterModel, Cmd.batch [ appCredCmd, updateTokenCmd ] )
                                        |> mapToOuterMsg

        ReceiveUnscopedAuthToken keystoneUrl ( metadata, response ) ->
            case Rest.Keystone.decodeUnscopedAuthToken <| Http.GoodStatus_ metadata response of
                Err error ->
                    State.Error.processStringError
                        sharedModel
                        (ErrorContext
                            "decode unscoped auth token"
                            ErrorCrit
                            Nothing
                        )
                        error
                        |> mapToOuterMsg
                        |> mapToOuterModel outerModel

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
                                |> mapToOuterMsg
                                |> mapToOuterModel outerModel

                        Nothing ->
                            -- We don't have an unscoped provider with the same auth URL, create it
                            createUnscopedProvider sharedModel authToken keystoneUrl
                                |> mapToOuterMsg
                                |> mapToOuterModel outerModel

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
                        NonProjectView (SelectProjects _) ->
                            ( newOuterModel, Cmd.none )

                        _ ->
                            ( newOuterModel
                            , Route.pushUrl viewContext (Route.SelectProjects keystoneUrl)
                            )

                Nothing ->
                    -- Provider not found, may have been removed, nothing to do
                    ( outerModel, Cmd.none )

        RequestProjectLoginFromProvider keystoneUrl desiredProjectIdentifiers ->
            case GetterSetters.providerLookup sharedModel keystoneUrl of
                Just provider ->
                    let
                        buildLoginRequest : UnscopedProviderProject -> Cmd SharedMsg
                        buildLoginRequest project =
                            Rest.Keystone.requestScopedAuthToken
                                sharedModel.cloudCorsProxyUrl
                            <|
                                OSTypes.TokenCreds
                                    keystoneUrl
                                    provider.token
                                    project.project.uuid

                        unscopedProjects =
                            desiredProjectIdentifiers
                                |> Set.toList
                                |> List.filterMap (GetterSetters.unscopedProjectLookup provider)

                        loginRequests =
                            List.map buildLoginRequest unscopedProjects
                                |> Cmd.batch

                        -- Remove unscoped provider from model now that we have selected projects from it
                        newUnscopedProviders =
                            List.filter
                                (\p -> p.authUrl /= keystoneUrl)
                                sharedModel.unscopedProviders

                        -- If we still have at least one unscoped provider in the model then ask the user to choose projects from it
                        newViewStateCmd =
                            case List.head newUnscopedProviders of
                                Just unscopedProvider ->
                                    Route.pushUrl viewContext (Route.SelectProjects unscopedProvider.authUrl)

                                Nothing ->
                                    -- If we have at least one project then show it, else show the login page
                                    case List.head sharedModel.projects of
                                        Just project ->
                                            Route.pushUrl viewContext <|
                                                Route.ProjectRoute project.auth.project.uuid <|
                                                    Route.AllResourcesList

                                        Nothing ->
                                            Route.pushUrl viewContext Route.LoginPicker

                        sharedModelUpdatedUnscopedProviders =
                            { sharedModel | unscopedProviders = newUnscopedProviders }
                    in
                    ( { outerModel | sharedModel = sharedModelUpdatedUnscopedProviders }
                    , Cmd.batch [ Cmd.map SharedMsg loginRequests, newViewStateCmd ]
                    )

                Nothing ->
                    State.Error.processStringError
                        sharedModel
                        (ErrorContext
                            ("look for OpenStack provider with Keystone URL " ++ keystoneUrl)
                            ErrorCrit
                            Nothing
                        )
                        "Provider could not found in Exosphere's list of Providers."
                        |> mapToOuterMsg
                        |> mapToOuterModel outerModel

        ProjectMsg projectIdentifier innerMsg ->
            case GetterSetters.projectLookup sharedModel projectIdentifier of
                Nothing ->
                    -- Project not found, may have been removed, nothing to do
                    ( outerModel, Cmd.none )

                Just project ->
                    processProjectSpecificMsg outerModel project innerMsg

        OpenNewWindow url ->
            ( outerModel, Ports.openNewWindow url )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( outerModel, Browser.Navigation.pushUrl viewContext.navigationKey (Url.toString url) )

                Browser.External url ->
                    ( outerModel, Browser.Navigation.load url )

        UrlChanged url ->
            ViewStateHelpers.navigateToPage url outerModel

        SetStyle styleMode ->
            let
                oldStyle =
                    sharedModel.style

                newStyle =
                    { oldStyle | styleMode = styleMode }
            in
            ( { sharedModel
                | style = newStyle
                , viewContext = { viewContext | palette = toExoPalette newStyle }
              }
            , Cmd.none
            )
                |> mapToOuterModel outerModel

        NoOp ->
            ( outerModel, Cmd.none )

        SetExperimentalFeaturesEnabled choice ->
            ( { sharedModel | viewContext = { viewContext | experimentalFeaturesEnabled = choice } }, Cmd.none )
                |> mapToOuterModel outerModel


processTick : OuterModel -> TickInterval -> Time.Posix -> ( SharedModel, Cmd OuterMsg )
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
                NonProjectView _ ->
                    ( outerModel.sharedModel, Cmd.none )

                ProjectView projectName _ projectViewState ->
                    case GetterSetters.projectLookup outerModel.sharedModel projectName of
                        Nothing ->
                            {- Should this throw an error? -}
                            ( outerModel.sharedModel, Cmd.none )

                        Just project ->
                            let
                                pollVolumes : ( SharedModel, Cmd SharedMsg )
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
                                AllResourcesList _ ->
                                    pollVolumes

                                ServerDetail model ->
                                    let
                                        volCmd =
                                            OSVolumes.requestVolumes project
                                    in
                                    case interval of
                                        5 ->
                                            case GetterSetters.serverLookup project model.serverUuid of
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

                                VolumeDetail pageModel ->
                                    ( outerModel.sharedModel
                                    , case interval of
                                        5 ->
                                            case GetterSetters.volumeLookup project pageModel.volumeUuid of
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

                                VolumeList _ ->
                                    pollVolumes

                                _ ->
                                    ( outerModel.sharedModel, Cmd.none )
    in
    ( { viewDependentModel | clientCurrentTime = time }
    , Cmd.batch
        [ viewDependentCmd
        , viewIndependentCmd
        ]
        |> Cmd.map SharedMsg
    )


processProjectSpecificMsg : OuterModel -> Project -> ProjectSpecificMsgConstructor -> ( OuterModel, Cmd OuterMsg )
processProjectSpecificMsg outerModel project msg =
    let
        sharedModel =
            outerModel.sharedModel
    in
    case msg of
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
                    |> mapToOuterMsg
                    |> mapToOuterModel outerModel

            else
                -- Token is expired (or nearly expired) so we add request to list of pending requests and refresh that token
                let
                    newPQRs =
                        ( project.auth.project.uuid, requestNeedingToken ) :: outerModel.pendingCredentialedRequests

                    cmdResult =
                        State.Auth.requestAuthToken sharedModel project

                    newOuterModel =
                        { outerModel | pendingCredentialedRequests = newPQRs }
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
                        State.Error.processStringError sharedModel errorContext e
                            |> mapToOuterMsg
                            |> mapToOuterModel newOuterModel

                    Ok cmd ->
                        ( sharedModel, cmd )
                            |> mapToOuterMsg
                            |> mapToOuterModel newOuterModel

        ToggleCreatePopup ->
            case outerModel.viewState of
                ProjectView projectId projectViewModel viewConstructor ->
                    let
                        newViewState =
                            ProjectView
                                projectId
                                { projectViewModel
                                    | createPopup = not projectViewModel.createPopup
                                }
                                viewConstructor
                    in
                    ( { outerModel | viewState = newViewState }, Cmd.none )

                _ ->
                    ( outerModel, Cmd.none )

        RemoveProject ->
            let
                newProjects =
                    List.filter (\p -> p.auth.project.uuid /= project.auth.project.uuid) sharedModel.projects

                newOuterModel =
                    { outerModel | sharedModel = { sharedModel | projects = newProjects } }
            in
            case outerModel.viewState of
                NonProjectView _ ->
                    -- If we are not in a project-specific view then stay there
                    ( newOuterModel, Cmd.none )

                ProjectView _ _ _ ->
                    -- If we have any projects switch to the first one in the list, otherwise switch to login view
                    let
                        route =
                            case List.head newProjects of
                                Just p ->
                                    Route.ProjectRoute p.auth.project.uuid Route.AllResourcesList

                                Nothing ->
                                    Route.LoginPicker
                    in
                    ( newOuterModel, Route.pushUrl sharedModel.viewContext route )

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
                        |> mapToOuterMsg
                        |> mapToOuterModel outerModel

                Just server ->
                    processServerSpecificMsg outerModel project server serverMsgConstructor

        RequestServers ->
            ApiModelHelpers.requestServers project.auth.project.uuid sharedModel
                |> mapToOuterMsg
                |> mapToOuterModel outerModel

        RequestCreateServer pageModel networkUuid ->
            let
                customWorkFlowSource =
                    if pageModel.includeWorkflow && Maybe.withDefault False pageModel.workflowInputIsValid then
                        Just
                            { repository = pageModel.workflowInputRepository
                            , reference = pageModel.workflowInputReference
                            , path = pageModel.workflowInputPath
                            }

                    else
                        Nothing

                createServerRequest =
                    { name = pageModel.serverName
                    , count = pageModel.count
                    , imageUuid = pageModel.imageUuid
                    , flavorUuid = pageModel.flavorUuid
                    , volBackedSizeGb =
                        pageModel.volSizeTextInput
                            |> Maybe.andThen Style.Widgets.NumericTextInput.NumericTextInput.toMaybe
                    , networkUuid = networkUuid
                    , keypairName = pageModel.keypairName
                    , userData =
                        Helpers.renderUserDataTemplate
                            project
                            pageModel.userDataTemplate
                            pageModel.keypairName
                            (pageModel.deployGuacamole |> Maybe.withDefault False)
                            pageModel.deployDesktopEnvironment
                            customWorkFlowSource
                            pageModel.installOperatingSystemUpdates
                            sharedModel.instanceConfigMgtRepoUrl
                            sharedModel.instanceConfigMgtRepoCheckout
                    , metadata =
                        Helpers.newServerMetadata
                            currentExoServerVersion
                            sharedModel.clientUuid
                            (pageModel.deployGuacamole |> Maybe.withDefault False)
                            pageModel.deployDesktopEnvironment
                            project.auth.user.name
                            pageModel.floatingIpCreationOption
                            customWorkFlowSource
                    }
            in
            ( outerModel, Rest.Nova.requestCreateServer project createServerRequest )
                |> mapToOuterMsg

        RequestCreateVolume name size ->
            let
                createVolumeRequest =
                    { name = name
                    , size = size
                    }
            in
            ( outerModel, OSVolumes.requestCreateVolume project createVolumeRequest )
                |> mapToOuterMsg

        RequestDeleteVolume volumeUuid ->
            ( outerModel, OSVolumes.requestDeleteVolume project volumeUuid )
                |> mapToOuterMsg

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
                        |> mapToOuterMsg

                Nothing ->
                    State.Error.processStringError
                        sharedModel
                        (ErrorContext
                            ("look for server UUID with attached volume " ++ volumeUuid)
                            ErrorCrit
                            Nothing
                        )
                        "Could not determine server attached to this volume."
                        |> mapToOuterMsg
                        |> mapToOuterModel outerModel

        RequestDeleteFloatingIp floatingIpAddress ->
            ( outerModel, Rest.Neutron.requestDeleteFloatingIp project floatingIpAddress )
                |> mapToOuterMsg

        RequestAssignFloatingIp port_ floatingIpUuid ->
            let
                setViewCmd =
                    Route.pushUrl sharedModel.viewContext
                        (Route.ProjectRoute project.auth.project.uuid <| Route.FloatingIpList)
            in
            ( outerModel
            , Cmd.batch
                [ setViewCmd
                , Cmd.map SharedMsg <| Rest.Neutron.requestAssignFloatingIp project port_ floatingIpUuid
                ]
            )

        RequestUnassignFloatingIp floatingIpUuid ->
            ( outerModel, Rest.Neutron.requestUnassignFloatingIp project floatingIpUuid )
                |> mapToOuterMsg

        ReceiveImages images ->
            Rest.Glance.receiveImages sharedModel project images
                |> mapToOuterMsg
                |> mapToOuterModel outerModel

        RequestDeleteServers serverUuidsToDelete ->
            let
                applyDelete : OSTypes.ServerUuid -> ( Project, Cmd SharedMsg ) -> ( Project, Cmd SharedMsg )
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
                |> mapToOuterMsg
                |> mapToOuterModel outerModel

        ReceiveServers errorContext result ->
            case result of
                Ok servers ->
                    Rest.Nova.receiveServers sharedModel project servers
                        |> mapToOuterMsg
                        |> mapToOuterModel outerModel

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
                        |> mapToOuterMsg
                        |> mapToOuterModel outerModel

        ReceiveServer serverUuid errorContext result ->
            case result of
                Ok server ->
                    Rest.Nova.receiveServer sharedModel project server
                        |> mapToOuterMsg
                        |> mapToOuterModel outerModel

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
                                        |> mapToOuterMsg
                                        |> mapToOuterModel outerModel
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
                                    |> mapToOuterMsg
                                    |> mapToOuterModel outerModel

                            else
                                non404

                        _ ->
                            non404

        ReceiveFlavors flavors ->
            let
                -- If we are creating a server and no flavor is selected, select the smallest flavor
                maybeSmallestFlavor =
                    GetterSetters.sortedFlavors flavors |> List.head

                ( newOuterModel, newCmd ) =
                    Rest.Nova.receiveFlavors sharedModel project flavors
                        |> mapToOuterMsg
                        |> mapToOuterModel outerModel
            in
            case maybeSmallestFlavor of
                Just smallestFlavor ->
                    ( newOuterModel, newCmd )
                        |> pipelineCmdOuterModelMsg
                            (updateUnderlying (ServerCreateMsg <| Page.ServerCreate.GotDefaultFlavor smallestFlavor.uuid))

                Nothing ->
                    ( newOuterModel, newCmd )

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
                |> mapToOuterMsg
                |> mapToOuterModel outerModel

        ReceiveKeypairs keypairs ->
            Rest.Nova.receiveKeypairs sharedModel project keypairs
                |> mapToOuterMsg
                |> mapToOuterModel outerModel

        RequestCreateKeypair keypairName publicKey ->
            ( outerModel, Rest.Nova.requestCreateKeypair project keypairName publicKey )
                |> mapToOuterMsg

        ReceiveCreateKeypair keypair ->
            let
                newProject =
                    GetterSetters.projectUpdateKeypair project keypair

                newSharedModel =
                    GetterSetters.modelUpdateProject sharedModel newProject
            in
            ( { outerModel | sharedModel = newSharedModel }
            , Route.pushUrl sharedModel.viewContext (Route.ProjectRoute newProject.auth.project.uuid Route.KeypairList)
            )

        RequestDeleteKeypair keypairId ->
            ( outerModel, Rest.Nova.requestDeleteKeypair project keypairId )
                |> mapToOuterMsg

        ReceiveDeleteKeypair errorContext keypairName result ->
            case result of
                Err httpError ->
                    State.Error.processStringError sharedModel errorContext (Helpers.httpErrorToString httpError)
                        |> mapToOuterMsg
                        |> mapToOuterModel outerModel

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
                        |> mapToOuterModel outerModel

        ReceiveCreateServer _ ->
            let
                newRoute =
                    Route.ProjectRoute
                        project.auth.project.uuid
                        Route.AllResourcesList

                ( newSharedModel, newCmd ) =
                    ( sharedModel, Cmd.none )
                        |> Helpers.pipelineCmd
                            (ApiModelHelpers.requestServers project.auth.project.uuid)
                        |> Helpers.pipelineCmd
                            (ApiModelHelpers.requestNetworks project.auth.project.uuid)
                        |> Helpers.pipelineCmd
                            (ApiModelHelpers.requestPorts project.auth.project.uuid)
            in
            ( { outerModel | sharedModel = newSharedModel }
            , Cmd.batch
                [ Cmd.map SharedMsg newCmd
                , Route.pushUrl sharedModel.viewContext newRoute
                ]
            )

        ReceiveNetworks errorContext result ->
            case result of
                Ok networks ->
                    let
                        newProject =
                            Rest.Neutron.receiveNetworks sharedModel project networks

                        newSharedModel =
                            GetterSetters.modelUpdateProject sharedModel newProject
                    in
                    ( newSharedModel, Cmd.none )
                        |> mapToOuterMsg
                        |> mapToOuterModel outerModel
                        |> pipelineCmdOuterModelMsg (updateUnderlying (ServerCreateMsg <| Page.ServerCreate.GotNetworks))

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
                        |> mapToOuterMsg
                        |> mapToOuterModel outerModel

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

                ( newOuterModel, underlyingCmd ) =
                    case Helpers.newServerNetworkOptions newProject of
                        AutoSelectedNetwork netUuid ->
                            updateUnderlying
                                (ServerCreateMsg <| Page.ServerCreate.GotAutoAllocatedNetwork netUuid)
                                { outerModel | sharedModel = newNewSharedModel }

                        _ ->
                            ( { outerModel | sharedModel = newNewSharedModel }, Cmd.none )
            in
            ( newOuterModel, Cmd.batch [ Cmd.map SharedMsg newCmd, underlyingCmd ] )

        ReceiveFloatingIps ips ->
            Rest.Neutron.receiveFloatingIps sharedModel project ips
                |> mapToOuterMsg
                |> mapToOuterModel outerModel

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
                        |> mapToOuterModel outerModel

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
                        |> mapToOuterMsg
                        |> mapToOuterModel outerModel

        ReceiveDeleteFloatingIp uuid ->
            Rest.Neutron.receiveDeleteFloatingIp sharedModel project uuid
                |> mapToOuterMsg
                |> mapToOuterModel outerModel

        ReceiveAssignFloatingIp floatingIp ->
            -- TODO update servers so that new assignment is reflected in the UI
            let
                newProject =
                    processNewFloatingIp sharedModel.clientCurrentTime project floatingIp

                newSharedModel =
                    GetterSetters.modelUpdateProject sharedModel newProject
            in
            ( newSharedModel, Cmd.none )
                |> mapToOuterModel outerModel

        ReceiveUnassignFloatingIp floatingIp ->
            -- TODO update servers so that unassignment is reflected in the UI
            let
                newProject =
                    processNewFloatingIp sharedModel.clientCurrentTime project floatingIp
            in
            ( GetterSetters.modelUpdateProject sharedModel newProject, Cmd.none )
                |> mapToOuterModel outerModel

        ReceiveSecurityGroups groups ->
            Rest.Neutron.receiveSecurityGroupsAndEnsureExoGroup sharedModel project groups
                |> mapToOuterMsg
                |> mapToOuterModel outerModel

        ReceiveCreateExoSecurityGroup group ->
            Rest.Neutron.receiveCreateExoSecurityGroupAndRequestCreateRules sharedModel project group
                |> mapToOuterMsg
                |> mapToOuterModel outerModel

        ReceiveCreateVolume ->
            {- Should we add new volume to model now? -}
            ( outerModel
            , Route.pushUrl sharedModel.viewContext (Route.ProjectRoute project.auth.project.uuid Route.VolumeList)
            )

        ReceiveVolumes volumes ->
            let
                -- Look for any server backing volumes that were created with no name, and give them a reasonable name
                updateVolNameCmds : List (Cmd SharedMsg)
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
                |> mapToOuterMsg
                |> mapToOuterModel outerModel

        ReceiveDeleteVolume ->
            ( outerModel, OSVolumes.requestVolumes project )
                |> mapToOuterMsg

        ReceiveUpdateVolumeName ->
            ( outerModel, OSVolumes.requestVolumes project )
                |> mapToOuterMsg

        ReceiveAttachVolume attachment ->
            ( outerModel
            , Route.pushUrl sharedModel.viewContext (Route.ProjectRoute project.auth.project.uuid <| Route.VolumeMountInstructions attachment)
            )

        ReceiveDetachVolume ->
            ( outerModel
            , Route.pushUrl sharedModel.viewContext (Route.ProjectRoute project.auth.project.uuid Route.VolumeList)
            )

        ReceiveAppCredential appCredential ->
            let
                newProject =
                    { project | secret = ApplicationCredential appCredential }
            in
            ( GetterSetters.modelUpdateProject sharedModel newProject, Cmd.none )
                |> mapToOuterModel outerModel

        ReceiveComputeQuota quota ->
            let
                newProject =
                    { project | computeQuota = RemoteData.Success quota }
            in
            ( GetterSetters.modelUpdateProject sharedModel newProject, Cmd.none )
                |> mapToOuterModel outerModel

        ReceiveVolumeQuota quota ->
            let
                newProject =
                    { project | volumeQuota = RemoteData.Success quota }
            in
            ( GetterSetters.modelUpdateProject sharedModel newProject, Cmd.none )
                |> mapToOuterModel outerModel

        ReceiveRandomServerName serverName ->
            updateUnderlying (ServerCreateMsg <| Page.ServerCreate.GotServerName serverName) outerModel


processServerSpecificMsg : OuterModel -> Project -> Server -> ServerSpecificMsgConstructor -> ( OuterModel, Cmd OuterMsg )
processServerSpecificMsg outerModel project server serverMsgConstructor =
    let
        sharedModel =
            outerModel.sharedModel
    in
    case serverMsgConstructor of
        RequestServer ->
            ApiModelHelpers.requestServer project.auth.project.uuid server.osProps.uuid sharedModel
                |> mapToOuterMsg
                |> mapToOuterModel outerModel

        RequestDeleteServer retainFloatingIps ->
            let
                ( newProject, cmd ) =
                    requestDeleteServer project server.osProps.uuid retainFloatingIps
            in
            ( GetterSetters.modelUpdateProject sharedModel newProject, cmd )
                |> mapToOuterMsg
                |> mapToOuterModel outerModel

        RequestAttachVolume volumeUuid ->
            ( outerModel, OSSvrVols.requestAttachVolume project server.osProps.uuid volumeUuid )
                |> mapToOuterMsg

        RequestCreateServerImage imageName ->
            let
                newRoute =
                    Route.ProjectRoute
                        project.auth.project.uuid
                        Route.AllResourcesList

                createImageCmd =
                    Rest.Nova.requestCreateServerImage project server.osProps.uuid imageName
            in
            ( outerModel
            , Cmd.batch
                [ Cmd.map SharedMsg createImageCmd
                , Route.pushUrl sharedModel.viewContext newRoute
                ]
            )

        RequestSetServerName newServerName ->
            ( outerModel, Rest.Nova.requestSetServerName project server.osProps.uuid newServerName )
                |> mapToOuterMsg

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
                        |> mapToOuterModel outerModel

                Err _ ->
                    -- Dropping this on the floor for now, someday we may want to do something different
                    ( outerModel, Cmd.none )

        ReceiveConsoleUrl url ->
            Rest.Nova.receiveConsoleUrl sharedModel project server url
                |> mapToOuterMsg
                |> mapToOuterModel outerModel

        ReceiveDeleteServer ->
            let
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

                newOuterModel =
                    { outerModel | sharedModel = GetterSetters.modelUpdateProject sharedModel newProject }
            in
            ( newOuterModel
            , case outerModel.viewState of
                ProjectView projectId _ (ServerDetail pageModel) ->
                    if pageModel.serverUuid == server.osProps.uuid then
                        Route.pushUrl sharedModel.viewContext (Route.ProjectRoute projectId Route.AllResourcesList)

                    else
                        Cmd.none

                _ ->
                    Cmd.none
            )

        ReceiveCreateFloatingIp errorContext result ->
            case result of
                Ok ip ->
                    Rest.Neutron.receiveCreateFloatingIp sharedModel project server ip
                        |> mapToOuterMsg
                        |> mapToOuterModel outerModel

                Err httpErrorWithBody ->
                    let
                        newErrorContext =
                            if GetterSetters.serverPresentNotDeleting sharedModel server.osProps.uuid then
                                errorContext

                            else
                                { errorContext | level = ErrorDebug }
                    in
                    State.Error.processSynchronousApiError sharedModel newErrorContext httpErrorWithBody
                        |> mapToOuterMsg
                        |> mapToOuterModel outerModel

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
                    |> mapToOuterMsg

        ReceiveSetServerName _ errorContext result ->
            case result of
                Err e ->
                    State.Error.processSynchronousApiError sharedModel errorContext e
                        |> mapToOuterMsg
                        |> mapToOuterModel outerModel

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

                        newOuterModel =
                            { outerModel | sharedModel = GetterSetters.modelUpdateProject sharedModel newProject }

                        -- Later, maybe: Check that newServerName == actualNewServerName
                    in
                    updateUnderlying (ServerDetailMsg <| Page.ServerDetail.GotServerNamePendingConfirmation Nothing)
                        newOuterModel

        ReceiveSetServerMetadata intendedMetadataItem errorContext result ->
            case result of
                Err e ->
                    -- Error from API
                    State.Error.processSynchronousApiError sharedModel errorContext e
                        |> mapToOuterMsg
                        |> mapToOuterModel outerModel

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
                            |> mapToOuterMsg
                            |> mapToOuterModel outerModel

                    else
                        -- This is bonkers, throw an error
                        State.Error.processStringError
                            sharedModel
                            errorContext
                            "The metadata items returned by OpenStack did not include the metadata item that we tried to set."
                            |> mapToOuterMsg
                            |> mapToOuterModel outerModel

        ReceiveDeleteServerMetadata metadataKey errorContext result ->
            case result of
                Err e ->
                    -- Error from API
                    State.Error.processSynchronousApiError sharedModel errorContext e
                        |> mapToOuterMsg
                        |> mapToOuterModel outerModel

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
                        |> mapToOuterModel outerModel

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
                                |> mapToOuterMsg
                                |> mapToOuterModel outerModel

                        GuacTypes.NotLaunchedWithGuacamole ->
                            State.Error.processStringError
                                sharedModel
                                errorContext
                                "Server does not appear to have been launched with Guacamole support"
                                |> mapToOuterMsg
                                |> mapToOuterModel outerModel

                ServerNotFromExo ->
                    State.Error.processStringError
                        sharedModel
                        errorContext
                        "Server does not appear to have been launched from Exosphere"
                        |> mapToOuterMsg
                        |> mapToOuterModel outerModel

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
            ( newSharedModel, func newProject.endpoints.nova )
                |> mapToOuterMsg
                |> mapToOuterModel outerModel

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

                        newWorkflowToken : CustomWorkflow -> CustomWorkflowTokenRDPP
                        newWorkflowToken currentWorkflow =
                            case result of
                                Err httpError ->
                                    RDPP.RemoteDataPlusPlus
                                        RDPP.DontHave
                                        (RDPP.NotLoading (Just ( httpError, sharedModel.clientCurrentTime )))

                                Ok consoleLog ->
                                    let
                                        maybeParsedToken =
                                            Helpers.parseConsoleLogForWorkflowToken consoleLog
                                    in
                                    case maybeParsedToken of
                                        Just parsedToken ->
                                            RDPP.RemoteDataPlusPlus
                                                (RDPP.DoHave
                                                    parsedToken
                                                    sharedModel.clientCurrentTime
                                                )
                                                (RDPP.NotLoading Nothing)

                                        Nothing ->
                                            currentWorkflow.authToken

                        newWorkflowStatus : ServerCustomWorkflowStatus
                        newWorkflowStatus =
                            case exoOriginProps.customWorkflowStatus of
                                NotLaunchedWithCustomWorkflow ->
                                    exoOriginProps.customWorkflowStatus

                                LaunchedWithCustomWorkflow customWorkflow ->
                                    LaunchedWithCustomWorkflow { customWorkflow | authToken = newWorkflowToken customWorkflow }

                        newOriginProps =
                            { exoOriginProps
                                | resourceUsage = newResourceUsage
                                , exoSetupStatus = newExoSetupStatusRDPP
                                , customWorkflowStatus = newWorkflowStatus
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
                                |> mapToOuterMsg
                                |> mapToOuterModel outerModel

                        Ok _ ->
                            ( newSharedModel, exoSetupStatusMetadataCmd )
                                |> mapToOuterMsg
                                |> mapToOuterModel outerModel


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


createProject : OuterModel -> OSTypes.ScopedAuthToken -> Endpoints -> ( OuterModel, Cmd OuterMsg )
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
            }

        newProjects =
            newProject :: outerModel.sharedModel.projects

        newViewStateCmd =
            -- If the user is selecting projects from an unscoped provider then don't interrupt them
            case outerModel.viewState of
                NonProjectView (SelectProjects _) ->
                    Cmd.none

                _ ->
                    Route.pushUrl sharedModel.viewContext <|
                        Route.ProjectRoute newProject.auth.project.uuid Route.AllResourcesList

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
    in
    ( { outerModel | sharedModel = newSharedModel }
    , Cmd.batch [ newViewStateCmd, Cmd.map SharedMsg newCmd ]
    )


createUnscopedProvider : SharedModel -> OSTypes.UnscopedAuthToken -> HelperTypes.Url -> ( SharedModel, Cmd SharedMsg )
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


requestDeleteServer : Project -> OSTypes.ServerUuid -> Bool -> ( Project, Cmd SharedMsg )
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
