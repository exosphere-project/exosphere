module State.State exposing (update)

import Browser
import Browser.Navigation
import Helpers.ExoSetupStatus
import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers as Helpers
import Helpers.RemoteDataPlusPlus as RDPP
import Helpers.ServerResourceUsage
import Http
import List.Extra
import LocalStorage.LocalStorage as LocalStorage
import Maybe
import OpenStack.ServerPassword as OSServerPassword
import OpenStack.ServerTags as OSServerTags
import OpenStack.ServerVolumes as OSSvrVols
import OpenStack.Types as OSTypes
import OpenStack.Volumes as OSVolumes
import Orchestration.Orchestration as Orchestration
import Page.FloatingIpAssign
import Page.FloatingIpList
import Page.GetSupport
import Page.Home
import Page.ImageList
import Page.InstanceSourcePicker
import Page.KeypairCreate
import Page.KeypairList
import Page.LoginOpenstack
import Page.LoginPicker
import Page.MessageLog
import Page.ProjectOverview
import Page.SelectProjectRegions
import Page.SelectProjects
import Page.ServerCreate
import Page.ServerCreateImage
import Page.ServerDetail
import Page.ServerList
import Page.ServerResize
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
import Style.Widgets.NumericTextInput.NumericTextInput
import Style.Widgets.Toast as Toast
import Task
import Time
import Types.Error as Error exposing (AppError, ErrorContext, ErrorLevel(..))
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


update : OuterMsg -> Result AppError OuterModel -> ( Result AppError OuterModel, Cmd OuterMsg )
update msg result =
    case result of
        Err appError ->
            ( Err appError, Cmd.none )

        Ok model ->
            case updateValid msg model of
                ( newModel, nextMsg ) ->
                    ( Ok newModel, nextMsg )


updateValid : OuterMsg -> OuterModel -> ( OuterModel, Cmd OuterMsg )
updateValid msg outerModel =
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
            , Cmd.map GetSupportMsg cmd
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
            , Cmd.map HomeMsg cmd
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
            , Cmd.map LoginOpenstackMsg cmd
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
            , Cmd.map LoginPickerMsg cmd
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
            , Cmd.map MessageLogMsg cmd
            )
                |> pipelineCmdOuterModelMsg
                    (processSharedMsg sharedMsg)

        ( SelectProjectRegionsMsg pageMsg, NonProjectView (SelectProjectRegions pageModel) ) ->
            let
                ( newPageModel, cmd, sharedMsg ) =
                    Page.SelectProjectRegions.update pageMsg sharedModel pageModel
            in
            ( { outerModel
                | viewState = NonProjectView <| SelectProjectRegions newPageModel
              }
            , Cmd.map SelectProjectRegionsMsg cmd
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
            , Cmd.map SelectProjectsMsg cmd
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
            , Cmd.map SettingsMsg cmd
            )
                |> pipelineCmdOuterModelMsg
                    (processSharedMsg sharedMsg)

        ( pageSpecificMsg, ProjectView projectId projectViewConstructor ) ->
            case GetterSetters.projectLookup sharedModel projectId of
                Just project ->
                    case ( pageSpecificMsg, projectViewConstructor ) of
                        -- This is the repetitive dispatch code that people warn you about when you use the nested Elm architecture.
                        -- Maybe there is a way to make it less repetitive (or at least more compact) in the future.
                        ( ProjectOverviewMsg pageMsg, ProjectOverview pageModel ) ->
                            let
                                ( newSharedModel, cmd, sharedMsg ) =
                                    Page.ProjectOverview.update pageMsg project pageModel
                            in
                            ( { outerModel
                                | viewState =
                                    ProjectView projectId <|
                                        ProjectOverview newSharedModel
                              }
                            , Cmd.map ProjectOverviewMsg cmd
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
                                    ProjectView projectId <|
                                        FloatingIpAssign newSharedModel
                              }
                            , Cmd.map FloatingIpAssignMsg cmd
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
                                    ProjectView projectId <|
                                        FloatingIpList newSharedModel
                              }
                            , Cmd.map FloatingIpListMsg cmd
                            )
                                |> pipelineCmdOuterModelMsg
                                    (processSharedMsg sharedMsg)

                        ( ImageListMsg pageMsg, ImageList pageModel ) ->
                            let
                                ( newSharedModel, cmd, sharedMsg ) =
                                    Page.ImageList.update pageMsg project pageModel
                            in
                            ( { outerModel
                                | viewState =
                                    ProjectView projectId <|
                                        ImageList newSharedModel
                              }
                            , Cmd.map ImageListMsg cmd
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
                                    ProjectView projectId <|
                                        InstanceSourcePicker newSharedModel
                              }
                            , Cmd.map InstanceSourcePickerMsg cmd
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
                                    ProjectView projectId <|
                                        KeypairCreate newSharedModel
                              }
                            , Cmd.map KeypairCreateMsg cmd
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
                                    ProjectView projectId <|
                                        KeypairList newSharedModel
                              }
                            , Cmd.map KeypairListMsg cmd
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
                                    ProjectView projectId <|
                                        ServerCreate newSharedModel
                              }
                            , Cmd.map ServerCreateMsg cmd
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
                                    ProjectView projectId <|
                                        ServerCreateImage newSharedModel
                              }
                            , Cmd.map ServerCreateImageMsg cmd
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
                                    ProjectView projectId <|
                                        ServerDetail newSharedModel
                              }
                            , Cmd.map ServerDetailMsg cmd
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
                                    ProjectView projectId <|
                                        ServerList newSharedModel
                              }
                            , Cmd.map ServerListMsg cmd
                            )
                                |> pipelineCmdOuterModelMsg
                                    (processSharedMsg sharedMsg)

                        ( ServerResizeMsg pageMsg, ServerResize pageModel ) ->
                            let
                                ( newSharedModel, cmd, sharedMsg ) =
                                    Page.ServerResize.update pageMsg sharedModel project pageModel
                            in
                            ( { outerModel
                                | viewState =
                                    ProjectView projectId <|
                                        ServerResize newSharedModel
                              }
                            , Cmd.map ServerResizeMsg cmd
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
                                    ProjectView projectId <|
                                        VolumeAttach newSharedModel
                              }
                            , Cmd.map VolumeAttachMsg cmd
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
                                    ProjectView projectId <|
                                        VolumeCreate newSharedModel
                              }
                            , Cmd.map VolumeCreateMsg cmd
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
                                    ProjectView projectId <|
                                        VolumeDetail newSharedModel
                              }
                            , Cmd.map VolumeDetailMsg cmd
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
                                    ProjectView projectId <|
                                        VolumeList newSharedModel
                              }
                            , Cmd.map VolumeListMsg cmd
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
        ChangeSystemThemePreference preference ->
            let
                { style } =
                    sharedModel

                { styleMode } =
                    style

                newStyleMode =
                    { styleMode | systemPreference = Just preference }

                newStyle =
                    { style | styleMode = newStyleMode }

                newPalette =
                    toExoPalette newStyle

                newViewContext =
                    { viewContext | palette = newPalette }
            in
            mapToOuterModel outerModel
                ( { sharedModel
                    | style = newStyle
                    , viewContext = newViewContext
                  }
                , Cmd.none
                )

        Logout ->
            ( outerModel, Ports.logout () )

        ToastMsg subMsg ->
            Toast.update ToastMsg subMsg outerModel.sharedModel
                |> mapToOuterMsg
                |> mapToOuterModel outerModel

        NetworkConnection online ->
            Toast.showToast (Toast.makeNetworkConnectivityToast online) ToastMsg ( sharedModel, Cmd.none )
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

        Jetstream1Login jetstream1Creds ->
            let
                openstackCredsList =
                    State.Auth.jetstream1ToOpenstackCreds jetstream1Creds

                cmds =
                    List.map
                        (\creds -> Rest.Keystone.requestUnscopedAuthToken sharedModel.cloudCorsProxyUrl creds)
                        openstackCredsList
            in
            ( outerModel, Cmd.batch cmds )
                |> mapToOuterMsg

        ReceiveProjectScopedToken keystoneUrl ( metadata, response ) ->
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
                    let
                        updateTokenForExistingProjects : OuterModel -> ( OuterModel, Cmd OuterMsg )
                        updateTokenForExistingProjects outerModel_ =
                            sharedModel.projects
                                |> List.filter (\p -> p.auth.project.uuid == authToken.project.uuid)
                                |> List.map
                                    (\p ->
                                        \outerModel__ ->
                                            State.Auth.projectUpdateAuthToken outerModel__ p authToken
                                                |> mapToOuterMsg
                                    )
                                |> List.foldl pipelineCmdOuterModelMsg ( outerModel_, Cmd.none )

                        handleCaseOfNewProject : OuterModel -> ( OuterModel, Cmd OuterMsg )
                        handleCaseOfNewProject outerModel_ =
                            -- If there is an unscoped provider project in the model, we either create the full project right away (if there is only one region) or direct user to the SelectProjectRegions page
                            case
                                GetterSetters.unscopedProviderLookup outerModel_.sharedModel keystoneUrl
                                    |> Maybe.andThen (\provider -> GetterSetters.unscopedProjectLookup provider authToken.project.uuid)
                            of
                                Nothing ->
                                    ( outerModel_, Cmd.none )

                                Just unscopedProject ->
                                    case GetterSetters.getCatalogRegionIds authToken.catalog of
                                        [] ->
                                            State.Error.processStringError
                                                outerModel_.sharedModel
                                                (ErrorContext
                                                    "Get project regions"
                                                    ErrorCrit
                                                    (Just "Please check with your cloud administrator or the Exosphere developers.")
                                                )
                                                "Could not find any endpoints with a region ID."
                                                |> mapToOuterMsg
                                                |> mapToOuterModel outerModel_

                                        [ singleRegionId ] ->
                                            -- Only one region in the catalog so create the project right now
                                            let
                                                navigateToCorrectPage : OuterModel -> ( OuterModel, Cmd OuterMsg )
                                                navigateToCorrectPage outerModel__ =
                                                    -- send user to Home page, except if there are unscoped providers remaining to choose projects for, then send user to SelectProjects page
                                                    -- This can likely go away after Jetstream1 is decommissioned.
                                                    case outerModel__.sharedModel.unscopedProviders of
                                                        [] ->
                                                            ( outerModel__
                                                            , case outerModel__.viewState of
                                                                NonProjectView (Home _) ->
                                                                    Cmd.none

                                                                _ ->
                                                                    Route.pushUrl outerModel__.sharedModel.viewContext Route.Home
                                                            )

                                                        firstUnscopedProvider :: _ ->
                                                            ( outerModel__
                                                            , Route.pushUrl
                                                                outerModel__.sharedModel.viewContext
                                                                (Route.SelectProjects firstUnscopedProvider.authUrl)
                                                            )
                                            in
                                            createProject keystoneUrl authToken singleRegionId outerModel_
                                                |> pipelineCmdOuterModelMsg
                                                    (removeUnscopedProject
                                                        keystoneUrl
                                                        unscopedProject.project.uuid
                                                    )
                                                |> pipelineCmdOuterModelMsg navigateToCorrectPage

                                        _ ->
                                            -- Multiple regions, ask the user to choose from among them
                                            let
                                                newTokens =
                                                    authToken :: outerModel_.sharedModel.scopedAuthTokensWaitingRegionSelection

                                                oldSharedmodel =
                                                    outerModel_.sharedModel

                                                newModel =
                                                    { oldSharedmodel | scopedAuthTokensWaitingRegionSelection = newTokens }
                                            in
                                            ( newModel
                                            , Route.pushUrl
                                                outerModel_.sharedModel.viewContext
                                                (Route.SelectProjectRegions keystoneUrl authToken.project.uuid)
                                            )
                                                |> mapToOuterMsg
                                                |> mapToOuterModel outerModel_
                    in
                    ( outerModel, Cmd.none )
                        |> pipelineCmdOuterModelMsg updateTokenForExistingProjects
                        |> pipelineCmdOuterModelMsg handleCaseOfNewProject

        CreateProjectsFromRegionSelections keystoneUrl projectUuid regionIds ->
            let
                dealWithNextProject : OuterModel -> ( OuterModel, Cmd OuterMsg )
                dealWithNextProject outerModel_ =
                    let
                        maybeNextUnscopedProject =
                            GetterSetters.unscopedProviderLookup outerModel_.sharedModel keystoneUrl
                                |> Maybe.map .projectsAvailable
                                |> Maybe.map (RemoteData.withDefault [])
                                |> Maybe.andThen List.head
                    in
                    case maybeNextUnscopedProject of
                        Just nextUnscopedProject ->
                            -- user to choose regions for next unscoped project
                            ( outerModel_
                            , Route.pushUrl
                                outerModel_.sharedModel.viewContext
                                (Route.SelectProjectRegions keystoneUrl nextUnscopedProject.project.uuid)
                            )

                        Nothing ->
                            -- no more unscoped projects to choose regions for, go to home page
                            ( outerModel_, Route.pushUrl outerModel_.sharedModel.viewContext Route.Home )
                                |> pipelineCmdOuterModelMsg (removeScopedAuthTokenWaitingRegionSelection projectUuid)
            in
            case
                List.Extra.find
                    (\token -> token.project.uuid == projectUuid)
                    sharedModel.scopedAuthTokensWaitingRegionSelection
            of
                Just authToken ->
                    regionIds
                        |> List.map (createProject keystoneUrl authToken)
                        |> List.foldl pipelineCmdOuterModelMsg ( outerModel, Cmd.none )
                        |> pipelineCmdOuterModelMsg (removeUnscopedProject keystoneUrl projectUuid)
                        |> pipelineCmdOuterModelMsg dealWithNextProject

                Nothing ->
                    -- Could not find auth token, nothing to do
                    ( outerModel, Cmd.none )

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
                        GetterSetters.unscopedProviderLookup sharedModel keystoneUrl
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
                GetterSetters.unscopedProviderLookup sharedModel keystoneUrl
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

        ReceiveUnscopedRegions keystoneUrl unscopedRegions ->
            case
                GetterSetters.unscopedProviderLookup sharedModel keystoneUrl
            of
                Just provider ->
                    let
                        newProvider =
                            { provider | regionsAvailable = RemoteData.Success unscopedRegions }

                        newSharedModel =
                            GetterSetters.modelUpdateUnscopedProvider sharedModel newProvider

                        newOuterModel =
                            { outerModel | sharedModel = newSharedModel }
                    in
                    ( newOuterModel, Cmd.none )

                Nothing ->
                    -- Provider not found, may have been removed, nothing to do
                    ( outerModel, Cmd.none )

        RequestProjectScopedToken keystoneUrl selectedUnscopedProjects ->
            case GetterSetters.unscopedProviderLookup sharedModel keystoneUrl of
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

                        loginRequests =
                            List.map buildLoginRequest selectedUnscopedProjects
                                |> Cmd.batch

                        selectedProjectUuids =
                            List.map (\p -> p.project.uuid) selectedUnscopedProjects

                        notSelectedProjectUuids =
                            provider.projectsAvailable
                                |> RemoteData.withDefault []
                                |> List.map (\p -> p.project.uuid)
                                |> List.filter (\uuid -> not (List.member uuid selectedProjectUuids))

                        removeNotSelectedUnscopedProjects : OuterModel -> ( OuterModel, Cmd OuterMsg )
                        removeNotSelectedUnscopedProjects outerModel_ =
                            notSelectedProjectUuids
                                |> List.map (removeUnscopedProject keystoneUrl)
                                |> List.foldl pipelineCmdOuterModelMsg ( outerModel_, Cmd.none )
                    in
                    ( outerModel
                    , Cmd.map SharedMsg loginRequests
                    )
                        |> pipelineCmdOuterModelMsg removeNotSelectedUnscopedProjects

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

        SelectTheme themeChoice ->
            let
                oldStyle =
                    sharedModel.style

                oldStyleMode =
                    oldStyle.styleMode

                newStyleMode =
                    { oldStyleMode | theme = themeChoice }

                newStyle =
                    { oldStyle | styleMode = newStyleMode }
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

        TogglePopover popoverId ->
            ( { sharedModel
                | viewContext =
                    { viewContext
                        | showPopovers =
                            if Set.member popoverId sharedModel.viewContext.showPopovers then
                                Set.remove popoverId sharedModel.viewContext.showPopovers

                            else
                                Set.insert popoverId sharedModel.viewContext.showPopovers
                    }
              }
            , Cmd.none
            )
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
                Task.perform DoOrchestration Time.now

            else
                Cmd.none

        ( viewDependentModel, viewDependentCmd ) =
            {- TODO move some of this to Orchestration? -}
            case outerModel.viewState of
                NonProjectView _ ->
                    ( outerModel.sharedModel, Cmd.none )

                ProjectView projectName projectViewState ->
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
                                ProjectOverview _ ->
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
                        ( GetterSetters.projectIdentifier project, requestNeedingToken )
                            :: outerModel.pendingCredentialedRequests

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

        RemoveProject ->
            let
                newProjects =
                    List.filter (\p -> p.auth.project.uuid /= project.auth.project.uuid || p.region /= project.region) sharedModel.projects

                newOuterModel =
                    { outerModel | sharedModel = { sharedModel | projects = newProjects } }

                cmd =
                    -- if we are in a view specific to this project then navigate to the home page
                    case outerModel.viewState of
                        ProjectView projectId _ ->
                            if projectId == GetterSetters.projectIdentifier project then
                                Route.pushUrl sharedModel.viewContext Route.Home

                            else
                                Cmd.none

                        _ ->
                            Cmd.none
            in
            ( newOuterModel, cmd )

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
            ApiModelHelpers.requestServers (GetterSetters.projectIdentifier project) sharedModel
                |> mapToOuterMsg
                |> mapToOuterModel outerModel

        RequestCreateServer pageModel networkUuid flavorId ->
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
                    , flavorId = flavorId
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
                            pageModel.createCluster
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

        RequestDeleteFloatingIp errorContext floatingIpAddress ->
            ( outerModel, Rest.Neutron.requestDeleteFloatingIp project errorContext floatingIpAddress )
                |> mapToOuterMsg

        RequestAssignFloatingIp port_ floatingIpUuid ->
            let
                setViewCmd =
                    Route.pushUrl sharedModel.viewContext
                        (Route.ProjectRoute (GetterSetters.projectIdentifier project) <| Route.FloatingIpList)
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
                ( newOuterModel, newCmd ) =
                    Rest.Nova.receiveFlavors sharedModel project flavors
                        |> mapToOuterMsg
                        |> mapToOuterModel outerModel
            in
            ( newOuterModel, newCmd )
                |> pipelineCmdOuterModelMsg
                    (updateUnderlying (ServerCreateMsg <| Page.ServerCreate.GotFlavorList))

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
            , Route.pushUrl sharedModel.viewContext (Route.ProjectRoute (GetterSetters.projectIdentifier project) Route.KeypairList)
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

        ReceiveCreateServer errorContext result ->
            case result of
                Ok _ ->
                    let
                        newRoute =
                            Route.ProjectRoute
                                (GetterSetters.projectIdentifier project)
                                Route.ProjectOverview

                        ( newSharedModel, newCmd ) =
                            ( sharedModel, Cmd.none )
                                |> Helpers.pipelineCmd
                                    (ApiModelHelpers.requestServers (GetterSetters.projectIdentifier project))
                                |> Helpers.pipelineCmd
                                    (ApiModelHelpers.requestNetworks (GetterSetters.projectIdentifier project))
                                |> Helpers.pipelineCmd
                                    (ApiModelHelpers.requestPorts (GetterSetters.projectIdentifier project))
                    in
                    ( { outerModel | sharedModel = newSharedModel }
                    , Cmd.batch
                        [ Cmd.map SharedMsg newCmd
                        , Route.pushUrl sharedModel.viewContext newRoute
                        ]
                    )

                Err httpError ->
                    let
                        newOuterModel =
                            case outerModel.viewState of
                                ProjectView projectId (ServerCreate serverCreateModel) ->
                                    let
                                        newViewState =
                                            ProjectView
                                                projectId
                                                (ServerCreate { serverCreateModel | createServerAttempted = False })
                                    in
                                    { outerModel | viewState = newViewState }

                                _ ->
                                    outerModel
                    in
                    State.Error.processStringError sharedModel errorContext httpError.body
                        |> mapToOuterMsg
                        |> mapToOuterModel newOuterModel

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
                            ApiModelHelpers.requestNetworks (GetterSetters.projectIdentifier project) newSharedModel

                        Err httpError ->
                            State.Error.processSynchronousApiError newSharedModel errorContext httpError
                                |> Helpers.pipelineCmd (ApiModelHelpers.requestNetworks (GetterSetters.projectIdentifier project))

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
            , Route.pushUrl sharedModel.viewContext (Route.ProjectRoute (GetterSetters.projectIdentifier project) Route.VolumeList)
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
                                , GetterSetters.getBootVolume
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
                                    |> Maybe.withDefault ""
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
            , Route.pushUrl sharedModel.viewContext (Route.ProjectRoute (GetterSetters.projectIdentifier project) <| Route.VolumeMountInstructions attachment)
            )

        ReceiveDetachVolume ->
            ( outerModel
            , Route.pushUrl sharedModel.viewContext (Route.ProjectRoute (GetterSetters.projectIdentifier project) Route.VolumeList)
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

        ReceiveNetworkQuota quota ->
            let
                newProject =
                    { project | networkQuota = RemoteData.Success quota }
            in
            ( GetterSetters.modelUpdateProject sharedModel newProject, Cmd.none )
                |> mapToOuterModel outerModel

        ReceiveRandomServerName serverName ->
            updateUnderlying (ServerCreateMsg <| Page.ServerCreate.GotRandomServerName serverName) outerModel

        RequestDeleteImage imageUuid ->
            ( outerModel, Rest.Glance.requestDeleteImage project imageUuid )
                |> mapToOuterMsg

        ReceiveDeleteImage imageUuid ->
            Rest.Glance.receiveDeleteImage sharedModel project imageUuid
                |> mapToOuterMsg
                |> mapToOuterModel outerModel

        ReceiveJetstream2Allocation result ->
            case result of
                Ok allocation ->
                    let
                        newProject =
                            { project
                                | jetstream2Allocation =
                                    RDPP.RemoteDataPlusPlus
                                        (RDPP.DoHave allocation sharedModel.clientCurrentTime)
                                        (RDPP.NotLoading Nothing)
                            }
                    in
                    ( GetterSetters.modelUpdateProject sharedModel newProject, Cmd.none )
                        |> mapToOuterModel outerModel

                Err httpError ->
                    let
                        oldAllocationData =
                            project.jetstream2Allocation.data

                        newProject =
                            { project
                                | jetstream2Allocation =
                                    RDPP.RemoteDataPlusPlus oldAllocationData
                                        (RDPP.NotLoading (Just ( httpError, sharedModel.clientCurrentTime )))
                            }

                        newModel =
                            GetterSetters.modelUpdateProject sharedModel newProject

                        errorContext =
                            ErrorContext
                                "Receive Jetstream2 allocation information"
                                ErrorCrit
                                (Just "Please open a ticket with Jetstream2 support.")
                    in
                    State.Error.processSynchronousApiError newModel errorContext httpError
                        |> mapToOuterMsg
                        |> mapToOuterModel outerModel


processServerSpecificMsg : OuterModel -> Project -> Server -> ServerSpecificMsgConstructor -> ( OuterModel, Cmd OuterMsg )
processServerSpecificMsg outerModel project server serverMsgConstructor =
    let
        sharedModel =
            outerModel.sharedModel
    in
    case serverMsgConstructor of
        RequestServer ->
            ApiModelHelpers.requestServer (GetterSetters.projectIdentifier project) server.osProps.uuid sharedModel
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
                        (GetterSetters.projectIdentifier project)
                        Route.ProjectOverview

                createImageCmd =
                    Rest.Nova.requestCreateServerImage project server.osProps.uuid imageName
            in
            ( outerModel
            , Cmd.batch
                [ Cmd.map SharedMsg createImageCmd
                , Route.pushUrl sharedModel.viewContext newRoute
                ]
            )

        RequestResizeServer flavorId ->
            let
                oldExoProps =
                    server.exoProps

                newServer =
                    Server server.osProps { oldExoProps | targetOpenstackStatus = Just [ OSTypes.ServerResize ] } server.events

                newProject =
                    GetterSetters.projectUpdateServer project newServer

                newSharedModel =
                    GetterSetters.modelUpdateProject sharedModel newProject
            in
            ( { outerModel | sharedModel = newSharedModel }
            , Rest.Nova.requestServerResize project server.osProps.uuid flavorId
            )
                |> mapToOuterMsg

        RequestSetServerName newServerName ->
            ( outerModel, Rest.Nova.requestSetServerName project server.osProps.uuid newServerName )
                |> mapToOuterMsg

        ReceiveServerAction ->
            ApiModelHelpers.requestServer (GetterSetters.projectIdentifier project) server.osProps.uuid sharedModel
                |> Helpers.pipelineCmd (ApiModelHelpers.requestServerEvents (GetterSetters.projectIdentifier project) server.osProps.uuid)
                |> mapToOuterMsg
                |> mapToOuterModel outerModel

        ReceiveServerEvents _ result ->
            case result of
                Ok serverEvents ->
                    let
                        newServer =
                            { server
                                | events =
                                    RDPP.RemoteDataPlusPlus
                                        (RDPP.DoHave serverEvents sharedModel.clientCurrentTime)
                                        (RDPP.NotLoading Nothing)
                            }

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
                ProjectView projectId (ServerDetail pageModel) ->
                    if pageModel.serverUuid == server.osProps.uuid then
                        Route.pushUrl sharedModel.viewContext (Route.ProjectRoute projectId Route.ProjectOverview)

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

        ReceiveServerPassphrase passphrase ->
            if String.isEmpty passphrase then
                ( outerModel, Cmd.none )

            else
                let
                    tag =
                        "exoPw:" ++ passphrase

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
                                            if
                                                List.member server.osProps.details.openstackStatus
                                                    [ OSTypes.ServerActive, OSTypes.ServerVerifyResize ]
                                            then
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
                        ( oldExoSetupStatus, oldTimestamp ) =
                            case exoOriginProps.exoSetupStatus.data of
                                RDPP.DontHave ->
                                    ( ExoSetupUnknown, Nothing )

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
                                        ( newExoSetupStatus, newTimestamp ) =
                                            Helpers.ExoSetupStatus.parseConsoleLogExoSetupStatus
                                                ( oldExoSetupStatus, oldTimestamp )
                                                consoleLog
                                                server.osProps.details.created
                                                sharedModel.clientCurrentTime

                                        cmd =
                                            if newExoSetupStatus == oldExoSetupStatus then
                                                Cmd.none

                                            else
                                                let
                                                    metadataItem =
                                                        OSTypes.MetadataItem
                                                            "exoSetup"
                                                            (Helpers.ExoSetupStatus.encodeMetadataItem newExoSetupStatus newTimestamp)
                                                in
                                                Rest.Nova.requestSetServerMetadata project server.osProps.uuid metadataItem
                                    in
                                    ( RDPP.RemoteDataPlusPlus
                                        (RDPP.DoHave
                                            ( newExoSetupStatus, newTimestamp )
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


removeScopedAuthTokenWaitingRegionSelection : OSTypes.ProjectUuid -> OuterModel -> ( OuterModel, Cmd OuterMsg )
removeScopedAuthTokenWaitingRegionSelection projectUuid outerModel =
    let
        newScopedAuthTokensWaitingRegionSelection =
            outerModel.sharedModel.scopedAuthTokensWaitingRegionSelection
                |> List.filter (\t -> t.project.uuid /= projectUuid)

        oldSharedModel =
            outerModel.sharedModel

        newSharedModel =
            { oldSharedModel | scopedAuthTokensWaitingRegionSelection = newScopedAuthTokensWaitingRegionSelection }
    in
    ( { outerModel | sharedModel = newSharedModel }, Cmd.none )


removeUnscopedProject : OSTypes.KeystoneUrl -> OSTypes.ProjectUuid -> OuterModel -> ( OuterModel, Cmd OuterMsg )
removeUnscopedProject keystoneUrl projectUuid outerModel =
    case GetterSetters.unscopedProviderLookup outerModel.sharedModel keystoneUrl of
        Nothing ->
            ( outerModel, Cmd.none )

        Just unscopedProvider ->
            case unscopedProvider.projectsAvailable of
                RemoteData.Success projectsAvailable ->
                    let
                        newProjectsAvailable =
                            projectsAvailable
                                |> List.filter (\p -> p.project.uuid /= projectUuid)
                                |> RemoteData.Success

                        newUnscopedProvider =
                            { unscopedProvider | projectsAvailable = newProjectsAvailable }

                        oldSharedModel =
                            outerModel.sharedModel

                        newSharedModel =
                            -- Remove unscoped provider if there are no projects left in it
                            if
                                newUnscopedProvider.projectsAvailable
                                    |> RemoteData.withDefault []
                                    |> List.isEmpty
                            then
                                { oldSharedModel
                                    | unscopedProviders =
                                        List.filter
                                            (\p -> p.authUrl /= unscopedProvider.authUrl)
                                            outerModel.sharedModel.unscopedProviders
                                }

                            else
                                GetterSetters.modelUpdateUnscopedProvider oldSharedModel newUnscopedProvider
                    in
                    ( { outerModel | sharedModel = newSharedModel }, Cmd.none )

                _ ->
                    ( outerModel, Cmd.none )


createProject : OSTypes.KeystoneUrl -> OSTypes.ScopedAuthToken -> OSTypes.RegionId -> OuterModel -> ( OuterModel, Cmd OuterMsg )
createProject keystoneUrl token regionId outerModel =
    let
        processError : String -> ( OuterModel, Cmd OuterMsg )
        processError error =
            State.Error.processStringError
                outerModel.sharedModel
                (ErrorContext
                    "Create project"
                    ErrorCrit
                    Nothing
                )
                error
                |> mapToOuterMsg
                |> mapToOuterModel outerModel

        endpointsResult =
            Helpers.serviceCatalogToEndpoints token.catalog (Just regionId)

        maybeUnscopedProvider =
            GetterSetters.unscopedProviderLookup outerModel.sharedModel keystoneUrl

        maybeRegion =
            maybeUnscopedProvider
                |> Maybe.andThen (\provider -> GetterSetters.unscopedRegionLookup provider regionId)

        maybeDescription =
            maybeUnscopedProvider
                |> Maybe.andThen (\provider -> GetterSetters.unscopedProjectLookup provider token.project.uuid)
                |> Maybe.map .description
    in
    case ( endpointsResult, maybeRegion, maybeDescription ) of
        ( Ok endpoints, Just region, Just description ) ->
            createProject_ outerModel description token region endpoints

        ( Err e, _, _ ) ->
            processError e

        ( _, Nothing, _ ) ->
            processError ("Could not look up Keystone region with ID " ++ regionId)

        ( _, _, Nothing ) ->
            processError ("Could not look up description of project with ID " ++ token.project.uuid)


createProject_ : OuterModel -> Maybe OSTypes.ProjectDescription -> OSTypes.ScopedAuthToken -> OSTypes.Region -> Endpoints -> ( OuterModel, Cmd OuterMsg )
createProject_ outerModel description authToken region endpoints =
    let
        sharedModel =
            outerModel.sharedModel

        newProject =
            { secret = NoProjectSecret
            , auth = authToken
            , region = Just region

            -- Maybe todo, eliminate parallel data structures in auth and endpoints?
            , endpoints = endpoints
            , description = description
            , images = RDPP.RemoteDataPlusPlus RDPP.DontHave RDPP.Loading
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
            , networkQuota = RemoteData.NotAsked
            , jetstream2Allocation = RDPP.empty
            }

        newSharedModel =
            GetterSetters.modelUpdateProject outerModel.sharedModel newProject

        ( newNewSharedModel, newCmd ) =
            ( newSharedModel
            , [ Rest.Nova.requestServers
              , Rest.Neutron.requestSecurityGroups
              , Rest.Keystone.requestAppCredential sharedModel.clientUuid sharedModel.clientCurrentTime
              ]
                |> List.map (\x -> x newProject)
                |> Cmd.batch
            )
                |> Helpers.pipelineCmd
                    (ApiModelHelpers.requestVolumes (GetterSetters.projectIdentifier newProject))
                |> Helpers.pipelineCmd
                    (ApiModelHelpers.requestFloatingIps (GetterSetters.projectIdentifier newProject))
                |> Helpers.pipelineCmd
                    (ApiModelHelpers.requestPorts (GetterSetters.projectIdentifier newProject))
                |> Helpers.pipelineCmd
                    (ApiModelHelpers.requestImages (GetterSetters.projectIdentifier newProject))
    in
    ( { outerModel | sharedModel = newNewSharedModel }
    , Cmd.map SharedMsg newCmd
    )


createUnscopedProvider : SharedModel -> OSTypes.UnscopedAuthToken -> HelperTypes.Url -> ( SharedModel, Cmd SharedMsg )
createUnscopedProvider model authToken authUrl =
    let
        newProvider =
            { authUrl = authUrl
            , token = authToken
            , projectsAvailable = RemoteData.Loading
            , regionsAvailable = RemoteData.Loading
            }

        newProviders =
            newProvider :: model.unscopedProviders
    in
    ( { model | unscopedProviders = newProviders }
    , Cmd.batch
        [ Rest.Keystone.requestUnscopedProjects newProvider model.cloudCorsProxyUrl
        , Rest.Keystone.requestUnscopedRegions newProvider model.cloudCorsProxyUrl
        ]
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
                        let
                            errorContext : OSTypes.IpAddressUuid -> ErrorContext
                            errorContext ipUuid =
                                ErrorContext
                                    ("delete floating IP address with UUID " ++ ipUuid)
                                    ErrorDebug
                                    Nothing
                        in
                        GetterSetters.getServerFloatingIps project server.osProps.uuid
                            |> List.map .uuid
                            |> List.map
                                (\ipUuid ->
                                    Rest.Neutron.requestDeleteFloatingIp project (errorContext ipUuid) ipUuid
                                )

                newProject =
                    GetterSetters.projectUpdateServer project newServer
            in
            ( newProject
            , Cmd.batch
                [ Rest.Nova.requestDeleteServer
                    (GetterSetters.projectIdentifier newProject)
                    newProject.endpoints.nova
                    newServer.osProps.uuid
                , Cmd.batch deleteFloatingIpCmds
                ]
            )
