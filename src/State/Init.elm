module State.Init exposing (init)

import AppUrl.Parser
import Browser.Navigation
import Color
import Dict
import Helpers.GetterSetters as GetterSetters
import Helpers.Helpers as Helpers
import Json.Decode as Decode
import LocalStorage.LocalStorage as LocalStorage
import LocalStorage.Types as LocalStorageTypes
import Maybe
import Ports
import Random
import Rest.ApiModelHelpers as ApiModelHelpers
import Rest.Keystone
import State.ViewState exposing (setNonProjectView, setProjectView)
import Style.Types
import Time
import Toasty
import Types.Defaults as Defaults
import Types.HelperTypes exposing (DefaultLoginView(..), HttpRequestMethod(..), ProjectIdentifier)
import Types.Msg exposing (Msg(..), ProjectSpecificMsgConstructor(..))
import Types.OuterModel exposing (OuterModel)
import Types.Project exposing (Project, ProjectSecret(..))
import Types.Types
    exposing
        ( Flags
        , SharedModel
        )
import Types.View exposing (LoginView(..), NonProjectViewConstructor(..), ProjectViewConstructor(..), ViewState(..))
import UUID
import Url


init : Flags -> ( Url.Url, Browser.Navigation.Key ) -> ( OuterModel, Cmd Msg )
init flags urlKey =
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
            , styleMode = Nothing
            }

        ( primaryColor, secondaryColor ) =
            case flags.palette of
                Just palette ->
                    ( Color.rgb255 palette.primary.r palette.primary.g palette.primary.b
                    , Color.rgb255 palette.secondary.r palette.secondary.g palette.secondary.b
                    )

                Nothing ->
                    ( Style.Types.defaultPrimaryColor, Style.Types.defaultSecondaryColor )

        defaultLoginView : Maybe DefaultLoginView
        defaultLoginView =
            flags.defaultLoginView
                |> Maybe.andThen
                    (\viewStr ->
                        case viewStr of
                            "openstack" ->
                                Just DefaultLoginOpenstack

                            "jetstream" ->
                                Just DefaultLoginJetstream

                            _ ->
                                Nothing
                    )

        emptyModel : Bool -> UUID.UUID -> SharedModel
        emptyModel showDebugMsgs uuid =
            { logMessages = []
            , urlPathPrefix = flags.urlPathPrefix
            , navigationKey = Tuple.second urlKey
            , prevUrl = ""
            , windowSize = { width = flags.width, height = flags.height }
            , unscopedProviders = []
            , projects = []
            , toasties = Toasty.initialState
            , cloudCorsProxyUrl = flags.cloudCorsProxyUrl
            , clientUuid = uuid
            , clientCurrentTime = currentTime
            , timeZone = timeZone
            , showDebugMsgs = showDebugMsgs
            , style =
                { logo =
                    case flags.logo of
                        Just logoUrl ->
                            logoUrl

                        Nothing ->
                            "assets/img/logo-alt.svg"
                , primaryColor = primaryColor
                , secondaryColor = secondaryColor
                , styleMode = Style.Types.LightMode
                , appTitle =
                    flags.appTitle |> Maybe.withDefault "Exosphere"
                , topBarShowAppTitle = flags.topBarShowAppTitle
                , defaultLoginView = defaultLoginView
                , aboutAppMarkdown = flags.aboutAppMarkdown
                , supportInfoMarkdown = flags.supportInfoMarkdown
                , userSupportEmail =
                    flags.userSupportEmail
                        |> Maybe.withDefault "incoming+exosphere-exosphere-6891229-issue-@incoming.gitlab.com"
                , localization = Maybe.withDefault Defaults.localization flags.localization
                }
            , openIdConnectLoginConfig = flags.openIdConnectLoginConfig
            , cloudSpecificConfigs =
                flags.clouds
                    |> List.map
                        (\c ->
                            ( c.keystoneHostname
                            , { userAppProxy = c.userAppProxy
                              , imageExcludeFilter = c.imageExcludeFilter
                              , featuredImageNamePrefix = c.featuredImageNamePrefix
                              }
                            )
                        )
                    |> Dict.fromList
            , instanceConfigMgtRepoUrl =
                flags.instanceConfigMgtRepoUrl
                    |> Maybe.withDefault "https://gitlab.com/exosphere/exosphere.git"
            , instanceConfigMgtRepoCheckout =
                flags.instanceConfigMgtRepoCheckout
                    |> Maybe.withDefault "master"
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

        hydratedModel : SharedModel
        hydratedModel =
            LocalStorage.hydrateModelFromStoredState (emptyModel flags.showDebugMsgs) newClientUuid storedState

        defaultViewState =
            State.ViewState.defaultViewState hydratedModel

        viewState =
            AppUrl.Parser.urlToViewState flags.urlPathPrefix defaultViewState (Tuple.first urlKey)
                |> Maybe.withDefault (NonProjectView PageNotFound)

        -- If any projects are password-authenticated, get Application Credentials for them so we can forget the passwords
        projectsNeedingAppCredentials : List Project
        projectsNeedingAppCredentials =
            let
                projectNeedsAppCredential p =
                    case p.secret of
                        ApplicationCredential _ ->
                            False

                        _ ->
                            True
            in
            List.filter projectNeedsAppCredential hydratedModel.projects

        setFaviconCmd =
            flags.favicon
                |> Maybe.map Ports.setFavicon
                |> Maybe.withDefault Cmd.none

        otherCmds =
            [ List.map
                (Rest.Keystone.requestAppCredential
                    hydratedModel.clientUuid
                    hydratedModel.clientCurrentTime
                )
                projectsNeedingAppCredentials
                |> Cmd.batch
            , setFaviconCmd
            ]
                |> Cmd.batch

        ( requestResourcesModel, requestResourcesCmd ) =
            let
                applyRequestsToProject : ProjectIdentifier -> SharedModel -> ( SharedModel, Cmd Msg )
                applyRequestsToProject projectId model =
                    ( model, otherCmds )
                        |> Helpers.pipelineCmd (ApiModelHelpers.requestServers projectId)
                        |> Helpers.pipelineCmd (ApiModelHelpers.requestFloatingIps projectId)
                        |> Helpers.pipelineCmd (ApiModelHelpers.requestPorts projectId)
            in
            hydratedModel.projects
                |> List.map (\p -> p.auth.project.uuid)
                |> List.foldl
                    (\uuid modelCmdTuple ->
                        Helpers.pipelineCmd (applyRequestsToProject uuid) modelCmdTuple
                    )
                    ( hydratedModel, Cmd.none )

        outerModel =
            { sharedModel = requestResourcesModel
            , viewState = defaultViewState
            , pendingCredentialedRequests = []
            }

        ( setViewModel, setViewCmd ) =
            case viewState of
                ExampleNestedView model ->
                    ( { outerModel | viewState = ExampleNestedView model }, Cmd.none )

                NonProjectView nonProjectViewConstructor ->
                    setNonProjectView
                        nonProjectViewConstructor
                        outerModel

                ProjectView projectId _ projectViewConstructor ->
                    -- If initial view is a project-specific view then we call setProjectView to fire any needed API calls
                    case GetterSetters.projectLookup requestResourcesModel projectId of
                        Just project ->
                            setProjectView
                                project
                                projectViewConstructor
                                outerModel

                        Nothing ->
                            ( outerModel, Cmd.none )
    in
    ( setViewModel, Cmd.batch [ requestResourcesCmd, setViewCmd ] )
