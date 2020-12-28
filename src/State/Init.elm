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
import Rest.Neutron
import State.ViewState exposing (setProjectView)
import Style.Types
import Time
import Toasty
import Types.Defaults as Defaults
import Types.Types
    exposing
        ( CockpitLoginStatus(..)
        , ExoSetupStatus(..)
        , Flags
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
        , ServerOrigin(..)
        , ViewState(..)
        )
import UUID
import Url


init : Flags -> Maybe ( Url.Url, Browser.Navigation.Key ) -> ( Model, Cmd Msg )
init flags maybeUrlKey =
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

        defaultLoginView : Maybe LoginView
        defaultLoginView =
            flags.defaultLoginView
                |> Maybe.andThen
                    (\viewStr ->
                        case viewStr of
                            "openstack" ->
                                Just <| LoginOpenstack Defaults.openstackCreds

                            "jetstream" ->
                                Just <| LoginJetstream Defaults.jetstreamCreds

                            _ ->
                                Nothing
                    )

        defaultViewState : ViewState
        defaultViewState =
            defaultLoginView
                |> Maybe.map (\loginView -> NonProjectView (Login loginView))
                |> Maybe.withDefault (NonProjectView LoginPicker)

        emptyModel : Bool -> UUID.UUID -> Model
        emptyModel showDebugMsgs uuid =
            { logMessages = []
            , urlPathPrefix = flags.urlPathPrefix
            , maybeNavigationKey = maybeUrlKey |> Maybe.map Tuple.second
            , prevUrl = ""
            , viewState =
                defaultViewState
            , maybeWindowSize = Just { width = flags.width, height = flags.height }
            , unscopedProviders = []
            , projects = []
            , toasties = Toasty.initialState
            , cloudCorsProxyUrl = flags.cloudCorsProxyUrl
            , cloudsWithUserAppProxy = Dict.fromList flags.cloudsWithUserAppProxy
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
                , defaultLoginView = defaultLoginView
                , aboutAppMarkdown = flags.aboutAppMarkdown
                , supportInfoMarkdown = flags.supportInfoMarkdown
                , userSupportEmail =
                    flags.userSupportEmail
                        |> Maybe.withDefault "incoming+exosphere-exosphere-6891229-issue-@incoming.gitlab.com"
                }
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
                viewStateIfNoUrl =
                    case hydratedModel.projects of
                        [] ->
                            defaultViewState

                        firstProject :: _ ->
                            ProjectView
                                firstProject.auth.project.uuid
                                { createPopup = False }
                                (ListProjectServers
                                    Defaults.serverListViewParams
                                )
            in
            case maybeUrlKey of
                Just ( url, _ ) ->
                    AppUrl.Parser.urlToViewState flags.urlPathPrefix url
                        |> Maybe.withDefault viewStateIfNoUrl

                Nothing ->
                    viewStateIfNoUrl

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
            , List.map Rest.Neutron.requestFloatingIps hydratedModel.projects |> Cmd.batch
            , setFaviconCmd
            ]
                |> Cmd.batch

        ( newModel, newCmd ) =
            hydratedModel.projects
                |> List.map (\p -> p.auth.project.uuid)
                |> List.map ApiModelHelpers.requestServers
                |> List.foldl Helpers.pipelineCmd ( hydratedModel, otherCmds )
    in
    case viewState of
        NonProjectView _ ->
            ( { newModel | viewState = viewState }
            , newCmd
            )

        ProjectView projectId _ projectViewConstructor ->
            -- If initial view is a project-specific view then we call setProjectView to fire any needed API calls
            let
                ( setProjectViewModel, setProjectViewCmd ) =
                    case GetterSetters.projectLookup newModel projectId of
                        Just project ->
                            setProjectView newModel project projectViewConstructor

                        Nothing ->
                            ( newModel, Cmd.none )
            in
            ( setProjectViewModel
            , Cmd.batch [ newCmd, setProjectViewCmd ]
            )
