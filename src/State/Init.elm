module State.Init exposing (init)

import AppUrl.Parser
import Browser.Navigation
import Dict
import Helpers.Helpers as Helpers
import Helpers.ModelLookups as ModelLookups
import Json.Decode as Decode
import LocalStorage.LocalStorage as LocalStorage
import LocalStorage.Types as LocalStorageTypes
import Maybe
import Random
import Rest.Keystone
import Rest.Neutron
import Rest.Nova
import State.ViewState exposing (setProjectView)
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
            }

        emptyModel : Bool -> UUID.UUID -> Model
        emptyModel showDebugMsgs uuid =
            { logMessages = []
            , urlPathPrefix = flags.urlPathPrefix
            , maybeNavigationKey = maybeUrlKey |> Maybe.map Tuple.second
            , prevUrl = ""
            , viewState = NonProjectView LoginPicker
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
                                firstProject.auth.project.uuid
                                { createPopup = False }
                                (ListProjectServers
                                    Defaults.serverListViewParams
                                )
            in
            case maybeUrlKey of
                Just ( url, _ ) ->
                    AppUrl.Parser.urlToViewState flags.urlPathPrefix url
                        |> Maybe.withDefault defaultViewState

                Nothing ->
                    defaultViewState

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
            [ List.map
                (Rest.Keystone.requestAppCredential
                    hydratedModel.clientUuid
                    hydratedModel.clientCurrentTime
                )
                projectsNeedingAppCredentials
                |> Cmd.batch
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
            { hydratedModel | projects = projectsServersLoading }
    in
    case viewState of
        NonProjectView _ ->
            ( { newModel | viewState = viewState }
            , otherCmds
            )

        ProjectView projectId _ projectViewConstructor ->
            -- If initial view is a project-specific view then we call setProjectView to fire any needed API calls
            let
                ( setProjectViewModel, setProjectViewCmd ) =
                    case ModelLookups.projectLookup newModel projectId of
                        Just project ->
                            setProjectView newModel project projectViewConstructor

                        Nothing ->
                            ( newModel, Cmd.none )
            in
            ( setProjectViewModel
            , Cmd.batch [ otherCmds, setProjectViewCmd ]
            )
