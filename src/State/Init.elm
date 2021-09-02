module State.Init exposing (init)

import Browser.Navigation
import Color
import Dict
import Helpers.Helpers as Helpers
import Json.Decode as Decode
import LocalStorage.LocalStorage as LocalStorage
import LocalStorage.Types as LocalStorageTypes
import Maybe
import OpenStack.Types as OSTypes
import Ports
import Random
import Rest.ApiModelHelpers as ApiModelHelpers
import Rest.Keystone
import State.ViewState
import Style.Types
import Time
import Toasty
import Types.Defaults as Defaults
import Types.Error
import Types.Flags exposing (Flags)
import Types.HelperTypes as HelperTypes exposing (DefaultLoginView(..), HttpRequestMethod(..), ProjectIdentifier)
import Types.OuterModel exposing (OuterModel)
import Types.OuterMsg exposing (OuterMsg(..))
import Types.Project exposing (Project, ProjectSecret(..))
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))
import Types.View exposing (LoginView(..), NonProjectViewConstructor(..), ProjectViewConstructor(..), ViewState(..))
import UUID
import Url


init : Flags -> ( Url.Url, Browser.Navigation.Key ) -> ( OuterModel, Cmd OuterMsg )
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
            , experimentalFeaturesEnabled = Nothing
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

        ( cloudSpecificConfigs, decodeErrorLogMessages ) =
            -- TODO should this be a toast message or something else super obviously visible? Can the app die loudly?
            case decodeCloudSpecificConfigs flags.clouds of
                Ok configs ->
                    ( configs, [] )

                Err error ->
                    ( Dict.empty
                    , [ { message = Debug.toString error
                        , context =
                            Types.Error.ErrorContext
                                "decode cloud-specific configs from Flags JSON"
                                Types.Error.ErrorCrit
                                Nothing
                        , timestamp = currentTime
                        }
                      ]
                    )

        emptyModel : Bool -> UUID.UUID -> SharedModel
        emptyModel showDebugMsgs uuid =
            { logMessages = decodeErrorLogMessages
            , urlPathPrefix = flags.urlPathPrefix
            , navigationKey = Tuple.second urlKey
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
            , cloudSpecificConfigs = cloudSpecificConfigs
            , instanceConfigMgtRepoUrl =
                flags.instanceConfigMgtRepoUrl
                    |> Maybe.withDefault "https://gitlab.com/exosphere/exosphere.git"
            , instanceConfigMgtRepoCheckout =
                flags.instanceConfigMgtRepoCheckout
                    |> Maybe.withDefault "master"
            , experimentalFeaturesEnabled = False
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
                applyRequestsToProject : ProjectIdentifier -> SharedModel -> ( SharedModel, Cmd SharedMsg )
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
            , viewState =
                -- This will get replaced with the appropriate view from viewStateFromUrl
                NonProjectView LoginPicker
            , pendingCredentialedRequests = []
            }

        ( setViewModel, setViewCmd ) =
            State.ViewState.navigateToPage (Tuple.first urlKey) outerModel
    in
    ( setViewModel
    , Cmd.batch [ Cmd.map SharedMsg requestResourcesCmd, setViewCmd ]
    )



-- Flag JSON Decoders


decodeCloudSpecificConfigs : Decode.Value -> Result Decode.Error (Dict.Dict HelperTypes.KeystoneHostname HelperTypes.CloudSpecificConfig)
decodeCloudSpecificConfigs value =
    Decode.decodeValue (Decode.list decodeCloudSpecificConfig |> Decode.map Dict.fromList) value


decodeCloudSpecificConfig : Decode.Decoder ( HelperTypes.KeystoneHostname, HelperTypes.CloudSpecificConfig )
decodeCloudSpecificConfig =
    Decode.map4 HelperTypes.CloudSpecificConfig
        (Decode.field "userAppProxy" (Decode.nullable Decode.string))
        (Decode.field "imageExcludeFilter" (Decode.nullable imageExcludeFilterDecoder))
        (Decode.field "featuredImageNamePrefix" (Decode.nullable Decode.string))
        (Decode.field "operatingSystemChoices" (Decode.list operatingSystemChoiceDecoder))
        |> Decode.andThen
            (\cloudSpecificConfig ->
                Decode.field "keystoneHostname" Decode.string
                    |> Decode.map (\keystoneHostname -> ( keystoneHostname, cloudSpecificConfig ))
            )


imageExcludeFilterDecoder : Decode.Decoder HelperTypes.ExcludeFilter
imageExcludeFilterDecoder =
    Decode.map2 HelperTypes.ExcludeFilter
        (Decode.field "filterKey" Decode.string)
        (Decode.field "filterValue" Decode.string)


operatingSystemChoiceDecoder : Decode.Decoder HelperTypes.OperatingSystemChoice
operatingSystemChoiceDecoder =
    Decode.map4 HelperTypes.OperatingSystemChoice
        (Decode.field "friendlyName" Decode.string)
        (Decode.field "description" Decode.string)
        (Decode.field "logo" Decode.string)
        (Decode.field "versions" (Decode.list operatingSystemChoiceVersionDecoder))


operatingSystemChoiceVersionDecoder : Decode.Decoder HelperTypes.OperatingSystemChoiceVersion
operatingSystemChoiceVersionDecoder =
    Decode.map2 HelperTypes.OperatingSystemChoiceVersion
        (Decode.field "friendlyName" Decode.string)
        (Decode.field "filters" imageFiltersDecoder)


imageFiltersDecoder : Decode.Decoder HelperTypes.ImageFilters
imageFiltersDecoder =
    Decode.map5 HelperTypes.ImageFilters
        (Decode.maybe (Decode.field "uuid" Decode.string))
        (Decode.maybe
            (Decode.field "visibility" Decode.string
                |> Decode.andThen
                    (\v ->
                        case v of
                            "private" ->
                                Decode.succeed OSTypes.ImagePrivate

                            "shared" ->
                                Decode.succeed OSTypes.ImageShared

                            "community" ->
                                Decode.succeed OSTypes.ImageCommunity

                            "public" ->
                                Decode.succeed OSTypes.ImagePublic

                            _ ->
                                Decode.fail "unrecognized value for image visibility"
                    )
            )
        )
        (Decode.maybe (Decode.field "name" Decode.string))
        (Decode.maybe (Decode.field "osDistro" Decode.string))
        (Decode.maybe (Decode.field "osVersion" Decode.string))
