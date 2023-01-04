module State.Init exposing (init)

import Browser.Navigation
import Color
import Dict
import FormatNumber.Locales
import Helpers.GetterSetters as GetterSetters
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
import Set
import State.ViewState
import Style.Theme
import Style.Types as ST
import Time
import Toasty
import Types.Defaults as Defaults
import Types.Error exposing (AppError)
import Types.Flags exposing (Flags)
import Types.HelperTypes as HelperTypes exposing (CloudSpecificConfigMap, DefaultLoginView(..), HttpRequestMethod(..), ProjectIdentifier)
import Types.OuterModel exposing (OuterModel)
import Types.OuterMsg exposing (OuterMsg(..))
import Types.Project exposing (Project, ProjectSecret(..))
import Types.SharedModel exposing (SharedModel)
import Types.SharedMsg exposing (ProjectSpecificMsgConstructor(..), SharedMsg(..))
import Types.View exposing (LoginView(..), NonProjectViewConstructor(..), ProjectViewConstructor(..), ViewState(..))
import UUID
import Url
import View.Helpers exposing (toExoPalette)


init : Flags -> ( Url.Url, Browser.Navigation.Key ) -> ( Result AppError OuterModel, Cmd OuterMsg )
init flags urlKey =
    case validateCloudSpecificConfigs flags.clouds of
        Ok cloudSpecificConfigs ->
            case initWithValidFlags flags cloudSpecificConfigs urlKey of
                ( model, cmd ) ->
                    ( Ok model, cmd )

        Err appError ->
            ( Err appError, Cmd.none )


validateCloudSpecificConfigs : Decode.Value -> Result AppError CloudSpecificConfigMap
validateCloudSpecificConfigs clouds =
    decodeCloudSpecificConfigs clouds
        |> Result.mapError Decode.errorToString
        |> Result.mapError AppError


initWithValidFlags : Flags -> CloudSpecificConfigMap -> ( Url.Url, Browser.Navigation.Key ) -> ( OuterModel, Cmd OuterMsg )
initWithValidFlags flags cloudSpecificConfigs urlKey =
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

        deployerColors =
            case flags.palette of
                Just palette ->
                    { light =
                        { primary = Color.rgb255 palette.light.primary.r palette.light.primary.g palette.light.primary.b
                        , secondary = Color.rgb255 palette.light.secondary.r palette.light.secondary.g palette.light.secondary.b
                        }
                    , dark =
                        { primary = Color.rgb255 palette.dark.primary.r palette.dark.primary.g palette.dark.primary.b
                        , secondary = Color.rgb255 palette.dark.secondary.r palette.dark.secondary.g palette.dark.secondary.b
                        }
                    }

                Nothing ->
                    ST.defaultColors

        defaultLoginView : Maybe DefaultLoginView
        defaultLoginView =
            flags.defaultLoginView
                |> Maybe.andThen
                    (\viewStr ->
                        case viewStr of
                            "openstack" ->
                                Just DefaultLoginOpenstack

                            "oidc" ->
                                Just DefaultLoginOpenIdConnect

                            _ ->
                                Nothing
                    )

        style =
            { logo =
                case flags.logo of
                    Just logoUrl ->
                        logoUrl

                    Nothing ->
                        "assets/img/logo-alt.svg"
            , deployerColors = deployerColors
            , styleMode =
                { theme = ST.System
                , systemPreference =
                    flags.themePreference
                        |> Maybe.map Style.Theme.fromString
                        |> Maybe.withDefault Nothing
                }
            , appTitle =
                flags.appTitle |> Maybe.withDefault "Exosphere"
            , topBarShowAppTitle = flags.topBarShowAppTitle
            , defaultLoginView = defaultLoginView
            , aboutAppMarkdown = flags.aboutAppMarkdown
            , supportInfoMarkdown = flags.supportInfoMarkdown
            , userSupportEmail =
                flags.userSupportEmail
                    |> Maybe.withDefault "incoming+exosphere-exosphere-6891229-issue-@incoming.gitlab.com"
            }

        ( storedState, storedStateDecodeError ) =
            case flags.storedState of
                Nothing ->
                    ( emptyStoredState, Nothing )

                Just storedStateValue ->
                    let
                        decodedValueResult =
                            Decode.decodeValue LocalStorage.decodeStoredState storedStateValue
                    in
                    case decodedValueResult of
                        Result.Err e ->
                            ( emptyStoredState, Just <| Decode.errorToString e )

                        Result.Ok decodedValue ->
                            ( decodedValue, Nothing )

        logMessages =
            case storedStateDecodeError of
                Just error ->
                    let
                        context =
                            Types.Error.ErrorContext
                                "decode stored application state retrieved from browser local storage"
                                Types.Error.ErrorWarn
                                Nothing
                    in
                    [ { message = error, context = context, timestamp = currentTime } ]

                Nothing ->
                    []

        emptyModel : Bool -> UUID.UUID -> SharedModel
        emptyModel showDebugMsgs uuid =
            { logMessages = logMessages
            , unscopedProviders = []
            , scopedAuthTokensWaitingRegionSelection = []
            , projects = []
            , toasties = Toasty.initialState
            , networkConnectivity = Nothing
            , cloudCorsProxyUrl = flags.cloudCorsProxyUrl
            , clientUuid = uuid
            , clientCurrentTime = currentTime
            , timeZone = timeZone
            , showDebugMsgs = showDebugMsgs
            , style = style
            , openIdConnectLoginConfig = flags.openIdConnectLoginConfig
            , instanceConfigMgtRepoUrl =
                flags.instanceConfigMgtRepoUrl
                    |> Maybe.withDefault "https://gitlab.com/exosphere/exosphere.git"
            , instanceConfigMgtRepoCheckout =
                flags.instanceConfigMgtRepoCheckout
                    |> Maybe.withDefault "master"
            , viewContext =
                { cloudSpecificConfigs = cloudSpecificConfigs
                , experimentalFeaturesEnabled = False
                , locale =
                    flags.localeGuessingString
                        |> FormatNumber.Locales.fromString
                        >> Maybe.withDefault FormatNumber.Locales.usLocale
                , localization = Maybe.withDefault Defaults.localization flags.localization
                , navigationKey = Tuple.second urlKey
                , palette = toExoPalette style
                , urlPathPrefix = flags.urlPathPrefix
                , windowSize = { width = flags.width, height = flags.height }
                , showPopovers = Set.empty
                }
            , sentryConfig = flags.sentryConfig
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
                        |> Helpers.pipelineCmd (ApiModelHelpers.requestVolumes projectId)
                        |> Helpers.pipelineCmd (ApiModelHelpers.requestFloatingIps projectId)
                        |> Helpers.pipelineCmd (ApiModelHelpers.requestPorts projectId)
                        |> Helpers.pipelineCmd (ApiModelHelpers.requestImages projectId)
            in
            hydratedModel.projects
                |> List.map GetterSetters.projectIdentifier
                |> List.foldl
                    (\projectId modelCmdTuple ->
                        Helpers.pipelineCmd (applyRequestsToProject projectId) modelCmdTuple
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


decodeCloudSpecificConfigs : Decode.Value -> Result Decode.Error CloudSpecificConfigMap
decodeCloudSpecificConfigs value =
    Decode.decodeValue (Decode.list decodeCloudSpecificConfig |> Decode.map Dict.fromList) value


decodeCloudSpecificConfig : Decode.Decoder ( HelperTypes.KeystoneHostname, HelperTypes.CloudSpecificConfig )
decodeCloudSpecificConfig =
    Decode.map7 HelperTypes.CloudSpecificConfig
        (Decode.field "friendlyName" Decode.string)
        (Decode.field "userAppProxy" (Decode.nullable (Decode.list userAppProxyConfigDecoder)))
        (Decode.field "imageExcludeFilter" (Decode.nullable metadataFilterDecoder))
        (Decode.field "featuredImageNamePrefix" (Decode.nullable Decode.string))
        (Decode.field "instanceTypes" (Decode.list instanceTypeDecoder))
        (Decode.field "flavorGroups" (Decode.list flavorGroupDecoder))
        (Decode.field "desktopMessage" (Decode.nullable Decode.string))
        |> Decode.andThen
            (\cloudSpecificConfig ->
                Decode.field "keystoneHostname" Decode.string
                    |> Decode.map (\keystoneHostname -> ( keystoneHostname, cloudSpecificConfig ))
            )


userAppProxyConfigDecoder : Decode.Decoder HelperTypes.UserAppProxyConfig
userAppProxyConfigDecoder =
    Decode.map2 HelperTypes.UserAppProxyConfig
        (Decode.field "region" (Decode.nullable Decode.string))
        (Decode.field "hostname" Decode.string)


metadataFilterDecoder : Decode.Decoder HelperTypes.MetadataFilter
metadataFilterDecoder =
    Decode.map2 HelperTypes.MetadataFilter
        (Decode.field "filterKey" Decode.string)
        (Decode.field "filterValue" Decode.string)


instanceTypeDecoder : Decode.Decoder HelperTypes.InstanceType
instanceTypeDecoder =
    Decode.map4 HelperTypes.InstanceType
        (Decode.field "friendlyName" Decode.string)
        (Decode.field "description" Decode.string)
        (Decode.field "logo" Decode.string)
        (Decode.field "versions" (Decode.list instanceTypeVersionDecoder))


instanceTypeVersionDecoder : Decode.Decoder HelperTypes.InstanceTypeVersion
instanceTypeVersionDecoder =
    Decode.map4 HelperTypes.InstanceTypeVersion
        (Decode.field "friendlyName" Decode.string)
        (Decode.field "isPrimary" Decode.bool)
        (Decode.field "imageFilters" imageFiltersDecoder)
        (Decode.field "restrictFlavorIds" (Decode.list Decode.string |> Decode.nullable))


imageFiltersDecoder : Decode.Decoder HelperTypes.InstanceTypeImageFilters
imageFiltersDecoder =
    Decode.map6 HelperTypes.InstanceTypeImageFilters
        (Decode.maybe (Decode.field "name" Decode.string))
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
        (Decode.maybe (Decode.field "osDistro" Decode.string))
        (Decode.maybe (Decode.field "osVersion" Decode.string))
        (Decode.maybe (Decode.field "metadata" metadataFilterDecoder))


flavorGroupDecoder : Decode.Decoder HelperTypes.FlavorGroup
flavorGroupDecoder =
    Decode.map3 HelperTypes.FlavorGroup
        (Decode.field "matchOn" Decode.string)
        (Decode.field "title" Decode.string)
        (Decode.field "description" (Decode.nullable Decode.string))
