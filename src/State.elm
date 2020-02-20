module State exposing (init, subscriptions, update)

import Browser.Events
import Error exposing (ErrorContext, ErrorLevel(..))
import Helpers.Helpers as Helpers
import Helpers.Random as RandomHelpers
import Http
import Json.Decode as Decode
import LocalStorage.LocalStorage as LocalStorage
import LocalStorage.Types as LocalStorageTypes
import Maybe
import OpenStack.Quotas
import OpenStack.ServerVolumes as OSSvrVols
import OpenStack.Types as OSTypes
import OpenStack.Volumes as OSVolumes
import Ports
import RemoteData
import Rest.Rest as Rest
import Task
import Time
import Toasty
import Types.HelperTypes as HelperTypes
import Types.Types
    exposing
        ( CockpitLoginStatus(..)
        , Flags
        , FloatingIpState(..)
        , HttpRequestMethod(..)
        , LogMessage
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
        , Toast
        , UnscopedProvider
        , UnscopedProviderProject
        , ViewState(..)
        )


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        globalDefaults =
            { shellUserData =
                """#cloud-config
users:
  - default
  - name: exouser
    shell: /bin/bash
    groups: sudo, admin
    sudo: ['ALL=(ALL) NOPASSWD:ALL']
    {ssh-authorized-keys}
package_update: true
packages:
  - cockpit
runcmd:
  - |
    WORDS_URL=https://gitlab.com/exosphere/exosphere/snippets/1943357/raw
    WORDS_SHA256=c889072801fec7f102dacb618ead4b2d3454b8e45229d18d9f7493909faf00b7
    wget --quiet --output-document=words $WORDS_URL
    if echo $WORDS_SHA256 words | sha256sum --check --quiet; then
      PASSPHRASE=$(shuf --random-source=/dev/urandom --head-count 8 words | paste --delimiters=' ' --serial | head -c -1)
      POST_URL=http://169.254.169.254/openstack/latest/password
      if curl --fail --silent --request POST $POST_URL --data "$PASSPHRASE"; then
        echo exouser:$PASSPHRASE | chpasswd
      fi
      unset PASSPHRASE
    fi
  - systemctl enable cockpit.socket
  - systemctl start cockpit.socket
  - systemctl daemon-reload
  - "mkdir /media/volume"
  - "cd /media/volume; for x in b c d e f g h i j k; do mkdir sd$x; mkdir vd$x; done"
  - "systemctl daemon-reload"
  - "for x in b c d e f g h i j k; do systemctl start media-volume-sd$x.automount; systemctl start media-volume-vd$x.automount; done"
  - "chown exouser:exouser /media/volume/*"
mount_default_fields: [None, None, "ext4", "user,rw,auto,nofail,x-systemd.makefs,x-systemd.automount", "0", "2"]
mounts:
  - [ /dev/sdb, /media/volume/sdb ]
  - [ /dev/sdc, /media/volume/sdc ]
  - [ /dev/sdd, /media/volume/sdd ]
  - [ /dev/sde, /media/volume/sde ]
  - [ /dev/sdf, /media/volume/sdf ]
  - [ /dev/sdg, /media/volume/sdg ]
  - [ /dev/sdh, /media/volume/sdh ]
  - [ /dev/sdi, /media/volume/sdi ]
  - [ /dev/sdj, /media/volume/sdj ]
  - [ /dev/sdk, /media/volume/sdk ]
  - [ /dev/vdb, /media/volume/vdb ]
  - [ /dev/vdc, /media/volume/vdc ]
  - [ /dev/vdd, /media/volume/vdd ]
  - [ /dev/vde, /media/volume/vde ]
  - [ /dev/vdf, /media/volume/vdf ]
  - [ /dev/vdg, /media/volume/vdg ]
  - [ /dev/vdh, /media/volume/vdh ]
  - [ /dev/vdi, /media/volume/vdi ]
  - [ /dev/vdj, /media/volume/vdj ]
  - [ /dev/vdk, /media/volume/vdk ]
"""
            }

        emptyStoredState : LocalStorageTypes.StoredState
        emptyStoredState =
            { projects = []
            }

        emptyModel : Model
        emptyModel =
            { logMessages = []
            , viewState = NonProjectView LoginPicker
            , maybeWindowSize = Just { width = flags.width, height = flags.height }
            , unscopedProviders = []
            , projects = []
            , globalDefaults = globalDefaults
            , toasties = Toasty.initialState
            , proxyUrl = flags.proxyUrl
            , isElectron = flags.isElectron
            }

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
            LocalStorage.hydrateModelFromStoredState emptyModel storedState

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

        getAppCredentialCmds =
            List.map getTimeForAppCredential projectsNeedingAppCredentials
    in
    case hydratedModel.viewState of
        ProjectView projectName _ (ListProjectServers _ _) ->
            let
                ( newModel, newCmds ) =
                    update (ProjectMsg projectName RequestServers) hydratedModel
            in
            ( newModel, Cmd.batch (newCmds :: getAppCredentialCmds) )

        _ ->
            ( hydratedModel, Cmd.batch getAppCredentialCmds )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ -- 10 seconds
          Time.every (10 * 1000) Tick
        , Browser.Events.onResize MsgChangeWindowSize
        ]



{- We want to `setStorage` on every update. This function adds the setStorage
   command for every step of the update function.
-}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( newModel, cmds ) =
            updateUnderlying msg model
    in
    ( newModel
    , Cmd.batch [ Ports.setStorage (LocalStorage.generateStoredState newModel), cmds ]
    )


updateUnderlying : Msg -> Model -> ( Model, Cmd Msg )
updateUnderlying msg model =
    case msg of
        ToastyMsg subMsg ->
            Toasty.update Helpers.toastConfig ToastyMsg subMsg model

        NewLogMessage logMessage ->
            let
                newLogMessages =
                    logMessage :: model.logMessages
            in
            ( { model | logMessages = newLogMessages }, Cmd.none )

        MsgChangeWindowSize x y ->
            ( { model | maybeWindowSize = Just { width = x, height = y } }, Cmd.none )

        Tick _ ->
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
                                ListProjectServers _ _ ->
                                    update (ProjectMsg projectName RequestServers) model

                                ServerDetail serverUuid _ ->
                                    let
                                        ( newModel, newCmd ) =
                                            update (ProjectMsg projectName (RequestServer serverUuid)) model

                                        requestVolCmd =
                                            OSVolumes.requestVolumes project
                                    in
                                    ( newModel, Cmd.batch [ newCmd, requestVolCmd ] )

                                ListProjectVolumes _ ->
                                    ( model, OSVolumes.requestVolumes project )

                                VolumeDetail _ _ ->
                                    ( model, OSVolumes.requestVolumes project )

                                _ ->
                                    ( model, Cmd.none )

        SetNonProjectView nonProjectViewConstructor ->
            let
                newModel =
                    { model | viewState = NonProjectView nonProjectViewConstructor }
            in
            case nonProjectViewConstructor of
                _ ->
                    ( newModel, Cmd.none )

        HandleApiError errorContext error ->
            processApiError model errorContext error

        RequestUnscopedToken openstackLoginUnscoped ->
            ( model, Rest.requestUnscopedAuthToken model.proxyUrl openstackLoginUnscoped )

        RequestNewProjectToken openstackCreds ->
            let
                -- If user does not provide a port number and path (API version) then we guess it
                newOpenstackCreds =
                    { openstackCreds | authUrl = Helpers.authUrlWithPortAndVersion openstackCreds.authUrl }
            in
            ( model, Rest.requestScopedAuthToken model.proxyUrl <| OSTypes.PasswordCreds newOpenstackCreds )

        JetstreamLogin jetstreamCreds ->
            let
                openstackCredsList =
                    Helpers.jetstreamToOpenstackCreds jetstreamCreds

                cmds =
                    List.map
                        (\creds -> Rest.requestUnscopedAuthToken model.proxyUrl creds)
                        openstackCredsList
            in
            ( model, Cmd.batch cmds )

        ReceiveScopedAuthToken maybePassword ( metadata, response ) ->
            case Rest.decodeScopedAuthToken <| Http.GoodStatus_ metadata response of
                Err error ->
                    Helpers.processError
                        model
                        (ErrorContext
                            "decode scoped auth token"
                            ErrorCrit
                            Nothing
                        )
                        error

                Ok authToken ->
                    let
                        projectId =
                            ProjectIdentifier
                                authToken.project.name
                                (Helpers.serviceCatalogToEndpoints authToken.catalog).keystone
                    in
                    -- If we don't have a project with same name + authUrl then create one, if we do then update its OSTypes.AuthToken
                    -- This code ensures we don't end up with duplicate projects on the same provider in our model.
                    case
                        ( Helpers.projectLookup model <| projectId, maybePassword )
                    of
                        ( Nothing, Nothing ) ->
                            Helpers.processError
                                model
                                (ErrorContext
                                    "this is an impossible state"
                                    ErrorCrit
                                    (Just "The laws of physics and logic have been violated, check with your universe administrator")
                                )
                                "This is an impossible state"

                        ( Nothing, Just password ) ->
                            createProject model password authToken

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
            case Rest.decodeUnscopedAuthToken <| Http.GoodStatus_ metadata response of
                Err error ->
                    Helpers.processError
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

                        newModelWithView =
                            -- If we are not already on a SelectProjects view, then go there
                            case newModel.viewState of
                                NonProjectView (SelectProjects _ _) ->
                                    newModel

                                _ ->
                                    { newModel
                                        | viewState =
                                            NonProjectView <|
                                                SelectProjects newProvider.authUrl []
                                    }
                    in
                    ( newModelWithView, Cmd.none )

                Nothing ->
                    -- Provider not found, may have been removed, nothing to do
                    ( model, Cmd.none )

        RequestProjectLoginFromProvider keystoneUrl password desiredProjects ->
            case Helpers.providerLookup model keystoneUrl of
                Just provider ->
                    let
                        buildLoginRequest : UnscopedProviderProject -> Cmd Msg
                        buildLoginRequest project =
                            Rest.requestScopedAuthToken
                                model.proxyUrl
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
                                                ListProjectServers { onlyOwnServers = False } []

                                        Nothing ->
                                            NonProjectView LoginPicker

                        newModel =
                            { model | unscopedProviders = newUnscopedProviders, viewState = newViewState }
                    in
                    ( newModel, Cmd.batch loginRequests )

                Nothing ->
                    Helpers.processError
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
            ( { model | viewState = newViewState }, Cmd.none )

        OpenInBrowser url ->
            ( model, Ports.openInBrowser url )

        OpenNewWindow url ->
            ( model, Ports.openNewWindow url )

        NoOp ->
            ( model, Cmd.none )


processProjectSpecificMsg : Model -> Project -> ProjectSpecificMsgConstructor -> ( Model, Cmd Msg )
processProjectSpecificMsg model project msg =
    case msg of
        SetProjectView projectViewConstructor ->
            let
                newModel =
                    { model | viewState = ProjectView (Helpers.getProjectId project) { createPopup = False } projectViewConstructor }
            in
            case projectViewConstructor of
                ListImages _ ->
                    ( newModel, Rest.requestImages project )

                ListProjectServers _ _ ->
                    ( newModel
                    , [ Rest.requestServers
                      , Rest.requestFloatingIps
                      ]
                        |> List.map (\x -> x project)
                        |> Cmd.batch
                    )

                ServerDetail serverUuid _ ->
                    ( newModel
                    , Cmd.batch
                        [ Rest.requestServer project serverUuid
                        , Rest.requestFlavors project
                        , Rest.requestImages project
                        , OSVolumes.requestVolumes project
                        , Ports.instantiateClipboardJs ()
                        ]
                    )

                CreateServerImage _ _ ->
                    ( newModel, Cmd.none )

                CreateServer createServerRequest ->
                    case model.viewState of
                        -- If we are already in this view state then ensure user isn't trying to choose a server count
                        -- that would exceed quota; if so, reduce server count to comply with quota.
                        ProjectView _ _ (CreateServer _) ->
                            let
                                newCSR =
                                    case
                                        ( Helpers.flavorLookup project createServerRequest.flavorUuid
                                        , project.computeQuota
                                        , project.volumeQuota
                                        )
                                    of
                                        ( Just flavor, RemoteData.Success computeQuota, RemoteData.Success volumeQuota ) ->
                                            let
                                                availServers =
                                                    Helpers.overallQuotaAvailServers
                                                        createServerRequest
                                                        flavor
                                                        computeQuota
                                                        volumeQuota
                                            in
                                            { createServerRequest
                                                | count =
                                                    case availServers of
                                                        Just availServers_ ->
                                                            if createServerRequest.count > availServers_ then
                                                                availServers_

                                                            else
                                                                createServerRequest.count

                                                        Nothing ->
                                                            createServerRequest.count
                                            }

                                        ( _, _, _ ) ->
                                            createServerRequest

                                newNewModel =
                                    { newModel
                                        | viewState =
                                            ProjectView
                                                (Helpers.getProjectId project)
                                                { createPopup = False }
                                            <|
                                                CreateServer newCSR
                                    }
                            in
                            ( newNewModel, Cmd.none )

                        -- If we are just entering this view then gather everything we need
                        _ ->
                            let
                                newCSRMsg password_ serverName_ =
                                    let
                                        newUserData =
                                            String.split "{exouser-password}" createServerRequest.userData
                                                |> String.join password_

                                        newCSR =
                                            { createServerRequest
                                                | userData = newUserData
                                                , exouserPassword = password_
                                                , name = serverName_
                                            }
                                    in
                                    ProjectMsg (Helpers.getProjectId project) <|
                                        SetProjectView <|
                                            CreateServer newCSR

                                newProject =
                                    { project
                                        | computeQuota = RemoteData.Loading
                                        , volumeQuota = RemoteData.Loading
                                    }

                                newNewModel =
                                    Helpers.modelUpdateProject newModel newProject
                            in
                            ( newNewModel
                            , Cmd.batch
                                [ Rest.requestFlavors project
                                , Rest.requestKeypairs project
                                , Rest.requestNetworks project
                                , RandomHelpers.generatePasswordAndServerName (\( password, serverName ) -> newCSRMsg password serverName)
                                , OpenStack.Quotas.requestComputeQuota project
                                , OpenStack.Quotas.requestVolumeQuota project
                                ]
                            )

                ListProjectVolumes _ ->
                    ( newModel, OSVolumes.requestVolumes project )

                VolumeDetail _ _ ->
                    ( newModel, Cmd.none )

                AttachVolumeModal _ _ ->
                    ( newModel
                    , Cmd.batch
                        [ Rest.requestServers project
                        , OSVolumes.requestVolumes project
                        ]
                    )

                MountVolInstructions _ ->
                    ( newModel, Cmd.none )

                CreateVolume _ _ ->
                    ( newModel, Cmd.none )

        PrepareCredentialedRequest requestProto posixTime ->
            let
                -- Add proxy URL
                requestNeedingToken =
                    requestProto model.proxyUrl

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
                    ( { model
                        | viewState =
                            ProjectView
                                projectId
                                { viewParams
                                    | createPopup = not viewParams.createPopup
                                }
                                viewConstructor
                      }
                    , Cmd.none
                    )

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
                                            { onlyOwnServers = False }
                                            []

                                Nothing ->
                                    NonProjectView <| LoginPicker

                newModel =
                    { model | projects = newProjects, viewState = newViewState }
            in
            ( newModel, Cmd.none )

        RequestServers ->
            ( model, Rest.requestServers project )

        RequestServer serverUuid ->
            ( model, Rest.requestServer project serverUuid )

        RequestCreateServer createServerRequest ->
            ( model, Rest.requestCreateServer project createServerRequest )

        RequestDeleteServer server ->
            let
                oldExoProps =
                    server.exoProps

                newServer =
                    Server server.osProps { oldExoProps | deletionAttempted = True }

                newProject =
                    Helpers.projectUpdateServer project newServer

                newModel =
                    Helpers.modelUpdateProject model newProject
            in
            ( newModel, Rest.requestDeleteServer newProject newServer )

        RequestServerAction server func targetStatus ->
            let
                oldExoProps =
                    server.exoProps

                newServer =
                    Server server.osProps { oldExoProps | targetOpenstackStatus = Just targetStatus }

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
                    Helpers.processError
                        model
                        (ErrorContext
                            ("look for server UUID with attached volume " ++ volumeUuid)
                            ErrorCrit
                            Nothing
                        )
                        "Could not determine server attached to this volume."

        RequestCreateServerImage serverUuid imageName ->
            let
                newModel =
                    { model
                        | viewState =
                            ProjectView
                                (Helpers.getProjectId project)
                                { createPopup = False }
                            <|
                                ListProjectServers
                                    { onlyOwnServers = False }
                                    []
                    }
            in
            ( newModel, Rest.requestCreateServerImage project serverUuid imageName )

        ReceiveImages images ->
            Rest.receiveImages model project images

        RequestDeleteServers serversToDelete ->
            let
                markDeletionAttempted someServer =
                    let
                        oldExoProps =
                            someServer.exoProps
                    in
                    Server someServer.osProps { oldExoProps | deletionAttempted = True }

                newServers =
                    List.map markDeletionAttempted serversToDelete

                newProject =
                    Helpers.projectUpdateServers project newServers

                newModel =
                    Helpers.modelUpdateProject model newProject
            in
            ( newModel, Rest.requestDeleteServers newProject serversToDelete )

        SelectServer server newSelectionState ->
            let
                updateServer someServer =
                    if someServer.osProps.uuid == server.osProps.uuid then
                        let
                            oldExoProps =
                                someServer.exoProps
                        in
                        Server someServer.osProps { oldExoProps | selected = newSelectionState }

                    else
                        someServer

                newProject =
                    { project
                        | servers =
                            RemoteData.Success (List.map updateServer (RemoteData.withDefault [] project.servers))
                    }

                newModel =
                    Helpers.modelUpdateProject model newProject
            in
            ( newModel
            , Cmd.none
            )

        SelectAllServers allServersSelected ->
            let
                updateServer someServer =
                    let
                        oldExoProps =
                            someServer.exoProps
                    in
                    Server someServer.osProps { oldExoProps | selected = allServersSelected }

                newProject =
                    { project | servers = RemoteData.Success (List.map updateServer (RemoteData.withDefault [] project.servers)) }

                newModel =
                    Helpers.modelUpdateProject model newProject
            in
            ( newModel
            , Cmd.none
            )

        ReceiveServers servers ->
            Rest.receiveServers model project servers

        ReceiveServer serverUuid server ->
            Rest.receiveServer model project serverUuid server

        ReceiveConsoleUrl serverUuid url ->
            Rest.receiveConsoleUrl model project serverUuid url

        ReceiveFlavors flavors ->
            Rest.receiveFlavors model project flavors

        ReceiveKeypairs keypairs ->
            Rest.receiveKeypairs model project keypairs

        ReceiveCreateServer serverUuid ->
            Rest.receiveCreateServer model project serverUuid

        ReceiveDeleteServer serverUuid maybeIpAddress ->
            let
                serverDeletedModel =
                    let
                        newViewState =
                            case model.viewState of
                                ProjectView projectId viewParams (ServerDetail viewServerUuid _) ->
                                    if viewServerUuid == serverUuid then
                                        ProjectView
                                            projectId
                                            viewParams
                                            (ListProjectServers { onlyOwnServers = False } [])

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

                        newModelProto =
                            Helpers.modelUpdateProject model newProject
                    in
                    { newModelProto
                        | viewState = newViewState
                    }

                ( deleteIpAddressModel, deleteIpAddressCmd ) =
                    case maybeIpAddress of
                        Nothing ->
                            ( serverDeletedModel, Cmd.none )

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
                                    ( serverDeletedModel, Cmd.none )

                                Just uuid ->
                                    ( serverDeletedModel, Rest.requestDeleteFloatingIp project uuid )
            in
            ( deleteIpAddressModel, deleteIpAddressCmd )

        ReceiveNetworks nets ->
            Rest.receiveNetworks model project nets

        ReceiveFloatingIps ips ->
            Rest.receiveFloatingIps model project ips

        GetFloatingIpReceivePorts serverUuid ports ->
            Rest.receivePortsAndRequestFloatingIp model project serverUuid ports

        ReceiveCreateFloatingIp serverUuid ip ->
            Rest.receiveCreateFloatingIp model project serverUuid ip

        ReceiveDeleteFloatingIp uuid ->
            Rest.receiveDeleteFloatingIp model project uuid

        ReceiveSecurityGroups groups ->
            Rest.receiveSecurityGroupsAndEnsureExoGroup model project groups

        ReceiveCreateExoSecurityGroup group ->
            Rest.receiveCreateExoSecurityGroupAndRequestCreateRules model project group

        ReceiveCockpitLoginStatus serverUuid result ->
            Rest.receiveCockpitLoginStatus model project serverUuid result

        ReceiveCreateVolume ->
            {- Should we add new volume to model now? -}
            update (ProjectMsg (Helpers.getProjectId project) <| SetProjectView <| ListProjectVolumes []) model

        ReceiveVolumes volumes ->
            let
                -- Look for any server backing volumes that were created with no name, and give them a reasonable name
                updateVolNameCmds : List (Cmd Msg)
                updateVolNameCmds =
                    RemoteData.withDefault [] project.servers
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
                                (Tuple.first t).osProps.details.metadata
                                    |> List.map .key
                                    |> List.filter (\key -> key == "exouserPassword")
                                    |> List.isEmpty
                                    |> not
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
            ( model, Rest.requestAppCredential project posix )

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


createProject : Model -> HelperTypes.Password -> OSTypes.ScopedAuthToken -> ( Model, Cmd Msg )
createProject model password authToken =
    let
        endpoints =
            Helpers.serviceCatalogToEndpoints authToken.catalog

        newProject =
            { secret = OpenstackPassword password
            , auth = authToken

            -- Maybe todo, eliminate parallel data structures in auth and endpoints?
            , endpoints = endpoints
            , images = []
            , servers = RemoteData.NotAsked
            , flavors = []
            , keypairs = []
            , volumes = RemoteData.NotAsked
            , networks = []
            , floatingIps = []
            , ports = []
            , securityGroups = []
            , computeQuota = RemoteData.NotAsked
            , volumeQuota = RemoteData.NotAsked
            , pendingCredentialedRequests = []
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
                        ListProjectServers { onlyOwnServers = False } []

                ProjectView _ projectViewParams _ ->
                    ProjectView
                        (Helpers.getProjectId newProject)
                        projectViewParams
                    <|
                        ListProjectServers { onlyOwnServers = False } []

        newModel =
            { model
                | projects = newProjects
                , viewState = newViewState
            }
    in
    ( newModel
    , [ Rest.requestServers
      , Rest.requestSecurityGroups
      , Rest.requestFloatingIps
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
    , Rest.requestUnscopedProjects newProvider model.proxyUrl
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
    Rest.requestScopedAuthToken model.proxyUrl creds


processApiError : Model -> ErrorContext -> Http.Error -> ( Model, Cmd Msg )
processApiError model errorContext httpError =
    let
        logMessageProto =
            LogMessage
                (Debug.toString httpError)
                errorContext
    in
    Toasty.addToastIfUnique
        Helpers.toastConfig
        ToastyMsg
        (Toast errorContext (Debug.toString httpError))
        ( model
        , Task.perform
            (\posix -> NewLogMessage (logMessageProto posix))
            Time.now
        )
