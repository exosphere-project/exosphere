module State exposing (init, subscriptions, update)

import Browser.Events
import Helpers.Helpers as Helpers
import Helpers.Random as RandomHelpers
import Http
import Json.Decode as Decode
import LocalStorage.LocalStorage as LocalStorage
import LocalStorage.Types as LocalStorageTypes
import Maybe
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
packages:
  - cockpit
runcmd:
  - systemctl enable cockpit.socket
  - systemctl start cockpit.socket
  - systemctl daemon-reload
chpasswd:
  list: |
    exouser:{exouser-password}
  expire: False
"""
            }

        emptyStoredState : LocalStorageTypes.StoredState
        emptyStoredState =
            { projects = []
            }

        emptyModel : Model
        emptyModel =
            { messages = []
            , viewState = NonProjectView LoginPicker
            , maybeWindowSize = Just { width = flags.width, height = flags.height }
            , projects = []
            , imageFilterTag = Maybe.Just "distro-base"
            , imageFilterSearchText = Maybe.Just "JS-API-Featured-"
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
        ProjectView projectName ListProjectServers ->
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

        MsgChangeWindowSize x y ->
            ( { model | maybeWindowSize = Just { width = x, height = y } }, Cmd.none )

        Tick _ ->
            case model.viewState of
                NonProjectView _ ->
                    ( model, Cmd.none )

                ProjectView projectName projectViewState ->
                    case Helpers.projectLookup model projectName of
                        Nothing ->
                            {- Should this throw an error? -}
                            ( model, Cmd.none )

                        Just project ->
                            case projectViewState of
                                ListProjectServers ->
                                    update (ProjectMsg projectName RequestServers) model

                                ServerDetail serverUuid _ ->
                                    update (ProjectMsg projectName (RequestServer serverUuid)) model

                                ListProjectVolumes ->
                                    ( model, OSVolumes.requestVolumes project model.proxyUrl )

                                VolumeDetail _ ->
                                    ( model, OSVolumes.requestVolumes project model.proxyUrl )

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

        RequestNewProjectToken openstackCreds ->
            let
                -- If user does not provide a port number and path (API version) then we guess it
                newOpenstackCreds =
                    { openstackCreds | authUrl = Helpers.authUrlWithPortAndVersion openstackCreds.authUrl }
            in
            ( model, Rest.requestAuthToken model.proxyUrl <| OSTypes.PasswordCreds newOpenstackCreds )

        JetstreamLogin jetstreamCreds ->
            let
                openstackCreds =
                    Helpers.jetstreamToOpenstackCreds jetstreamCreds
            in
            ( model, Rest.requestAuthToken model.proxyUrl <| OSTypes.PasswordCreds openstackCreds )

        ReceiveAuthToken maybePassword responseResult ->
            case responseResult of
                Err error ->
                    Helpers.processError model error

                Ok ( metadata, response ) ->
                    case Rest.decodeAuthToken <| Http.GoodStatus_ metadata response of
                        Err error ->
                            Helpers.processError model error

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
                                    Helpers.processError model "This is an impossible state"

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

        InputImageFilterTag inputTag ->
            let
                maybeTag =
                    if inputTag == "" then
                        Nothing

                    else
                        Just inputTag

                newModel =
                    { model | imageFilterTag = maybeTag }
            in
            ( newModel, Cmd.none )

        InputImageFilterSearchText inputSearchText ->
            let
                maybeSearchText =
                    if inputSearchText == "" then
                        Nothing

                    else
                        Just inputSearchText

                newModel =
                    { model | imageFilterSearchText = maybeSearchText }
            in
            ( newModel, Cmd.none )

        OpenInBrowser url ->
            ( model, Ports.openInBrowser url )

        OpenNewWindow url ->
            ( model, Ports.openNewWindow url )


processProjectSpecificMsg : Model -> Project -> ProjectSpecificMsgConstructor -> ( Model, Cmd Msg )
processProjectSpecificMsg model project msg =
    case msg of
        SetProjectView projectViewConstructor ->
            let
                newModel =
                    { model | viewState = ProjectView (Helpers.getProjectId project) projectViewConstructor }
            in
            case projectViewConstructor of
                ListImages ->
                    ( newModel, Rest.requestImages project model.proxyUrl )

                ListProjectServers ->
                    ( newModel
                    , [ Rest.requestServers
                      , Rest.requestFloatingIps
                      ]
                        |> List.map (\x -> x project model.proxyUrl)
                        |> Cmd.batch
                    )

                ServerDetail serverUuid _ ->
                    ( newModel
                    , Cmd.batch
                        [ Rest.requestServer project model.proxyUrl serverUuid
                        , Rest.requestFlavors project model.proxyUrl
                        , Rest.requestImages project model.proxyUrl
                        , OSVolumes.requestVolumes project model.proxyUrl
                        ]
                    )

                CreateServerImage _ _ ->
                    ( newModel, Cmd.none )

                CreateServer createServerRequest ->
                    case model.viewState of
                        -- If we are already in this view state then don't do all this stuff
                        ProjectView _ (CreateServer _) ->
                            ( newModel, Cmd.none )

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
                            in
                            ( newModel
                            , Cmd.batch
                                [ Rest.requestFlavors project model.proxyUrl
                                , Rest.requestKeypairs project model.proxyUrl
                                , Rest.requestNetworks project model.proxyUrl
                                , RandomHelpers.generatePasswordAndServerName (\( password, serverName ) -> newCSRMsg password serverName)
                                ]
                            )

                ListProjectVolumes ->
                    ( newModel, OSVolumes.requestVolumes project model.proxyUrl )

                VolumeDetail _ ->
                    ( newModel, Cmd.none )

                AttachVolumeModal _ _ ->
                    ( newModel
                    , Cmd.batch
                        [ Rest.requestServers project model.proxyUrl
                        , OSVolumes.requestVolumes project model.proxyUrl
                        ]
                    )

                MountVolInstructions _ ->
                    ( newModel, Cmd.none )

                CreateVolume _ _ ->
                    ( newModel, Cmd.none )

        ValidateTokenForCredentialedRequest requestNeedingToken posixTime ->
            let
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

        RemoveProject ->
            let
                newProjects =
                    List.filter (\p -> Helpers.getProjectId p /= Helpers.getProjectId project) model.projects

                newViewState =
                    case model.viewState of
                        NonProjectView _ ->
                            -- If we are not in a project-specific view then stay there
                            model.viewState

                        ProjectView _ _ ->
                            -- If we have any projects switch to the first one in the list, otherwise switch to login view
                            case List.head newProjects of
                                Just p ->
                                    ProjectView (Helpers.getProjectId p) ListProjectServers

                                Nothing ->
                                    NonProjectView <| LoginPicker

                newModel =
                    { model | projects = newProjects, viewState = newViewState }
            in
            ( newModel, Cmd.none )

        RequestServers ->
            ( model, Rest.requestServers project model.proxyUrl )

        RequestServer serverUuid ->
            ( model, Rest.requestServer project model.proxyUrl serverUuid )

        RequestCreateServer createServerRequest ->
            ( model, Rest.requestCreateServer project model.proxyUrl createServerRequest )

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
            ( newModel, Rest.requestDeleteServer newProject model.proxyUrl newServer )

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
            ( newModel, func newProject model.proxyUrl newServer )

        RequestCreateVolume name size ->
            let
                createVolumeRequest =
                    { name = name
                    , size = size
                    }
            in
            ( model, OSVolumes.requestCreateVolume project model.proxyUrl createVolumeRequest )

        RequestDeleteVolume volumeUuid ->
            ( model, OSVolumes.requestDeleteVolume project model.proxyUrl volumeUuid )

        RequestAttachVolume serverUuid volumeUuid ->
            ( model, OSSvrVols.requestAttachVolume project model.proxyUrl serverUuid volumeUuid )

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
                    ( model, OSSvrVols.requestDetachVolume project model.proxyUrl serverUuid volumeUuid )

                Nothing ->
                    Helpers.processError model "Could not determine server UUID"

        RequestCreateServerImage serverUuid imageName ->
            let
                newModel =
                    { model | viewState = ProjectView (Helpers.getProjectId project) ListProjectServers }
            in
            ( newModel, Rest.requestCreateServerImage project model.proxyUrl serverUuid imageName )

        ReceiveImages result ->
            Rest.receiveImages model project result

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
            ( newModel, Rest.requestDeleteServers newProject model.proxyUrl serversToDelete )

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

        ReceiveServers result ->
            Rest.receiveServers model project result

        ReceiveServer serverUuid result ->
            Rest.receiveServer model project serverUuid result

        ReceiveConsoleUrl serverUuid result ->
            Rest.receiveConsoleUrl model project serverUuid result

        ReceiveFlavors result ->
            Rest.receiveFlavors model project result

        ReceiveKeypairs result ->
            Rest.receiveKeypairs model project result

        ReceiveCreateServer result ->
            Rest.receiveCreateServer model project result

        ReceiveDeleteServer serverUuid maybeIpAddress result ->
            let
                ( serverDeletedModel, newCmd ) =
                    let
                        viewState =
                            ProjectView (Helpers.getProjectId project) ListProjectServers

                        newModel =
                            { model | viewState = viewState }
                    in
                    Rest.receiveDeleteServer newModel project serverUuid result

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
                                    ( serverDeletedModel, Rest.requestDeleteFloatingIp project model.proxyUrl uuid )
            in
            ( deleteIpAddressModel, Cmd.batch [ newCmd, deleteIpAddressCmd ] )

        ReceiveNetworks result ->
            Rest.receiveNetworks model project result

        ReceiveFloatingIps result ->
            Rest.receiveFloatingIps model project result

        GetFloatingIpReceivePorts serverUuid result ->
            Rest.receivePortsAndRequestFloatingIp model project serverUuid result

        ReceiveCreateFloatingIp serverUuid result ->
            Rest.receiveCreateFloatingIp model project serverUuid result

        ReceiveDeleteFloatingIp uuid result ->
            Rest.receiveDeleteFloatingIp model project uuid result

        ReceiveSecurityGroups result ->
            Rest.receiveSecurityGroupsAndEnsureExoGroup model project result

        ReceiveCreateExoSecurityGroup result ->
            Rest.receiveCreateExoSecurityGroupAndRequestCreateRules model project result

        ReceiveCreateExoSecurityGroupRules _ ->
            {- Todo this ignores the result of security group rule creation API call, we should display errors to user -}
            ( model, Cmd.none )

        ReceiveCockpitLoginStatus serverUuid result ->
            Rest.receiveCockpitLoginStatus model project serverUuid result

        ReceiveServerAction _ result ->
            case result of
                Err error ->
                    Helpers.processError model error

                Ok _ ->
                    ( model, Cmd.none )

        ReceiveCreateVolume result ->
            case result of
                Err error ->
                    Helpers.processError model error

                Ok _ ->
                    {- Should we add new volume to model now? -}
                    update (ProjectMsg (Helpers.getProjectId project) <| SetProjectView ListProjectVolumes) model

        ReceiveVolumes result ->
            case result of
                Err error ->
                    Helpers.processError model error

                Ok volumes ->
                    let
                        newProject =
                            { project | volumes = RemoteData.succeed volumes }

                        newModel =
                            Helpers.modelUpdateProject model newProject
                    in
                    ( newModel, Cmd.none )

        ReceiveDeleteVolume result ->
            case result of
                Err error ->
                    Helpers.processError model error

                Ok _ ->
                    ( model, OSVolumes.requestVolumes project model.proxyUrl )

        ReceiveAttachVolume result ->
            case result of
                Err error ->
                    Helpers.processError model error

                Ok attachment ->
                    {- TODO opportunity for future optimization, just update the model instead of doing another API roundtrip -}
                    update (ProjectMsg (Helpers.getProjectId project) <| SetProjectView <| MountVolInstructions attachment) model

        ReceiveDetachVolume result ->
            case result of
                Err error ->
                    Helpers.processError model error

                Ok _ ->
                    {- TODO opportunity for future optimization, just update the model instead of doing another API roundtrip -}
                    update (ProjectMsg (Helpers.getProjectId project) <| SetProjectView ListProjectVolumes) model

        ReceiveAppCredential result ->
            case result of
                Err error ->
                    Helpers.processError model error

                Ok appCredential ->
                    let
                        newProject =
                            { project | secret = ApplicationCredential appCredential }
                    in
                    ( Helpers.modelUpdateProject model newProject, Cmd.none )

        RequestAppCredential posix ->
            ( model, Rest.requestAppCredential project model.proxyUrl posix )


createProject : Model -> HelperTypes.Password -> OSTypes.AuthToken -> ( Model, Cmd Msg )
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
            , pendingCredentialedRequests = []
            }

        newProjects =
            newProject :: model.projects

        newModel =
            { model
                | projects = newProjects
                , viewState = ProjectView (Helpers.getProjectId newProject) ListProjectServers
            }
    in
    ( newModel
    , [ Rest.requestServers
      , Rest.requestSecurityGroups
      , Rest.requestFloatingIps
      ]
        |> List.map (\x -> x newProject model.proxyUrl)
        |> (\l -> getTimeForAppCredential newProject :: l)
        |> Cmd.batch
    )


projectUpdateAuthToken : Model -> Project -> OSTypes.AuthToken -> ( Model, Cmd Msg )
projectUpdateAuthToken model project authToken =
    -- Update auth token for existing project
    let
        newProject =
            { project | auth = authToken }

        newModel =
            Helpers.modelUpdateProject model newProject
    in
    sendPendingRequests newModel newProject


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
                    OSTypes.AppCreds project.endpoints.keystone appCred
    in
    Rest.requestAuthToken model.proxyUrl creds
