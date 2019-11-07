module State exposing (init, subscriptions, update)

import Browser.Events
import Helpers.Helpers as Helpers
import Helpers.Random as RandomHelpers
import Json.Decode as Decode
import LocalStorage.LocalStorage as LocalStorage
import LocalStorage.Types as LocalStorageTypes
import Maybe
import OpenStack.ServerVolumes as OSSvrVols
import OpenStack.Volumes as OSVolumes
import Ports
import RemoteData
import Rest.Rest as Rest
import Time
import Toasty
import Types.Types
    exposing
        ( CockpitLoginStatus(..)
        , CreateServerField(..)
        , Flags
        , FloatingIpState(..)
        , HttpRequestMethod(..)
        , LoginField(..)
        , Model
        , Msg(..)
        , NewServerNetworkOptions(..)
        , NonProjectViewConstructor(..)
        , OpenstackCreds
        , Project
        , ProjectSpecificMsgConstructor(..)
        , ProjectViewConstructor(..)
        , Server
        , ViewState(..)
        )



{- Todo remove default creds once storing this in local storage -}


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
            , viewState = NonProjectView Login
            , maybeWindowSize = Just { width = flags.width, height = flags.height }
            , projects = []
            , creds = OpenstackCreds "" "" "" "" "" ""
            , imageFilterTag = Maybe.Just "distro-base"
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
    in
    case hydratedModel.viewState of
        ProjectView projectName ListProjectServers ->
            update (ProjectMsg projectName RequestServers) hydratedModel

        _ ->
            ( hydratedModel, Cmd.none )


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

        RequestNewProjectToken ->
            let
                -- If user does not provide a port number and path (API version) then we guess it
                oldCreds =
                    model.creds

                newCreds =
                    { oldCreds | authUrl = Helpers.authUrlWithPortAndVersion oldCreds.authUrl }
            in
            ( model, Rest.requestAuthToken model.proxyUrl newCreds )

        ReceiveAuthToken creds response ->
            Rest.receiveAuthToken model creds response

        ProjectMsg projectIdentifier innerMsg ->
            case Helpers.projectLookup model projectIdentifier of
                Nothing ->
                    -- Project not found, may have been removed, nothing to do
                    ( model, Cmd.none )

                Just project ->
                    processProjectSpecificMsg model project innerMsg

        {- Form inputs -}
        InputLoginField loginField ->
            let
                creds =
                    model.creds

                newCreds =
                    case loginField of
                        AuthUrl authUrl ->
                            { creds | authUrl = authUrl }

                        ProjectDomain projectDomain ->
                            { creds | projectDomain = projectDomain }

                        ProjectName projectName ->
                            { creds | projectName = projectName }

                        UserDomain userDomain ->
                            { creds | userDomain = userDomain }

                        Username username ->
                            { creds | username = username }

                        Password password ->
                            { creds | password = password }

                        OpenRc openRc ->
                            Helpers.processOpenRc creds openRc

                newModel =
                    { model | creds = newCreds }
            in
            ( newModel, Cmd.none )

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

        InputCreateServerField createServerRequest createServerField ->
            let
                newCreateServerRequest =
                    case createServerField of
                        CreateServerName name ->
                            { createServerRequest | name = name }

                        CreateServerCount count ->
                            { createServerRequest | count = count }

                        CreateServerUserData userData ->
                            { createServerRequest | userData = userData }

                        CreateServerSize flavorUuid ->
                            { createServerRequest | flavorUuid = flavorUuid }

                        CreateServerKeypairName keypairName ->
                            { createServerRequest | keypairName = Just keypairName }

                        CreateServerVolBacked volBacked ->
                            { createServerRequest | volBacked = volBacked }

                        CreateServerVolBackedSize sizeStr ->
                            { createServerRequest | volBackedSizeGb = sizeStr }

                        CreateServerNetworkUuid networkUuid ->
                            { createServerRequest | networkUuid = networkUuid }

                        CreateServerShowAdvancedOptions showAdvancedOptions ->
                            { createServerRequest | showAdvancedOptions = showAdvancedOptions }

                newViewState =
                    ProjectView createServerRequest.projectId (CreateServer newCreateServerRequest)
            in
            ( { model | viewState = newViewState }, Cmd.none )

        OpenInBrowser url ->
            ( model, Ports.openInBrowser url )

        OpenNewWindow url ->
            ( model, Ports.openNewWindow url )

        RandomPassword project password ->
            -- This is the start of a code smell for two reasons:
            -- 1. We have parallel data structures, storing password in userdata string and separately
            -- 2. We must reach deep into model.viewState in order to change these fields
            -- See also Rest.receiveFlavors
            case model.viewState of
                NonProjectView _ ->
                    ( model, Cmd.none )

                ProjectView projectId projectViewConstructor ->
                    if projectId /= Helpers.getProjectId project then
                        ( model, Cmd.none )

                    else
                        case projectViewConstructor of
                            CreateServer createServerRequest ->
                                let
                                    newUserData =
                                        String.split "{exouser-password}" createServerRequest.userData
                                            |> String.join password

                                    newCSR =
                                        { createServerRequest
                                            | userData = newUserData
                                            , exouserPassword = password
                                        }

                                    newViewState =
                                        ProjectView projectId (CreateServer newCSR)
                                in
                                ( { model | viewState = newViewState }, Cmd.none )

                            _ ->
                                ( model, Cmd.none )


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

                CreateServer createServerRequest ->
                    ( newModel
                    , Cmd.batch
                        [ Rest.requestFlavors project model.proxyUrl
                        , Rest.requestKeypairs project model.proxyUrl
                        , Rest.requestNetworks project model.proxyUrl
                        , RandomHelpers.generatePassword
                            (\password ->
                                RandomPassword project password
                            )
                        , RandomHelpers.generateServerName
                            (\serverName ->
                                InputCreateServerField createServerRequest (CreateServerName serverName)
                            )
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
                ( newModel, Rest.requestAuthToken model.proxyUrl newProject.creds )

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
                                    NonProjectView Login

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
