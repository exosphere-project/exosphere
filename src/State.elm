module State exposing (init, subscriptions, update)

import Helpers.Helpers as Helpers
import Helpers.Random as RandomHelpers
import Maybe
import Ports
import RemoteData
import Rest.Rest as Rest
import Time
import Types.Types exposing (..)



{- Todo remove default creds once storing this in local storage -}


init : () -> ( Model, Cmd Msg )
init _ =
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
packages:
  - cockpit
runcmd:
  - systemctl enable cockpit.socket
  - systemctl start cockpit.socket
  - systemctl daemon-reload
chpasswd:
  list: |
    exouser:changeme123
  expire: False
"""
            }
    in
    ( { messages = []
      , viewState = NonProviderView Login
      , providers = []
      , creds =
            Creds
                "https://tombstone-cloud.cyverse.org:5000/v3/auth/tokens"
                "default"
                "demo"
                "default"
                "demo"
                ""
      , imageFilterTag = Maybe.Just "distro-base"
      , globalDefaults = globalDefaults
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    -- 10 seconds
    Time.every (10 * 1000) Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            case model.viewState of
                NonProviderView _ ->
                    ( model, Cmd.none )

                ProviderView providerName ListProviderServers ->
                    update (ProviderMsg providerName RequestServers) model

                ProviderView providerName (ServerDetail serverUuid _) ->
                    update (ProviderMsg providerName (RequestServerDetail serverUuid)) model

                _ ->
                    ( model, Cmd.none )

        SetNonProviderView nonProviderViewConstructor ->
            let
                newModel =
                    { model | viewState = NonProviderView nonProviderViewConstructor }
            in
            case nonProviderViewConstructor of
                Login ->
                    ( newModel, Cmd.none )

        RequestNewProviderToken ->
            ( model, Rest.requestAuthToken model.creds )

        ReceiveAuthToken creds response ->
            Rest.receiveAuthToken model creds response

        ProviderMsg providerName innerMsg ->
            case Helpers.providerLookup model providerName of
                Nothing ->
                    Helpers.processError model "Provider not found"

                Just provider ->
                    processProviderSpecificMsg model provider innerMsg

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

                newViewState =
                    ProviderView createServerRequest.providerName (CreateServer newCreateServerRequest)
            in
            ( { model | viewState = newViewState }, Cmd.none )

        OpenInBrowser url ->
            ( model, Ports.openInBrowser url )

        OpenNewWindow url ->
            ( model, Ports.openNewWindow url )

        RandomPassword provider password ->
            -- This is the start of a code smell for two reasons:
            -- 1. We have parallel data structures, storing password in userdata string and separately
            -- 2. We must reach deep into model.viewState in order to change these fields
            -- See also Rest.receiveFlavors
            case model.viewState of
                NonProviderView _ ->
                    ( model, Cmd.none )

                ProviderView providerName providerViewConstructor ->
                    if providerName /= provider.name then
                        ( model, Cmd.none )

                    else
                        case providerViewConstructor of
                            CreateServer createServerRequest ->
                                let
                                    newUserData =
                                        String.split "changeme123" createServerRequest.userData
                                            |> String.join password

                                    newCSR =
                                        { createServerRequest
                                            | userData = newUserData
                                            , exouserPassword = password
                                        }

                                    newViewState =
                                        ProviderView provider.name (CreateServer newCSR)
                                in
                                ( { model | viewState = newViewState }, Cmd.none )

                            _ ->
                                ( model, Cmd.none )



--            Helpers.processError model password


processProviderSpecificMsg : Model -> Provider -> ProviderSpecificMsgConstructor -> ( Model, Cmd Msg )
processProviderSpecificMsg model provider msg =
    case msg of
        SetProviderView providerViewConstructor ->
            let
                newModel =
                    { model | viewState = ProviderView provider.name providerViewConstructor }
            in
            case providerViewConstructor of
                ListImages ->
                    ( newModel, Rest.requestImages provider )

                ListProviderServers ->
                    ( newModel, Rest.requestServers provider )

                ServerDetail serverUuid _ ->
                    ( newModel
                    , Cmd.batch
                        [ Rest.requestServerDetail provider serverUuid
                        , Rest.requestFlavors provider
                        , Rest.requestImages provider
                        ]
                    )

                CreateServer createServerRequest ->
                    ( newModel
                    , Cmd.batch
                        [ Rest.requestFlavors provider
                        , Rest.requestKeypairs provider
                        , Rest.requestNetworks provider
                        , RandomHelpers.generatePassword provider
                        ]
                    )

        ValidateTokenForCredentialedRequest requestNeedingToken posixTime ->
            let
                currentTimeMillis =
                    posixTime |> Time.posixToMillis

                tokenExpireTimeMillis =
                    provider.auth.expiresAt |> Time.posixToMillis

                tokenExpired =
                    -- Token expiring within 10 minutes
                    tokenExpireTimeMillis < currentTimeMillis + 600000
            in
            case tokenExpired of
                False ->
                    -- Token still valid, fire the request with current token
                    ( model, requestNeedingToken provider.auth.tokenValue )

                True ->
                    -- Token is expired (or nearly expired) so we add request to list of pending requests and refresh that token
                    let
                        newPQRs =
                            requestNeedingToken :: provider.pendingCredentialedRequests

                        newProvider =
                            { provider | pendingCredentialedRequests = newPQRs }

                        newModel =
                            Helpers.modelUpdateProvider model newProvider
                    in
                    ( newModel, Rest.requestAuthToken newProvider.creds )

        RequestServers ->
            ( model, Rest.requestServers provider )

        RequestServerDetail serverUuid ->
            ( model, Rest.requestServerDetail provider serverUuid )

        RequestCreateServer createServerRequest ->
            ( model, Rest.requestCreateServer provider createServerRequest )

        RequestDeleteServer server ->
            let
                updateServer someServer =
                    if someServer.osProps.uuid == server.osProps.uuid then
                        {- TODO DRY with below -}
                        let
                            oldExoProps =
                                someServer.exoProps
                        in
                        Server someServer.osProps { oldExoProps | deletionAttempted = True }

                    else
                        someServer

                newProvider =
                    { provider
                        | servers =
                            RemoteData.Success (List.map updateServer (RemoteData.withDefault [] provider.servers))
                    }

                newModel =
                    Helpers.modelUpdateProvider model newProvider
            in
            ( newModel, Rest.requestDeleteServer newProvider server )

        ReceiveImages result ->
            Rest.receiveImages model provider result

        RequestDeleteServers serversToDelete ->
            let
                updateServer someServer =
                    if List.member someServer.osProps.uuid (List.map (\s -> s.osProps.uuid) serversToDelete) then
                        {- TODO DRY with above -}
                        let
                            oldExoProps =
                                someServer.exoProps
                        in
                        Server someServer.osProps { oldExoProps | deletionAttempted = True }

                    else
                        someServer

                newProvider =
                    { provider
                        | servers =
                            RemoteData.Success
                                (List.map updateServer (RemoteData.withDefault [] provider.servers))
                    }

                newModel =
                    Helpers.modelUpdateProvider model newProvider
            in
            ( newModel
            , Rest.requestDeleteServers newProvider serversToDelete
            )

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

                newProvider =
                    { provider
                        | servers =
                            RemoteData.Success (List.map updateServer (RemoteData.withDefault [] provider.servers))
                    }

                newModel =
                    Helpers.modelUpdateProvider model newProvider
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

                newProvider =
                    { provider | servers = RemoteData.Success (List.map updateServer (RemoteData.withDefault [] provider.servers)) }

                newModel =
                    Helpers.modelUpdateProvider model newProvider
            in
            ( newModel
            , Cmd.none
            )

        ReceiveServers result ->
            Rest.receiveServers model provider result

        ReceiveServerDetail serverUuid result ->
            Rest.receiveServerDetail model provider serverUuid result

        ReceiveFlavors result ->
            Rest.receiveFlavors model provider result

        ReceiveKeypairs result ->
            Rest.receiveKeypairs model provider result

        ReceiveCreateServer result ->
            Rest.receiveCreateServer model provider result

        ReceiveDeleteServer _ ->
            {- Todo this ignores the result of server deletion API call, we should display errors to user -}
            update (ProviderMsg provider.name (SetProviderView ListProviderServers)) model

        ReceiveNetworks result ->
            Rest.receiveNetworks model provider result

        GetFloatingIpReceivePorts serverUuid result ->
            Rest.receivePortsAndRequestFloatingIp model provider serverUuid result

        ReceiveFloatingIp serverUuid result ->
            Rest.receiveFloatingIp model provider serverUuid result

        ReceiveSecurityGroups result ->
            Rest.receiveSecurityGroupsAndEnsureExoGroup model provider result

        ReceiveCreateExoSecurityGroup result ->
            Rest.receiveCreateExoSecurityGroupAndRequestCreateRules model provider result

        ReceiveCreateExoSecurityGroupRules _ ->
            {- Todo this ignores the result of security group rule creation API call, we should display errors to user -}
            ( model, Cmd.none )

        ReceiveCockpitLoginStatus serverUuid result ->
            Rest.receiveCockpitLoginStatus model provider serverUuid result
