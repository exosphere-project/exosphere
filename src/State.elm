module State exposing (init, subscriptions, update)

import Helpers
import Maybe
import Rest
import Time
import Types.Types exposing (..)



{- Todo remove default creds once storing this in local storage -}


init : () -> ( Model, Cmd Msg )
init _ =
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

                ProviderView providerName (ServerDetail serverUuid) ->
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
            ( model, Rest.requestAuthToken model )

        ReceiveAuthToken response ->
            Rest.receiveAuthToken model response

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
                            { createServerRequest | keypairName = keypairName }

                        CreateServerVolBacked volBacked ->
                            { createServerRequest | volBacked = volBacked }

                        CreateServerVolBackedSize sizeStr ->
                            { createServerRequest | volBackedSizeGb = sizeStr }

                newViewState =
                    ProviderView createServerRequest.providerName (CreateServer newCreateServerRequest)
            in
            ( { model | viewState = newViewState }, Cmd.none )


processProviderSpecificMsg : Model -> Provider -> ProviderSpecificMsgConstructor -> ( Model, Cmd Msg )
processProviderSpecificMsg model provider msg =
    case msg of
        SetProviderView providerViewConstructor ->
            let
                newModel =
                    { model | viewState = ProviderView provider.name providerViewConstructor }
            in
            case providerViewConstructor of
                ProviderHome ->
                    ( newModel, Cmd.none )

                ListImages ->
                    ( newModel, Rest.requestImages provider )

                ListProviderServers ->
                    ( newModel, Rest.requestServers provider )

                ServerDetail serverUuid ->
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
                        ]
                    )

        RequestServers ->
            ( model, Rest.requestServers provider )

        RequestServerDetail serverUuid ->
            ( model, Rest.requestServerDetail provider serverUuid )

        RequestCreateServer createServerRequest ->
            ( model, Rest.requestCreateServer provider createServerRequest )

        RequestDeleteServer server ->
            let
                newProvider =
                    { provider
                        | servers =
                            List.filter
                                (\s -> s /= server)
                                provider.servers
                    }

                newModel =
                    Helpers.modelUpdateProvider model newProvider
            in
            ( newModel, Rest.requestDeleteServer newProvider server )

        ReceiveImages result ->
            Rest.receiveImages model provider result

        RequestDeleteServers serversToDelete ->
            let
                newProvider =
                    { provider | servers = List.filter (\s -> not (List.member s serversToDelete)) provider.servers }

                newModel =
                    Helpers.modelUpdateProvider model newProvider
            in
            ( newModel
            , Rest.requestDeleteServers newProvider serversToDelete
            )

        SelectServer server newSelectionState ->
            let
                updateServer someServer =
                    if someServer.uuid == server.uuid then
                        { someServer | selected = newSelectionState }

                    else
                        someServer

                newProvider =
                    { provider
                        | servers =
                            List.map updateServer provider.servers
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
                    { someServer | selected = allServersSelected }

                newProvider =
                    { provider | servers = List.map updateServer provider.servers }

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
            update (ProviderMsg provider.name (SetProviderView ProviderHome)) model

        ReceiveNetworks result ->
            Rest.receiveNetworks model provider result

        GetFloatingIpReceivePorts serverUuid result ->
            Rest.receivePortsAndRequestFloatingIp model provider serverUuid result

        ReceiveFloatingIp serverUuid result ->
            Rest.receiveFloatingIp model provider serverUuid result
