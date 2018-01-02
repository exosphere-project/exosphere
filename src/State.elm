module State exposing (init, subscriptions, update)

import Time
import Helpers
import Types.Types exposing (..)
import Rest


{- Todo remove default creds once storing this in local storage -}


init : ( Model, Cmd Msg )
init =
    ( { messages = []
      , viewState = Login
      , providers = []
      , creds =
            Creds
                "https://tombstone-cloud.cyverse.org:8000/v3/auth/tokens"
                "default"
                "demo"
                "default"
                "demo"
                ""
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (10 * Time.second) Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            case model.viewState of
                ListUserServers providerName ->
                    case Helpers.providerLookup model providerName of
                        Nothing ->
                            Helpers.processError model "provider not found"

                        Just provider ->
                            ( model, Rest.requestServers provider )

                ServerDetail providerName serverUuid ->
                    case Helpers.providerLookup model providerName of
                        Nothing ->
                            Helpers.processError model "provider not found"

                        Just provider ->
                            ( model
                            , Rest.requestServerDetail provider serverUuid
                            )

                _ ->
                    ( model, Cmd.none )

        ChangeViewState state ->
            let
                newModel =
                    { model | viewState = state }
            in
                case state of
                    Login ->
                        ( newModel, Cmd.none )

                    Home ->
                        ( newModel, Cmd.none )

                    ListImages providerName ->
                        case Helpers.providerLookup model providerName of
                            Nothing ->
                                Helpers.processError model "provider not found"

                            Just provider ->
                                ( newModel
                                , Rest.requestImages provider
                                )

                    ListUserServers providerName ->
                        case Helpers.providerLookup model providerName of
                            Nothing ->
                                Helpers.processError model "provider not found"

                            Just provider ->
                                ( newModel
                                , Rest.requestServers provider
                                )

                    ServerDetail providerName serverUuid ->
                        case Helpers.providerLookup model providerName of
                            Nothing ->
                                Helpers.processError model "provider not found"

                            Just provider ->
                                ( newModel
                                , Rest.requestServerDetail
                                    provider
                                    serverUuid
                                )

                    CreateServer providerName _ ->
                        case Helpers.providerLookup model providerName of
                            Nothing ->
                                Helpers.processError model "provider not found"

                            Just provider ->
                                ( newModel
                                , Cmd.batch
                                    [ Rest.requestFlavors provider
                                    , Rest.requestKeypairs provider
                                    ]
                                )

        RequestNewProviderToken ->
            ( model, Rest.requestAuthToken model )

        RequestCreateServer providerName createServerRequest ->
            case Helpers.providerLookup model providerName of
                Nothing ->
                    Helpers.processError model "provider not found"

                Just provider ->
                    ( model
                    , Rest.requestCreateServer provider createServerRequest
                    )

        RequestDeleteServer providerName server ->
            case Helpers.providerLookup model providerName of
                Nothing ->
                    Helpers.processError model "provider not found"

                Just provider ->
                    let
                        newProvider =
                            { provider
                                | servers =
                                    List.filter
                                        (\s -> s /= server)
                                        provider.servers
                            }

                        otherProviders =
                            List.filter
                                (\p -> p.name /= provider.name)
                                model.providers

                        newProviders =
                            newProvider :: otherProviders

                        newModel =
                            { model | providers = newProviders }
                    in
                        ( newModel
                        , Rest.requestDeleteServer provider server
                        )

        ReceiveAuthToken response ->
            Rest.receiveAuthToken model response

        ReceiveImages providerName result ->
            case Helpers.providerLookup model providerName of
                Nothing ->
                    Helpers.processError model "provider not found"

                Just provider ->
                    Rest.receiveImages model provider result

        RequestDeleteServers providerName serversToDelete ->
            case Helpers.providerLookup model providerName of
                Nothing ->
                    Helpers.processError model "provider not found"

                Just provider ->
                    let
                        newProvider =
                            { provider | servers = List.filter (\s -> (not (List.member s serversToDelete))) provider.servers }

                        otherProviders =
                            List.filter
                                (\p -> p.name /= provider.name)
                                model.providers

                        newProviders =
                            newProvider :: otherProviders

                        newModel =
                            { model | providers = newProviders }
                    in
                        ( newModel
                        , Rest.requestDeleteServers newProvider serversToDelete
                        )

        SelectServer providerName server newSelectionState ->
            case Helpers.providerLookup model providerName of
                Nothing ->
                    Helpers.processError model "provider not found"

                Just provider ->
                    let
                        updateServer someServer =
                            if someServer.uuid == server.uuid then
                                { someServer | selected = newSelectionState }
                            else
                                someServer

                        newProvider =
                            { provider | servers = List.map updateServer provider.servers }

                        otherProviders =
                            List.filter
                                (\p -> p.name /= provider.name)
                                model.providers

                        newProviders =
                            newProvider :: otherProviders

                        newModel =
                            { model | providers = newProviders }
                    in
                        newModel
                            ! []

        SelectAllServers providerName allServersSelected ->
            case Helpers.providerLookup model providerName of
                Nothing ->
                    Helpers.processError model "provider not found"

                Just provider ->
                    let
                        updateServer someServer =
                            { someServer | selected = allServersSelected }

                        newProvider =
                            { provider | servers = List.map updateServer provider.servers }

                        otherProviders =
                            List.filter
                                (\p -> p.name /= provider.name)
                                model.providers

                        newProviders =
                            newProvider :: otherProviders

                        newModel =
                            { model | providers = newProviders }
                    in
                        newModel
                            ! []

        ReceiveServers providerName result ->
            case Helpers.providerLookup model providerName of
                Nothing ->
                    Helpers.processError model "provider not found"

                Just provider ->
                    Rest.receiveServers model provider result

        ReceiveServerDetail providerName serverUuid result ->
            case Helpers.providerLookup model providerName of
                Nothing ->
                    Helpers.processError model "provider not found"

                Just provider ->
                    Rest.receiveServerDetail model provider serverUuid result

        ReceiveFlavors providerName result ->
            case Helpers.providerLookup model providerName of
                Nothing ->
                    Helpers.processError model "provider not found"

                Just provider ->
                    Rest.receiveFlavors model provider result

        ReceiveKeypairs providerName result ->
            case Helpers.providerLookup model providerName of
                Nothing ->
                    Helpers.processError model "provider not found"

                Just provider ->
                    Rest.receiveKeypairs model provider result

        ReceiveCreateServer providerName result ->
            case Helpers.providerLookup model providerName of
                Nothing ->
                    Helpers.processError model "provider not found"

                Just provider ->
                    Rest.receiveCreateServer model provider result

        ReceiveDeleteServer _ _ ->
            {- Todo this ignores the result of server deletion API call, we should display errors to user -}
            update (ChangeViewState Home) model

        ReceiveNetworks providerName result ->
            case Helpers.providerLookup model providerName of
                Nothing ->
                    Helpers.processError model "provider not found"

                Just provider ->
                    Rest.receiveNetworks model provider result

        GetFloatingIpReceivePorts providerName server result ->
            case Helpers.providerLookup model providerName of
                Nothing ->
                    Helpers.processError model "provider not found"

                Just provider ->
                    Rest.receivePortsAndRequestFloatingIp
                        model
                        provider
                        server
                        result

        ReceiveFloatingIp providerName server result ->
            case Helpers.providerLookup model providerName of
                Nothing ->
                    Helpers.processError model "provider not found"

                Just provider ->
                    Rest.receiveFloatingIp model provider server result

        {- Form inputs -}
        InputAuthURL authURL ->
            let
                creds =
                    model.creds
            in
                ( { model | creds = { creds | authURL = authURL } }, Cmd.none )

        InputProjectDomain projectDomain ->
            let
                creds =
                    model.creds
            in
                ( { model | creds = { creds | projectDomain = projectDomain } }, Cmd.none )

        InputProjectName projectName ->
            let
                creds =
                    model.creds
            in
                ( { model | creds = { creds | projectName = projectName } }, Cmd.none )

        InputUserDomain userDomain ->
            let
                creds =
                    model.creds
            in
                ( { model | creds = { creds | userDomain = userDomain } }, Cmd.none )

        InputUsername username ->
            let
                creds =
                    model.creds
            in
                ( { model | creds = { creds | username = username } }, Cmd.none )

        InputPassword password ->
            let
                creds =
                    model.creds
            in
                ( { model | creds = { creds | password = password } }, Cmd.none )

        InputCreateServerName providerName createServerRequest name ->
            let
                viewState =
                    CreateServer providerName { createServerRequest | name = name }
            in
                ( { model | viewState = viewState }, Cmd.none )

        InputCreateServerCount providerName createServerRequest count ->
            let
                viewState =
                    CreateServer providerName { createServerRequest | count = count }
            in
                ( { model | viewState = viewState }, Cmd.none )

        InputCreateServerUserData providerName createServerRequest userData ->
            let
                viewState =
                    CreateServer providerName { createServerRequest | userData = userData }
            in
                ( { model | viewState = viewState }, Cmd.none )

        InputCreateServerSize providerName createServerRequest flavorUuid ->
            let
                viewState =
                    CreateServer providerName { createServerRequest | flavorUuid = flavorUuid }
            in
                ( { model | viewState = viewState }, Cmd.none )

        InputCreateServerKeypairName providerName createServerRequest keypairName ->
            let
                viewState =
                    CreateServer providerName { createServerRequest | keypairName = keypairName }
            in
                ( { model | viewState = viewState }, Cmd.none )
