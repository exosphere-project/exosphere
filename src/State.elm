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
                "https://tombstone-cloud.cyverse.org:5000/v3/auth/tokens"
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
                ListProviderServers providerName ->
                    case Helpers.providerLookup model providerName of
                        Nothing ->
                            Helpers.processError model "Provider not found"

                        Just provider ->
                            ( model, Rest.requestServers provider )

                ServerDetail providerName serverUuid ->
                    case Helpers.providerLookup model providerName of
                        Nothing ->
                            Helpers.processError model "Provider not found"

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

                    ProviderHome providerName ->
                        ( newModel, Cmd.none )

                    ListImages providerName ->
                        case Helpers.providerLookup model providerName of
                            Just provider ->
                                ( newModel, Rest.requestImages provider )

                            Nothing ->
                                Helpers.processError model "Provider not found"

                    ListProviderServers providerName ->
                        case Helpers.providerLookup model providerName of
                            Just provider ->
                                ( newModel, Rest.requestServers provider )

                            Nothing ->
                                Helpers.processError model "Provider not found"

                    ServerDetail providerName serverUuid ->
                        case Helpers.providerLookup model providerName of
                            Just provider ->
                                ( newModel
                                , Cmd.batch
                                    [ Rest.requestServerDetail provider serverUuid
                                    , Rest.requestFlavors provider
                                    , Rest.requestImages provider
                                    ]
                                )

                            Nothing ->
                                Helpers.processError model "Provider not found"

                    CreateServer providerName _ ->
                        case Helpers.providerLookup newModel providerName of
                            Just provider ->
                                ( newModel
                                , Cmd.batch
                                    [ Rest.requestFlavors provider
                                    , Rest.requestKeypairs provider
                                    ]
                                )

                            Nothing ->
                                Helpers.processError model "Provider not found"

        RequestNewProviderToken ->
            ( model, Rest.requestAuthToken model )

        RequestCreateServer providerName createServerRequest ->
            case Helpers.providerLookup model providerName of
                Just provider ->
                    ( model
                    , Rest.requestCreateServer provider createServerRequest
                    )

                Nothing ->
                    Helpers.processError model "Provider not found"

        RequestDeleteServer providerName server ->
            case Helpers.providerLookup model providerName of
                Just provider ->
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

                Nothing ->
                    Helpers.processError model "Provider not found"

        ReceiveAuthToken response ->
            Rest.receiveAuthToken model response

        ReceiveImages providerName result ->
            case Helpers.providerLookup model providerName of
                Just provider ->
                    Rest.receiveImages model provider result

                Nothing ->
                    Helpers.processError model "Provider not found"

        RequestDeleteServers providerName serversToDelete ->
            case Helpers.providerLookup model providerName of
                Just provider ->
                    let
                        newProvider =
                            { provider | servers = List.filter (\s -> (not (List.member s serversToDelete))) provider.servers }

                        newModel =
                            Helpers.modelUpdateProvider model newProvider
                    in
                        ( newModel
                        , Rest.requestDeleteServers newProvider serversToDelete
                        )

                Nothing ->
                    Helpers.processError model "Provider not found"

        SelectServer providerName server newSelectionState ->
            case Helpers.providerLookup model providerName of
                Nothing ->
                    Helpers.processError model "Provider not found"

                Just provider ->
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
                        newModel
                            ! []

        SelectAllServers providerName allServersSelected ->
            case Helpers.providerLookup model providerName of
                Nothing ->
                    Helpers.processError model "Provider not found"

                Just provider ->
                    let
                        updateServer someServer =
                            { someServer | selected = allServersSelected }

                        newProvider =
                            { provider | servers = List.map updateServer provider.servers }

                        newModel =
                            Helpers.modelUpdateProvider model newProvider
                    in
                        newModel
                            ! []

        ReceiveServers providerName result ->
            case Helpers.providerLookup model providerName of
                Just provider ->
                    Rest.receiveServers model provider result

                Nothing ->
                    Helpers.processError model "Provider not found"

        ReceiveServerDetail providerName serverUuid result ->
            case Helpers.providerLookup model providerName of
                Just provider ->
                    Rest.receiveServerDetail model provider serverUuid result

                Nothing ->
                    Helpers.processError model "Provider not found"

        ReceiveFlavors providerName result ->
            case Helpers.providerLookup model providerName of
                Just provider ->
                    Rest.receiveFlavors model provider result

                Nothing ->
                    Helpers.processError model "Provider not found"

        ReceiveKeypairs providerName result ->
            case Helpers.providerLookup model providerName of
                Just provider ->
                    Rest.receiveKeypairs model provider result

                Nothing ->
                    Helpers.processError model "Provider not found"

        ReceiveCreateServer providerName result ->
            case Helpers.providerLookup model providerName of
                Just provider ->
                    Rest.receiveCreateServer model provider result

                Nothing ->
                    Helpers.processError model "Provider not found"

        ReceiveDeleteServer providerName _ ->
            {- Todo this ignores the result of server deletion API call, we should display errors to user -}
            update (ChangeViewState (ProviderHome providerName)) model

        ReceiveNetworks providerName result ->
            case Helpers.providerLookup model providerName of
                Just provider ->
                    Rest.receiveNetworks model provider result

                Nothing ->
                    Helpers.processError model "Provider not found"

        GetFloatingIpReceivePorts providerName serverUuid result ->
            case Helpers.providerLookup model providerName of
                Just provider ->
                    Rest.receivePortsAndRequestFloatingIp model provider serverUuid result

                Nothing ->
                    Helpers.processError model "Provider not found"

        ReceiveFloatingIp providerName serverUuid result ->
            case Helpers.providerLookup model providerName of
                Just provider ->
                    Rest.receiveFloatingIp model provider serverUuid result

                Nothing ->
                    Helpers.processError model "Provider not found"

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

        InputOpenRc openRc ->
            Helpers.processOpenRc model openRc

        InputCreateServerName createServerRequest name ->
            let
                viewState =
                    CreateServer createServerRequest.providerName { createServerRequest | name = name }
            in
                ( { model | viewState = viewState }, Cmd.none )

        InputCreateServerCount createServerRequest count ->
            let
                viewState =
                    CreateServer createServerRequest.providerName { createServerRequest | count = count }
            in
                ( { model | viewState = viewState }, Cmd.none )

        InputCreateServerUserData createServerRequest userData ->
            let
                viewState =
                    CreateServer createServerRequest.providerName { createServerRequest | userData = userData }
            in
                ( { model | viewState = viewState }, Cmd.none )

        InputCreateServerSize createServerRequest flavorUuid ->
            let
                viewState =
                    CreateServer createServerRequest.providerName { createServerRequest | flavorUuid = flavorUuid }
            in
                ( { model | viewState = viewState }, Cmd.none )

        InputCreateServerKeypairName createServerRequest keypairName ->
            let
                viewState =
                    CreateServer createServerRequest.providerName { createServerRequest | keypairName = keypairName }
            in
                ( { model | viewState = viewState }, Cmd.none )
