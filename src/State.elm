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
      , selectedProvider =
            Provider
                ""
                ""
                { glance = "", nova = "", neutron = "" }
                []
                []
                []
                []
                []
                []
      , otherProviders = []
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
                ListUserServers ->
                    ( model, Rest.requestServers model.selectedProvider )

                ServerDetail serverUuid ->
                    ( model
                    , Rest.requestServerDetail model.selectedProvider serverUuid
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

                    ListImages ->
                        ( newModel, Rest.requestImages model.selectedProvider )

                    ListUserServers ->
                        ( newModel, Rest.requestServers model.selectedProvider )

                    ServerDetail serverUuid ->
                        ( newModel
                        , Rest.requestServerDetail model.selectedProvider serverUuid
                        )

                    CreateServer _ ->
                        ( newModel
                        , Cmd.batch
                            [ Rest.requestFlavors newModel.selectedProvider
                            , Rest.requestKeypairs newModel.selectedProvider
                            ]
                        )

        RequestNewProviderToken ->
            ( model, Rest.requestAuthToken model )

        SelectProvider providerName ->
            case Helpers.providerLookup model providerName of
                Just provider ->
                    let
                        allProviders =
                            model.selectedProvider :: model.otherProviders

                        selectedProvider =
                            provider

                        otherProviders =
                            List.filter
                                (\p -> p.name /= selectedProvider.name)
                                allProviders

                        newModel =
                            { model
                                | selectedProvider = selectedProvider
                                , otherProviders = otherProviders
                            }
                    in
                        ( newModel, Cmd.none )

                Nothing ->
                    Helpers.processError model "Provider not found"

        RequestCreateServer createServerRequest ->
            ( model
            , Rest.requestCreateServer model.selectedProvider createServerRequest
            )

        RequestDeleteServer server ->
            let
                oldProvider =
                    model.selectedProvider

                newProvider =
                    { oldProvider
                        | servers =
                            List.filter
                                (\s -> s /= server)
                                oldProvider.servers
                    }

                newModel =
                    { model | selectedProvider = newProvider }
            in
                ( newModel, Rest.requestDeleteServer newProvider server )

        ReceiveAuthToken response ->
            Rest.receiveAuthToken model response

        ReceiveImages result ->
            Rest.receiveImages model result

        RequestDeleteServers serversToDelete ->
            let
                oldProvider =
                    model.selectedProvider

                newProvider =
                    { oldProvider | servers = List.filter (\s -> (not (List.member s serversToDelete))) oldProvider.servers }

                newModel =
                    { model | selectedProvider = newProvider }
            in
                ( newModel
                , Rest.requestDeleteServers model.selectedProvider serversToDelete
                )

        SelectServer server newSelectionState ->
            let
                updateServer someServer =
                    if someServer.uuid == server.uuid then
                        { someServer | selected = newSelectionState }
                    else
                        someServer

                oldProvider =
                    model.selectedProvider

                newProvider =
                    { oldProvider
                        | servers =
                            List.map updateServer oldProvider.servers
                    }

                newModel =
                    { model | selectedProvider = newProvider }
            in
                newModel
                    ! []

        SelectAllServers allServersSelected ->
            let
                updateServer someServer =
                    { someServer | selected = allServersSelected }

                oldProvider =
                    model.selectedProvider

                newProvider =
                    { oldProvider | servers = List.map updateServer oldProvider.servers }

                newModel =
                    { model | selectedProvider = newProvider }
            in
                newModel
                    ! []

        ReceiveServers result ->
            Rest.receiveServers model result

        ReceiveServerDetail serverUuid result ->
            Rest.receiveServerDetail model serverUuid result

        ReceiveFlavors result ->
            Rest.receiveFlavors model result

        ReceiveKeypairs result ->
            Rest.receiveKeypairs model result

        ReceiveCreateServer result ->
            Rest.receiveCreateServer model result

        ReceiveDeleteServer _ ->
            {- Todo this ignores the result of server deletion API call, we should display errors to user -}
            update (ChangeViewState Home) model

        ReceiveNetworks result ->
            Rest.receiveNetworks model result

        GetFloatingIpReceivePorts server result ->
            Rest.receivePortsAndRequestFloatingIp model server result

        ReceiveFloatingIp server result ->
            Rest.receiveFloatingIp model server result

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

        InputCreateServerName createServerRequest name ->
            let
                viewState =
                    CreateServer { createServerRequest | name = name }
            in
                ( { model | viewState = viewState }, Cmd.none )

        InputCreateServerCount createServerRequest count ->
            let
                viewState =
                    CreateServer { createServerRequest | count = count }
            in
                ( { model | viewState = viewState }, Cmd.none )

        InputCreateServerUserData createServerRequest userData ->
            let
                viewState =
                    CreateServer { createServerRequest | userData = userData }
            in
                ( { model | viewState = viewState }, Cmd.none )

        InputCreateServerSize createServerRequest flavorUuid ->
            let
                viewState =
                    CreateServer { createServerRequest | flavorUuid = flavorUuid }
            in
                ( { model | viewState = viewState }, Cmd.none )

        InputCreateServerKeypairName createServerRequest keypairName ->
            let
                viewState =
                    CreateServer { createServerRequest | keypairName = keypairName }
            in
                ( { model | viewState = viewState }, Cmd.none )
