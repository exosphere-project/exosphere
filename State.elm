module State exposing (init, subscriptions, update)

import Time
import Types exposing (..)
import Rest


{- Todo remove default creds once storing this in local storage -}


init : ( Model, Cmd Msg )
init =
    ( { authToken = "" {- Todo remove the following hard coding and decode JSON in auth token response -}
      , endpoints =
            { glance = "https://tombstone-cloud.cyverse.org:9292"
            , nova = "https://tombstone-cloud.cyverse.org:8774"
            , neutron = "https://tombstone-cloud.cyverse.org:9696"
            }
      , creds =
            Creds
                "https://tombstone-cloud.cyverse.org:8000/v3/auth/tokens"
                "default"
                "demo"
                "default"
                "demo"
                ""
            {- password -}
      , messages = []
      , images = []
      , servers = []
      , viewState = Login
      , flavors = []
      , keypairs = []
      , networks = []
      , ports = []
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
                    ( model, Rest.requestServers model )

                ServerDetail server ->
                    ( model, Rest.requestServerDetail model server )

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
                        ( newModel, Rest.requestImages newModel )

                    ListUserServers ->
                        ( newModel, Rest.requestServers newModel )

                    ServerDetail serverUuid ->
                        ( newModel, Rest.requestServerDetail newModel serverUuid )

                    CreateServer _ ->
                        ( newModel
                        , Cmd.batch
                            [ Rest.requestFlavors newModel
                            , Rest.requestKeypairs newModel
                            ]
                        )

        RequestAuth ->
            ( model, Rest.requestAuthToken model )

        RequestCreateServer createServerRequest ->
            ( model, Rest.requestCreateServer model createServerRequest )

        RequestDeleteServer server ->
            ( { model | servers = List.filter (\s -> s /= server) model.servers }
            , Rest.requestDeleteServer model server
            )

        ReceiveAuth response ->
            Rest.receiveAuth model response

        ReceiveImages result ->
            Rest.receiveImages model result

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

        InputCreateServerImage createServerRequest imageUuid ->
            let
                viewState =
                    CreateServer { createServerRequest | imageUuid = imageUuid }
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
