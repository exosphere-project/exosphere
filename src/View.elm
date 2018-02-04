module View exposing (view)

import Html exposing (Html, a, button, div, fieldset, h2, input, label, legend, p, strong, table, td, text, textarea, th, tr)
import Html.Attributes exposing (cols, for, name, hidden, href, placeholder, rows, type_, value, class, checked, disabled)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import Base64
import Filesize exposing (format)
import Types.Types exposing (..)
import Helpers


view : Model -> Html Msg
view model =
    div []
        [ viewMessages model
        , viewNav model
        , case model.viewState of
            Login ->
                viewLogin model

            ListAllServers ->
                div [] [ viewAllServers (model.selectedProvider :: model.otherProviders) ]

            ProviderHome ->
                div []
                    [ p []
                        [ text ("Home page for " ++ model.selectedProvider.name ++ ", todo put things here")
                        ]
                    ]

            ListImages ->
                div []
                    [ viewImages model.selectedProvider
                    ]

            ListProviderServers ->
                div []
                    [ viewServersWithActions model.selectedProvider
                    ]

            ServerDetail serverUuid ->
                div []
                    [ viewServerDetail model.selectedProvider serverUuid
                    ]

            CreateServer createServerRequest ->
                div []
                    [ viewCreateServer model.selectedProvider createServerRequest
                    ]
        ]



{- Sub-views for most/all pages -}


viewMessages : Model -> Html Msg
viewMessages model =
    div [] (List.map renderMessage model.messages)


viewNav : Model -> Html Msg
viewNav model =
    div []
        [ viewOverallNav model
        , viewProviderSpecificNav model
        ]


viewOverallNav : Model -> Html Msg
viewOverallNav model =
    div []
        [ button [ onClick (ChangeViewState ListAllServers) ] [ text "All My Stuff" ]
        , div []
            [ text model.selectedProvider.name
            , div [] (List.map renderProviderPicker model.otherProviders)
            ]
        , button [ onClick (ChangeViewState Login) ] [ text "Add Provider" ]
        ]


viewProviderSpecificNav : Model -> Html Msg
viewProviderSpecificNav model =
    div []
        [ h2 [] [ text "Provider-specific Navigation" ]
        , button [ onClick (ChangeViewState ProviderHome) ] [ text "Home" ]
        , button [ onClick (ChangeViewState ListProviderServers) ] [ text "My Servers" ]
        , button [ onClick (ChangeViewState ListImages) ] [ text "Create Server" ]
        ]



{- Resource-specific views -}


viewLogin : Model -> Html Msg
viewLogin model =
    div []
        [ h2 [] [ text "Please log in" ]
        , p [] [ text "Either enter your credentials..." ]
        , table []
            [ tr []
                [ td [] [ text "Keystone auth URL" ]
                , td []
                    [ input
                        [ type_ "text"
                        , value model.creds.authURL
                        , placeholder "Auth URL e.g. https://mycloud.net:5000/v3"
                        , onInput InputAuthURL
                        ]
                        []
                    ]
                ]
            , tr []
                [ td [] [ text "Project Domain" ]
                , td []
                    [ input
                        [ type_ "text"
                        , value model.creds.projectDomain
                        , onInput InputProjectDomain
                        ]
                        []
                    ]
                ]
            , tr []
                [ td [] [ text "Project Name" ]
                , td []
                    [ input
                        [ type_ "text"
                        , value model.creds.projectName
                        , onInput InputProjectName
                        ]
                        []
                    ]
                ]
            , tr []
                [ td [] [ text "User Domain" ]
                , td []
                    [ input
                        [ type_ "text"
                        , value model.creds.userDomain
                        , onInput InputUserDomain
                        ]
                        []
                    ]
                ]
            , tr []
                [ td [] [ text "User Name" ]
                , td []
                    [ input
                        [ type_ "text"
                        , value model.creds.username
                        , onInput InputUsername
                        ]
                        []
                    ]
                ]
            , tr []
                [ td [] [ text "Password" ]
                , td []
                    [ input
                        [ type_ "password"
                        , value model.creds.password
                        , onInput InputPassword
                        ]
                        []
                    ]
                ]
            ]
        , p []
            [ text "...or paste an "
              {-
                 Todo this link opens in Electron, should open in user's browser
                 https://github.com/electron/electron/blob/master/docs/api/shell.md#shellopenexternalurl-options-callback
              -}
            , a
                [ href "https://docs.openstack.org/newton/install-guide-rdo/keystone-openrc.html" ]
                [ text "OpenRC"
                ]
            , text " file"
            ]
        , textarea
            [ rows 10
            , cols 40
            , onInput (InputOpenRc)
            , placeholder "export..."
            ]
            []
        , div []
            [ button [ onClick RequestNewProviderToken ] [ text "Log in" ]
            ]
        ]


viewImages : Provider -> Html Msg
viewImages provider =
    case List.isEmpty provider.images of
        True ->
            div [] [ p [] [ text "Images loading" ] ]

        False ->
            div []
                [ h2 [] [ text "Choose an image" ]
                , div [] (List.map (renderImage provider) provider.images)
                ]


viewAllServers : List Provider -> Html Msg
viewAllServers providers =
    let
        providerServerTuples provider =
            List.map (\s -> ( provider, s )) provider.servers

        providersServerTuples =
            List.concatMap providerServerTuples providers
    in
        div [] (List.map (\( p, s ) -> renderServer p s) providersServerTuples)


viewServersWithActions : Provider -> Html Msg
viewServersWithActions provider =
    case List.isEmpty provider.servers of
        True ->
            div [] [ p [] [ text "You don't have any servers yet, go create one!" ] ]

        False ->
            let
                noServersSelected =
                    List.any .selected provider.servers |> not

                allServersSelected =
                    List.all .selected provider.servers

                selectedServers =
                    List.filter .selected provider.servers
            in
                div []
                    [ h2 [] [ text "My Servers" ]
                    , div []
                        [ fieldset []
                            [ legend [] [ text "Bulk Actions" ]
                            , input
                                [ type_ "checkbox"
                                , name "toggle-all"
                                , checked allServersSelected
                                , onClick (SelectAllServers (not allServersSelected))
                                ]
                                []
                            , label
                                [ for "toggle-all" ]
                                [ text "Select All" ]
                            , button
                                [ disabled noServersSelected
                                , onClick (RequestDeleteServers selectedServers)
                                ]
                                [ text "Delete" ]
                            ]
                        ]
                    , div [] (List.map (renderServer provider) provider.servers)
                    ]


viewServerDetail : Provider -> ServerUuid -> Html Msg
viewServerDetail provider serverUuid =
    let
        maybeServer =
            Helpers.serverLookup provider serverUuid
    in
        case maybeServer of
            Nothing ->
                text "No server found"

            Just server ->
                case server.details of
                    Nothing ->
                        text "Retrieving details??"

                    Just details ->
                        let
                            maybeFlavor =
                                Helpers.flavorLookup provider details.flavorUuid

                            flavorText =
                                case maybeFlavor of
                                    Just flavor ->
                                        flavor.name

                                    Nothing ->
                                        "Unknown flavor"

                            maybeImage =
                                Helpers.imageLookup provider details.imageUuid

                            imageText =
                                case maybeImage of
                                    Just image ->
                                        image.name

                                    Nothing ->
                                        "Unknown image"
                        in
                            div []
                                [ h2 [] [ text "Server details" ]
                                , table []
                                    [ tr []
                                        [ th [] [ text "Property" ]
                                        , th [] [ text "Value" ]
                                        ]
                                    , tr []
                                        [ td [] [ text "Name" ]
                                        , td [] [ text server.name ]
                                        ]
                                    , tr []
                                        [ td [] [ text "UUID" ]
                                        , td [] [ text server.uuid ]
                                        ]
                                    , tr []
                                        [ td [] [ text "Created on" ]
                                        , td [] [ text details.created ]
                                        ]
                                    , tr []
                                        [ td [] [ text "Status" ]
                                        , td [] [ text details.status ]
                                        ]
                                    , tr []
                                        [ td [] [ text "Power state" ]
                                        , td [] [ text (toString details.powerState) ]
                                        ]
                                    , tr []
                                        [ td [] [ text "Image" ]
                                        , td [] [ text imageText ]
                                        ]
                                    , tr []
                                        [ td [] [ text "Flavor" ]
                                        , td [] [ text flavorText ]
                                        ]
                                    , tr []
                                        [ td [] [ text "SSH Key Name" ]
                                        , td [] [ text details.keypairName ]
                                        ]
                                    , tr []
                                        [ td [] [ text "IP addresses" ]
                                        , td [] [ renderIpAddresses details.ipAddresses ]
                                        ]
                                    ]
                                ]


viewCreateServer : Provider -> CreateServerRequest -> Html Msg
viewCreateServer provider createServerRequest =
    div []
        [ h2 [] [ text "Create Server" ]
        , table []
            [ tr []
                [ th [] [ text "Property" ]
                , th [] [ text "Value" ]
                ]
            , tr []
                [ td [] [ text "Server Name" ]
                , td []
                    [ input
                        [ type_ "text"
                        , placeholder "My Server"
                        , value createServerRequest.name
                        , onInput (InputCreateServerName createServerRequest)
                        ]
                        []
                    ]
                ]
            , tr []
                [ td [] [ text "Image" ]
                , td []
                    [ text createServerRequest.imageName
                    ]
                ]
            , tr []
                [ td [] [ text "How Many?" ]
                , td []
                    [ input
                        [ type_ "number"
                        , Attr.min "1"
                        , Attr.max "10"
                        , value createServerRequest.count
                        , onInput (InputCreateServerCount createServerRequest)
                        ]
                        []
                    ]
                ]
            , tr []
                [ td [] [ text "Size" ]
                , td []
                    [ viewFlavorPicker provider createServerRequest
                    ]
                ]
            , tr []
                [ td [] [ text "SSH Keypair" ]
                , td []
                    [ viewKeypairPicker provider createServerRequest
                    ]
                ]
            , tr []
                [ td []
                    [ text "User Data"
                    , Html.br [] []
                    , text "(Boot Script)"
                    ]
                , td []
                    [ div []
                        [ textarea
                            [ value createServerRequest.userData
                            , rows 20
                            , cols 80
                            , onInput (InputCreateServerUserData createServerRequest)
                            , placeholder "#!/bin/bash"
                            ]
                            []
                        ]
                    , div [] [ text (getEffectiveUserDataSize createServerRequest) ]
                    ]
                ]
            ]
        , button [ onClick (RequestCreateServer createServerRequest) ] [ text "Create" ]
        ]



{- View Helpers -}


renderMessage : String -> Html Msg
renderMessage message =
    p [] [ text message ]


renderProviderPicker : Provider -> Html Msg
renderProviderPicker provider =
    case provider.name of
        "" ->
            div [] []

        _ ->
            button [ onClick (SelectProvider provider.name) ] [ text provider.name ]


renderImage : Provider -> Image -> Html Msg
renderImage provider image =
    let
        size =
            case image.size of
                Just size ->
                    format size

                Nothing ->
                    "N/A"

        checksum =
            case image.checksum of
                Just checksum ->
                    toString checksum

                Nothing ->
                    "N/A"
    in
        div []
            [ p [] [ strong [] [ text image.name ] ]
            , button [ onClick (ChangeViewState (CreateServer (CreateServerRequest "" provider.name image.uuid image.name "1" "" "" ""))) ] [ text "Launch" ]
            , table []
                [ tr []
                    [ th [] [ text "Property" ]
                    , th [] [ text "Value" ]
                    ]
                , tr []
                    [ td [] [ text "Status" ]
                    , td [] [ text (toString image.status) ]
                    ]
                , tr []
                    [ td [] [ text "Size" ]
                    , td [] [ text size ]
                    ]
                , tr []
                    [ td [] [ text "Checksum" ]
                    , td [] [ text checksum ]
                    ]
                , tr []
                    [ td [] [ text "Disk format" ]
                    , td [] [ text image.diskFormat ]
                    ]
                , tr []
                    [ td [] [ text "Container format" ]
                    , td [] [ text image.containerFormat ]
                    ]
                , tr []
                    [ td [] [ text "UUID" ]
                    , td [] [ text image.uuid ]
                    ]
                , tr []
                    [ td [] [ text "Tags" ]
                    , td [] [ text (List.foldl (\a b -> a ++ ", " ++ b) "" image.tags) ]
                    ]
                ]
            ]


renderServer : Provider -> Server -> Html Msg
renderServer provider server =
    div []
        [ p []
            [ input
                [ type_ "checkbox"
                , checked server.selected
                , onClick (SelectServer server (not server.selected))
                ]
                []
            , strong [] [ text server.name ]
            ]
        , text ("UUID: " ++ server.uuid)
        , button [ onClick (ChangeViewState (ServerDetail server.uuid)) ] [ text "Details" ]
        , button [ onClick (RequestDeleteServer server) ] [ text "Delete" ]
        ]


getEffectiveUserDataSize : CreateServerRequest -> String
getEffectiveUserDataSize createServerRequest =
    let
        rawLength =
            String.length createServerRequest.userData

        base64Value =
            Base64.encode createServerRequest.userData

        base64Length =
            String.length base64Value
    in
        Basics.toString rawLength
            ++ " characters,  "
            ++ Basics.toString base64Length
            ++ "/16384 allowed bytes (Base64 encoded)"


renderIpAddresses : List IpAddress -> Html Msg
renderIpAddresses ipAddresses =
    div [] (List.map renderIpAddress ipAddresses)


renderIpAddress : IpAddress -> Html Msg
renderIpAddress ipAddress =
    p []
        [ text (toString ipAddress.openstackType ++ ": " ++ ipAddress.address)
        ]


viewFlavorPicker : Provider -> CreateServerRequest -> Html Msg
viewFlavorPicker provider createServerRequest =
    let
        viewFlavorPickerLabel flavor =
            label []
                [ input [ type_ "radio", onClick (InputCreateServerSize createServerRequest flavor.uuid) ] []
                , text flavor.name
                ]
    in
        fieldset [] (List.map viewFlavorPickerLabel provider.flavors)


viewKeypairPicker : Provider -> CreateServerRequest -> Html Msg
viewKeypairPicker provider createServerRequest =
    let
        viewKeypairPickerLabel keypair =
            label []
                [ input [ type_ "radio", onClick (InputCreateServerKeypairName createServerRequest keypair.name) ] []
                , text keypair.name
                ]
    in
        fieldset [] (List.map viewKeypairPickerLabel provider.keypairs)
