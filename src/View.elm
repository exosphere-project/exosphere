module View exposing (view)

import Html exposing (Html, button, div, fieldset, h2, input, label, legend, p, strong, table, td, text, textarea, th, tr)
import Html.Attributes exposing (cols, for, name, hidden, placeholder, rows, type_, value, class, checked, disabled)
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
        , viewProviderPicker model
        , case model.viewState of
            Login ->
                viewLogin model

            Home providerName ->
                case Helpers.providerLookup model providerName of
                    Nothing ->
                        text "Provider not found"

                    Just provider ->
                        div []
                            [ p []
                                [ viewNav provider
                                , text ("Home page for " ++ provider.name ++ ", todo put things here")
                                ]
                            ]

            ListImages providerName ->
                case Helpers.providerLookup model providerName of
                    Nothing ->
                        text "Provider not found"

                    Just provider ->
                        div []
                            [ viewNav provider
                            , viewImages provider
                            ]

            ListUserServers providerName ->
                case Helpers.providerLookup model providerName of
                    Nothing ->
                        text "Provider not found"

                    Just provider ->
                        div []
                            [ viewNav provider
                            , viewServers provider
                            ]

            ServerDetail providerName serverUuid ->
                case Helpers.providerLookup model providerName of
                    Nothing ->
                        text "Provider not found"

                    Just provider ->
                        div []
                            [ viewNav provider
                            , viewServerDetail provider serverUuid
                            ]

            CreateServer providerName createServerRequest ->
                case Helpers.providerLookup model providerName of
                    Nothing ->
                        text "Provider not found"

                    Just provider ->
                        div []
                            [ viewNav provider
                            , viewCreateServer provider createServerRequest
                            ]
        ]



{- Sub-views for most/all pages -}


viewMessages : Model -> Html Msg
viewMessages model =
    div [] (List.map renderMessage model.messages)


viewProviderPicker : Model -> Html Msg
viewProviderPicker model =
    div []
        [ h2 [] [ text "Providers" ]
        , button [ onClick (ChangeViewState Login) ] [ text "Add Provider" ]
        , div [] (List.map renderProviderPicker model.providers)
        ]


viewNav : Provider -> Html Msg
viewNav provider =
    div []
        [ h2 [] [ text "Navigation" ]
        , button [ onClick (ChangeViewState (Home provider.name)) ] [ text "Home" ]
        , button [ onClick (ChangeViewState (ListUserServers provider.name)) ] [ text "My Servers" ]
        , button [ onClick (ChangeViewState (ListImages provider.name)) ] [ text "Create Server" ]
        ]



{- Resource-specific views -}


viewLogin : Model -> Html Msg
viewLogin model =
    div []
        [ h2 [] [ text "Please log in" ]
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
                        [ type_ "text"
                        , value model.creds.password
                        , onInput InputPassword
                        ]
                        []
                    ]
                ]
            ]
        , button [ onClick RequestNewProviderToken ] [ text "Log in" ]
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


viewServers : Provider -> Html Msg
viewServers provider =
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
                                , onClick (SelectAllServers provider.name (not allServersSelected))
                                ]
                                []
                            , label
                                [ for "toggle-all" ]
                                [ text "Select All" ]
                            , button
                                [ disabled noServersSelected
                                , onClick (RequestDeleteServers provider.name selectedServers)
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
                        , onInput (InputCreateServerName provider.name createServerRequest)
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
                        , onInput (InputCreateServerCount provider.name createServerRequest)
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
                [ td [] [ text "User Data" ]
                , td []
                    [ div []
                        [ textarea
                            [ value createServerRequest.userData
                            , rows 20
                            , cols 80
                            , onInput (InputCreateServerUserData provider.name createServerRequest)
                            ]
                            []
                        ]
                    , div [] [ text (getEffectiveUserDataSize createServerRequest) ]
                    ]
                ]
            ]
        , button [ onClick (RequestCreateServer provider.name createServerRequest) ] [ text "Create" ]
        ]



{- View Helpers -}


renderMessage : String -> Html Msg
renderMessage message =
    p [] [ text message ]


renderProviderPicker : Provider -> Html Msg
renderProviderPicker provider =
    button [ onClick (ChangeViewState (Home provider.name)) ] [ text provider.name ]


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
            , button [ onClick (ChangeViewState (CreateServer provider.name (CreateServerRequest "" provider.name image.uuid image.name "1" "" "" ""))) ] [ text "Launch" ]
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
                , onClick (SelectServer provider.name server (not server.selected))
                ]
                []
            , strong [] [ text server.name ]
            ]
        , text ("UUID: " ++ server.uuid)
        , button [ onClick (ChangeViewState (ServerDetail provider.name server.uuid)) ] [ text "Details" ]
        , button [ onClick (RequestDeleteServer provider.name server) ] [ text "Delete" ]
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
                [ input [ type_ "radio", onClick (InputCreateServerSize provider.name createServerRequest flavor.uuid) ] []
                , text flavor.name
                ]
    in
        fieldset [] (List.map viewFlavorPickerLabel provider.flavors)


viewKeypairPicker : Provider -> CreateServerRequest -> Html Msg
viewKeypairPicker provider createServerRequest =
    let
        viewKeypairPickerLabel keypair =
            label []
                [ input [ type_ "radio", onClick (InputCreateServerKeypairName provider.name createServerRequest keypair.name) ] []
                , text keypair.name
                ]
    in
        fieldset [] (List.map viewKeypairPickerLabel provider.keypairs)
