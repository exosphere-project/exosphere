module View exposing (view)

import Html exposing (Html, button, div, fieldset, h2, input, label, p, strong, table, td, text, textarea, th, tr)
import Html.Attributes exposing (cols, placeholder, rows, type_, value)
import Html.Events exposing (onClick, onInput)
import Base64
import Filesize exposing (format)
import Types exposing (..)


view : Model -> Html Msg
view model =
    div []
        [ viewMessages model
        , case model.viewState of
            Login ->
                div [] []

            _ ->
                viewNav model
        , case model.viewState of
            Login ->
                viewLogin model

            Home ->
                div [] []

            ListImages ->
                viewImages model

            ListUserServers ->
                viewServers model

            ServerDetail server ->
                viewServerDetail server

            CreateServer createServerRequest ->
                viewCreateServer model createServerRequest
        ]



{- Sub-views for most/all pages -}


viewMessages : Model -> Html Msg
viewMessages model =
    div [] (List.map renderMessage model.messages)


viewNav : Model -> Html Msg
viewNav _ =
    div []
        [ h2 [] [ text "Navigation" ]
        , button [ onClick (ChangeViewState Home) ] [ text "Home" ]
        , button [ onClick (ChangeViewState ListImages) ] [ text "Images" ]
        , button [ onClick (ChangeViewState ListUserServers) ] [ text "My Servers" ]
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
        , button [ onClick RequestAuth ] [ text "Log in" ]
        ]


viewImages : Model -> Html Msg
viewImages model =
    div [] (List.map renderImage model.images)


viewServers : Model -> Html Msg
viewServers model =
    div [] (List.map renderServer model.servers)


viewServerDetail : Server -> Html Msg
viewServerDetail server =
    case server.details of
        Nothing ->
            text "Retrieving details??"

        Just details ->
            div []
                [ table []
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


viewCreateServer : Model -> CreateServerRequest -> Html Msg
viewCreateServer model createServerRequest =
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
                    [ input
                        [ type_ "text"
                        , value createServerRequest.imageUuid
                        , onInput (InputCreateServerImage createServerRequest)
                        ]
                        []
                    ]
                ]
            , tr []
                [ td [] [ text "Size" ]
                , td []
                    [ viewFlavorPicker model.flavors createServerRequest
                    ]
                ]
            , tr []
                [ td [] [ text "SSH Keypair" ]
                , td []
                    [ viewKeypairPicker model.keypairs createServerRequest
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
                            , onInput (InputCreateServerUserData createServerRequest)
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


renderImage : Image -> Html Msg
renderImage image =
    div []
        [ p [] [ strong [] [ text image.name ] ]
        , button [ onClick (ChangeViewState (CreateServer (CreateServerRequest "" image.uuid "" "" ""))) ] [ text "Launch" ]
        , table []
            [ tr []
                [ th [] [ text "Property" ]
                , th [] [ text "Value" ]
                ]
            , tr []
                [ td [] [ text "Size" ]
                , td [] [ text (format image.size) ]
                ]
            , tr []
                [ td [] [ text "Checksum" ]
                , td [] [ text image.checksum ]
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
            ]
        ]


renderServer : Server -> Html Msg
renderServer server =
    div []
        [ p [] [ strong [] [ text server.name ] ]
        , text ("UUID: " ++ server.uuid)
        , button [ onClick (ChangeViewState (ServerDetail server)) ] [ text "Details" ]
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


viewFlavorPicker : List Flavor -> CreateServerRequest -> Html Msg
viewFlavorPicker flavors createServerRequest =
    let
        viewFlavorPickerLabel flavor =
            label []
                [ input [ type_ "radio", onClick (InputCreateServerSize createServerRequest flavor.uuid) ] []
                , text flavor.name
                ]
    in
        fieldset [] (List.map viewFlavorPickerLabel flavors)


viewKeypairPicker : List Keypair -> CreateServerRequest -> Html Msg
viewKeypairPicker keypairs createServerRequest =
    let
        viewKeypairPickerLabel keypair =
            label []
                [ input [ type_ "radio", onClick (InputCreateServerKeypairName createServerRequest keypair.name) ] []
                , text keypair.name
                ]
    in
        fieldset [] (List.map viewKeypairPickerLabel keypairs)
