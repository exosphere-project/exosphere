module View exposing (view)

import Base64
import Filesize exposing (format)
import Helpers
import Html exposing (Html, a, button, div, fieldset, h2, h3, input, label, legend, li, p, strong, table, td, text, textarea, th, tr, ul)
import Html.Attributes exposing (cols, for, name, hidden, href, placeholder, rows, type_, value, class, checked, disabled)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import Maybe
import Toast
import Types.Types exposing (..)


view : Model -> Html Msg
view model =
    div []
        [ notificationsView model
        , viewProviderPicker model
        , case model.viewState of
            NonProviderView viewConstructor ->
                case viewConstructor of
                    Login ->
                        viewLogin model

            ProviderView providerName viewConstructor ->
                case Helpers.providerLookup model providerName of
                    Nothing ->
                        text "Oops! Provider not found"

                    Just provider ->
                        providerView model provider viewConstructor
        , viewMessages model
        ]


notificationsView : Model -> Html Msg
notificationsView model =
    Html.ul [ class "notifications-list" ] (Toast.views model.toast model.time notificationView)


notificationView : Toast.NotificationState -> String -> Html Msg
notificationView state notification =
    Html.li [] [ Html.text notification ]


providerView : Model -> Provider -> ProviderViewConstructor -> Html Msg
providerView model provider viewConstructor =
    case viewConstructor of
        ProviderHome ->
            div []
                [ p []
                    [ viewNav provider
                    , text ("Home page for " ++ provider.name ++ ", todo put things here")
                    ]
                ]

        ListImages ->
            div []
                [ viewNav provider
                , viewImagesIfLoaded provider model.imageFilterTag
                ]

        ListProviderServers ->
            div []
                [ viewNav provider
                , viewServers provider
                ]

        ServerDetail serverUuid ->
            div []
                [ viewNav provider
                , viewServerDetail provider serverUuid
                ]

        CreateServer createServerRequest ->
            div []
                [ viewNav provider
                , viewCreateServer provider createServerRequest
                ]



{- Sub-views for most/all pages -}


viewMessages : Model -> Html Msg
viewMessages model =
    div [ Attr.style [ ( "height", "240px" ), ( "border", "1px black solid" ) ] ]
        [ h3 [] [ text "Messages" ]
        , ul [ Attr.style [ ( "height", "180px" ), ( "overflow", "auto" ) ] ]
            (List.map renderMessage model.messages)
        ]


viewProviderPicker : Model -> Html Msg
viewProviderPicker model =
    div []
        [ h2 [] [ text "Providers" ]
        , div []
            [ div [] (List.map (renderProviderPicker model) model.providers)
            ]
        , button [ onClick (SetNonProviderView Login) ] [ text "Add Provider" ]
        ]


viewNav : Provider -> Html Msg
viewNav provider =
    div []
        [ h2 [] [ text "Navigation" ]
        , button [ onClick (ProviderMsg provider.name (SetProviderView ProviderHome)) ] [ text "Home" ]
        , button [ onClick (ProviderMsg provider.name (SetProviderView ListProviderServers)) ] [ text "My Servers" ]
        , button [ onClick (ProviderMsg provider.name (SetProviderView (ListImages))) ] [ text "Create Server" ]
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
                        , value model.creds.authUrl
                        , placeholder "Auth URL e.g. https://mycloud.net:5000/v3"
                        , onInput (\u -> InputLoginField (AuthUrl u))
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
                        , onInput (\d -> InputLoginField (ProjectDomain d))
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
                        , onInput (\pn -> InputLoginField (ProjectName pn))
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
                        , onInput (\d -> InputLoginField (UserDomain d))
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
                        , onInput (\u -> InputLoginField (Username u))
                        ]
                        []
                    ]
                ]
            , tr []
                [ td [] [ text "Password" ]
                , td []
                    [ input
                        [ type_ "password"
                        , Attr.style (Helpers.providePasswordHint model.creds.username model.creds.password)
                        , value model.creds.password
                        , onInput (\p -> InputLoginField (Password p))
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
            , onInput (\o -> InputLoginField (OpenRc o))
            , placeholder "export..."
            ]
            []
        , div []
            [ button [ onClick RequestNewProviderToken ] [ text "Log in" ]
            ]
        ]


viewImagesIfLoaded : Provider -> Maybe String -> Html Msg
viewImagesIfLoaded provider maybeFilterTag =
    case List.isEmpty provider.images of
        True ->
            div [] [ p [] [ text "Images loading" ] ]

        False ->
            viewImages provider maybeFilterTag


viewImages : Provider -> Maybe String -> Html Msg
viewImages provider maybeFilterTag =
    let
        imageContainsTag tag image =
            List.member tag image.tags

        filteredImages =
            case maybeFilterTag of
                Nothing ->
                    provider.images

                Just filterTag ->
                    List.filter (imageContainsTag filterTag) provider.images

        noMatchWarning =
            (maybeFilterTag /= Nothing) && (List.length filteredImages == 0)

        displayedImages =
            if noMatchWarning == False then
                filteredImages
            else
                provider.images
    in
        div []
            [ h2 [] [ text "Choose an image" ]
            , div []
                [ text "Filter on tag: "
                , input
                    [ type_ "text"
                    , value (Maybe.withDefault "" maybeFilterTag)
                    , onInput (\t -> InputImageFilterTag t)
                    , placeholder "try \"distro-base\""
                    ]
                    []
                , Html.br [] []
                , button
                    [ onClick (InputImageFilterTag "")
                    ]
                    [ text "Clear filter (show all)" ]
                , Html.br [] []
                , if noMatchWarning then
                    text "No matches found, showing all images"
                  else
                    div [] []
                ]
            , div [] (List.map (renderImage provider) displayedImages)
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
                                , onClick (ProviderMsg provider.name (SelectAllServers (not allServersSelected)))
                                ]
                                []
                            , label
                                [ for "toggle-all" ]
                                [ text "Select All" ]
                            , button
                                [ disabled noServersSelected
                                , onClick (ProviderMsg provider.name (RequestDeleteServers selectedServers))
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
                        , onInput (\n -> InputCreateServerField createServerRequest (CreateServerName n))
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
                        , onInput (\c -> InputCreateServerField createServerRequest (CreateServerCount c))
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
                            , onInput (\u -> InputCreateServerField createServerRequest (CreateServerUserData u))
                            , placeholder "#!/bin/bash"
                            ]
                            []
                        ]
                    , div [] [ text (getEffectiveUserDataSize createServerRequest) ]
                    ]
                ]
            ]
        , button [ onClick (ProviderMsg provider.name (RequestCreateServer createServerRequest)) ] [ text "Create" ]
        ]



{- View Helpers -}


renderMessage : String -> Html Msg
renderMessage message =
    li [] [ text message ]


renderProviderPicker : Model -> Provider -> Html Msg
renderProviderPicker model provider =
    let
        isSelected p =
            case model.viewState of
                NonProviderView _ ->
                    False

                ProviderView selectedProvName _ ->
                    p.name == selectedProvName
    in
        case isSelected provider of
            False ->
                button [ onClick (ProviderMsg provider.name (SetProviderView ProviderHome)) ] [ text provider.name ]

            True ->
                text provider.name


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
            , button [ onClick (ProviderMsg provider.name (SetProviderView (CreateServer (CreateServerRequest "" provider.name image.uuid image.name "1" "" "" "")))) ] [ text "Launch" ]
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
                    , td [] [ text (Maybe.withDefault "" image.diskFormat) ]
                    ]
                , tr []
                    [ td [] [ text "Container format" ]
                    , td [] [ text (Maybe.withDefault "" image.containerFormat) ]
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
                , onClick (ProviderMsg provider.name (SelectServer server (not server.selected)))
                ]
                []
            , strong [] [ text server.name ]
            ]
        , text ("UUID: " ++ server.uuid)
        , button [ onClick (ProviderMsg provider.name (SetProviderView (ServerDetail server.uuid))) ] [ text "Details" ]
        , button [ onClick (ProviderMsg provider.name (RequestDeleteServer server)) ] [ text "Delete" ]
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
        sortedFlavors flavors =
            flavors
                |> List.sortBy .disk_ephemeral
                |> List.sortBy .disk_root
                |> List.sortBy .ram_mb
                |> List.sortBy .vcpu

        flavorAsStr flavor =
            flavor.name ++ " (" ++ (toString flavor.vcpu) ++ " CPU, " ++ (flavor.ram_mb // 1024 |> toString) ++ " GB RAM, " ++ (toString flavor.disk_root) ++ " GB root disk, " ++ (toString flavor.disk_ephemeral) ++ " GB ephemeral disk)"

        viewFlavorPickerLabel flavor =
            div []
                [ label []
                    [ input [ type_ "radio", onClick (InputCreateServerField createServerRequest (CreateServerSize flavor.uuid)) ] []
                    , text (flavorAsStr flavor)
                    ]
                , Html.br [] []
                ]
    in
        fieldset [] (List.map viewFlavorPickerLabel (sortedFlavors provider.flavors))


viewKeypairPicker : Provider -> CreateServerRequest -> Html Msg
viewKeypairPicker provider createServerRequest =
    let
        viewKeypairPickerLabel keypair =
            label []
                [ input [ type_ "radio", onClick (InputCreateServerField createServerRequest (CreateServerKeypairName keypair.name)) ] []
                , text keypair.name
                ]
    in
        fieldset [] (List.map viewKeypairPickerLabel provider.keypairs)
