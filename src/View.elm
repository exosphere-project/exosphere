module View exposing (view)

import Base64
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Filesize exposing (format)
import Helpers
import Html exposing (Html, a, button, div, fieldset, h2, input, label, legend, p, strong, table, td, text, textarea, th, tr)
import Html.Attributes as Attr exposing (checked, class, cols, disabled, for, hidden, href, name, placeholder, rows, type_, value)
import Html.Events exposing (onClick, onInput)
import Maybe
import RemoteData
import Types.Types exposing (..)


view : Model -> Html Msg
view model =
    Element.layout []
        (elementView model)


elementView : Model -> Element.Element Msg
elementView model =
    Element.column []
        [ Element.html (viewProviderPicker model)
        , case model.viewState of
            NonProviderView viewConstructor ->
                case viewConstructor of
                    Login ->
                        viewLogin model

            ProviderView providerName viewConstructor ->
                case Helpers.providerLookup model providerName of
                    Nothing ->
                        Element.html (text "Oops! Provider not found")

                    Just provider ->
                        Element.html (providerView model provider viewConstructor)
        , Element.html (viewMessages model)
        ]


providerView : Model -> Provider -> ProviderViewConstructor -> Html Msg
providerView model provider viewConstructor =
    case viewConstructor of
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
    div [] (List.map renderMessage model.messages)


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
        , button [ onClick (ProviderMsg provider.name (SetProviderView ListProviderServers)) ] [ text "My Servers" ]
        , button [ onClick (ProviderMsg provider.name (SetProviderView ListImages)) ] [ text "Create Server" ]
        ]



{- Resource-specific views -}


viewLogin : Model -> Element.Element Msg
viewLogin model =
    Element.column [ Element.spacing 20 ]
        [ Element.el
            [ Region.heading 2
            , Font.size 24
            , Font.bold
            ]
            (Element.text "Please log in")
        , Element.wrappedRow
            [ Element.spacing 10 ]
            [ viewLoginCredsEntry model
            , viewLoginOpenRcEntry model
            ]
        , Element.el [ Element.alignRight ] (uiButton { label = Element.text "Log in", onPress = Just RequestNewProviderToken })
        ]


viewLoginCredsEntry : Model -> Element.Element Msg
viewLoginCredsEntry model =
    Element.column
        [ Element.height Element.fill
        , Element.spacing 15
        ]
        [ Element.el [] (Element.text "Either enter your credentials...")
        , Element.html
            (table []
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
                            ([ type_ "password"
                             , value model.creds.password
                             , onInput (\p -> InputLoginField (Password p))
                             ]
                                ++ List.map (\r -> Attr.style r.styleKey r.styleValue)
                                    (Helpers.providePasswordHint model.creds.username model.creds.password)
                            )
                            []
                        ]
                    ]
                ]
            )
        ]


viewLoginOpenRcEntry : Model -> Element.Element Msg
viewLoginOpenRcEntry model =
    Element.column
        [ Element.height Element.fill
        , Element.spacing 15
        ]
        [ Element.paragraph []
            [ Element.text "...or paste an "

            {-
               Todo this link opens in Electron, should open in user's browser
               https://github.com/electron/electron/blob/master/docs/api/shell.md#shellopenexternalurl-options-callback
            -}
            , Element.link []
                { url = "https://docs.openstack.org/newton/install-guide-rdo/keystone-openrc.html"
                , label = Element.text "OpenRC"
                }
            , Element.text " file"
            ]
        , Input.multiline
            [ Element.width (Element.px 300)
            , Element.height (Element.px 200)
            , Font.size 12
            ]
            { onChange = \o -> InputLoginField (OpenRc o)
            , text = "export..."
            , placeholder = Nothing
            , label = Input.labelLeft [] Element.none
            , spellcheck = False
            }
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
    case provider.servers of
        RemoteData.NotAsked ->
            div [] [ p [] [ text "Please wait..." ] ]

        RemoteData.Loading ->
            div [] [ p [] [ text "Loading..." ] ]

        RemoteData.Failure e ->
            div [] [ p [] [ text ("Cannot display servers. Error message: " ++ Debug.toString e) ] ]

        RemoteData.Success servers ->
            case List.isEmpty servers of
                True ->
                    div [] [ p [] [ text "You don't have any servers yet, go create one!" ] ]

                False ->
                    let
                        noServersSelected =
                            List.any .selected servers |> not

                        allServersSelected =
                            List.all .selected servers

                        selectedServers =
                            List.filter .selected servers
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
                        , div [] (List.map (renderServer provider) servers)
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
                                , td [] [ text (Debug.toString details.powerState) ]
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
                [ td [] [ text "Choose a root disk size:" ]
                , td []
                    [ viewVolBackedPrompt provider createServerRequest
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
    p [] [ text message ]


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
            button [ onClick (ProviderMsg provider.name (SetProviderView ListProviderServers)) ] [ text provider.name ]

        True ->
            text provider.name


renderImage : Provider -> Image -> Html Msg
renderImage provider image =
    let
        size =
            case image.size of
                Just s ->
                    format s

                Nothing ->
                    "N/A"

        checksum =
            case image.checksum of
                Just c ->
                    c

                Nothing ->
                    "N/A"
    in
    div []
        [ p [] [ strong [] [ text image.name ] ]
        , button [ onClick (ProviderMsg provider.name (SetProviderView (CreateServer (CreateServerRequest "" provider.name image.uuid image.name "1" "" False "" "" "")))) ] [ text "Launch" ]
        , table []
            [ tr []
                [ th [] [ text "Property" ]
                , th [] [ text "Value" ]
                ]
            , tr []
                [ td [] [ text "Status" ]
                , td [] [ text (Debug.toString image.status) ]
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
        , if server.deletionAttempted == True then
            text "Deleting..."

          else
            button [ onClick (ProviderMsg provider.name (RequestDeleteServer server)) ] [ text "Delete" ]
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
    String.fromInt rawLength
        ++ " characters,  "
        ++ String.fromInt base64Length
        ++ "/16384 allowed bytes (Base64 encoded)"


renderIpAddresses : List IpAddress -> Html Msg
renderIpAddresses ipAddresses =
    div [] (List.map renderIpAddress ipAddresses)


renderIpAddress : IpAddress -> Html Msg
renderIpAddress ipAddress =
    p []
        [ text (Debug.toString ipAddress.openstackType ++ ": " ++ ipAddress.address)
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
            flavor.name ++ " (" ++ String.fromInt flavor.vcpu ++ " CPU, " ++ (flavor.ram_mb // 1024 |> String.fromInt) ++ " GB RAM, " ++ String.fromInt flavor.disk_root ++ " GB root disk, " ++ String.fromInt flavor.disk_ephemeral ++ " GB ephemeral disk)"

        viewFlavorPickerLabel flavor =
            div []
                [ label []
                    [ input [ type_ "radio", name "flavor", onClick (InputCreateServerField createServerRequest (CreateServerSize flavor.uuid)) ] []
                    , text (flavorAsStr flavor)
                    ]
                , Html.br [] []
                ]
    in
    fieldset [] (List.map viewFlavorPickerLabel (sortedFlavors provider.flavors))


viewVolBackedPrompt : Provider -> CreateServerRequest -> Html Msg
viewVolBackedPrompt provider createServerRequest =
    let
        maybeFlavor =
            List.filter (\f -> f.uuid == createServerRequest.flavorUuid) provider.flavors
                |> List.head

        flavorRootDiskSize =
            case maybeFlavor of
                Nothing ->
                    {- This should be an impossible state -}
                    0

                Just flavor ->
                    flavor.disk_root

        nonVolBackedOptionText =
            if flavorRootDiskSize == 0 then
                "Default for selected image (warning, could be too small for your work)"

            else
                String.fromInt flavorRootDiskSize ++ "  GB (default for selected size)"
    in
    div []
        [ label []
            [ input [ type_ "radio", name "volbacked", onClick (InputCreateServerField createServerRequest (CreateServerVolBacked False)) ] []
            , text nonVolBackedOptionText
            ]
        , Html.br [] []
        , label []
            [ input [ type_ "radio", name "volbacked", onClick (InputCreateServerField createServerRequest (CreateServerVolBacked True)) ] []
            , input
                [ type_ "number"
                , Attr.min "2"
                , value createServerRequest.volBackedSizeGb
                , onInput (\s -> InputCreateServerField createServerRequest (CreateServerVolBackedSize s))
                ]
                []
            , text " GB (will use a volume for root disk)"
            ]
        ]


viewKeypairPicker : Provider -> CreateServerRequest -> Html Msg
viewKeypairPicker provider createServerRequest =
    let
        viewKeypairPickerLabel keypair =
            label []
                [ input [ type_ "radio", name "keypair", onClick (InputCreateServerField createServerRequest (CreateServerKeypairName keypair.name)) ] []
                , text keypair.name
                ]
    in
    fieldset [] (List.map viewKeypairPickerLabel provider.keypairs)



{- Elm UI Doodads -}


uiButton : { onPress : Maybe Msg, label : Element.Element Msg } -> Element.Element Msg
uiButton props =
    Input.button
        [ Element.padding 5
        , Border.rounded 6
        , Border.color (Element.rgb 0 0 0)
        , Border.width 1
        ]
        props
