module View exposing (view)

import Base64
import Color
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Filesize exposing (format)
import Framework.Button as Button
import Framework.Card as Card
import Framework.Color
import Framework.Modifier as Modifier
import Framework.Spinner as Spinner
import Helpers.Helpers as Helpers
import Html exposing (Html)
import Html.Attributes
import Http
import Maybe
import OpenStack.ServerActions as ServerActions
import OpenStack.Types as OSTypes
import RemoteData
import Style.Widgets.Card as ExoCard
import Style.Widgets.Icon as Icon
import Style.Widgets.IconButton as IconButton
import Style.Widgets.MenuItem as MenuItem
import Toasty
import Toasty.Defaults
import Types.Types exposing (..)


navMenuWidth : Int
navMenuWidth =
    180


navBarHeight : Int
navBarHeight =
    70


view : Model -> Html Msg
view model =
    Element.layout
        [ Font.size 17
        , Font.family
            [ Font.typeface "Open Sans"
            , Font.sansSerif
            ]
        ]
        (elementView model.maybeWindowSize model)


elementView : Maybe WindowSize -> Model -> Element.Element Msg
elementView maybeWindowSize model =
    let
        mainContentContainerView =
            Element.column
                [ Element.padding 10
                , Element.alignTop
                , Element.width <|
                    case maybeWindowSize of
                        Just windowSize ->
                            Element.px (windowSize.width - navMenuWidth)

                        Nothing ->
                            Element.fill
                , Element.height Element.fill
                , Element.scrollbars
                ]
                [ case model.viewState of
                    NonProjectView viewConstructor ->
                        case viewConstructor of
                            Login ->
                                viewLogin model

                            MessageLog ->
                                viewMessageLog model

                    ProjectView projectName viewConstructor ->
                        case Helpers.projectLookup model projectName of
                            Nothing ->
                                Element.text "Oops! Project not found"

                            Just project ->
                                projectView model project viewConstructor
                , Element.html (Toasty.view Helpers.toastConfig toastView ToastyMsg model.toasties)
                ]
    in
    Element.row
        [ Element.padding 0
        , Element.spacing 0
        , Element.width Element.fill
        , Element.height <|
            case maybeWindowSize of
                Just windowSize ->
                    Element.px windowSize.height

                Nothing ->
                    Element.fill
        ]
        [ Element.column
            [ Element.padding 0
            , Element.spacing 0
            , Element.width Element.fill
            , Element.height <|
                case maybeWindowSize of
                    Just windowSize ->
                        Element.px windowSize.height

                    Nothing ->
                        Element.fill
            ]
            [ navBarView model
            , Element.row
                [ Element.padding 0
                , Element.spacing 0
                , Element.width Element.fill
                , Element.height <|
                    case maybeWindowSize of
                        Just windowSize ->
                            Element.px (windowSize.height - navBarHeight)

                        Nothing ->
                            Element.fill
                ]
                [ navMenuView model
                , mainContentContainerView
                ]
            ]
        ]


toastView : Toasty.Defaults.Toast -> Html Msg
toastView toast =
    let
        toastElement =
            case toast of
                Toasty.Defaults.Success title message ->
                    genericToast "toasty-success" title message

                Toasty.Defaults.Warning title message ->
                    genericToast "toasty-warning" title message

                Toasty.Defaults.Error title message ->
                    genericToast "toasty-error" title message
    in
    Element.layoutWith { options = [ Element.noStaticStyleSheet ] } [] toastElement


genericToast : String -> String -> String -> Element.Element Msg
genericToast variantClass title message =
    Element.column
        [ Element.htmlAttribute (Html.Attributes.class "toasty-container")
        , Element.htmlAttribute (Html.Attributes.class variantClass)
        , Element.padding 10
        , Element.spacing 10
        , Font.color (Element.rgb 1 1 1)
        ]
        [ Element.el
            [ Region.heading 1
            , Font.bold
            , Font.size 14
            ]
            (Element.text title)
        , if String.isEmpty message then
            Element.text ""

          else
            Element.paragraph
                [ Element.htmlAttribute (Html.Attributes.class "toasty-message")
                , Font.size 12
                ]
                [ Element.text message
                ]
        ]


projectTitleForNavMenu : Model -> Project -> String
projectTitleForNavMenu model project =
    -- If we have multiple projects on the same provider then append the project name to the provider name
    let
        providerTitle =
            project.creds.authUrl
                |> Helpers.hostnameFromUrl
                |> Helpers.titleFromHostname

        multipleProjects =
            let
                projectCountOnSameProvider =
                    let
                        projectsOnSameProvider : Project -> Project -> Bool
                        projectsOnSameProvider proj1 proj2 =
                            Helpers.hostnameFromUrl proj1.creds.authUrl == Helpers.hostnameFromUrl proj2.creds.authUrl
                    in
                    List.filter (projectsOnSameProvider project) model.projects
                        |> List.length
            in
            projectCountOnSameProvider > 1
    in
    case multipleProjects of
        True ->
            providerTitle ++ String.fromChar '\n' ++ project.creds.projectName

        False ->
            providerTitle


navMenuView : Model -> Element.Element Msg
navMenuView model =
    let
        projectMenuItem : Project -> Element.Element Msg
        projectMenuItem project =
            let
                projectTitle =
                    projectTitleForNavMenu model project

                status =
                    case model.viewState of
                        ProjectView p _ ->
                            if p == Helpers.getProjectId project then
                                MenuItem.Active

                            else
                                MenuItem.Inactive

                        _ ->
                            MenuItem.Inactive
            in
            MenuItem.menuItem status projectTitle (Just (ProjectMsg (Helpers.getProjectId project) (SetProjectView ListProjectServers)))

        projectMenuItems : List Project -> List (Element.Element Msg)
        projectMenuItems projects =
            List.map projectMenuItem projects

        addProjectMenuItem =
            let
                active =
                    case model.viewState of
                        NonProjectView Login ->
                            MenuItem.Active

                        _ ->
                            MenuItem.Inactive
            in
            MenuItem.menuItem active "Add Project" (Just (SetNonProjectView Login))
    in
    Element.column
        [ Background.color <| Color.toElementColor <| Framework.Color.black_ter
        , Font.color (Element.rgb255 209 209 209)
        , Element.width (Element.px navMenuWidth)
        , Element.height Element.shrink
        , Element.scrollbarY
        , Element.height Element.fill
        ]
        (projectMenuItems model.projects
            ++ [ addProjectMenuItem ]
        )


navBarView : Model -> Element.Element Msg
navBarView model =
    let
        navBarContainerAttributes =
            [ Background.color (Element.rgb255 29 29 29)
            , Element.width Element.fill
            , Element.height (Element.px navBarHeight)
            ]

        -- TODO: Responsiveness - Depending on how wide the screen is, return Element.column for navBarContainerElement.
        -- https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element#responsiveness
        navBarContainerElement =
            Element.row

        navBarBrand =
            Element.row
                [ Element.padding 10
                , Element.spacing 20
                ]
                [ Element.el
                    [ Region.heading 1
                    , Font.bold
                    , Font.size 26
                    , Font.color (Element.rgb 1 1 1)
                    ]
                    (Element.text "exosphere")
                , Element.image [ Element.height (Element.px 40) ] { src = "https://exosphere.gitlab.io/exosphere/assets/img/logo-alt.svg", description = "" }
                ]

        navBarRight =
            Element.row
                [ Element.alignRight, Element.paddingXY 20 0 ]
                [ Element.el
                    [ Font.color (Element.rgb255 209 209 209)
                    ]
                    (Element.text "")
                , Element.el
                    [ Font.color (Element.rgb255 209 209 209)
                    ]
                    (Input.button
                        []
                        { onPress = Just (SetNonProjectView MessageLog)
                        , label =
                            Element.row
                                exoRowAttributes
                                [ Icon.bell Framework.Color.white 20
                                , Element.text "Messages"
                                ]
                        }
                    )

                -- This is where the right-hand side menu would go
                ]

        navBarHeaderView =
            Element.row
                [ Element.padding 10
                , Element.spacing 10
                , Element.height (Element.px navBarHeight)
                , Element.width Element.fill
                ]
                [ navBarBrand
                , navBarRight
                ]
    in
    navBarContainerElement
        navBarContainerAttributes
        [ navBarHeaderView ]


projectView : Model -> Project -> ProjectViewConstructor -> Element.Element Msg
projectView model project viewConstructor =
    let
        v =
            case viewConstructor of
                ListImages ->
                    viewImagesIfLoaded model.globalDefaults project model.imageFilterTag

                ListProjectServers ->
                    viewServers project

                ServerDetail serverUuid viewStateParams ->
                    viewServerDetail project serverUuid viewStateParams

                CreateServer createServerRequest ->
                    viewCreateServer project createServerRequest
    in
    Element.column
        (Element.width Element.fill
            :: exoColumnAttributes
        )
        [ viewProjectNav project
        , v
        ]



{- Sub-views for most/all pages -}


viewProjectNav : Project -> Element.Element Msg
viewProjectNav project =
    Element.column [ Element.width Element.fill, Element.spacing 10 ]
        [ Element.el
            heading2
          <|
            Element.text <|
                Helpers.hostnameFromUrl project.creds.authUrl
                    ++ " - "
                    ++ project.creds.projectName
        , Element.row [ Element.width Element.fill, Element.spacing 10 ]
            [ Element.el
                []
                (Button.button
                    []
                    (Just <|
                        ProjectMsg (Helpers.getProjectId project) <|
                            SetProjectView ListProjectServers
                    )
                    "My Servers"
                )
            , Element.el []
                (Button.button
                    []
                    (Just <| ProjectMsg (Helpers.getProjectId project) <| SetProjectView ListImages)
                    "Create Server"
                )
            , Element.el
                [ Element.alignRight ]
                (Button.button [ Modifier.Muted ] (Just <| ProjectMsg (Helpers.getProjectId project) RemoveProject) "Remove Project")
            ]
        ]



{- Resource-specific views -}


viewLogin : Model -> Element.Element Msg
viewLogin model =
    Element.column exoColumnAttributes
        [ Element.el
            heading2
            (Element.text "Add an OpenStack Account")
        , Element.wrappedRow
            exoRowAttributes
            [ viewLoginCredsEntry model
            , viewLoginOpenRcEntry model
            ]
        , Element.el (exoPaddingSpacingAttributes ++ [ Element.alignRight ])
            (Button.button
                [ Modifier.Primary ]
                (Just RequestNewProjectToken)
                "Log In"
            )
        ]


viewMessageLog : Model -> Element.Element Msg
viewMessageLog model =
    Element.column
        exoColumnAttributes
        [ Element.el
            heading2
            (Element.text "Messages")
        , if List.isEmpty model.messages then
            Element.text "(No Messages)"

          else
            Element.column exoColumnAttributes (List.map renderMessage model.messages)
        ]


viewLoginCredsEntry : Model -> Element.Element Msg
viewLoginCredsEntry model =
    Element.column
        (exoColumnAttributes
            ++ [ Element.width (Element.px 500)
               , Element.alignTop
               ]
        )
        [ Element.el [] (Element.text "Either enter your credentials...")
        , Input.text
            [ Element.spacing 12
            ]
            { text = model.creds.authUrl
            , placeholder = Just (Input.placeholder [] (Element.text "OS_AUTH_URL e.g. https://mycloud.net:5000/v3"))
            , onChange = \u -> InputLoginField (AuthUrl u)
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "Keystone auth URL")
            }
        , Input.text
            [ Element.spacing 12
            ]
            { text = model.creds.projectDomain
            , placeholder = Just (Input.placeholder [] (Element.text "OS_PROJECT_DOMAIN_ID e.g. default"))
            , onChange = \d -> InputLoginField (ProjectDomain d)
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "Project Domain (name or ID)")
            }
        , Input.text
            [ Element.spacing 12
            ]
            { text = model.creds.projectName
            , placeholder = Just (Input.placeholder [] (Element.text "Project name e.g. demo"))
            , onChange = \pn -> InputLoginField (ProjectName pn)
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "Project Name")
            }
        , Input.text
            [ Element.spacing 12
            ]
            { text = model.creds.userDomain
            , placeholder = Just (Input.placeholder [] (Element.text "User domain e.g. default"))
            , onChange = \d -> InputLoginField (UserDomain d)
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "User Domain (name or ID)")
            }
        , Input.text
            [ Element.spacing 12
            ]
            { text = model.creds.username
            , placeholder = Just (Input.placeholder [] (Element.text "User name e.g. demo"))
            , onChange = \u -> InputLoginField (Username u)
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "User Name")
            }
        , Input.currentPassword
            [ Element.spacing 12
            ]
            { text = model.creds.password
            , placeholder = Just (Input.placeholder [] (Element.text "Password"))
            , show = False
            , onChange = \p -> InputLoginField (Password p)
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "Password")
            }
        ]


viewLoginOpenRcEntry : Model -> Element.Element Msg
viewLoginOpenRcEntry model =
    Element.column
        (exoColumnAttributes
            ++ [ Element.spacing 15
               , Element.height (Element.fill |> Element.minimum 250)
               ]
        )
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
            , Element.height Element.fill
            , Font.size 12
            ]
            { onChange = \o -> InputLoginField (OpenRc o)
            , text = "export..."
            , placeholder = Nothing
            , label = Input.labelLeft [] Element.none
            , spellcheck = False
            }
        ]


viewImagesIfLoaded : GlobalDefaults -> Project -> Maybe String -> Element.Element Msg
viewImagesIfLoaded globalDefaults project maybeFilterTag =
    case List.isEmpty project.images of
        True ->
            Element.text "Images loading"

        False ->
            viewImages globalDefaults project maybeFilterTag


viewImages : GlobalDefaults -> Project -> Maybe String -> Element.Element Msg
viewImages globalDefaults project maybeFilterTag =
    let
        imageContainsTag tag image =
            List.member tag image.tags

        filteredImages =
            case maybeFilterTag of
                Nothing ->
                    project.images

                Just filterTag ->
                    List.filter (imageContainsTag filterTag) project.images

        noMatchWarning =
            (maybeFilterTag /= Nothing) && (List.length filteredImages == 0)

        displayedImages =
            if noMatchWarning == False then
                filteredImages

            else
                project.images
    in
    Element.column exoColumnAttributes
        [ Element.el heading2 (Element.text "Choose an image")
        , Input.text []
            { text = Maybe.withDefault "" maybeFilterTag
            , placeholder = Just (Input.placeholder [] (Element.text "try \"distro-base\""))
            , onChange = \t -> InputImageFilterTag t
            , label = Input.labelAbove [ Font.size 14 ] (Element.text "Filter on tag:")
            }
        , Button.button [] (Just <| InputImageFilterTag "") "Clear filter (show all)"
        , if noMatchWarning then
            Element.text "No matches found, showing all images"

          else
            Element.none
        , Element.wrappedRow
            (exoRowAttributes ++ [ Element.spacing 15 ])
            (List.map (renderImage globalDefaults project) displayedImages)
        ]


viewServers : Project -> Element.Element Msg
viewServers project =
    case project.servers of
        RemoteData.NotAsked ->
            Element.paragraph [] [ Element.text "Please wait..." ]

        RemoteData.Loading ->
            Element.paragraph [] [ Element.text "Loading..." ]

        RemoteData.Failure e ->
            Element.paragraph [] [ Element.text ("Cannot display servers. Error message: " ++ Debug.toString e) ]

        RemoteData.Success servers ->
            case List.isEmpty servers of
                True ->
                    Element.paragraph [] [ Element.text "You don't have any servers yet, go create one!" ]

                False ->
                    let
                        noServersSelected =
                            List.any (\s -> s.exoProps.selected) servers |> not

                        allServersSelected =
                            List.all (\s -> s.exoProps.selected) servers

                        selectedServers =
                            List.filter (\s -> s.exoProps.selected) servers

                        deleteButtonOnPress =
                            if noServersSelected == True then
                                Nothing

                            else
                                Just (ProjectMsg (Helpers.getProjectId project) (RequestDeleteServers selectedServers))

                        deleteButtonModifiers =
                            if noServersSelected == True then
                                [ Modifier.Danger, Modifier.Disabled ]

                            else
                                [ Modifier.Danger ]
                    in
                    Element.column exoColumnAttributes
                        [ Element.el heading2 (Element.text "My Servers")
                        , Element.column (exoColumnAttributes ++ [ Element.padding 5, Border.width 1 ])
                            [ Element.text "Bulk Actions"
                            , Input.checkbox []
                                { checked = allServersSelected
                                , onChange = \new -> ProjectMsg (Helpers.getProjectId project) (SelectAllServers new)
                                , icon = Input.defaultCheckbox
                                , label = Input.labelRight [] (Element.text "Select All")
                                }
                            , Button.button deleteButtonModifiers deleteButtonOnPress "Delete"
                            ]
                        , Element.column exoColumnAttributes (List.map (renderServer project) servers)
                        ]


viewServerDetail : Project -> OSTypes.ServerUuid -> ViewStateParams -> Element.Element Msg
viewServerDetail project serverUuid viewStateParams =
    let
        maybeServer =
            Helpers.serverLookup project serverUuid
    in
    case maybeServer of
        Nothing ->
            Element.text "No server found"

        Just server ->
            case server.osProps.details of
                Nothing ->
                    Element.text "Retrieving details??"

                Just details ->
                    let
                        friendlyOpenstackStatus =
                            Debug.toString details.openstackStatus
                                |> String.dropLeft 6

                        friendlyPowerState =
                            Debug.toString details.powerState
                                |> String.dropLeft 5

                        statusView =
                            let
                                graphic =
                                    case details.openstackStatus of
                                        OSTypes.ServerBuilding ->
                                            Spinner.spinner Spinner.ThreeCircles 30 Framework.Color.yellow

                                        _ ->
                                            case server.exoProps.targetOpenstackStatus of
                                                Just _ ->
                                                    Spinner.spinner Spinner.ThreeCircles 30 Framework.Color.grey_darker

                                                Nothing ->
                                                    Icon.roundRect (server |> Helpers.getServerUiStatus |> Helpers.getServerUiStatusColor) 32
                            in
                            Element.column
                                (exoColumnAttributes ++ [ Element.padding 0 ])
                                ([ Element.row [ Font.bold ]
                                    [ Element.el [ Element.paddingEach { edges | right = 15 } ] graphic
                                    , Element.text (server |> Helpers.getServerUiStatus |> Helpers.getServerUiStatusStr)
                                    ]
                                 ]
                                    ++ verboseStatusView
                                )

                        verboseStatusView =
                            case viewStateParams.verboseStatus of
                                False ->
                                    [ Button.button
                                        []
                                        (Just <|
                                            ProjectMsg (Helpers.getProjectId project) <|
                                                SetProjectView <|
                                                    ServerDetail
                                                        server.osProps.uuid
                                                        { viewStateParams | verboseStatus = True }
                                        )
                                        "See detail"
                                    ]

                                True ->
                                    [ Element.text "Detailed status"
                                    , compactKVSubRow "OpenStack status" (Element.text friendlyOpenstackStatus)
                                    , compactKVSubRow "Power state" (Element.text friendlyPowerState)
                                    , compactKVSubRow "Server Dashboard/Terminal readiness" (Element.text (friendlyCockpitReadiness server.exoProps.cockpitStatus))
                                    ]

                        maybeFlavor =
                            Helpers.flavorLookup project details.flavorUuid

                        flavorText =
                            case maybeFlavor of
                                Just flavor ->
                                    flavor.name

                                Nothing ->
                                    "Unknown flavor"

                        maybeImage =
                            Helpers.imageLookup project details.imageUuid

                        imageText =
                            case maybeImage of
                                Just image ->
                                    image.name

                                Nothing ->
                                    "Unknown image"

                        consoleLink =
                            case details.openstackStatus of
                                OSTypes.ServerActive ->
                                    case server.osProps.consoleUrl of
                                        RemoteData.NotAsked ->
                                            Element.text "Console not available yet"

                                        RemoteData.Loading ->
                                            Element.text "Requesting console link..."

                                        RemoteData.Failure error ->
                                            let
                                                genericError =
                                                    Element.column exoColumnAttributes
                                                        [ Element.text "Console not available. The following error was returned when Exosphere asked for a console:"
                                                        , Element.paragraph [] [ Element.text (Debug.toString error) ]
                                                        ]
                                            in
                                            case error of
                                                Http.BadStatus innerError ->
                                                    if innerError.body == "{\"badRequest\": {\"message\": \"Unavailable console type spice-html5.\", \"code\": 400}}" then
                                                        Element.paragraph []
                                                            [ Element.text "Console unavailable due to cloud configuration."
                                                            , Element.text " Try asking the administrator of "
                                                            , Element.text project.creds.projectName
                                                            , Element.text " to enable the SPICE+HTML5 or NoVNC console."
                                                            ]

                                                    else
                                                        genericError

                                                _ ->
                                                    genericError

                                        RemoteData.Success consoleUrl ->
                                            let
                                                flippyCardContents : PasswordVisibility -> String -> Element.Element Msg
                                                flippyCardContents pwVizOnClick text =
                                                    Element.el
                                                        [ Events.onClick
                                                            (ProjectMsg (Helpers.getProjectId project) <|
                                                                SetProjectView <|
                                                                    ServerDetail serverUuid
                                                                        { viewStateParams | passwordVisibility = pwVizOnClick }
                                                            )
                                                        , Element.centerX
                                                        , Element.centerY
                                                        ]
                                                        (Element.text text)

                                                passwordFlippyCard password =
                                                    Card.flipping
                                                        { width = 250
                                                        , height = 30
                                                        , activeFront =
                                                            case viewStateParams.passwordVisibility of
                                                                PasswordShown ->
                                                                    False

                                                                PasswordHidden ->
                                                                    True
                                                        , front = flippyCardContents PasswordShown "(click to view password)"
                                                        , back = flippyCardContents PasswordHidden password
                                                        }

                                                passwordHint =
                                                    case Helpers.getServerExouserPassword details of
                                                        Just password ->
                                                            Element.column
                                                                [ Element.spacing 10
                                                                ]
                                                                [ Element.text "Try logging in with username \"exouser\" and the following password:"
                                                                , passwordFlippyCard password
                                                                ]

                                                        Nothing ->
                                                            Element.none
                                            in
                                            Element.column
                                                exoColumnAttributes
                                                [ Button.button
                                                    []
                                                    (Just <|
                                                        OpenNewWindow consoleUrl
                                                    )
                                                    "Console"
                                                , Element.paragraph []
                                                    [ Element.text "Launching the console is like connecting a screen, mouse, and keyboard to your server. If your server has a desktop environment then you can interact with it here."
                                                    , passwordHint
                                                    ]
                                                ]

                                OSTypes.ServerBuilding ->
                                    Element.text "Server building, console not available yet."

                                _ ->
                                    Element.text "Console not available with server in this state."

                        maybeFloatingIp =
                            Helpers.getServerFloatingIp details.ipAddresses

                        cockpitInteractionLinks =
                            case maybeFloatingIp of
                                Just floatingIp ->
                                    let
                                        interactionLinksBase =
                                            [ Element.row exoRowAttributes
                                                [ Button.button
                                                    []
                                                    (Just <|
                                                        OpenNewWindow <|
                                                            "https://"
                                                                ++ floatingIp
                                                                ++ ":9090/cockpit/@localhost/system/terminal.html"
                                                    )
                                                    "Type commands in a shell!"
                                                , Element.text "Type commands in a shell!"
                                                ]
                                            , Element.row
                                                exoRowAttributes
                                                [ Button.button
                                                    []
                                                    (Just <|
                                                        OpenNewWindow <|
                                                            "https://"
                                                                ++ floatingIp
                                                                ++ ":9090"
                                                    )
                                                    "Server Dashboard"
                                                , Element.text "Manage your server with an interactive dashboard!"
                                                ]
                                            ]
                                    in
                                    case server.exoProps.cockpitStatus of
                                        NotChecked ->
                                            Element.text "Status of server dashboard and terminal not available yet."

                                        CheckedNotReady ->
                                            Element.text "Server Dashboard and Terminal not ready yet."

                                        Ready ->
                                            Element.column exoColumnAttributes
                                                ([ Element.text "Server Dashboard and Terminal are ready..." ]
                                                    ++ interactionLinksBase
                                                )

                                Nothing ->
                                    Element.text "Server Dashboard and Terminal not ready yet."

                        actionButtons =
                            let
                                allowedActions =
                                    ServerActions.getAllowed details.openstackStatus

                                renderActionButton action =
                                    Element.row
                                        [ Element.spacing 10 ]
                                        [ Element.el
                                            [ Element.width <| Element.px 100 ]
                                          <|
                                            Button.button
                                                action.selectMods
                                                (Just <| ProjectMsg (Helpers.getProjectId project) <| RequestServerAction server action.action action.targetStatus)
                                                action.name
                                        , Element.text action.description
                                        ]

                                -- TODO hover text with description
                            in
                            Element.column
                                [ Element.spacingXY 0 10 ]
                            <|
                                List.map renderActionButton allowedActions

                        viewActions =
                            case server.exoProps.targetOpenstackStatus of
                                Nothing ->
                                    actionButtons

                                Just targetStatus ->
                                    Element.el
                                        [ Element.padding 10 ]
                                        Element.none

                        resourceUsageGraphs =
                            case maybeFloatingIp of
                                Just floatingIp ->
                                    case server.exoProps.cockpitStatus of
                                        Ready ->
                                            let
                                                graphsUrl =
                                                    "https://" ++ floatingIp ++ ":9090/cockpit/@localhost/system/index.html"
                                            in
                                            -- I am so sorry
                                            Element.html
                                                (Html.div
                                                    [ Html.Attributes.style "position" "relative"
                                                    , Html.Attributes.style "overflow" "hidden"
                                                    , Html.Attributes.style "width" "550px"
                                                    , Html.Attributes.style "height" "650px"
                                                    ]
                                                    [ Html.iframe
                                                        [ Html.Attributes.style "position" "absolute"
                                                        , Html.Attributes.style "top" "-320px"
                                                        , Html.Attributes.style "left" "-30px"
                                                        , Html.Attributes.style "width" "600px"
                                                        , Html.Attributes.style "height" "1000px"

                                                        -- https://stackoverflow.com/questions/15494568/html-iframe-disable-scroll
                                                        -- This is not compliant HTML5 but still works
                                                        , Html.Attributes.attribute "scrolling" "no"
                                                        , Html.Attributes.src graphsUrl
                                                        ]
                                                        []
                                                    ]
                                                )

                                        _ ->
                                            Element.text "Graphs not ready yet."

                                Nothing ->
                                    Element.text "Graphs not ready yet."
                    in
                    Element.wrappedRow []
                        [ Element.column
                            (Element.alignTop
                                :: Element.width (Element.px 585)
                                :: exoColumnAttributes
                            )
                            [ Element.el
                                heading2
                                (Element.text "Server Details")
                            , compactKVRow "Name" (Element.text server.osProps.name)
                            , compactKVRow "Status" statusView
                            , compactKVRow "UUID" (Element.text server.osProps.uuid)
                            , compactKVRow "Created on" (Element.text details.created)
                            , compactKVRow "Image" (Element.text imageText)
                            , compactKVRow "Flavor" (Element.text flavorText)
                            , compactKVRow "SSH Key Name" (Element.text (Maybe.withDefault "(none)" details.keypairName))
                            , compactKVRow "IP addresses"
                                (renderIpAddresses
                                    details.ipAddresses
                                    project
                                    server.osProps.uuid
                                    viewStateParams
                                )
                            , Element.el heading3 (Element.text "Interact with server")
                            , consoleLink
                            , cockpitInteractionLinks
                            ]
                        , Element.column (Element.alignTop :: Element.width (Element.px 585) :: exoColumnAttributes)
                            [ Element.el heading3 (Element.text "Server Actions")
                            , viewActions
                            , Element.el heading3 (Element.text "System Resource Usage")
                            , resourceUsageGraphs
                            ]
                        ]


hint : String -> Element.Attribute msg
hint hintText =
    Element.below
        (Element.el
            [ Font.color (Element.rgb 1 0 0)
            , Font.size 14
            , Element.alignRight
            , Element.moveDown 6
            ]
            (Element.text hintText)
        )


viewCreateServer : Project -> CreateServerRequest -> Element.Element Msg
viewCreateServer project createServerRequest =
    let
        serverNameEmptyHint =
            if createServerRequest.name == "" then
                [ hint "Server name can't be empty" ]

            else
                []

        requestIsValid =
            if createServerRequest.name == "" then
                False

            else if createServerRequest.flavorUuid == "" then
                False

            else
                True

        createOnPress =
            if requestIsValid == True then
                Just (ProjectMsg (Helpers.getProjectId project) (RequestCreateServer createServerRequest))

            else
                Nothing
    in
    Element.row exoRowAttributes
        [ Element.column
            (exoColumnAttributes
                ++ [ Element.width (Element.px 600) ]
            )
            [ Element.el heading2 (Element.text "Create Server")
            , Input.text
                ([ Element.spacing 12
                 ]
                    ++ serverNameEmptyHint
                )
                { text = createServerRequest.name
                , placeholder = Just (Input.placeholder [] (Element.text "My Server"))
                , onChange = \n -> InputCreateServerField createServerRequest (CreateServerName n)
                , label = Input.labelLeft [] (Element.text "Name")
                }
            , Element.row exoRowAttributes [ Element.text "Image: ", Element.text createServerRequest.imageName ]
            , Element.row exoRowAttributes
                [ Element.el [ Element.width Element.shrink ] (Element.text createServerRequest.count)
                , Input.slider
                    [ Element.height (Element.px 30)
                    , Element.width (Element.px 100 |> Element.minimum 200)

                    -- Here is where we're creating/styling the "track"
                    , Element.behindContent
                        (Element.el
                            [ Element.width Element.fill
                            , Element.height (Element.px 2)
                            , Element.centerY
                            , Background.color (Element.rgb 0.5 0.5 0.5)
                            , Border.rounded 2
                            ]
                            Element.none
                        )
                    ]
                    { onChange = \c -> InputCreateServerField createServerRequest (CreateServerCount (String.fromFloat c))
                    , label = Input.labelLeft [] (Element.text "How many?")
                    , min = 1
                    , max = 10
                    , step = Just 1
                    , value = String.toFloat createServerRequest.count |> Maybe.withDefault 1.0
                    , thumb =
                        Input.defaultThumb
                    }
                ]
            , viewFlavorPicker project createServerRequest
            , viewVolBackedPrompt project createServerRequest
            , viewNetworkPicker project createServerRequest
            , viewKeypairPicker project createServerRequest
            , viewUserDataInput project createServerRequest
            , Element.el [ Element.alignRight ] <|
                Button.button
                    [ Modifier.Primary ]
                    createOnPress
                    "Create"
            ]
        ]



{- View Helpers -}


renderMessage : String -> Element.Element Msg
renderMessage message =
    Element.paragraph [] [ Element.text message ]


renderImage : GlobalDefaults -> Project -> OSTypes.Image -> Element.Element Msg
renderImage globalDefaults project image =
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
    ExoCard.exoCard
        image.name
        size
    <|
        Element.column exoColumnAttributes
            [ Element.row exoRowAttributes
                [ Element.text "Status: "
                , Element.text (Debug.toString image.status)
                ]
            , Element.row exoRowAttributes
                [ Element.text "Tags: "
                , Element.paragraph [] [ Element.text (List.foldl (\a b -> a ++ ", " ++ b) "" image.tags) ]
                ]
            , Element.el
                [ Element.alignRight ]
                (Button.button
                    [ Modifier.Primary ]
                    (Just
                        (ProjectMsg
                            (Helpers.getProjectId project)
                            (SetProjectView
                                (CreateServer
                                    (CreateServerRequest
                                        image.name
                                        (Helpers.getProjectId project)
                                        image.uuid
                                        image.name
                                        "1"
                                        ""
                                        False
                                        ""
                                        Nothing
                                        globalDefaults.shellUserData
                                        "changeme123"
                                        ""
                                        False
                                    )
                                )
                            )
                        )
                    )
                    "Choose"
                )
            ]


renderServer : Project -> Server -> Element.Element Msg
renderServer project server =
    let
        statusIcon =
            Element.el [ Element.paddingEach { edges | right = 15 } ] (Icon.roundRect (server |> Helpers.getServerUiStatus |> Helpers.getServerUiStatusColor) 16)

        checkBoxLabel : Server -> Element.Element Msg
        checkBoxLabel aServer =
            Element.row []
                [ statusIcon
                , Element.el [ Font.bold ] (Element.text server.osProps.name)
                ]
    in
    Element.row (exoRowAttributes ++ [ Element.width Element.fill ])
        [ Input.checkbox []
            { checked = server.exoProps.selected
            , onChange = \new -> ProjectMsg (Helpers.getProjectId project) (SelectServer server new)
            , icon = Input.defaultCheckbox
            , label = Input.labelRight [] (checkBoxLabel server)
            }
        , Element.text (server |> Helpers.getServerUiStatus |> Helpers.getServerUiStatusStr)
        , Button.button
            []
            (Just <|
                ProjectMsg (Helpers.getProjectId project) <|
                    SetProjectView <|
                        ServerDetail
                            server.osProps.uuid
                            { verboseStatus = False
                            , passwordVisibility = PasswordHidden
                            , ipInfoLevel = IPSummary
                            }
            )
            "Details"
        , if server.exoProps.deletionAttempted == True then
            Element.text "Deleting..."

          else
            IconButton.iconButton [ Modifier.Danger, Modifier.Small ] (Just (ProjectMsg (Helpers.getProjectId project) (RequestDeleteServer server))) (Icon.remove Framework.Color.white 16)
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


renderIpAddresses : List OSTypes.IpAddress -> Project -> OSTypes.ServerUuid -> ViewStateParams -> Element.Element Msg
renderIpAddresses ipAddresses project serverUuid viewStateParams =
    let
        ipAddressesOfType : OSTypes.IpAddressType -> List OSTypes.IpAddress
        ipAddressesOfType ipAddressType =
            ipAddresses
                |> List.filter
                    (\ipAddress ->
                        ipAddress.openstackType == ipAddressType
                    )

        fixedIpAddressRows =
            ipAddressesOfType OSTypes.IpAddressFixed
                |> List.map
                    (\ipAddress ->
                        compactKVSubRow "Fixed IP" (Element.text ipAddress.address)
                    )

        floatingIpAddressRows =
            ipAddressesOfType OSTypes.IpAddressFloating
                |> List.map
                    (\ipAddress ->
                        compactKVSubRow "Floating IP" (Element.text ipAddress.address)
                    )

        gray : Element.Color
        gray =
            Element.rgb255 219 219 219

        ipButton : String -> String -> IPInfoLevel -> Element.Element Msg
        ipButton displayButtonString displayLabel ipMsg =
            Element.row
                [ Element.spacing 3 ]
                [ Input.button
                    [ Font.size 10
                    , Border.width 1
                    , Border.rounded 20
                    , Border.color gray
                    , Element.padding 3
                    ]
                    { onPress =
                        Just <|
                            ProjectMsg (Helpers.getProjectId project) <|
                                SetProjectView <|
                                    ServerDetail
                                        serverUuid
                                        { viewStateParams | ipInfoLevel = ipMsg }
                    , label = Element.text displayButtonString
                    }
                , Element.el [ Font.size 10 ] (Element.text displayLabel)
                ]
    in
    case viewStateParams.ipInfoLevel of
        IPDetails ->
            Element.column
                (exoColumnAttributes ++ [ Element.padding 0 ])
                (floatingIpAddressRows
                    ++ fixedIpAddressRows
                    ++ [ ipButton "^" "IP summary" IPSummary ]
                )

        IPSummary ->
            Element.column
                (exoColumnAttributes ++ [ Element.padding 0 ])
                (floatingIpAddressRows ++ [ ipButton ">" "IP details" IPDetails ])


viewFlavorPicker : Project -> CreateServerRequest -> Element.Element Msg
viewFlavorPicker project createServerRequest =
    let
        -- This is a kludge. Input.radio is intended to display a group of multiple radio buttons,
        -- but we want to embed a button in each table row, so we define several Input.radios,
        -- each containing just a single option.
        -- https://elmlang.slack.com/archives/C4F9NBLR1/p1539909855000100
        radioButton flavor =
            Input.radio
                []
                { label = Input.labelHidden flavor.name
                , onChange = \f -> InputCreateServerField createServerRequest (CreateServerSize f)
                , options = [ Input.option flavor.uuid (Element.text " ") ]
                , selected =
                    case flavor.uuid == createServerRequest.flavorUuid of
                        True ->
                            Just flavor.uuid

                        False ->
                            Nothing
                }

        paddingRight =
            Element.paddingEach { edges | right = 15 }

        headerAttribs =
            [ paddingRight
            , Font.bold
            , Font.center
            ]

        columns =
            [ { header = Element.none
              , width = Element.fill
              , view = \r -> radioButton r
              }
            , { header = Element.el (headerAttribs ++ [ Font.alignLeft ]) (Element.text "Name")
              , width = Element.fill
              , view = \r -> Element.el [ paddingRight ] (Element.text r.name)
              }
            , { header = Element.el headerAttribs (Element.text "CPUs")
              , width = Element.fill
              , view = \r -> Element.el [ paddingRight, Font.alignRight ] (Element.text (String.fromInt r.vcpu))
              }
            , { header = Element.el headerAttribs (Element.text "RAM (GB)")
              , width = Element.fill
              , view = \r -> Element.el [ paddingRight, Font.alignRight ] (Element.text (r.ram_mb // 1024 |> String.fromInt))
              }
            , { header = Element.el headerAttribs (Element.text "Root Disk")
              , width = Element.fill
              , view =
                    \r ->
                        Element.el
                            [ paddingRight, Font.alignRight ]
                            (if r.disk_root == 0 then
                                Element.text "- *"

                             else
                                Element.text (String.fromInt r.disk_root ++ " GB")
                            )
              }
            , { header = Element.el headerAttribs (Element.text "Ephemeral Disk")
              , width = Element.fill
              , view =
                    \r ->
                        Element.el
                            [ paddingRight, Font.alignRight ]
                            (if r.disk_ephemeral == 0 then
                                Element.text "none"

                             else
                                Element.text (String.fromInt r.disk_ephemeral ++ " GB")
                            )
              }
            ]

        zeroRootDiskExplainText =
            case List.filter (\f -> f.disk_root == 0) project.flavors |> List.head of
                Just _ ->
                    "* No default root disk size is defined for this server size, see below"

                Nothing ->
                    ""

        flavorEmptyHint =
            if createServerRequest.flavorUuid == "" then
                [ hint "Please pick a size" ]

            else
                []
    in
    Element.column
        exoColumnAttributes
        [ Element.el [ Font.bold ] (Element.text "Size")
        , Element.table
            flavorEmptyHint
            { data = Helpers.sortedFlavors project.flavors
            , columns = columns
            }
        , Element.paragraph [ Font.size 12 ] [ Element.text zeroRootDiskExplainText ]
        ]


viewVolBackedPrompt : Project -> CreateServerRequest -> Element.Element Msg
viewVolBackedPrompt project createServerRequest =
    let
        maybeFlavor =
            List.filter (\f -> f.uuid == createServerRequest.flavorUuid) project.flavors
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
                String.fromInt flavorRootDiskSize ++ " GB (default for selected size)"

        volSizeSlider =
            --                    Element.el [ Element.width Element.shrink ] (Element.text createServerRequest.volBackedSizeGb)
            Input.slider
                [ Element.height (Element.px 30)
                , Element.width (Element.px 100 |> Element.minimum 200)

                -- Here is where we're creating/styling the "track"
                , Element.behindContent
                    (Element.el
                        [ Element.width Element.fill
                        , Element.height (Element.px 2)
                        , Element.centerY
                        , Background.color (Element.rgb 0.5 0.5 0.5)
                        , Border.rounded 2
                        ]
                        Element.none
                    )
                ]
                { onChange = \c -> InputCreateServerField createServerRequest (CreateServerVolBackedSize (String.fromFloat c))
                , label = Input.labelRight [] (Element.text (createServerRequest.volBackedSizeGb ++ " GB"))
                , min = 2
                , max = 100
                , step = Just 1
                , value = String.toFloat createServerRequest.volBackedSizeGb |> Maybe.withDefault 2.0
                , thumb =
                    Input.defaultThumb
                }
    in
    Element.column exoColumnAttributes
        [ Input.radio []
            { label = Input.labelAbove [ Element.paddingXY 0 12 ] (Element.text "Choose a root disk size")
            , onChange = \new -> InputCreateServerField createServerRequest (CreateServerVolBacked new)
            , options =
                [ Input.option False (Element.text nonVolBackedOptionText)
                , Input.option True (Element.text "Custom disk size (volume-backed)")

                {- -}
                ]
            , selected = Just createServerRequest.volBacked
            }
        , case createServerRequest.volBacked of
            False ->
                Element.none

            True ->
                volSizeSlider
        ]


viewNetworkPicker : Project -> CreateServerRequest -> Element.Element Msg
viewNetworkPicker project createServerRequest =
    let
        networkOptions =
            Helpers.newServerNetworkOptions project

        contents =
            case networkOptions of
                NoNetsAutoAllocate ->
                    [ Element.paragraph
                        []
                        [ Element.text "There are no networks associated with your project so Exosphere will ask OpenStack to create one for you and hope for the best." ]
                    ]

                OneNet net ->
                    [ Element.paragraph
                        []
                        [ Element.text ("There is only one network, with name \"" ++ net.name ++ "\", so Exosphere will use that one.") ]
                    ]

                MultipleNetsWithGuess networks guessNet goodGuess ->
                    let
                        guessText =
                            case goodGuess of
                                True ->
                                    Element.paragraph
                                        []
                                        [ Element.text
                                            ("The network \"" ++ guessNet.name ++ "\" is probably a good guess so Exosphere has picked it by default.")
                                        ]

                                False ->
                                    Element.paragraph
                                        []
                                        [ Element.text "The selected network is a guess and might not be the best choice." ]

                        networkAsInputOption network =
                            Input.option network.uuid (Element.text network.name)

                        networkEmptyHint =
                            if createServerRequest.networkUuid == "" then
                                [ hint "Please pick a network" ]

                            else
                                []
                    in
                    [ Input.radio networkEmptyHint
                        { label = Input.labelAbove [ Element.paddingXY 0 12 ] (Element.text "Choose a Network")
                        , onChange = \networkUuid -> InputCreateServerField createServerRequest (CreateServerNetworkUuid networkUuid)
                        , options = List.map networkAsInputOption project.networks
                        , selected = Just createServerRequest.networkUuid
                        }
                    , guessText
                    ]
    in
    Element.column
        exoColumnAttributes
        ([ Element.el [ Font.bold ] (Element.text "Network") ]
            ++ contents
        )


viewKeypairPicker : Project -> CreateServerRequest -> Element.Element Msg
viewKeypairPicker project createServerRequest =
    let
        keypairAsOption keypair =
            Input.option keypair.name (Element.text keypair.name)

        contents =
            case project.keypairs of
                [] ->
                    Element.text "(This OpenStack project has no keypairs to choose from, but you can still create a server!)"

                keypairs ->
                    Input.radio []
                        { label = Input.labelAbove [ Element.paddingXY 0 12 ] (Element.text "Choose a keypair (this is optional, skip if unsure)")
                        , onChange = \keypairName -> InputCreateServerField createServerRequest (CreateServerKeypairName keypairName)
                        , options = List.map keypairAsOption project.keypairs
                        , selected = Just (Maybe.withDefault "" createServerRequest.keypairName)
                        }
    in
    Element.column
        exoColumnAttributes
        [ Element.el [ Font.bold ] (Element.text "SSH Keypair")
        , contents
        ]


viewUserDataInput : Project -> CreateServerRequest -> Element.Element Msg
viewUserDataInput project createServerRequest =
    Element.column
        exoColumnAttributes
        [ Input.radioRow [ Element.spacing 10 ]
            { label = Input.labelAbove [ Element.paddingXY 0 12, Font.bold ] (Element.text "Advanced Options")
            , onChange = \new -> InputCreateServerField createServerRequest (CreateServerShowAdvancedOptions new)
            , options =
                [ Input.option False (Element.text "Hide")
                , Input.option True (Element.text "Show")

                {- -}
                ]
            , selected = Just createServerRequest.showAdvancedOptions
            }
        , case createServerRequest.showAdvancedOptions of
            False ->
                Element.none

            True ->
                Input.multiline
                    [ Element.width (Element.px 600)
                    , Element.height (Element.px 500)
                    ]
                    { onChange = \u -> InputCreateServerField createServerRequest (CreateServerUserData u)
                    , text = createServerRequest.userData
                    , placeholder = Just (Input.placeholder [] (Element.text "#!/bin/bash\n\n# Your script here"))
                    , label =
                        Input.labelAbove
                            [ Element.paddingXY 20 0
                            , Font.bold
                            ]
                            (Element.text "User Data (Boot Script)")
                    , spellcheck = False
                    }
        ]


friendlyCockpitReadiness : CockpitLoginStatus -> String
friendlyCockpitReadiness cockpitLoginStatus =
    case cockpitLoginStatus of
        NotChecked ->
            "Not checked yet"

        CheckedNotReady ->
            "Checked but not ready yet (May become ready soon)"

        Ready ->
            "Ready"



{- Elm UI Doodads -}


exoRowAttributes : List (Element.Attribute Msg)
exoRowAttributes =
    exoElementAttributes


exoColumnAttributes : List (Element.Attribute Msg)
exoColumnAttributes =
    exoElementAttributes


exoElementAttributes : List (Element.Attribute Msg)
exoElementAttributes =
    exoPaddingSpacingAttributes


exoPaddingSpacingAttributes : List (Element.Attribute Msg)
exoPaddingSpacingAttributes =
    [ Element.padding 10
    , Element.spacing 10
    ]


heading2 : List (Element.Attribute Msg)
heading2 =
    [ Region.heading 2
    , Font.bold
    , Font.size 24
    ]


heading3 : List (Element.Attribute Msg)
heading3 =
    [ Region.heading 3
    , Font.bold
    , Font.size 20
    ]


compactKVRow : String -> Element.Element Msg -> Element.Element Msg
compactKVRow key value =
    Element.row
        (exoRowAttributes ++ [ Element.padding 0, Element.spacing 10 ])
        [ Element.paragraph [ Element.alignTop, Element.width (Element.px 200), Font.bold ] [ Element.text key ]
        , Element.el [] value
        ]


compactKVSubRow : String -> Element.Element Msg -> Element.Element Msg
compactKVSubRow key value =
    Element.row
        (exoRowAttributes ++ [ Element.padding 0, Element.spacing 10, Font.size 14 ])
        [ Element.paragraph [ Element.width (Element.px 200), Font.bold ] [ Element.text key ]
        , Element.el [] value
        ]


edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }
